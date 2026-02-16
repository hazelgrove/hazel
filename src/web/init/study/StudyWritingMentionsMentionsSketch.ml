let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         f4e10ffe-2571-46f8-9ffa-eb6472954a3c)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         b0d54b81-03c1-4475-9393-53a7e84336b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab0d6075-5a8a-4506-bf51-3632c7bce4b6)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         a6f824dc-39d6-44a4-8254-ee6aeb632a1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         97669f2f-0503-4350-94f9-2a2d00236ec3)(content(Comment\"# Extract \
         @mentions from a message string.      #\"))))(Secondary((id \
         ffcb310d-73fe-41c8-b38a-69859aa4cd3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d89e4984-d653-452e-a627-ae7260f57315)(content(Comment\"# Given \
         \\\"Hey @alice and @bob\\\", return           #\"))))(Secondary((id \
         9b5de297-0adc-4e20-91b0-964aad11bd92)(content(Whitespace\"\\n\"))))(Secondary((id \
         3dc1082c-5359-4f26-b9ab-c24a8f81bf58)(content(Comment\"# \
         [\\\"alice\\\", \\\"bob\\\"].                             \
         #\"))))(Secondary((id \
         6829a7dc-e58b-4c3e-b7dc-3550252d7a37)(content(Whitespace\"\\n\"))))(Secondary((id \
         63e11c11-0e8b-47ff-9cd4-15fdeaec7b9a)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         e8a34d9d-3ca4-48fe-8d40-962bea504161)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fd37c75-b74d-47eb-b5df-e20d9ced7bc5)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         7abf5190-e52d-41cc-a1cc-adf58c3031bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         1baa75a6-0ad6-4883-93cd-7d640c2fe810)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         fbb1fff2-f8b6-47fa-a738-e2b924da3197)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad61857c-6044-4867-b124-46a0b03002a7)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         c460c738-1e28-48dd-ae9b-9b3a7337c939)(content(Whitespace\"\\n\"))))(Secondary((id \
         f22f83b1-03fa-4f9e-ba7e-1c465c3bd95a)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         793b60ca-277d-4759-b06d-0e99046946a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4f7acac-df1f-44f9-9d36-8af4fa7e07ae)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         04ee5fbe-f114-453a-9cb4-c078a2309caa)(content(Whitespace\"\\n\"))))(Secondary((id \
         d034d75e-7172-4984-821f-bc8181ba7717)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         755eb989-ab3e-458b-8655-d495d73f6fc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7cc6d4c-d4be-49e0-a2e2-9d1a6b4bc29c)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         c5fc32b5-607e-4aae-900f-2978b24cf81d)(content(Whitespace\"\\n\"))))(Secondary((id \
         77a564b1-a22a-4a4a-89d1-505ca98c3e59)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         ed98e25e-ea17-4d3d-838c-b1929d792a7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         19cc835b-9dd1-40a8-b152-278fc45480ee)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         c863bf9f-3ab3-491e-b62a-d6e8cac5e763)(content(Whitespace\"\\n\"))))(Secondary((id \
         a934cf86-1c33-4b21-a1ed-630c26b4598a)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         d9acbba6-6b4d-49e5-b882-ea8278cc37fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1dd344f-62a1-4d99-bd90-985b67565f0b)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         e3689cc7-b686-478c-9663-437365a59eba)(content(Whitespace\"\\n\"))))(Secondary((id \
         def833ff-5ce5-4d26-ba1f-1a38f00a0e3a)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         4f2fe76e-07aa-43f7-833f-3a7476e436b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         bfc2e270-6867-45b0-b98c-c517f0509d71)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         bdd8352d-cd18-443f-b755-836015b20eb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         88917792-2e27-42a8-994a-45220b509135)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         293e01ef-42e4-49cf-bc4c-4b9d354e2546)(content(Whitespace\"\\n\"))))(Secondary((id \
         54c4f7c9-4f7c-48ae-b67d-e53e6da15ad0)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         2606107a-e90d-432a-95c2-c4d1954d71b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         82d58ca2-373b-47e4-ab0f-df515e879596)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         b1ed3119-3e45-4e6e-acb3-4f6c39e6e9b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         40fb549a-7525-426a-a706-2fb96c042dc9)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         fbb1dd71-3018-4982-85ba-5dd0a86f1bf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f7188a0-efd3-49ee-80f3-51483b505bb6)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         ed4a0775-fc7d-4de3-b865-349df32fa380)(content(Whitespace\"\\n\"))))(Secondary((id \
         5bd2bb93-3494-4083-b340-f6e537a67238)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1e4db15-c2cb-464f-98c8-8f6b1d159d41)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         43cc0eec-48de-4cea-b779-826abccafd82)(content(Whitespace\"\\n\"))))(Tile((id \
         9a9fae3f-f0a3-4234-b0c9-8ef901582947)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3261d501-3570-46a3-8082-859fff289bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         aaca5ff7-bd63-42d9-8735-ed3545d1eb07)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f2554363-76e1-44c0-a6e3-f6bf1be33502)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4e601b9-496a-45ee-9a73-bda15a41b549)(content(Whitespace\" \
         \"))))(Tile((id 48765e3e-27ad-4a7b-9511-9e22adbd093a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         627f925f-0561-4a14-8846-0dc2cb8c26dc)(content(Whitespace\" \
         \"))))(Tile((id \
         9fe0c132-9be1-458f-9385-6452a5981afc)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c03183a3-57dc-46e6-bae2-a4444f95a4b2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ad4197ad-8f4c-44c3-be86-560de1938db4)(content(Whitespace\"\\n\"))))(Tile((id \
         aa52af01-d87b-45f1-bb14-8dd96752ee71)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1c7ccb0-cf4d-4150-bfa2-3d93673a7f6b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bd6a42a0-75a3-49dd-9239-c8a9fb382995)(content(Whitespace\"\\n\"))))(Secondary((id \
         6266e964-9f47-46d1-83eb-6dd4ee0629f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b55d566-b5e3-4ccc-8e37-9586dd959dcc)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         56eaa1c2-4b20-4b10-b9b2-9ba298959157)(content(Whitespace\"\\n\"))))(Tile((id \
         7b098ff8-1d51-488f-bc3f-5d53c9779709)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         780ec55c-dd13-457c-97fd-f86a076a46d0)(content(Whitespace\" \
         \"))))(Tile((id \
         0ff5e7e7-ee2e-49c6-91ef-62249da1e577)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         79ab06c8-a22f-48ee-bb1a-fef206ddc9c5)(content(Whitespace\" \
         \")))))((Secondary((id \
         64f85119-9e4f-459c-b02e-a400ebe20be9)(content(Whitespace\" \
         \"))))(Tile((id b63309de-ac86-4562-addf-22b7f03e838e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5e588a7b-cdfc-4cf5-9a22-bbfcfd7e333a)(content(Whitespace\" \
         \"))))(Tile((id \
         19edbb15-625d-4673-9ece-e1b8365335b1)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f9acd6ff-a233-42d3-9ebf-be55897dfcf2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bb278954-6b1d-4e22-844e-0fd0fc87ccec)(content(Whitespace\"\\n\"))))(Tile((id \
         8348a546-84df-4a3f-b346-078db0a4740f)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f60e363c-a44c-475e-a1fc-54a40875cf84)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7719db2c-9c34-44b5-96c1-9636d1f58002)(content(Whitespace\"\\n\"))))(Secondary((id \
         8cca9a6a-ed52-490a-88c5-5635771cd129)(content(Whitespace\"\\n\"))))(Secondary((id \
         e934bca3-6c21-4696-8034-1687b5ed01d1)(content(Comment\"# Main \
         function: extract usernames from message #\"))))(Secondary((id \
         775bf8f5-55cd-4952-b436-639aa2149f38)(content(Whitespace\"\\n\"))))(Tile((id \
         4c973136-46e6-4f4f-9cd7-f4d75dee6ce7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48643ab6-92fd-4cd4-b801-88d512953280)(content(Whitespace\" \
         \"))))(Tile((id \
         04494b6e-b140-42f3-bc73-2dfbec064939)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b95a4684-b371-42f5-affa-d7e9b10eb153)(content(Whitespace\" \
         \")))))((Secondary((id \
         91b44eb3-3416-47d5-8748-31b350b5107b)(content(Whitespace\" \
         \"))))(Tile((id bddeb73d-c087-4327-a57a-0500adaf7763)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         29fc6583-2825-49a3-893c-db435f75ee7f)(content(Whitespace\" \
         \"))))(Tile((id \
         35ca7735-cc76-47de-a815-7df793403ecb)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         11f25379-2ecb-46fd-bdf0-9b6d20bcc095)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9bdcb3d6-7e11-4ec4-a06e-8fdd76f9ce4a)(content(Whitespace\"\\n\"))))(Tile((id \
         03b161b6-6d9c-4c51-868f-23e8999bdc75)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1645130c-e130-4e04-9cc6-4576574051d8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3e3f7bdb-3f02-4f21-b202-6bc2236a2212)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b20e5f6-b700-4b0d-a1e2-dbe13a247e1b)(content(Whitespace\"\\n\"))))(Tile((id \
         5543b6d6-d527-4a15-8954-dc69f83b53a9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6190b192-ce83-477f-a719-d32d6947a583)(content(Whitespace\"\\n\"))))(Tile((id \
         e7a97f1b-9c92-4d71-912e-e02376a8ebb2)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         936dc01b-4a3a-4f47-8d19-881a190c8b7c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e9ea9e9-a3c5-4fc8-8f52-7f0e247ad5b8)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d6911832-7f31-4193-9905-9c7ad983fa89)(content(Whitespace\"\\n\"))))(Tile((id \
         5fafa950-4c50-4a2e-a21a-15af3d5e4bc4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a46b88f-e718-4fdb-bfc4-c4be906740a7)(content(Whitespace\" \
         \"))))(Tile((id f0abb5f1-e14d-4fc1-8ba0-baf121666a2a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41d67706-0500-45fe-aae4-d6bfef63c286)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7fe238c4-99a0-44fd-8d77-1908d55f6c5d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         105ff06f-a177-44d4-8ef5-2d20571a1a94)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0266a38a-b4e4-4c6d-968c-2e1d3fc5d900)(content(Whitespace\"\\n\"))))(Secondary((id \
         371a8bda-2058-4260-8ad6-e286a485272c)(content(Whitespace\"\\n\"))))(Tile((id \
         d48a30eb-f3d3-4b85-9cf6-3728a9ea5986)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         385100a9-1f95-40dc-94e9-b2caa32db0b6)(content(Whitespace\"\\n\"))))(Tile((id \
         3a858b43-7535-4db7-9929-a2cc4a7a288b)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a3a6d02-1073-497c-991f-64d6df7dbb16)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         285e3c00-7836-4dff-ae34-e0bde5daf004)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3db23da9-7973-4b02-a795-e226d3d5c08c)(content(Whitespace\"\\n\"))))(Tile((id \
         65db0bec-c818-485b-b7d8-c800b9d81b06)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1379bb0d-5436-4611-97c4-ecc488b29e20)(content(Whitespace\" \
         \"))))(Tile((id 88a3565f-1893-4a4e-814b-2f5575251226)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1da5d1c8-1fe6-4294-a93d-eac995dd88d3)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da0d8dea-7a7d-4cd0-b2b7-c658156462c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf52fe50-0cb6-46cb-87b1-60c110ce8635)(content(Whitespace\" \
         \"))))(Tile((id \
         ec949fdd-29bf-42bf-a2db-ebe0814e8351)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b302dd27-bb80-42e0-ac5f-5bdab570dbff)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6dc161af-4b1d-4860-bc98-3d363c23f230)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e689cc8-1128-469d-9948-f7c73a2a5e8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         66d05822-1f02-4e59-a75a-b322cc163936)(content(Whitespace\"\\n\"))))(Tile((id \
         f462f9bb-8224-40ee-9e24-d0a916f678c8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cb2ecf77-e679-4093-b465-beb7c8d275d2)(content(Whitespace\"\\n\"))))(Tile((id \
         4dc5be19-d8b6-4c6b-8ca7-d80cad6ae404)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c3b518b-fe89-4f17-9631-148211bd0af3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fbbccbe7-2beb-492d-89ed-147397177c8f)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9de5a78e-bb87-4cbe-807b-6122182b060b)(content(Whitespace\"\\n\"))))(Tile((id \
         41d6caef-2b73-434a-bed2-a65c96dc413e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b6d80c8-c3c0-4a91-99b5-d48325d1305f)(content(Whitespace\" \
         \"))))(Tile((id \
         039708e1-40dc-49a9-ad86-d12c54c45476)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ec6c8b7-dddf-43cd-a484-65d50530666b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         acf40cf9-7f12-4af5-9ba1-c2e9c69a454d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6221037f-1e84-4a6b-8928-c8d2b86bb228)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a33c951-b7e7-4a0a-866f-5514e6d8a8ee)(content(Whitespace\"\\n\"))))(Tile((id \
         5d1ddcbb-c7a4-4874-bc68-70f99bb80a37)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7514f084-d6dc-4e7d-b0d0-640a5182918a)(content(Whitespace\"\\n\"))))(Tile((id \
         12f70fe7-1697-4266-82d0-7f3f4bbea4e4)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b4e3612-581c-4e97-aee3-a4ae0b208e58)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cef54f0b-3ce7-4dda-8f89-7bf51283568c)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         abbf612a-cbed-44cd-a1e7-ca4e2ae61ddf)(content(Whitespace\"\\n\"))))(Tile((id \
         e3809a60-c377-44ec-840f-ff94746d84cc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d6dc250-c0fe-44f7-8c41-56cd0ce4ccf7)(content(Whitespace\" \
         \"))))(Tile((id 29021960-266a-4936-bd01-33e168a6d0e3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f6c073b-3b5c-4e92-9c25-9c20c1343bed)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8d6ee25f-9a49-4838-9d6d-aba3d6df5275)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d07cbdaf-9bd2-4cec-baff-0826405a6dab)(content(Whitespace\"\\n\")))))";
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
