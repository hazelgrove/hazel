let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         bedfe46f-85ad-4d5f-b66f-6125030e301e)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         fa52e791-1239-4759-af64-9dd9475e2be5)(content(Whitespace\"\\n\"))))(Secondary((id \
         bed84a1e-2395-46b8-aaaf-1604645fab92)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         2ced6b8e-5d1b-46c8-8f56-26d827854cb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         292ec711-2462-4e63-98e7-99863dabeb7d)(content(Comment\"# Extract \
         @mentions from a garden message.      #\"))))(Secondary((id \
         d346f545-5e12-4292-9a31-8e352b5cf8e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         aee89188-3032-4e55-a302-a875238a2dc8)(content(Comment\"# Given \
         \\\"Hey @luna the moonblooms are opening\\\", #\"))))(Secondary((id \
         77559a9d-06d4-45c8-8007-441a64319e2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4979cd53-aef9-4f51-8a6b-7e2fb4e31cfe)(content(Comment\"# return \
         [\\\"luna\\\"].                              #\"))))(Secondary((id \
         88673cee-1f5b-4aae-97cf-47ceeb8568d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffa44439-3029-4a8f-b438-096638e68c2e)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         c536d324-7613-4342-a8a6-daac0500c152)(content(Whitespace\"\\n\"))))(Secondary((id \
         72bc9e43-89d4-4278-943f-cd223ef12068)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         bfcc1067-1a17-4455-bbe8-478c13c6aa35)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e50a9d3-b0d9-4b78-b8d7-f8a640be6d94)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         32db40f8-b979-47f5-bfdb-a28e7a42d4ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         041c9b31-95b6-4e91-9d6d-9ea8433d8e66)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         ab22e1fe-7785-4f85-ae8e-20978920691c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f868f74-1e50-405d-9f20-cbb1bf52f4ad)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         c04e6677-5981-4927-b3b3-2f82b8682066)(content(Whitespace\"\\n\"))))(Secondary((id \
         68b14222-4809-429e-8e7d-96c92f91a256)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8837c05b-19a4-4b3f-83f7-4eee0980e26f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2de1d1f-9a23-4624-9201-1480fafca68f)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         2bbfc0ed-3476-4b3f-983c-2752696ca59c)(content(Whitespace\"\\n\"))))(Secondary((id \
         b30eb671-e025-4599-96e6-5a05921db38a)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         ed6fbb84-5722-4bee-a5bf-e509082bc669)(content(Whitespace\"\\n\"))))(Secondary((id \
         08e702f5-b26f-407f-b7a8-e950df576bc1)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         0c7b748f-cd93-45e0-9566-ce3b5d17b36a)(content(Whitespace\"\\n\"))))(Secondary((id \
         11c7ca74-a6d6-45b4-a23e-8b00d71b6900)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         702ba6d8-5678-453b-8f7e-c6aed4584457)(content(Whitespace\"\\n\"))))(Secondary((id \
         25fb4569-fc76-4fec-a65f-b2cf7c86d4d6)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         fcdb0bfc-da67-406e-ac3c-cceb196c48a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         901a8bab-af48-4e9d-9e0d-fe955994c27c)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         47002b37-c2d1-4425-a67c-07bd93d2e020)(content(Whitespace\"\\n\"))))(Secondary((id \
         76989599-407a-47c9-90d6-c0bcebb83c3c)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         dfec2525-90fb-442f-b01a-47daa8b2d908)(content(Whitespace\"\\n\"))))(Secondary((id \
         dcc380a8-5e8d-404d-938d-b3720cdc8b69)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         df21565b-a054-48f8-bf27-1794008810e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a8dc811-fa03-45ac-bedd-07e5f956fc0b)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         3fe1dd4e-4d2e-499c-b721-43b0ec4d8fa4)(content(Whitespace\"\\n\"))))(Secondary((id \
         57c160ae-9496-44d1-8838-d8bc0c13e59c)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         c2eca6fa-b0cc-4610-91b4-be70e386cf13)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e784829-b051-402e-93e3-b213d638eddc)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         faa7fd9c-d42d-40c1-9632-4147a521df63)(content(Whitespace\"\\n\"))))(Secondary((id \
         200b3bc8-941a-4d83-96bd-b5ff8055de91)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         db281288-521e-4bc2-b09f-3601ab280bb3)(content(Whitespace\"\\n\"))))(Secondary((id \
         50ee3539-7114-408b-96fb-7dcb944ca314)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         b52aaa39-35c0-4e32-857a-b0bec01f7ca1)(content(Whitespace\"\\n\"))))(Secondary((id \
         59324e58-f658-4f85-8771-b34734dd16a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b8d516a-59a9-496c-8b98-ed908946974b)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         0ddb1262-5cb1-4f05-a435-7fe18fceb13c)(content(Whitespace\"\\n\"))))(Tile((id \
         2d0238c8-35cb-4802-bdcb-292dd50ae5ef)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7c18422c-9df3-4887-85cd-f6975b918222)(content(Whitespace\" \
         \"))))(Tile((id \
         236dc0c4-3814-4773-9c00-8e453c311dd7)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         027dcb2e-ea55-4cf3-b1a1-c35556f93b0f)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8844ee7-50d5-4ab5-ab08-f4391677bd93)(content(Whitespace\" \
         \"))))(Tile((id c826f7d6-2e12-4eba-be64-d7babc6d1e5c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         60bd64cc-9004-4f60-b5b4-9fefbfea0ca0)(content(Whitespace\" \
         \"))))(Tile((id \
         02197f3e-c733-419d-b3c3-16d3cb82de1e)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d5348b5-6388-4750-9c39-2b5e035322e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5f850794-b8d8-4353-b8fd-d9efacc5c9c9)(content(Whitespace\"\\n\"))))(Tile((id \
         adc7fa07-c7f3-4461-b530-df556d0d42c5)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         380cf7e0-4de3-4a7c-b9a5-84630484c4da)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1f501d7-adfb-403e-a785-dbc16477516f)(content(Whitespace\"\\n\"))))(Secondary((id \
         655abed6-6ce1-4901-bb08-743b59264f54)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff8b6bd1-dc7b-4734-8201-d2d46538e868)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         29a53f73-ed82-45e2-b74c-ab53b02afdef)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b931af7-6478-48c8-ad8f-689b6576f440)(content(Whitespace\"\\n\"))))(Secondary((id \
         43175051-d2ab-44cd-be61-dd7d19ae1858)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         e41f009d-0efb-40d6-ae0b-98c63d4d3123)(content(Whitespace\"\\n\"))))(Tile((id \
         eed89065-8d99-44c8-995f-4889deb31215)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8a969006-f629-4c4e-8cbb-7e20f053074a)(content(Whitespace\" \
         \"))))(Tile((id \
         5d77e7e3-ca1c-4262-833e-6beafa71288e)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7a81fbb1-7054-4298-982b-e231cbbab4ab)(content(Whitespace\" \
         \")))))((Secondary((id \
         dafaebe6-95d9-4577-a4e6-834cb0032a48)(content(Whitespace\" \
         \"))))(Tile((id ce1e88cf-aeac-4a65-bd24-c510bd8cef42)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         935c1f52-8113-4443-b6da-b5b203289fd1)(content(Whitespace\" \
         \"))))(Tile((id \
         5626c29f-8fcd-400f-acab-8e8c7b469441)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5a000040-1f72-406d-a5a6-7e8bc0d17bf5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8eaee2c0-925d-4625-baad-03f80f8d1b27)(content(Whitespace\"\\n\"))))(Tile((id \
         169b83a8-3445-401d-b817-035eeaf2fc77)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab45a293-9db0-4d2b-93a9-422281062ad7)(content(Whitespace\"\\n\"))))(Secondary((id \
         54e25eff-d2bb-4c28-949f-a469a2ef7879)(content(Whitespace\"\\n\"))))(Secondary((id \
         f57e87df-d0bc-4c25-a96e-5fb81a83d63d)(content(Whitespace\"\\n\"))))(Secondary((id \
         64144b96-89d1-4305-ad59-2380e43ed782)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9a292b2d-1715-4879-9a5b-230e6e913d98)(content(Whitespace\"\\n\"))))(Secondary((id \
         f04e9f34-6c8d-4cfd-bc1a-bc042c9467e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         b72cd143-d4ea-4f81-8749-781f61273a8b)(content(Comment\"# Main \
         function: extract mentions from message #\"))))(Secondary((id \
         646ee813-92f5-47b0-a8ae-0dbbe3a341d4)(content(Whitespace\"\\n\"))))(Tile((id \
         8dc5593e-3e00-4d26-a2d1-169b58b895b5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1d72f855-5cc4-4ed8-9b18-47167f6a3bb6)(content(Whitespace\" \
         \"))))(Tile((id \
         cc8a2166-b3d2-44bc-b365-78a839e5686d)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4bdeb49e-b293-4544-b7b4-a302cca31945)(content(Whitespace\" \
         \")))))((Secondary((id \
         5a561c02-2a4b-45af-948f-9995c5f9bca8)(content(Whitespace\" \
         \"))))(Tile((id 01e9031a-695a-40b0-a629-b398aec89e6e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         08031452-14e4-4f61-b310-8255109270ff)(content(Whitespace\" \
         \"))))(Tile((id \
         7d75911a-9ffc-436b-b054-14ff8b8477d5)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ca67d453-a88c-47a5-8ddf-4e7c8ed5fe09)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9f1656c-7b66-4637-bb83-b5513661f3d1)(content(Whitespace\"\\n\"))))(Tile((id \
         d3c45737-996e-45d0-89a5-3302c9e9ce11)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73539e44-bd0d-4771-85ba-eabd552fee70)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e426af0-1e06-4d59-944e-88e0155f8c9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a4283d2-772b-4a74-9d50-c381370ebcd1)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c3db0d1-778f-4a0d-8945-1be9bde99b8a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f71bfe26-862d-4a87-9255-35148eca01bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbbf513c-9ce2-43a9-8ed6-c96908d93ec8)(content(Whitespace\"\\n\"))))(Tile((id \
         2575fd0b-f071-44bc-aca8-e72616a5144c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b028fef5-7b17-4c1c-8ebe-f5383f658814)(content(Whitespace\"\\n\"))))(Tile((id \
         0a3a0b89-ec46-4cc1-8d08-cea7570777a4)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4a3fd04-1f39-4e13-8ea8-373c17db4b24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7fed6dc-f2db-46e7-93ea-866f211bf831)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aa376fc4-89d5-4485-a8c9-f25a9225416c)(content(Whitespace\"\\n\"))))(Tile((id \
         cf81fa94-9c5f-44d6-95ce-6f35668a9574)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee94f7cb-10ce-492c-9351-03a18e84d9d5)(content(Whitespace\" \
         \"))))(Tile((id f21e9a04-ee03-4845-b6f4-b837ccd0f010)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bdc2a25c-c0b8-4ab6-93fe-c6d9fe217d2c)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         edef67df-e605-4f11-b5b7-1c3bce84fd7f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5a42cf4c-8e76-4115-ada1-b5549d8c179e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         623b301c-2d47-4d36-a751-a8cd971db49b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7982db0e-1751-4268-ba80-28b460e460c2)(content(Whitespace\"\\n\"))))(Tile((id \
         c1e463df-9066-473a-bd51-17fee0f367c3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3b715469-f121-4130-aea4-3ae719fef32a)(content(Whitespace\"\\n\"))))(Tile((id \
         e9358d9c-4d5a-41d1-8200-333fd7ee1d64)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e015f76-70c1-4b5b-a8ea-d996126320db)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fdbbf74b-6735-4b2a-8458-288d8f6f5df3)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df763d24-0095-42d0-a5d7-0b06565aa27c)(content(Whitespace\"\\n\"))))(Tile((id \
         48f7d46a-245b-496d-a008-ada6d9b57527)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         439c4522-0536-40bf-bbb6-e1872bf71022)(content(Whitespace\" \
         \"))))(Tile((id ec8163dd-6f0c-4edf-aae1-3276aca25103)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ae47d46-10b3-4e6a-88cf-5087eafc985d)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8006a78-c6e2-4da4-9db9-a212b09ba2c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec325b36-6222-4357-9ea7-5ef9a1d35f2f)(content(Whitespace\" \
         \"))))(Tile((id \
         784da89e-d935-42af-8ea0-222a1bb70f41)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ce8f96ff-4716-481e-a7bd-bb94972da5e3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         67b833dc-2bb3-4c3f-b5ea-3db8e1e46977)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fe26265-00a9-4188-81c0-699874f996a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d99496f-7966-4846-95d9-b5577a679b8d)(content(Whitespace\"\\n\"))))(Tile((id \
         3728ed0a-073c-46c8-9dfb-1496fde46734)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8bc46a87-2d59-4960-88b4-1d56a1142b82)(content(Whitespace\"\\n\"))))(Tile((id \
         751cf2d7-464c-4a60-92da-cf9e5b839f87)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2888907-4d79-4392-8d3c-ffc8454e5e45)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         862ba48b-17d4-4330-b660-6387b79b7cd7)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         14df4c85-ee76-436d-b156-03574cb25799)(content(Whitespace\"\\n\"))))(Tile((id \
         3317e3d7-499d-47a0-a94c-8ac810d6d81f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         443c203f-4be9-4a15-8ea2-de1365e72a66)(content(Whitespace\" \
         \"))))(Tile((id \
         9f44e185-b140-4157-b5af-ab252135d1b3)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         181c08ac-13de-4b14-b0d5-93e88bac8570)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4ba81188-a0d5-4658-8f2e-83cbe435224e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64f76c11-67c7-4ba6-9bfe-cd0dc5afe5e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfd0cc99-8d10-46ea-902b-9b9cb90a6af6)(content(Whitespace\"\\n\"))))(Tile((id \
         dc8a950e-2e91-43e8-addd-9a01a0e6ebaf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ff2a8fb8-7a65-4ccb-8604-abcb1cbeaaad)(content(Whitespace\"\\n\"))))(Tile((id \
         4e8f2dea-aa9e-498e-b074-7626b552854c)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54f97920-3dab-41d5-a18f-ba81f1eb040a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d006ff5-5ea0-443e-8994-ec38adf50db7)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         24d0eb6b-0949-47d1-88e9-c976c928154e)(content(Whitespace\"\\n\"))))(Tile((id \
         ac9d9ff7-88b6-4f1e-bd6c-c88801a34a7d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22cd15ca-4dc8-4fc3-a6f1-aff9c5227ddb)(content(Whitespace\" \
         \"))))(Tile((id 22ba25bd-ca9b-47b1-b06a-0a26160d13c0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9cfa13ce-97bf-4f3a-8d06-6e56856efb9c)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         825e350d-6bad-4703-a5ae-f3e854c5604e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         95115552-3f5d-4c54-9b63-893e2ac5a14c)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR TASK                        #\n\
         #                                               #\n\
         # Extract @mentions from a garden message.      #\n\
         # Given \"Hey @luna the moonblooms are opening\", #\n\
         # return [\"luna\"].                              #\n\
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
         ?\n\n\n\n\
         in\n\n\
         # Helper: remove the @ prefix from a word #\n\
         let strip_at = fun word ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Main function: extract mentions from message #\n\
         let extract_mentions = fun message ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @luna the moonblooms are opening\")\n\
         == [\"luna\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@thorn @moss check the greenhouse\")\n\
         == [\"thorn\", \"moss\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"the night air is still\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@fern\")\n\
         == [\"fern\"]\n\
         end\n";
      refractors = "()";
    } )
