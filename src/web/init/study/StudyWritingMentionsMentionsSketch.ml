let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         ea065707-c8c5-4f18-aadf-fbe2c12b004e)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         cdad0ada-0722-429e-b7df-03b4edb47242)(content(Whitespace\"\\n\"))))(Secondary((id \
         aae12743-20fb-4320-bd7b-8d750e43d7ee)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         bfb3c4a2-5b47-4002-ade7-4253a16d047e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7474608-764d-489f-8671-2d85b373c723)(content(Comment\"# Extract \
         @mentions from a garden message.      #\"))))(Secondary((id \
         db8d3fbd-62d7-42db-b50c-3003b592c651)(content(Whitespace\"\\n\"))))(Secondary((id \
         78c2ab3a-5769-4e3e-bdb2-b599a2708ab1)(content(Comment\"# Given \
         \\\"Hey @luna the moonblooms are opening\\\", #\"))))(Secondary((id \
         cb9eed20-c8b3-48be-a07c-10701aa5abe8)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc87885b-8958-4e41-b8bb-652b1c7e0e30)(content(Comment\"# return \
         [\\\"luna\\\"].                              #\"))))(Secondary((id \
         15b707a0-eccf-46a1-8f39-7f4d956d9c13)(content(Whitespace\"\\n\"))))(Secondary((id \
         a45915e9-1576-4070-ade6-d719a018c474)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         5edb22cb-da1e-4d7a-92f5-95da9ce96555)(content(Whitespace\"\\n\"))))(Secondary((id \
         596d9598-0ed1-4b09-be2d-3b3818d4d324)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         64987996-a7ad-4cd6-b66a-7f1ce80b232d)(content(Whitespace\"\\n\"))))(Secondary((id \
         26f55677-dc32-4cc3-b8bd-7b49c2d2fdde)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         ceb7606d-ce8d-415c-a5b5-018ee60ec959)(content(Whitespace\"\\n\"))))(Secondary((id \
         d365d853-0b29-4a44-9ff4-ff57f595eee9)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         3bb63d03-f3eb-4ca3-90ba-f3af1f825793)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf25e1e9-5c8e-40bc-a6ab-73485ab9a30a)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         617615d2-7e61-4c76-b3fe-d9aa0e904263)(content(Whitespace\"\\n\"))))(Secondary((id \
         4975e327-ed4d-49e9-a82b-2c4e69cfb894)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         6525093c-0c61-4c3d-bfbc-ed7dbe8b3e76)(content(Whitespace\"\\n\"))))(Secondary((id \
         78aed068-ecdd-45bd-a501-08799f36a752)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         c8f2e9c9-a240-45d3-82f9-a5626b45cc72)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1b89f83-c787-4b77-8e1e-bcbc915d3919)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         c8284cc3-14d8-46df-8490-66d9f60ad3ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e9320e0-523a-4bc3-b511-2217f5b135ae)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         52a4c780-5c45-4fce-bdf8-8de03db1db55)(content(Whitespace\"\\n\"))))(Secondary((id \
         254e1b39-3e09-46a0-9aa8-696a941b19eb)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         dcc67ab9-f1fd-4b78-b7df-d2ccfcb3332e)(content(Whitespace\"\\n\"))))(Secondary((id \
         00cc55bc-bc4c-4538-9b53-d44de8a3c1b7)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         5c552015-1cca-45cf-940b-379a1f6143a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3ccda40-9ec0-42d7-88c1-0e33ed2a0bc8)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         25635c87-e18a-4af6-ad2d-e10ea9591aab)(content(Whitespace\"\\n\"))))(Secondary((id \
         e33e43c6-18c4-4b69-9c71-5ef1e9d060ea)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         93f87d5c-7115-410b-afad-9132afaa22ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         1189d3db-157c-473b-9060-251c54bbabc7)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         7b19534b-3b1f-4c02-9f88-0b5f7112b773)(content(Whitespace\"\\n\"))))(Secondary((id \
         b45b92b3-f2c8-4810-922f-2637b059a5ab)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         8aaa44af-a8a6-4562-b76f-ea09c8d7ac41)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff559e54-c1e0-4af8-bb78-8f87d4395256)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         8947b265-b6c5-44e5-952d-b55b659b81c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f2d7584-78ea-4797-a269-a41eabbaa8b3)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         ceed62eb-7d51-457b-b3f4-324d2680fd43)(content(Whitespace\"\\n\"))))(Secondary((id \
         41b31a70-3372-4e8b-8237-3232de6cb426)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         719be15b-2d90-424b-96f3-50d56c638048)(content(Whitespace\"\\n\"))))(Secondary((id \
         da3ee440-1d1e-4b2e-a533-fe817789917e)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         52bb8a33-3b12-4985-b839-ad020090db84)(content(Whitespace\"\\n\"))))(Secondary((id \
         d826ecaa-9ccf-4550-9f3b-0728139016cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac95b34a-a817-4493-a639-e4a246d8d111)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         1481ff8f-331d-4f30-9031-86e5a66570ae)(content(Whitespace\"\\n\"))))(Tile((id \
         a0339c9f-797a-4d9a-bcd4-3a5c206d6c8d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ed5f109c-0b00-41ad-a22f-1e11b44132d3)(content(Whitespace\" \
         \"))))(Tile((id \
         0ef1cbb0-a353-4710-a1cc-05c1971330b5)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ce14d7b4-33ee-4928-9b79-f52cf3c3d7f3)(content(Whitespace\" \
         \")))))((Secondary((id \
         1c1c42aa-3254-4a23-b1c1-eec206035533)(content(Whitespace\" \
         \"))))(Tile((id 2911d57e-eae7-4382-8c6e-0098b08a7f4d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         627c20b8-d3b7-4875-931d-03c751b6a81c)(content(Whitespace\" \
         \"))))(Tile((id \
         b6b804de-b585-44b7-8479-f5106a64b920)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ba63203c-7caf-4489-9c95-1b7e8d6f3eb8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ebca2bbd-f0ba-4b57-9d82-9066125dad81)(content(Whitespace\"\\n\"))))(Tile((id \
         99d8f08e-9513-4b4f-884b-ef387ebed709)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25bf1e9a-1085-4ead-a26b-eceee83ecb6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         40e3a3dc-9612-401e-ae22-6995319500ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         0dd87d3f-02e8-4e0e-a180-b92f92338d2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0100fa21-26b3-4925-a599-495e79db7c7e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         02f5f687-2593-4437-b7fb-3ff7a3bcf928)(content(Whitespace\"\\n\"))))(Secondary((id \
         95d217c7-a273-4b22-af48-fedfad156310)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f525f47-7e72-4037-8ed7-80c44d7365e3)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         5821fbd3-dfdc-4474-a625-9de7b3edcbce)(content(Whitespace\"\\n\"))))(Tile((id \
         73a98277-adc0-4b82-b1d1-944ed9797470)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7935a0b6-9731-4b97-b45f-0351836683ee)(content(Whitespace\" \
         \"))))(Tile((id \
         1c70ce06-b3dd-40b2-99a2-719800a9d228)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73efba61-ac92-4888-b935-04b4a4f90512)(content(Whitespace\" \
         \")))))((Secondary((id \
         9c234883-618c-4029-90e7-2a943f2f5d3f)(content(Whitespace\" \
         \"))))(Tile((id 68e7b86f-e7b9-4e15-9b0b-f96d91aa4136)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9a12423c-f30c-4bce-8d93-2bb3f9f71c8b)(content(Whitespace\" \
         \"))))(Tile((id \
         a1d10bcb-f3be-4921-b3cb-0c3e86d380b7)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8a8e28d7-7e13-4f1f-bf4e-d8f6346a4649)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0587c16f-b46c-4fdd-8f60-d70b93fa244a)(content(Whitespace\"\\n\"))))(Tile((id \
         b2411175-fc78-4491-88b6-467da8b9a7b7)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         396f4ea7-b839-4b0a-be5d-57ddbcc85228)(content(Whitespace\"\\n\"))))(Secondary((id \
         0546a9ff-1a12-4d09-ae30-3739efb91967)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ca7ea7e-e453-46d4-84b7-a710040101dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         04151678-c52f-4c31-9ae9-24cf1ac49cec)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         13a96e49-e2ba-48cc-aa64-d9f63d55b9fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0ceb724-6256-445c-a04f-76a00e6e6d2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1bd820bd-4372-4bad-808a-7c90c5e789cc)(content(Comment\"# Main \
         function: extract mentions from message #\"))))(Secondary((id \
         bb0ecb77-4abf-447e-87b4-ba1c4a66fbf7)(content(Whitespace\"\\n\"))))(Tile((id \
         47034f41-966c-4e6c-9979-b54001aecd9c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8bbaec71-33ba-4464-8a55-6a80fb84886d)(content(Whitespace\" \
         \"))))(Tile((id \
         aafbda6b-3c22-45b6-851e-ec240239c92a)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4f83ec68-1622-4034-9f27-0799ee0b95b4)(content(Whitespace\" \
         \")))))((Secondary((id \
         c64e4357-a045-4c5f-9ca8-0135cff9758d)(content(Whitespace\" \
         \"))))(Tile((id d9386eaa-7a52-4e99-bf52-d1276cb7ec0b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bdf46ce2-0896-47f0-9773-8b866c613939)(content(Whitespace\" \
         \"))))(Tile((id \
         27b1fce6-bd57-4b59-9aa0-83e6bbbb1e98)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         193a5653-e38e-4a38-be57-80e367d07c2f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         592cf293-ec0e-4a1b-8766-71799d12df76)(content(Whitespace\"\\n\"))))(Tile((id \
         b9fac333-b531-4060-8a1b-a4edbccba49f)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32462b38-c8b8-47c8-a141-070d7e16c6de)(content(Whitespace\"\\n\"))))(Secondary((id \
         675997b6-0c2f-4578-8b6f-55c19f14685a)(content(Whitespace\"\\n\"))))(Secondary((id \
         488c0da7-bd16-4dfe-bd31-579b65e2777b)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b678987-ac45-416e-b9aa-1dabe84fdf89)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5dcfa43c-1766-47ac-b4ce-11b9c5e11d7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7244413d-37d6-4e78-ad93-8b3a5e261f28)(content(Whitespace\"\\n\"))))(Tile((id \
         89ec8238-05b5-4fdc-973e-f7589db30c9e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6f36cf6f-4fc9-4a1f-a552-b544af582d5c)(content(Whitespace\"\\n\"))))(Tile((id \
         173993f1-c8a7-41e1-918c-b4d23843c4f4)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75c30e08-0021-46ff-870a-b1b3ae839b74)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d51e8f8-49c3-48f3-bb5a-20b9804a1998)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63ad2bd8-77e9-4f26-a6ac-66dba1f6ab04)(content(Whitespace\"\\n\"))))(Tile((id \
         ded60611-ef93-4d00-98e8-b6c42c6def4e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fdd191f-3b35-4bb9-aa51-a055738576ab)(content(Whitespace\" \
         \"))))(Tile((id 7ecc8dca-1a04-40c0-8c62-1e61505383d4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         45272128-298a-4fad-8511-ae3b16c79ad8)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d133a6d2-92fe-42f6-83a3-7ad1e89d245e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7765d878-a77f-47a4-8982-27d64db368bc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79986012-97a2-495f-ab8a-b12e61c818d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b593757-2a98-482c-8f7f-61d8b8e2c0f2)(content(Whitespace\"\\n\"))))(Tile((id \
         8f1d6229-e394-4ca3-8162-77734476a870)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ea60acaf-bb00-4f46-8ff2-687890708a95)(content(Whitespace\"\\n\"))))(Tile((id \
         83bad715-b874-461e-8d75-8778ba87a963)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecc0334c-ff99-43ac-8110-35511d955c61)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b917138e-6b70-4816-9cda-091526861984)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         26084a82-b7cf-485b-8fab-17c5cb6e4937)(content(Whitespace\"\\n\"))))(Tile((id \
         1616f0a5-b448-497b-b8c0-357f9d51be92)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2cbd0b23-ef21-431a-84fc-977f3fe43b03)(content(Whitespace\" \
         \"))))(Tile((id ededa7a5-36b0-4fe9-98ea-7a22a7912d64)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88507099-2a41-49c8-bfc5-2e8ec18260e4)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84e60cda-0c65-452d-af70-7966c93716bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e04f0668-a13c-45df-88a1-c875e203635d)(content(Whitespace\" \
         \"))))(Tile((id \
         dc831e18-35ce-4945-86b9-39c1931fd346)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fc5a0f6a-16a8-45c2-9e09-5864fefe668a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cd5afc19-da22-49da-803f-8862b846d66e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec5cbb3d-5413-492c-8172-393d71d66e8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         44bf7a12-a6aa-4e5e-91dd-075dd9779458)(content(Whitespace\"\\n\"))))(Tile((id \
         cb675aaf-d3d2-4645-b4d1-0eb0eee9a2c3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ac74a4b-f3f6-4914-babc-4a2cd4e03467)(content(Whitespace\"\\n\"))))(Tile((id \
         ff7b6893-32d8-47a6-ab2c-7c52964358a2)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4728686-1343-4fb3-b623-64c08e0b3e28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         859d225c-6167-4784-9067-74ae14f23ac6)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         00d93acf-d172-4aa2-a1e5-445e5d98603c)(content(Whitespace\"\\n\"))))(Tile((id \
         943769a5-4454-498d-9238-23c6027c399e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd79c145-c3ef-4436-86d9-33ae0ae39806)(content(Whitespace\" \
         \"))))(Tile((id \
         2034e393-2a00-4177-81a0-4d09c27ed1d1)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29ad676e-41ed-4935-97cc-40e0253b4462)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f09f83ab-e4a8-4cba-91f4-44d3b406d8b5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ecf78a2-349c-4fad-bd2f-13d490ce4f3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         4913665b-88d8-4762-b014-9d428ea3ffd1)(content(Whitespace\"\\n\"))))(Tile((id \
         c0c30d78-94dc-47ee-981e-550546e38e83)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ec5a9534-44cc-4743-891d-834702a22a99)(content(Whitespace\"\\n\"))))(Tile((id \
         b6cc72bb-79e2-40ad-9c51-5a2d2e232d4c)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a74897d-2e5b-449f-89bd-0f6a24d168af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         92db244a-215a-4373-aa9f-866d5af7ff5b)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7b55bb73-7126-4baf-a543-ea4581a63af7)(content(Whitespace\"\\n\"))))(Tile((id \
         be7a2f5a-8782-40ec-8435-2b0cef122f4e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a998ec4-70f2-46e1-a568-d4b20fd28d52)(content(Whitespace\" \
         \"))))(Tile((id 3e62d8b9-1de5-4891-976d-4a6fe3a4de12)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c471bf5d-dbba-45fb-a6bb-b34bb1a6cbcd)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         47cdf627-6685-40c7-b491-e36f13b2dba6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1317b58d-f059-4952-b2ea-be8acb0cb641)(content(Whitespace\"\\n\")))))";
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
