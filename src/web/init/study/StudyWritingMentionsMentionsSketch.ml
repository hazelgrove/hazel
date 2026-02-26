let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         9a51c0ae-e7c4-41b0-a06b-4948890545ec)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         72038425-505e-47ad-9933-8674ab9d695b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e69330a9-dd9f-46eb-a2e1-85def0269c8a)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         22289e22-88ac-425e-b1a8-d75f3b195d2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d9a8273-c4f6-4b4c-a578-4e349fddd6d2)(content(Comment\"# Extract \
         @mentions from a message string.      #\"))))(Secondary((id \
         a9f591a8-0837-4f96-b115-d4cb5d0e3bf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         db811a23-440f-40fa-ae1a-bf245114d077)(content(Comment\"# Given \
         \\\"Hey @alice and @bob\\\", return           #\"))))(Secondary((id \
         5501c4a0-e4ae-41ec-a1b4-a15ea400d7a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4a8d342-20f4-4bcf-bb44-5d9abb01f279)(content(Comment\"# \
         [\\\"alice\\\", \\\"bob\\\"].                             \
         #\"))))(Secondary((id \
         6fb77941-a63d-4c1c-802b-12ef1afe0f76)(content(Whitespace\"\\n\"))))(Secondary((id \
         055e3e7d-96b6-4136-ad07-2994399b4fbb)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         9b2d26a6-e229-4345-ada5-5bc8b9c68188)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a294aa9-ffaa-4dd3-842b-2727efe5a789)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         a5fc92a2-084f-4728-8a2e-4d71584f5dca)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2b9d059-d79a-4672-b24e-cd7898b4dd74)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         8c178434-c384-4898-87ef-6e1bc867b4af)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb2fd8da-1589-4ca9-bf3c-4535dffb25bd)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         1a88f2a8-a9d2-4bb0-ac12-949dfd2594e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c4467f0-50e9-4c83-b768-4a6c9e79be76)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         6213594e-1b8e-46bc-9d04-682fc0822942)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7525bf6-90ac-4732-b49d-621cebfb00da)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         6e1e2b59-d07c-47ba-bdc1-17979ced6e45)(content(Whitespace\"\\n\"))))(Secondary((id \
         393732b2-01af-4ef3-84cf-d483fab8f0c1)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         2f92565c-a049-49cd-8518-50933199c17c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf784725-402b-4f67-b4de-41e1c400944b)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         074cad81-4897-4cd3-997d-e08e73890326)(content(Whitespace\"\\n\"))))(Secondary((id \
         5365a592-287c-4a26-8514-b247e19b9130)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         f5a3db2e-28c0-41e2-bb61-01ecc69ecca4)(content(Whitespace\"\\n\"))))(Secondary((id \
         453c6afb-84f9-4b4d-a685-1ac79d593629)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         091b7887-0d13-4932-a888-1a0a6e47e9e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1938865d-38d3-4f8e-b0b6-6547a9ae22f1)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         b9851b1d-8a27-4030-80f1-b170189fefc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         29f3735d-e25a-47f5-acd0-e1cf11013269)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         07f66175-a1de-43d2-907d-df98617a389b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f302023-e8fb-483f-a314-0a18e3ee73c4)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         6dfe6955-2a5e-49da-9d5d-57b84c3135bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c0a120a-a681-4347-9de6-3441d65bd6c5)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         ea327817-9696-4ee3-9e8c-dd602a4f287e)(content(Whitespace\"\\n\"))))(Secondary((id \
         331b45e9-c0d6-484b-af65-87076d088faf)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         98d75dd8-248b-4c92-9e49-1442dc4bc2ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         52f515bb-5da6-4149-a174-0faaa5eaa9c8)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         2371326e-aa0b-44c6-903e-355cc079500f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6edd0dd-2347-4f77-ba62-af514904243e)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8bdcda3b-c607-44d5-80b7-c247196e8fae)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6aeb043-c5ec-4db3-94f9-8d96d26d1987)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         57816939-5f9a-4c85-8115-279289380ba9)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d593f8b-5b64-4b8c-b7b8-7ef6552417e3)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         02fced19-1183-49db-a494-037d63fba056)(content(Whitespace\"\\n\"))))(Secondary((id \
         7487613d-0f09-4711-801e-ebb0143d4232)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9bdda24-d5b2-4227-89ce-9f20982460e0)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         f7322259-f314-43ce-9a34-b29ddabf8640)(content(Whitespace\"\\n\"))))(Tile((id \
         c5a8eba4-2273-465f-8b22-2cc3b1591179)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ed1e81fc-cb24-4d47-a03f-d4c9bff4b3d4)(content(Whitespace\" \
         \"))))(Tile((id \
         71855b8d-023e-4c1d-b23e-f8e81dbd615d)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c03f265-c9d2-4137-b4c3-334d4e371072)(content(Whitespace\" \
         \")))))((Secondary((id \
         0ffe355a-2e3e-4137-be68-e66313c7267f)(content(Whitespace\" \
         \"))))(Tile((id 4e124a9f-1f32-43bf-afd7-d4968b105906)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         fc6d1d0f-c081-43e7-8c49-1ad82a10ce68)(content(Whitespace\" \
         \"))))(Tile((id \
         c0747572-c9c9-41ca-bcc5-c2f19519f5fc)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aebd2d53-59d4-4e07-a4a7-5f5febb82049)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         60778453-b882-44ef-a9d0-39972dd1987d)(content(Whitespace\"\\n\"))))(Tile((id \
         c751cfac-4887-454a-b003-0a1cda663f62)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8767016f-dbe3-4366-b460-b82ad6a33283)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f339a59f-dfcf-49a8-9e63-bf3badc35912)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e54659e-5ae2-44c3-afc5-0078e3372560)(content(Whitespace\"\\n\"))))(Secondary((id \
         58e88625-8395-45e6-8e30-145dc6a10220)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         dc3c7478-4748-49ff-a298-f70133fff561)(content(Whitespace\"\\n\"))))(Tile((id \
         388dbe38-4587-4ee9-9088-3d803a631018)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         06475037-a9d0-4c6b-9406-5b728b4d3977)(content(Whitespace\" \
         \"))))(Tile((id \
         76964b9f-a936-4fbb-b10a-604673c8df71)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5ac9f9cc-6037-45fb-b10f-c4e74bb617d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         9e6d074a-178b-46c1-a9c5-663f62ff1de2)(content(Whitespace\" \
         \"))))(Tile((id 5da19eaa-63c3-403d-9bcb-c3bbb3b09ef5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a1d9e333-2e59-4ae0-990d-380e1ea19f19)(content(Whitespace\" \
         \"))))(Tile((id \
         bfc07180-48aa-4410-bbcd-e3b5015d6228)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         26d55d96-f784-4c6b-906c-3693514b143e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33bfeae0-83e7-435f-a652-92edf541cf09)(content(Whitespace\"\\n\"))))(Tile((id \
         1cea9d40-a5a2-4a8f-963d-88c8dd7ea508)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1f7dca9-8379-4e22-997c-8944f3d495cf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a5683881-25d5-41f6-9d70-d4cbf149f6d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         e65dd8e1-da3e-4e9c-94c2-5ccc77a1cef2)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1f925a7-2173-43e9-8a96-79cd707bcf29)(content(Comment\"# Main \
         function: extract usernames from message #\"))))(Secondary((id \
         a0780add-e5e9-4a05-b2de-fbdde0416674)(content(Whitespace\"\\n\"))))(Tile((id \
         4e73b11b-50c0-4858-aa25-68939493ca66)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c0f119ff-e962-47f5-a6a6-d4fa47874c94)(content(Whitespace\" \
         \"))))(Tile((id \
         befb65a5-be66-4f2a-9ed6-d4bd97327026)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         155d9e34-66ec-4d00-beba-5756d2f9d529)(content(Whitespace\" \
         \")))))((Secondary((id \
         3190d186-3e1c-45a0-9743-6453aa47ea41)(content(Whitespace\" \
         \"))))(Tile((id a9565c43-5f98-4e78-95f8-fb83fcbf576f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f0228b6f-c8d1-45d4-acf6-5aadda47733c)(content(Whitespace\" \
         \"))))(Tile((id \
         d3a034d8-2fae-4017-8d6b-a6a211da1e33)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f875e72f-02c8-477e-8447-06dbfe20adfc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0bcb125-4ad3-48ff-bdc3-9b030ffe5fde)(content(Whitespace\"\\n\"))))(Tile((id \
         90029bf2-1d9e-4550-a033-544cd5db10b1)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2de47a3a-c6b8-4d30-9066-4a4a1881d83e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7c2c3c78-f302-4ec7-9e47-f67b6bb1397a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0496cad7-69a4-4dfb-a6ab-cdcbeb7d5aaf)(content(Whitespace\"\\n\"))))(Tile((id \
         04e91e48-a0e7-40eb-9099-0897b60857fe)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a75f2494-ddc9-4e25-8c41-a968940eefa6)(content(Whitespace\"\\n\"))))(Tile((id \
         bcdb951f-06e5-4a97-824d-490a25e925fe)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a1ed0b4-cfa6-44b8-ab56-4d7a6f88b1e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ce6d3dc-c451-44ac-a561-87a1b5ccfdf1)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7e10b394-a0a3-4d1d-8edd-75f8d8d8fc6e)(content(Whitespace\"\\n\"))))(Tile((id \
         ab6193dc-8183-40ea-8297-2939cb5c33e3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7eed570e-695d-453a-872a-c6ccd2414199)(content(Whitespace\" \
         \"))))(Tile((id 3e04f43d-5f01-4fa2-8d76-87707dc56c30)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dc0d4816-8874-46f6-b635-95a21d8cb7ce)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0684dbe0-1f05-484d-bbbb-d6bbf3d6745b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         495b5f56-8969-44bf-99a1-937a39f4d2e1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b200c70-a32a-4246-8a73-72e63b5ba7d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         85be2217-5ff3-47d9-b85f-795cf0e553d4)(content(Whitespace\"\\n\"))))(Tile((id \
         19b622eb-828a-4bd2-931c-9d0d8f144347)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         716e0a6a-7c03-4c7d-8b80-653f84b471d8)(content(Whitespace\"\\n\"))))(Tile((id \
         cc05b1cc-b17c-49d5-8959-f96d70f33b28)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d10db8f-7338-460c-bc5a-a81c208b1ee9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         09c3f27d-de8c-4e3b-9f16-735145e96071)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ddc370d1-444b-4d26-8717-0ef9f08bc5b8)(content(Whitespace\"\\n\"))))(Tile((id \
         268a0941-63d0-46f3-a1a1-3b3cb86452d9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99a761ec-9ddf-4426-9948-92b381abb0cf)(content(Whitespace\" \
         \"))))(Tile((id f77371d3-fa82-4fb8-a895-dff54c080d5d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7a061dde-1609-4214-91b8-d459991cf2a0)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         513cb62b-a279-48db-be7b-3951c39834ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a3f2242-ab58-4212-b62b-9a7a8eaa74be)(content(Whitespace\" \
         \"))))(Tile((id \
         61075dd5-6095-47d8-a51e-d607d3606405)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bdb8e416-1fd2-4992-9724-522f56594249)(content(Whitespace\"\\n\")))))))))(Tile((id \
         984b5d6b-c4ac-4b5e-8e3e-927f8a564dac)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1afc0c9b-c97f-4a23-933f-8952f83c8275)(content(Whitespace\"\\n\"))))(Secondary((id \
         67c33f8c-8ace-4f64-9d8a-79576e3aa6ce)(content(Whitespace\"\\n\"))))(Tile((id \
         90a2bd5e-6df9-4c63-b12d-69acf5c2e492)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cc22250d-e51a-4bc3-8cf4-5cc2069c2ebf)(content(Whitespace\"\\n\"))))(Tile((id \
         e04f0afd-e394-4b6a-89be-e101ec8af0a6)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f04656bb-52a9-499c-b689-bf5fecb28528)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d6c19f2a-31c3-4fa9-94b7-2333e70331ad)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         89a6c5fd-0e66-4b0a-bf8c-178f103f97ef)(content(Whitespace\"\\n\"))))(Tile((id \
         b7977002-9f7a-4eb4-96db-55d308223fd1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a7a6247-e0d5-43ce-aadd-2cc0d47b5852)(content(Whitespace\" \
         \"))))(Tile((id \
         2d0a4a15-3d80-482a-864d-4fca1ff90263)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b01f7f8-e5a6-4c08-b439-aece1caca4a0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cec82213-a519-4caf-910f-632bbf176e7d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f51f7b2-f1c8-4c04-b13f-1b62cdf9d60f)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa58614d-7835-449e-9df5-b1d4879356b3)(content(Whitespace\"\\n\"))))(Tile((id \
         83555c4b-5d44-44a2-9499-968f53a25023)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2a946d28-93a4-414e-856e-886f113522fb)(content(Whitespace\"\\n\"))))(Tile((id \
         5320190f-0e4e-40b2-b57e-7f3b322c012b)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c35693a7-8dbb-4c3b-b285-43ac59fd24f1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         682a1a3d-8e04-4113-864f-57c20e2b3ad6)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a1b75e1-6b45-4ee0-b23b-cb154b584836)(content(Whitespace\"\\n\"))))(Tile((id \
         d6b661d7-ff44-4bd3-a233-8c542c6ad165)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a80d944b-0c5b-4262-8eca-78424ffd68da)(content(Whitespace\" \
         \"))))(Tile((id 4f1c2125-1566-439b-8598-ba2b7b75f9e0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4643fdc-6800-4ae5-88e1-715e501f0950)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f51ba911-5816-4475-87be-7eaaf5551c05)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5df174a2-9112-4fe6-902e-62bd8c001d1c)(content(Whitespace\"\\n\")))))";
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
