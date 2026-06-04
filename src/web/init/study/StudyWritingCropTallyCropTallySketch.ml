let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / crop-tally / crop-tally-sketch",
    {
      segment =
        "((Secondary((id \
         72379489-cbf2-4522-af83-466c4e54bfb5)(content(Comment\"# Crop \
         Tally                                           #\"))))(Secondary((id \
         bef3f420-8906-4c50-a6d4-c507d0e665a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b232fbd1-9bdb-4759-b46f-dcdf44a5f1dd)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         3c845910-7ef8-4e46-bf9d-2863cd7b619f)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d50b8d1-e4db-4d34-877f-eea7c66fcf97)(content(Comment\"# Garden rows \
         are recorded as space-separated          #\"))))(Secondary((id \
         b98241b6-2550-4fdd-89df-0b8cc4833fff)(content(Whitespace\"\\n\"))))(Secondary((id \
         833ac510-10b0-4b0b-b9d9-99994ec64cc3)(content(Comment\"# strings of \
         plant names:                              #\"))))(Secondary((id \
         8a34142c-e76b-47f9-9013-2d90cb4d24a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         12f059c1-cc78-4a85-bc32-87794c7d9082)(content(Comment\"#   \\\"fern \
         orchid fern cactus\\\"                          #\"))))(Secondary((id \
         51a225f8-97ac-469a-ae1d-7484fe4aa8f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b92163fd-d608-4a94-97ac-efb3847ba629)(content(Comment\"#   \\\"orchid \
         starfern fern orchid\\\"                      #\"))))(Secondary((id \
         50340357-9771-4111-ad38-e37d57f90563)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e1e15a7-51e4-4aaf-972b-16ae1664e830)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         0e912f41-ac35-4243-bdf9-576d8bfd22b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7a0abda-b9a8-4c14-8e02-b2a748794ce6)(content(Comment\"# Count how \
         many times a given plant appears           #\"))))(Secondary((id \
         97909b83-4e9d-4f3e-9d5d-f13d0d68c8ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         efaa932b-a2b8-4677-9a31-2cd500e4f1c2)(content(Comment\"# across all \
         rows of the garden.                       #\"))))(Secondary((id \
         379e5bbe-0e9c-4a1c-b4a5-3795da2434da)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a644631-ede0-4c3e-934d-24456b3bac8a)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         6e8ac343-fcfa-4d04-9d1f-81a68a3bfffd)(content(Whitespace\"\\n\"))))(Secondary((id \
         4af32f08-7d7c-4abd-a4d4-7e39b6db7b1f)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         2783b48b-61e2-4b02-ba44-3c6ef3eb86ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1e053bd-8cdb-4f09-a408-69ae46d4b1e5)(content(Comment\"#   1. \
         count_in_row: count a plant in one row string   #\"))))(Secondary((id \
         9afc89e8-0e24-4135-8f0b-d0562ebec8eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         09cf7a1a-8c9a-4827-bc97-1e695ee5579b)(content(Comment\"#   2. \
         count_in_garden: total a plant across all rows  #\"))))(Secondary((id \
         a6d8f374-1a02-46b4-b0ea-b8ce9966ebf9)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a40d53f-35e0-4031-bc92-a6374f8afadf)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         c4c7b4c2-9848-4eac-9b07-99e962104f2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fccbadb-1ba4-4c15-84d8-69818e446cd4)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         95d78a35-5bec-406d-ad2e-56ebc6bd8f38)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d754819-0bbe-4df5-8008-09b03b7d0093)(content(Comment\"#   \
         string_split: (String, String) -> [String]         \
         #\"))))(Secondary((id \
         4a46130c-0e07-44a3-992b-fa2c1863de80)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e362036-5561-4824-81d0-86b5b676b293)(content(Comment\"#   filter: \
         ([?], ? -> Bool) -> [?]                    #\"))))(Secondary((id \
         220e5aae-20ba-42e3-92a9-dac1975c7d7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8953e100-c8db-4a6a-afc5-ec829f211b08)(content(Comment\"#   length: \
         [?] -> Int                                 #\"))))(Secondary((id \
         7e6b2728-da80-4599-bc79-9887c9347610)(content(Whitespace\"\\n\"))))(Secondary((id \
         67b04af6-d9a3-447e-850c-13c962e1c511)(content(Comment\"#   map: ([?], \
         ? -> ?) -> [?]                          #\"))))(Secondary((id \
         b7beffcb-5169-4fa2-925d-6e04e0809a1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c000b0c-53e2-4cc5-8c69-60f278cde3c3)(content(Comment\"#   fold_left: \
         ([?], (?, ?) -> ?, ?) -> ?              #\"))))(Secondary((id \
         7b896de5-1837-4f25-ad47-e2d13867b127)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8d570dc-09ba-42bc-a42a-1fdb902856d9)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         c3278b92-c1de-4838-88f9-7fd6303faf7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         19780298-dd50-44c0-b00f-b04e1cf63be9)(content(Comment\"# fold_left \
         combines list elements into one value      #\"))))(Secondary((id \
         9ee14118-a216-4cf5-9c31-f0753f7a699c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f30630d-8bdd-45d8-804b-933155f1baf5)(content(Comment\"# using a \
         function and a starting value.               #\"))))(Secondary((id \
         16f9edac-8da0-45dd-a3af-2cdbab23308a)(content(Whitespace\"\\n\"))))(Secondary((id \
         10162967-1532-4e67-a69b-89a98a67dc84)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         c4eb463e-1a02-4155-97ac-a27728a08aa6)(content(Whitespace\"\\n\"))))(Secondary((id \
         14a70b50-ec14-4643-a7c8-6280e783f77b)(content(Comment\"# Tip: Try \
         each function on a simple example first     #\"))))(Secondary((id \
         637696a1-152f-4865-909d-94ed59301159)(content(Whitespace\"\\n\"))))(Secondary((id \
         edd562f5-3c81-4942-ac25-85c49ec2d6ee)(content(Comment\"# and probe \
         the result. The argument order for these   #\"))))(Secondary((id \
         47c5e9ad-b2f0-4b8c-874c-669c7928dedd)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d5e246a-e131-4091-abeb-5fc6b4b56331)(content(Comment\"# functions \
         may not be what you expect!                #\"))))(Secondary((id \
         92d4ebef-685f-42b2-8739-904fcbbf5edc)(content(Whitespace\"\\n\"))))(Secondary((id \
         4839b370-3d97-492a-807a-57ed3f0ac3ba)(content(Whitespace\"\\n\"))))(Tile((id \
         c4efa7ac-5434-4a9b-a951-ea204efcc3a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ca5670dd-13a0-469f-a606-3b360faeac91)(content(Whitespace\" \
         \"))))(Tile((id \
         12706428-11d6-4b1f-9226-49bf0439c680)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         22ad5314-98ac-40e0-8592-decea554849b)(content(Whitespace\" \
         \")))))((Secondary((id \
         caf649b9-6864-4a89-abb5-d72eb9c6fa50)(content(Whitespace\" \
         \"))))(Tile((id 90201de7-0f8b-43f4-97b4-b32378cc38af)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f9d18971-7c7a-4cf8-9496-6eaad4c55957)(content(Whitespace\"\\n\"))))(Tile((id \
         05bac16b-f29d-4a9d-ab9b-8bf45d4db060)(label(\"\\\"fern orchid fern \
         cactus\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         11aa7d9f-cab4-483a-b304-5c0dc0c0a1cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c521879-10e1-4ba5-846e-1c8a49c4f7da)(content(Whitespace\"\\n\"))))(Tile((id \
         deb0c468-d9b5-4092-a904-5c2c1ee2eb3d)(label(\"\\\"orchid starfern \
         fern orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25dd2a63-1a98-4e79-8d54-360ed093c506)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e045e4a0-bc5f-4505-a110-5eefb29fd3cb)(content(Whitespace\"\\n\"))))(Tile((id \
         528d95d8-f829-4c44-b95b-24b9d989900f)(label(\"\\\"cactus fern orchid \
         orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         506ef0b1-bb45-45cc-8dc1-ff5b07822644)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0086af28-d38b-4f4a-9d15-c1740cc47655)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7a147d20-8107-46e4-ab93-1b2e8c1b7ae9)(content(Whitespace\"\\n\"))))(Secondary((id \
         368d508d-d780-48b9-89dd-774aff66e392)(content(Whitespace\"\\n\"))))(Secondary((id \
         252ae7c6-5fd1-475e-b2d9-998357f4239b)(content(Comment\"# Count how \
         many times plant appears in a row string #\"))))(Secondary((id \
         27c5bf70-b2d3-4623-af4f-41c9812aa29c)(content(Whitespace\"\\n\"))))(Tile((id \
         ad5a10d7-262b-4e56-a3f2-98a14ce8536e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df474e9d-0715-43a8-b4de-225dbdff52df)(content(Whitespace\" \
         \"))))(Tile((id \
         c50a5627-7d53-40c0-b6f1-4456aff4862d)(label(count_in_row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a8cea966-f05a-4792-93cf-956eddba93bf)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         71edc540-35fd-4bea-9dda-41a949df0ad5)(content(Whitespace\" \
         \"))))(Tile((id \
         c48a5a64-8ad5-4386-a082-bfd709704031)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         20978bd8-eaa6-40f6-9348-a2a1995cefda)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a728af9f-7b89-42a9-a4e2-f7d10c2f3279)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c09fe832-3a6c-4632-a991-875d2388ab2c)(content(Whitespace\" \
         \"))))(Tile((id \
         7fca57ca-34c9-4e0c-a4f4-7864f6381c9f)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0f71a57b-5f7b-4162-ad97-de588dc2a2b6)(content(Whitespace\" \
         \"))))(Tile((id \
         09b596c7-c636-46c2-990e-84b208ee4ff5)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         25721abe-2875-47e8-b448-495ec47051e9)(content(Whitespace\" \
         \"))))(Tile((id \
         39b30a46-cb25-4570-b37e-78f38a245aa5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d836f74c-9c34-4bd1-8b17-15be49f5c819)(content(Whitespace\" \
         \")))))((Secondary((id \
         d73a0325-959c-4ff8-a66f-b5f17ef620b1)(content(Whitespace\"\\n\"))))(Tile((id \
         bdaa06a4-b12e-463c-b588-82da0c72e91e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d3b54acd-5eac-4f11-9aca-e49e93e0587f)(content(Whitespace\" \
         \"))))(Tile((id \
         d8b9dc0d-b2cc-4fc9-b302-a54f4be3613f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b00a0303-f3f4-4a71-8579-baf3287311a0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dadcfc41-18d9-4a05-bbb4-51673f3a5a95)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0abccb16-b9d9-4666-ac84-4a23ecb719c9)(content(Whitespace\" \
         \"))))(Tile((id \
         9f3c0d51-8407-407f-92eb-c7acd3ddb636)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         709fdc7a-cfcc-423c-8d03-4a430fa03cb4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0cc77d05-f6fb-400d-b686-be628b9ed145)(content(Whitespace\"\\n\"))))(Tile((id \
         3305818c-6502-4b4a-a26a-aeb235593a2d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bc720204-0281-46b9-a528-fc99ccb37e37)(content(Whitespace\"\\n\"))))(Secondary((id \
         8954d684-cf9e-4460-84a5-7eae314ee710)(content(Whitespace\"\\n\"))))(Secondary((id \
         db1356cc-ce83-41a4-a2d7-a57fbfacf85d)(content(Whitespace\"\\n\"))))(Secondary((id \
         251737e7-3a3f-404f-b3d0-843501520fce)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9cc3eec4-96f7-4a84-9ef7-86e7df5d9d00)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cec60b1-6922-4ec5-b2ef-4d5e975bf73d)(content(Whitespace\"\\n\"))))(Secondary((id \
         da0f0bec-c391-469d-bb12-059a45bb1221)(content(Comment\"# Count a \
         plant across all garden rows #\"))))(Secondary((id \
         dc1a70e4-29e2-4e2c-b818-cc0fc7f22b8d)(content(Whitespace\"\\n\"))))(Tile((id \
         17338271-f4b0-4304-bbc5-e4b12a08846a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         66cfccc7-7100-4b91-b1f2-eb3463edfb11)(content(Whitespace\" \
         \"))))(Tile((id \
         9251e362-9fd8-4dda-9a90-9b2c90a0b1f0)(label(count_in_garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0a80685b-a929-4808-910d-662fe730a94b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         892da5a8-9db8-45dc-b3c2-50553de0662e)(content(Whitespace\" \
         \"))))(Tile((id \
         cc003650-e02a-44b0-ab2a-e10015102403)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         945eb758-44ba-42e5-a87f-4cb3236c6348)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         02cac2b0-4181-46db-b14b-1322853811c8)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         33dfad4b-d1ec-48b2-8190-55ed96ed7c87)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d379d51a-2785-41b5-a355-16aa5735919c)(content(Whitespace\" \
         \"))))(Tile((id \
         632cb811-055b-46fb-b5ef-bb7192b9f51b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e17932a7-5630-49d9-8e9d-39dba7ab30c9)(content(Whitespace\" \
         \"))))(Tile((id \
         a929bded-ce8d-4e20-a244-dfdb1c6e1c2b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23688608-6a3e-4d1e-afcf-1dfea9d99a22)(content(Whitespace\" \
         \"))))(Tile((id \
         1f3b2852-4327-4e38-babc-204d3f39ce8a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         31a479e1-b9b5-4fa3-b312-bb48b531e677)(content(Whitespace\" \
         \")))))((Secondary((id \
         592002a1-b64d-4519-af18-b1bc8102a832)(content(Whitespace\"\\n\"))))(Tile((id \
         e1e800b3-41bb-42a7-8a95-13b4a623ba60)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4dbf4853-1fd7-4d44-9cca-1ef984682b19)(content(Whitespace\" \
         \"))))(Tile((id \
         f8c0d8fa-8d76-48dc-9dbd-db4a3ff1eed0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         269746de-d2a2-446b-873c-0941a1fc91f4)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8991fdcb-7a74-43bc-911f-d3dfb665a16a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b966a1a4-8560-488a-a200-0a010bfa4487)(content(Whitespace\" \
         \"))))(Tile((id \
         6628ec95-d4ca-47be-8cac-3837546b7be5)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         70b11f20-1bbd-45f3-b5ec-af31d1f929c9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         412a581e-3b1f-4bc4-9c25-660039f15c4d)(content(Whitespace\"\\n\"))))(Tile((id \
         994f92a6-55dc-490c-819d-685e12abcf7b)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e899b6f-0a9b-4698-802b-c0643a7e547f)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3dcdb49-b03f-4f3d-a36c-4dd8f565ec85)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d4e6b65-7587-4626-8481-d72bcc55a55d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b79ae9a-94dc-4ab0-af13-6c373b39d62e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3cc0d4b6-11b6-4541-9cd5-53b08d8bd37c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5814767-820b-441d-9287-4ab6b8bcda95)(content(Whitespace\"\\n\"))))(Tile((id \
         47ad30ca-379f-4adf-9aad-bf8b7b35d8e4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4c461591-b2df-433c-bd45-60878799c30a)(content(Whitespace\" \
         \"))))(Tile((id \
         b3739075-814f-49f3-8c09-7ff818d2a091)(label(count_in_row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfc0e558-2269-40d8-9f99-4ad9e9eb7de9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d13581f4-4f1b-4d26-a655-7739965123a2)(label(\"\\\"fern orchid fern \
         cactus\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         b3ccf68c-6965-472d-9b47-4c446e99a3db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f45dd7eb-964e-499f-a514-3c775c2f79ae)(content(Whitespace\" \
         \"))))(Tile((id \
         279dbeef-f2de-45f8-8df6-2a3968ded446)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19b53ac4-c382-490c-8a18-22b38c4b46b3)(content(Whitespace\" \
         \"))))(Tile((id \
         91fe443a-180e-4d92-b8a2-3de0cb4402a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41e95f3e-ce6e-4531-9547-ba70ce692e4b)(content(Whitespace\" \
         \"))))(Tile((id \
         fd30b8dc-f5d7-49c3-8bae-342648d55aaf)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82131306-13b8-4f07-812d-6f93b3cde633)(content(Whitespace\" \
         \")))))))))(Tile((id \
         be02473b-6737-410b-8dea-5f405172859d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3faffb1-5b01-4f4a-8bd9-c77add2952ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         e77c9b54-7a56-4674-9bc1-b355f032a718)(content(Whitespace\"\\n\"))))(Tile((id \
         d16e995b-2224-4b5e-9879-7b2eed5cabcc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2963d6af-3abf-484a-a9b2-cdf169d7e678)(content(Whitespace\" \
         \"))))(Tile((id \
         eebd41f0-2f44-4a56-a050-376eea7f0617)(label(count_in_row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         475a8000-5b8c-46b3-9a45-347914b06efc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         931312bb-5e31-4420-9015-cf339d0657af)(label(\"\\\"orchid starfern \
         fern orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         057fb537-2ea8-487e-84c2-147c6500e609)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4efb17f0-96e0-41e4-9dd2-2e426eb24f3c)(content(Whitespace\" \
         \"))))(Tile((id \
         9fef3680-3cf2-404d-81e3-c0418de5813f)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         471a0d66-1a70-46d4-8cf6-655fa05c15df)(content(Whitespace\" \
         \"))))(Tile((id \
         e2853b75-2e10-49c5-8825-bd2591390be7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca2d2444-5bfd-429a-a1c8-c3ae8ff95d3d)(content(Whitespace\" \
         \"))))(Tile((id \
         77343f8f-e36f-4c16-a5f7-a461289488c6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f98417d-fd0b-4019-a900-fbb2d28c99de)(content(Whitespace\" \
         \")))))))))(Tile((id \
         100e4d78-7f49-474c-9d6b-496ed16e1360)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c58976a-3736-4547-8315-1d4491ec32f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         26e28133-5fee-4ff5-be09-18ad437bb340)(content(Whitespace\"\\n\"))))(Tile((id \
         93d60ce1-3a77-4ac9-a5ca-a239f6e88d73)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4cb21e8e-45af-4e1f-8efc-d98a6344ac2a)(content(Whitespace\" \
         \"))))(Tile((id \
         fe727900-f41c-4270-9dd2-f318c20aceb8)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19e3d434-6f78-4eed-b12c-b6bd25858aa6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2aad7635-7b23-49be-8221-05bf8fc021fb)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae47c79a-34df-4900-9469-01471f941a69)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31244f65-237c-41f1-b6fe-527ba1218011)(content(Whitespace\" \
         \"))))(Tile((id \
         29d9227f-5a35-44c9-884a-4348f25505b3)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b638f2eb-2def-4505-a706-628308d56c6b)(content(Whitespace\" \
         \"))))(Tile((id \
         f5025028-13c8-4294-ad91-1a244527b849)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         594884b2-98c4-45cd-a2fe-f8fff5612e85)(content(Whitespace\" \
         \"))))(Tile((id \
         e05f87c3-7f77-4752-adc2-14edae75fa09)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14ddee1e-bc11-4831-bfff-427bc947f5a2)(content(Whitespace\" \
         \")))))))))(Tile((id \
         c06ae044-efd8-4c9d-9b14-25ff06ad0a17)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd480ca9-0c21-4ccc-8609-3c865c6b5178)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ee392c9-a9cb-46f3-8ff2-cf7006b7e6d7)(content(Whitespace\"\\n\"))))(Tile((id \
         56521aed-4fe2-4391-8192-a4b067266caf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f2b3815e-472c-40b0-bf3e-93f287bf4e92)(content(Whitespace\" \
         \"))))(Tile((id \
         ca3ffef3-20c7-401a-a6ce-b3fad831a5e1)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba82c117-3604-4cce-b1eb-95de9e1a669e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df81b877-41f2-4b4c-ac1f-89c5481cf94f)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3bd1ead-1ad9-47c5-a412-3af8f26f6cf5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14c5a76b-0c36-4767-aae0-810a5fc8fd95)(content(Whitespace\" \
         \"))))(Tile((id \
         d8b431ec-ad43-4358-abc3-431ca215e1a6)(label(\"\\\"orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45ebcd79-82aa-4274-9d88-2dcb62c00fae)(content(Whitespace\" \
         \"))))(Tile((id \
         66e90a3a-e6e8-4194-8207-d32197bd8b0a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a8c9080-84a6-4d8b-8132-7f389f2e7b5c)(content(Whitespace\" \
         \"))))(Tile((id \
         cce32836-4e29-4c85-b9fa-5025a50e67e1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4966fe42-71ca-4f83-9dd1-94c132b33cc4)(content(Whitespace\" \
         \")))))))))(Tile((id \
         78b79863-c9de-407e-b933-17cc3d499694)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac56df6d-69dc-4318-9156-b3ed535eea46)(content(Whitespace\"\\n\"))))(Secondary((id \
         336803aa-c3d5-4192-b332-1a032dd7c3db)(content(Whitespace\"\\n\"))))(Tile((id \
         1813945b-587a-49b7-ab6b-0885e450fa96)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ee3cf5cd-f29b-43d1-bc0c-5222850da258)(content(Whitespace\" \
         \"))))(Tile((id \
         33af530e-508e-4a01-9d0b-7a6232296e09)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc090702-52c2-4b37-9b3b-f46165c1c582)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a714c92-3d41-4335-a24d-28cc31aec9c8)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c54dc374-882a-4945-bb65-1b2337848df1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a37694b-814f-4bd8-87f7-d221a04a6985)(content(Whitespace\" \
         \"))))(Tile((id \
         377ee972-9ce3-410f-9423-e3193f9324d8)(label(\"\\\"cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d5b83e20-2c11-4526-a3cc-11ff8730bb12)(content(Whitespace\" \
         \"))))(Tile((id \
         062ec789-b8ba-4d03-b775-ff0c9ff0d4ba)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62961dee-fc03-485c-9d42-e876d9b4dfb5)(content(Whitespace\" \
         \"))))(Tile((id \
         74e37e91-f336-4457-8983-b779cc34548d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c4127703-fe5a-46ee-be8e-4aeb7c883afc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dcc5e29e-13fb-47b0-b336-849ea1f82aff)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Crop Tally                                           #\n\
         #                                                      #\n\
         # Garden rows are recorded as space-separated          #\n\
         # strings of plant names:                              #\n\
         #   \"fern orchid fern cactus\"                          #\n\
         #   \"orchid starfern fern orchid\"                      #\n\
         #                                                      #\n\
         # Count how many times a given plant appears           #\n\
         # across all rows of the garden.                       #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. count_in_row: count a plant in one row string   #\n\
         #   2. count_in_garden: total a plant across all rows  #\n\
         #                                                      #\n\
         # Available functions:                                 #\n\
         #   string_split: (String, String) -> [String]         #\n\
         #   filter: ([?], ? -> Bool) -> [?]                    #\n\
         #   length: [?] -> Int                                 #\n\
         #   map: ([?], ? -> ?) -> [?]                          #\n\
         #   fold_left: ([?], (?, ?) -> ?, ?) -> ?              #\n\
         #                                                      #\n\
         # fold_left combines list elements into one value      #\n\
         # using a function and a starting value.               #\n\
         #                                                      #\n\
         # Tip: Try each function on a simple example first     #\n\
         # and probe the result. The argument order for these   #\n\
         # functions may not be what you expect!                #\n\n\
         let garden = [\n\
         \"fern orchid fern cactus\",\n\
         \"orchid starfern fern orchid\",\n\
         \"cactus fern orchid orchid\"\n\
         ] in\n\n\
         # Count how many times plant appears in a row string #\n\
         let count_in_row: (String, String) -> Int =\n\
         fun (row, plant) ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Count a plant across all garden rows #\n\
         let count_in_garden: ([String], String) -> Int =\n\
         fun (garden, plant) ->\n\
         ?\n\n\n\n\
         in\n\n\
         test count_in_row(\"fern orchid fern cactus\", \"fern\") == 2 end;\n\n\
         test count_in_row(\"orchid starfern fern orchid\", \"fern\") == 1 \
         end;\n\n\
         test count_in_garden(garden, \"fern\") == 4 end;\n\n\
         test count_in_garden(garden, \"orchid\") == 5 end;\n\n\
         test count_in_garden(garden, \"cactus\") == 2 end\n";
      refractors = "()";
    } )
