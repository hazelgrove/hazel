let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         bde5f20d-d828-45a4-ac63-2bd849615195)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         cc6a4637-3c5e-44a2-b05e-7c04e84c6174)(content(Whitespace\"\\n\"))))(Secondary((id \
         8979f60e-bd69-4565-a254-b91e48f0aff5)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b3de440-c9ad-4281-991a-0e1d6f240f16)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         5e78b90e-ec45-42c8-9a36-ca52bc86ae64)(content(Whitespace\"\\n\"))))(Tile((id \
         bbbbac31-d320-4aad-b3c9-80992a3df471)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6fdfe30f-ec62-460c-99ae-f8986b306e44)(content(Whitespace\" \
         \"))))(Tile((id \
         c3be68ea-39b4-43de-9267-9e23df5e2670)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         60654893-cc70-41b8-87d4-ad9644838a6e)(content(Whitespace\" \
         \")))))((Secondary((id \
         795498e6-7b1f-4d80-97d7-f1b97bf10c78)(content(Whitespace\" \
         \"))))(Tile((id 298c8aa4-1563-4ce3-88ce-17be31df334f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ebecc400-f1a8-4cbe-949c-19bb15c3279f)(content(Whitespace\" \
         \"))))(Tile((id \
         865c11c5-ceb7-4af6-8c8e-b2ac84322ab0)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef5d3556-573b-4a62-b17a-5eabe39cc2ba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c18f8f89-1329-4ac6-bc11-f5a55f0604d6)(content(Whitespace\"\\n\"))))(Tile((id \
         a7634c60-fd48-47f3-a696-739646aac842)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69ca466a-1f35-46da-9c96-7b1dcb2ba322)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9dc3b82b-1153-49b9-b3ae-697a224c7334)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6340656-3728-4bb3-84d5-bf27101c3a86)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7466de23-4240-4d71-a080-9155abee2531)(content(Whitespace\" \
         \"))))(Tile((id \
         def16998-8c05-4bd6-aed8-70e5a2abc6ce)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5b85c1e-3646-44b5-a6cb-f8bfe371384c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cfdda07-39dd-45a1-8662-de66d3185a1e)(content(Whitespace\" \
         \"))))(Tile((id \
         8abf98b2-5a00-4968-b288-c16e7dc0db62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b2ed4719-d557-4e99-bcef-053326ca4fcc)(content(Whitespace\" \
         \"))))(Tile((id \
         9bdaf57a-2338-43b6-bb05-95e300aa21e0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aad5b807-39c9-4773-8059-7e30fafbd494)(content(Whitespace\" \
         \"))))(Tile((id \
         c60a3b36-81a4-4c60-859e-ec1d181169bc)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0ce48ce4-bcc2-4311-b117-ce41454f606b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03fffa5d-8008-49c1-b03f-ef2b618042a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         b20e2ea2-6541-4a48-8845-7ad8e2c5e4b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf8de869-e0f6-41b7-8a6a-764ee07f297f)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         2d87b204-8c80-42b9-9018-e727529a40d3)(content(Whitespace\"\\n\"))))(Tile((id \
         ac376e4d-dfd0-4cd3-85e1-70e1e0b86564)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         79e19cf3-af78-4b18-8b44-2f94821cdae3)(content(Whitespace\" \
         \"))))(Tile((id \
         4826ad28-d03f-49c7-abfc-8c52f4deb5f7)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0149b8ec-02c3-4e69-b924-20b4f4c09742)(content(Whitespace\" \
         \")))))((Secondary((id \
         5c87a00a-0f0d-4a67-ae49-1df272632a11)(content(Whitespace\" \
         \"))))(Tile((id 2dddebd9-f3d3-4fec-912e-514b355f1e7a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         51c96af8-120f-4c47-bb42-d0a459782a5c)(content(Whitespace\" \
         \"))))(Tile((id \
         84ad748d-8a27-4048-b15f-3b5f1b13f1a9)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c2e0253-d1da-4212-98b1-a2822d7be3c1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         86a8e6b2-6bd9-4cc6-ab8d-92489772f202)(content(Whitespace\"\\n\"))))(Tile((id \
         66681673-deff-4ed5-8a31-9fc0c49a9e06)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3efd924-935b-4ee3-834d-05d69d9f0223)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6b40325-b361-4251-b4ad-6d8cb914931f)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57b24ba4-697c-4d6b-9d84-dc40937011d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23d2e0d2-336a-44ef-b140-ac95d97d4bcd)(content(Whitespace\" \
         \"))))(Tile((id \
         1b6aebc0-83c2-464e-9e1c-4b8dc60f11b6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50926fec-b256-48d7-8ce5-4ab105af7827)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c433ba0d-71d6-4564-a27e-139a222d850b)(content(Whitespace\" \
         \"))))(Tile((id \
         837cb563-f647-4f4a-8168-db999beb56eb)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b9ffcd7-6f00-47b6-8b1c-b146689b94d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d201d56-822b-47ca-b8d3-5ad104eaacc2)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e333a98a-3a8c-4f67-a8d7-b8280170c8c5)(content(Whitespace\" \
         \"))))(Tile((id \
         692b31aa-977d-4554-a39c-5100cf65819f)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         070e26cc-0668-4402-a1d9-d616ebce1955)(content(Whitespace\" \
         \"))))(Tile((id \
         eeaad132-a2c7-4f3f-b402-53cd59813f76)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b9a21512-7041-49be-a883-623406db5bac)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f96ec0d9-e771-4cc5-b40b-1304f146671f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5565515a-9892-4eb6-bc35-1ef0855234d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         fca12c51-a52c-4525-b9fb-28c1c897a4ea)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         968c7426-3a4b-46e8-b63b-4089b1012edd)(content(Whitespace\"\\n\"))))(Tile((id \
         a5c3d211-9d0a-4869-9a9f-4e547c5ab935)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34926b7c-80b7-41e0-beb0-bbe6010bd1fc)(content(Whitespace\" \
         \"))))(Tile((id \
         97b88144-2f17-4e6d-b116-7d73af4a911d)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0138258c-1bcf-45ad-b9a2-4ac057843777)(content(Whitespace\" \
         \")))))((Secondary((id \
         658a3b09-5622-4cd3-a4a1-e37d06835c1d)(content(Whitespace\" \
         \"))))(Tile((id eb4fe61a-489c-4171-9555-7b0458cbce78)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b38a3d77-dd0e-47c9-8279-8ee54560f0ec)(content(Whitespace\" \
         \"))))(Tile((id \
         caaa0cb5-ea42-4db2-ba03-5910674bcd6f)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         734092d3-84a8-4fd8-b11d-7a3538dbd64f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d5223dc5-7aa9-4532-812f-7bdd2ea828e7)(content(Whitespace\"\\n\"))))(Tile((id \
         6fdb3768-c56f-4329-a74d-6e4f3ebb09e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a107736-6b10-44ce-9860-5f73179daf88)(content(Whitespace\" \
         \"))))(Tile((id \
         cfe39186-8bfc-485f-9cbb-272534c0e3d1)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         27acfc56-7888-4c49-bca7-ace1093e86aa)(content(Whitespace\" \
         \")))))((Secondary((id \
         5420e4a1-97ef-4c7f-b920-6d7f29795531)(content(Whitespace\" \
         \"))))(Tile((id \
         321d652b-df95-4ac3-adea-4ade65f67071)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fedf379f-4636-461f-aef5-294b27879839)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ab41f0d-8fb9-49c1-81ff-f385f3df827b)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1271d9e-1cd6-44df-9786-b4563cb8d7e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         407aca7b-1be6-44f0-b231-9d2e654cb231)(content(Whitespace\" \
         \"))))(Tile((id \
         d25ecf74-22c8-47ed-9e69-f64c84891957)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3b998d06-ae8a-42ab-87c2-d2d67ddcb2ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bb62a7f-2214-4fcd-853e-46832d2fc670)(content(Whitespace\"\\n\"))))(Tile((id \
         75e30964-4c87-48cc-a12a-2e3b08de3386)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b2cd721d-ba9a-4165-828a-464533d358ab)(content(Whitespace\" \
         \"))))(Tile((id \
         8ba8c8ec-43dc-46bb-86c2-f0d1d17e2d8d)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         74da3b3f-6cfb-43da-8a73-0f7138f04fc1)(content(Whitespace\" \
         \")))))((Secondary((id \
         29746c84-62ad-44cc-be02-138357e72f63)(content(Whitespace\" \
         \"))))(Tile((id \
         f20f71a4-f595-459f-9e55-8cccdbe9f426)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79fea4d5-1c0e-42a6-a26e-b3172d28e10d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         39647f40-c786-4b85-9ace-7a3fb4991ba2)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa328929-a66c-4c93-a427-fee1ac973519)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbfe9adf-4074-4c02-bb05-e36874f7d209)(content(Whitespace\" \
         \"))))(Tile((id \
         34723d86-b288-418a-b82b-5fd6fa82c5e2)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aa0f91c6-7dac-4d01-8bb4-3255a490860a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         026dcac2-023a-4987-9f17-3ee11c5af4d5)(content(Whitespace\"\\n\"))))(Tile((id \
         4b7d8876-476f-42dc-93c8-3b98a1cdbfa5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         233067a2-e0c5-452d-828e-7189b6405da1)(content(Whitespace\" \
         \"))))(Tile((id \
         287912b7-41f8-4b06-9f17-dbe33c597619)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eab7943f-de15-4658-a8f9-89c7f54c8b3e)(content(Whitespace\" \
         \")))))((Secondary((id \
         b7a7e471-9c37-44d7-aa2f-ad4f1c170e8c)(content(Whitespace\" \
         \"))))(Tile((id \
         81e6aa9c-9eef-4683-bbb4-7d5fc0f5b657)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48104f39-d88b-49d8-85d0-7b6ba0f7635b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cc654ab2-2477-4b10-b02a-9b3655809432)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21024864-2b03-482c-858a-d2ccd0295a8f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca2f75df-c231-4966-9434-994879345e49)(content(Whitespace\" \
         \"))))(Tile((id \
         e75b0394-4998-42e2-8908-7260959273c1)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ab4b273e-cfa6-43d4-90b2-6278c2b18db6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2d954afe-f082-43c6-9a45-a94db8af7bde)(content(Whitespace\"\\n\"))))(Tile((id \
         f3ebfe59-cbc8-4514-a764-2f90e052a727)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d25afb42-4dac-4838-be15-c0c820ad06d3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d33a71ad-f387-44c4-94e0-d9588442db5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         09346a7d-f6bc-44c5-85e6-746db8503e02)(content(Whitespace\"\\n\"))))(Tile((id \
         c925cee9-a7a1-4e97-8b5c-c306cd6b5135)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8c166f69-48df-4c71-a4b7-3bb5e30c2dbf)(content(Whitespace\"\\n\"))))(Tile((id \
         04211a2f-d725-40fa-8402-87e24c5807e4)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd27de39-ce39-423e-ad49-7f04ec543f9d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b0bf371-1873-40b2-8c43-e9e6cc0abb86)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         42bdc815-5950-4826-98d6-fc5dd5c365b0)(content(Whitespace\"\\n\"))))(Tile((id \
         df44905a-07d4-4e42-adcc-a41b16bc0be4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edfd8b38-a966-4ea1-a003-06475cd99765)(content(Whitespace\" \
         \"))))(Tile((id cb23735a-6246-46bb-a1f6-90c116763dce)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea05c6b9-829f-47f7-a5b5-dee16561d37e)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         73b2563e-1b3b-40cc-a8b5-be733f6066cf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4a5e341b-375a-453f-802b-6bfde12113dc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         debc9aa9-bc44-4636-98e9-4535115f2bc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         eeecfe8e-650f-4303-81b2-f64801cccfbd)(content(Whitespace\"\\n\"))))(Tile((id \
         f46c558d-78c3-43c0-872d-d54928b4e1c5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         76abc427-6d22-45de-a0be-4d45dced6038)(content(Whitespace\"\\n\"))))(Tile((id \
         8e337270-c080-4b51-bc58-397affaed853)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6056b3c9-2b4a-4ad7-b894-fd6b72e30ea6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e324eabd-6fec-492e-8c11-ed877c887fe7)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         83ac83a1-6211-4764-a663-a184010d8ee6)(content(Whitespace\"\\n\"))))(Tile((id \
         1b578efa-ac3b-495c-b2af-2b68bc179a77)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         542e23ac-bb1e-4dda-88f1-cbe7beaa6c2d)(content(Whitespace\" \
         \"))))(Tile((id 7ec2219b-4ad9-4199-bbf7-99caecec5f38)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2058fbb4-bdbd-4b1d-81f0-6377c6aab3a5)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d55ee615-54ce-496d-9677-2abca08573de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd6d5cfc-ce98-4193-9c64-38ea2b056679)(content(Whitespace\" \
         \"))))(Tile((id \
         311afc5b-bc41-41d0-b193-49c0816cc89f)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2bcc0c5f-a2ae-417b-9ce9-97e9ef3b6513)(content(Whitespace\"\\n\")))))))))(Tile((id \
         69917cb4-8fa1-42dc-ad1b-8b6ec7daccd3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61bab95e-4bb9-4b40-ac0f-2217334e14ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         f58cacdf-4a1d-4d04-b210-851d68b64d7c)(content(Whitespace\"\\n\"))))(Tile((id \
         47591abc-e24a-44ca-baa0-d53259fcfaeb)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7ded935a-8d04-4f9c-b266-a3cc2bb76ae2)(content(Whitespace\"\\n\"))))(Tile((id \
         f4c0c5a9-60a7-42c8-981f-ef9e68243fcb)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         867860ed-056f-4a41-8fe8-f411af0fce0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b7664c6d-be9d-437b-a1b1-ce4b46478119)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6208ee6a-f4c9-4500-9015-0c776182bc2d)(content(Whitespace\"\\n\"))))(Tile((id \
         068347f2-b24f-4eb2-9b54-941168c4d1ac)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c570351a-0b84-4955-b5a0-31b50a67bfa9)(content(Whitespace\" \
         \"))))(Tile((id \
         c6b40a76-feec-44db-b455-13ed04fb0ef7)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4286392-6700-47e5-81b6-be89900bdad6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         adb27f85-a39a-4918-8c3c-d277dc056e50)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44796906-bf96-41c6-aeb9-41eed3310e5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         291c62c7-c9a9-480a-b1da-59a8eb4abfbe)(content(Whitespace\"\\n\"))))(Tile((id \
         7e731b4c-ef4e-4a1d-9e72-95e57493d634)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a95a74e1-b960-4d72-9b36-b0a66ffd0726)(content(Whitespace\"\\n\"))))(Tile((id \
         c487c37e-c05c-4f85-aa38-2b41c07c2189)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92c28c86-c15e-46dd-8c75-008d656718a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b338902-523b-4360-92e4-0bcd80d99c2a)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         53cd68a6-33cb-49cf-a7ac-97688b5dce4e)(content(Whitespace\"\\n\"))))(Tile((id \
         d686b2de-e93d-4a5a-92cb-2f706fb975a5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfbaf590-0750-4ddd-af79-8513bd3fba91)(content(Whitespace\" \
         \"))))(Tile((id 233d5bd7-ea63-48e6-b516-0716a09fd64d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b12b0493-c680-4518-ad0f-f02cfea256e9)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b989e181-5afa-445f-ae03-0cca68e66fb1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bb6ba1fe-6a0c-4a27-bc94-2c834c5b5d94)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR - SOLUTION #\n\n\
         # Check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         string_sub(word, 0, 1) == \"@\"\n\
         in\n\n\
         # Remove the @ prefix (take everything after index 0) #\n\
         let strip_at = fun word ->\n\
         string_sub(word, 1, string_length(word) - 1)\n\
         in\n\n\
         # Extract usernames: split -> filter -> map #\n\
         let extract_mentions = fun message ->\n\
         let words = string_split(\" \", message) in\n\
         let mentions = filter(words, starts_with_at) in\n\
         let usernames = map(mentions, strip_at) in\n\
         usernames\n\
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
