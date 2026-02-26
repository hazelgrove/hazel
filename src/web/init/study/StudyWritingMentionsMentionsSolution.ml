let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         f8da3515-a1fa-435c-8d47-fb2506d4a817)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         c2175259-6ed7-4bb6-9f32-49e889a1250f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6106c208-e824-4dd7-acad-e8683d1f7cbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         9da8e807-1fdc-4400-8253-54cdc965f752)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         0af25356-5a56-4f76-9671-db3f1bc95b5c)(content(Whitespace\"\\n\"))))(Tile((id \
         17a73197-00b9-4901-850f-768d2baf3e57)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e1a2b392-1cfe-46c7-b816-e5d73a92b7d2)(content(Whitespace\" \
         \"))))(Tile((id \
         efcc6ed5-797c-4fe6-8cc6-cb5f8771f642)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8f4405a-3f86-41b5-b9e6-88b0757a089c)(content(Whitespace\" \
         \")))))((Secondary((id \
         daa68d6d-45a8-4bae-a60c-9dfd1d446e6d)(content(Whitespace\" \
         \"))))(Tile((id 6c2828df-629b-43ed-b4fe-f3529eb72fe9)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d4a5e662-3502-4a81-bfda-5d9cd48cea84)(content(Whitespace\" \
         \"))))(Tile((id \
         7cf44883-570d-44cb-acfc-b33a60269630)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3402a238-0aa8-47d5-9bf2-378dea76c756)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         366be762-ca79-43b1-b2ae-7950be51f2ef)(content(Whitespace\"\\n\"))))(Tile((id \
         545987f7-021c-496a-9485-81389397636e)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc80b4a7-f53c-4edb-a984-f01bb713f4e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         023ef154-7b7b-4140-a074-841e806592d7)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17f667ae-f3a8-4441-b342-7ddb6ee16ad8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94aa6c8a-9ae1-4e7e-aabc-d06daf334fa9)(content(Whitespace\" \
         \"))))(Tile((id \
         9073712b-e8fc-457d-9361-d8cd7f468be5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec67297f-c3ca-4485-9cf8-f4fd2ad08c11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41c2e047-ed6f-45dc-8dd7-90d3365282ba)(content(Whitespace\" \
         \"))))(Tile((id \
         888ff160-3c72-4a4a-be67-326ad8bf4905)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         61f418db-71e3-47ee-a51c-793551a310d9)(content(Whitespace\" \
         \"))))(Tile((id \
         f3567aa4-0183-4197-ac42-17fd81a79781)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de23b2c7-52ee-4682-9a73-73ed017e4f2d)(content(Whitespace\" \
         \"))))(Tile((id \
         2298285c-d04f-401e-9c49-fcbb3f6373b0)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d31c66c-d580-456d-9362-f2005fed1fe8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         638c5829-3d3f-449f-b224-b6ce58d56612)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc3b352c-6444-43ac-bdb7-856443082990)(content(Whitespace\"\\n\"))))(Secondary((id \
         57529243-ee41-415d-8b3c-204b9d7fcae8)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         fa15ea41-c687-4e7d-8bab-64402d277e49)(content(Whitespace\"\\n\"))))(Tile((id \
         4ebc3862-df2f-44a0-b1f8-88174f2682b6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         22fa8a9f-6f1c-4698-b6dc-5ed95621e57b)(content(Whitespace\" \
         \"))))(Tile((id \
         7ca081f2-beab-41bf-a0fd-86389439242d)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         09f116a7-22d0-4f86-957d-466c8790ab7b)(content(Whitespace\" \
         \")))))((Secondary((id \
         9cd11ab6-7131-41d4-92d0-fdd0836ac34d)(content(Whitespace\" \
         \"))))(Tile((id 29c124c2-f756-4bd9-9eeb-5fca71d7b431)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         93af95eb-51d5-4d09-af68-ab2efcd7e3c1)(content(Whitespace\" \
         \"))))(Tile((id \
         d111e874-684c-4fc6-9dda-ff0b1006a1fd)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b99e73f2-308d-4a88-8844-e3cf57ce997a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         67644e39-1770-465b-8be2-335650738b17)(content(Whitespace\"\\n\"))))(Tile((id \
         4a49177d-be62-4896-aff3-64277def9be5)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6dc1afa-e43b-4764-b3a3-a69198137557)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6aaaec59-c172-4ccd-955a-e187ccd067b0)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26d810c3-a626-481a-8384-20ba4dcdec24)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         213c69a5-0a54-4bb1-aa0f-61e72a7e2de2)(content(Whitespace\" \
         \"))))(Tile((id \
         9bbf54fa-a765-47df-b465-996d00e04a6b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8eb1af8e-8420-4b45-bb56-a91b759862c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66096190-8f79-4baa-834d-edd7dbc9fb98)(content(Whitespace\" \
         \"))))(Tile((id \
         8d4b3372-e395-44ee-9ab9-bc81995b8ff5)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c7a2ee7-c0f3-41c4-ab06-4d3e707c8809)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c1dd1866-ec5b-41eb-b8bd-03c9594bbc77)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         085701ea-b40c-4f71-a0ee-8c860f83d366)(content(Whitespace\" \
         \"))))(Tile((id \
         d7729d17-a456-4c10-a446-c757f3cdbf85)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f523e6e-93d1-44a9-b85b-3a39f89579c5)(content(Whitespace\" \
         \"))))(Tile((id \
         d256c4a4-0c5b-460b-b86d-3765f60a948f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         980bf660-1f5c-4d7c-adf6-887b241afd20)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7d113f5b-d7d6-4cd8-a41e-0ca7adfceba4)(content(Whitespace\"\\n\"))))(Secondary((id \
         be50aad4-ac2e-4699-9c70-2bda6d849bd0)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e2b08b9-db40-4fdb-8b15-8892c2d36296)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         9b6f2b86-b14e-483b-b736-b77030c2f02d)(content(Whitespace\"\\n\"))))(Tile((id \
         0906f847-c583-48c3-b373-eec3cfa48f5c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         220295d2-cb5d-4125-96ca-a8b9995a582a)(content(Whitespace\" \
         \"))))(Tile((id \
         4c7c8d69-38e8-44c2-ba32-74ae4ce2d380)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1a838faa-0ba5-4527-b543-8e272f4af812)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3537d4a-3395-4eb2-bb83-782c44e50fc3)(content(Whitespace\" \
         \"))))(Tile((id 2e6f6c0c-c014-4a0b-9d7c-304368af3fb4)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         530515ae-7754-4624-a9ed-0a42cda9ad6e)(content(Whitespace\" \
         \"))))(Tile((id \
         bf71f327-1db9-4c33-aea7-90f0ae177270)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         16dabb3d-97dc-4b00-bfcd-d2620a1e8de0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fcfcdbbb-259f-464c-8d98-d3417ba36c22)(content(Whitespace\"\\n\"))))(Tile((id \
         d77c3543-4519-4b39-b6e9-fd526320bf87)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d1783d1-f5c7-4dfa-87bc-954777cea941)(content(Whitespace\" \
         \"))))(Tile((id \
         627a5072-6a76-4295-a311-e8affbbd2160)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         56f42217-131b-4283-804d-ac61fee64090)(content(Whitespace\" \
         \")))))((Secondary((id \
         aabb92ef-3835-4dbd-b286-1a3c869551ff)(content(Whitespace\" \
         \"))))(Tile((id \
         7b39d610-7066-40e2-a88d-24ed5875b9b8)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df8e9f53-950d-4c7c-9c79-ae8fc5a35526)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20b7e8aa-a6a1-4777-ac3e-ba578f6475a9)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98d7dd7b-39cb-4a4b-b000-3e391355e9d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fc16a92-69de-4b62-b018-e3d9c5bd7954)(content(Whitespace\" \
         \"))))(Tile((id \
         fae4a2cd-2a10-40f0-83a9-224553e6748c)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         866e8c0c-dcb5-416a-827a-27b93db2dde7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bbde697c-cea7-4b13-8108-af08b2de85b4)(content(Whitespace\"\\n\"))))(Tile((id \
         1316c245-999b-4b29-837c-13d2b2cf615c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a83e9fe9-ab2f-459e-94b2-8e5241069d5f)(content(Whitespace\" \
         \"))))(Tile((id \
         5b909466-21d8-4f00-b958-a5264ef113ff)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d51e3e12-adf4-44fb-a026-1c41584d940d)(content(Whitespace\" \
         \")))))((Secondary((id \
         91d4b12f-583a-41c5-a10a-9b31cea55fac)(content(Whitespace\" \
         \"))))(Tile((id \
         94d3968a-c7d0-4849-b9b0-43076c274ddd)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e40eeea-787f-4e81-8f98-98fad941b1ca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c6f172f-b9ad-4426-8808-db89a5fda96f)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3da40e6c-f4ab-4fff-94df-db121f79e7f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce47ce14-f0f7-42f0-8bfd-75ee94fc666a)(content(Whitespace\" \
         \"))))(Tile((id \
         217eed3f-3ee1-47a8-9048-3c0dd920b3ec)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8ee52555-89c7-43e4-81ee-113e2a8b8d52)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d1d13f13-17fe-4b11-a558-60ea3b838adb)(content(Whitespace\"\\n\"))))(Tile((id \
         093312c0-cd3c-4cae-8aec-a075adca1ed5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aafffe98-822a-4eb2-81c8-a948fb8c2f15)(content(Whitespace\" \
         \"))))(Tile((id \
         0a7eab20-8358-4972-bf5b-fe20f0ba81ac)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4761368e-6765-47b4-916c-0d4bfc4216e9)(content(Whitespace\" \
         \")))))((Secondary((id \
         fb220511-f5f4-4e30-ab22-8741c06285c7)(content(Whitespace\" \
         \"))))(Tile((id \
         cb5e64a3-b97b-4a19-b057-df9e2aea4435)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11395e93-0594-4f7b-9f9d-fe9ddaa43edb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4dff198d-5601-43f6-a167-224611e5fa66)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f87eff2-c2de-4368-8bab-a145d1dd97d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7bd9ab82-b7bc-49a1-ba92-5cc9598e6ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         2affef0b-3984-451f-a594-72fde5bdc9b9)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f6d6ccd0-10d8-4fe4-bf7f-185add37101b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f24313d3-c87a-4cc7-b0f4-ca13eab909b0)(content(Whitespace\"\\n\"))))(Tile((id \
         2bbd2d2a-29a9-4b01-a054-86ecaa4686f9)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         622273b3-12ef-402a-8a4e-2095f5fd3f92)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         27de120d-9e26-4f61-8a92-4ea7e536ce48)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f0d7fbb-3106-44fc-b268-0508b1db91da)(content(Whitespace\"\\n\"))))(Tile((id \
         4ee4d40f-7f4a-4d83-b4a8-b2b6cc5f13c9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         68a6e163-553b-4134-9468-4d92ed90e63c)(content(Whitespace\"\\n\"))))(Tile((id \
         a41a489f-10f7-477c-97b9-d8fe0d6bdeb0)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d15b72ea-137c-432e-9853-e18cb0cfb56d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fec9854f-9d26-440c-a907-5a7c3d49515b)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         98a06b8d-2306-4009-b043-a1e64c4a3eda)(content(Whitespace\"\\n\"))))(Tile((id \
         cc678dfc-9b44-4bf0-8a71-89f9394b2360)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10eff50f-c451-4165-b339-37f74fcd6a4c)(content(Whitespace\" \
         \"))))(Tile((id 23880cd1-6b6d-40b7-a692-dc53b3fdb226)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         776bd24c-3954-4016-a0bc-b4349bf8ac57)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b3310a9-92e0-455b-ba18-7373a73b9d6c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         32236166-f62a-422b-89d4-07e5c89bd4a1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         109d1c28-0894-40b6-a13b-ab614dfc294c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7490a972-653f-402e-aa7c-ebdeec567b76)(content(Whitespace\"\\n\"))))(Tile((id \
         0846a66c-fef9-4c39-a51d-d27ab2c57c5c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         57d0ecb8-10da-4a4a-ad0e-7600a545c7da)(content(Whitespace\"\\n\"))))(Tile((id \
         76c0ed05-dcd7-4d52-9d85-3e24589601e7)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3996601a-dc7d-4625-873d-0ac911257f90)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         133c2030-74a6-4d65-91e1-15199582ac52)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aa38ba48-fe0b-4f29-a26c-e4bcde329a38)(content(Whitespace\"\\n\"))))(Tile((id \
         d81f6f88-417f-444b-b0b8-6003bbcb5a20)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ee431d2-78f3-47b0-a49e-0c74cf9fa3cd)(content(Whitespace\" \
         \"))))(Tile((id 9d3f9edb-c331-487b-b184-743d2463f5c2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2fa42fc5-6b35-4e73-94e7-33a9d584e569)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe972508-d4e7-4414-81c4-7825afb411b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9168e5d8-e839-42d6-9081-502c3de757c1)(content(Whitespace\" \
         \"))))(Tile((id \
         a34972c6-f92f-4281-ae16-022de03c6b16)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cf44d824-676b-4ebf-86bc-5d2b7d76809e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6358506a-2b4f-420f-bff0-1ac7ab12731b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f149daa-af0b-405c-95d0-95fb077572ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         54a0e679-9f3c-4781-b1d7-044509ff0fb0)(content(Whitespace\"\\n\"))))(Tile((id \
         c0772f98-d4b7-4e10-a00d-9fe3610be6a9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         522eefca-6bb8-473a-a930-e592c93bb3fc)(content(Whitespace\"\\n\"))))(Tile((id \
         bcb1511b-cc5d-4f0f-8db0-02a3d0a2bd2e)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb3ed85b-e95e-42aa-b1bf-6c88da3d18b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         625d9034-12ec-4402-8fdf-f8bbeb8d4a86)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2512ea08-75fa-49af-a412-e74134419461)(content(Whitespace\"\\n\"))))(Tile((id \
         432224bc-a339-4946-8b6f-d461c95f5cfe)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ed17fee-d5d4-4411-9bd7-c9aca95fefa1)(content(Whitespace\" \
         \"))))(Tile((id \
         b2c222a2-402f-416f-a089-ff0d7dfbd0c6)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8dc4af3f-50b0-4fcf-bbf1-0a0695f55de7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9d45a224-01fb-4ce8-b277-f87a80d26993)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb020ea7-3aff-4cc8-b1bf-213b45f7fb5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba33ca5b-0b25-470d-b602-a375471c4116)(content(Whitespace\"\\n\"))))(Tile((id \
         19e136bd-a4db-484c-9cca-0f3a371fc29a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f43e7f3c-51b3-4d29-84b7-7f313cb0af1b)(content(Whitespace\"\\n\"))))(Tile((id \
         82e1a76c-e102-43d1-a55c-d5bd6b034117)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         667e1302-3f18-4885-8f2f-36bbe5e10643)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         28f727eb-aad1-46ce-abab-8762c1f619c4)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3938d187-3594-4e90-b71b-aa50e622dcd9)(content(Whitespace\"\\n\"))))(Tile((id \
         f27c46a4-fc73-4044-92e2-af8155008c66)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dc86f61-cd64-4bf3-b560-c4143be08806)(content(Whitespace\" \
         \"))))(Tile((id e30c3204-e741-4308-89eb-c7f793307ab5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1167cbe4-b4b1-4142-b922-2653e12c52a5)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac466891-d1a0-4e9d-b586-5ec4a3667ae3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf4f1f21-7b27-47f8-a7b1-ebdc38f5113d)(content(Whitespace\"\\n\")))))";
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
