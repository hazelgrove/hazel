let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         2f1b53a3-a50d-4c04-96a9-3870978949e5)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         382ad8be-21e9-41ce-816a-a51267f28529)(content(Whitespace\"\\n\"))))(Secondary((id \
         91eaecfd-b982-4eb9-b809-e736d57e12f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9bad085-49ad-47eb-bf6c-1d92f32e2159)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         5ef19c5a-ff51-4380-86ac-e68b069578eb)(content(Whitespace\"\\n\"))))(Tile((id \
         3804b6b6-9bec-40ab-aa30-f9f898aa38cd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1fbf762-6786-4e7b-983d-9a9214a80429)(content(Whitespace\" \
         \"))))(Tile((id \
         4d93b1e4-e0f3-4d5e-8e88-7d8c322034ec)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c8335e64-09b3-4522-8ae2-c516fe348520)(content(Whitespace\" \
         \")))))((Secondary((id \
         e43ef7e2-866f-4bfd-ad2a-70e7ce01f338)(content(Whitespace\" \
         \"))))(Tile((id 8621d8d0-283d-4696-a18d-8e65ee0c116c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8c646e25-3eb0-41c0-821e-4b8a6dacff42)(content(Whitespace\" \
         \"))))(Tile((id \
         cf47aac0-5d79-4eb1-9be6-7dfb9b560824)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d2b8546b-47cc-41ef-aa50-868242cec04a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b07267cd-e2bd-4a2c-b1de-1d07358719c3)(content(Whitespace\"\\n\"))))(Tile((id \
         187d596c-33e5-479f-86d2-6e4bc28faf51)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b3240fe-1490-47e6-90a5-ec0159d0940c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7769689e-7add-4ccf-99b1-3b5407b293f3)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e47b4f0-0b18-4dc9-af38-0215b565595c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5fb053a-48df-4343-8989-323a2954b125)(content(Whitespace\" \
         \"))))(Tile((id \
         db2640cc-3cf4-4263-9bca-3c3a4e26fea8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         844dbcd2-0914-4fee-af4d-5c3927c64239)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d429b64c-1158-4204-af81-455245261398)(content(Whitespace\" \
         \"))))(Tile((id \
         312a19be-eb11-4a03-baa7-334471006efb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88bbbd57-da8d-49f7-9fb0-76afa78d5e29)(content(Whitespace\" \
         \"))))(Tile((id \
         35409bec-9e0e-4e2d-8fdc-e0919df99a16)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5806d02f-97ca-4ee6-a1f3-ef69cebff48d)(content(Whitespace\" \
         \"))))(Tile((id \
         c17c9f62-aad7-4e78-b2a2-165e415f8eef)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38ad9b86-7bb5-4d39-a534-f1b530f38d7b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         39acd04c-a926-4758-8f1f-b29f85d1e895)(content(Whitespace\"\\n\"))))(Secondary((id \
         17093ba2-bf85-4954-bed2-7f4fb54d9d55)(content(Whitespace\"\\n\"))))(Secondary((id \
         40f611dd-6cf6-49f4-ac4d-c2461de4b141)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         7aca4fb1-d0c9-413d-8e0a-ccba4535d717)(content(Whitespace\"\\n\"))))(Tile((id \
         c1ff1049-d40c-4057-83cd-732244448b59)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc6fe6de-fb71-4c70-95f6-16216fffae18)(content(Whitespace\" \
         \"))))(Tile((id \
         b26f3892-7465-497e-a29e-579bdf0c1334)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bfe19d03-6400-4c48-8649-0af80ddc2d43)(content(Whitespace\" \
         \")))))((Secondary((id \
         d2151689-882b-4cc4-930c-ad0d233588bb)(content(Whitespace\" \
         \"))))(Tile((id 35530008-4673-4533-9f7c-16b129159591)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d5f82a49-b901-4a07-8879-fbe234ce1e90)(content(Whitespace\" \
         \"))))(Tile((id \
         f831439b-50af-4517-a6f9-10948b6e7582)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c1b4f9a-94ff-4d18-87d2-db6371283b8d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af62ea87-2fb0-4326-8406-ba953a8494f8)(content(Whitespace\"\\n\"))))(Tile((id \
         4e30f0e8-1f31-4b00-a327-577ba4db74d2)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54f053b9-eae8-4035-b98a-6c3d93bd6e4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fca3df0b-2908-415a-9500-261b0a7e5bbb)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f459d56-5c7e-4c8e-a48c-38fa17c6ad6c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4c2984d-b28d-4258-a65f-76f9b495d22f)(content(Whitespace\" \
         \"))))(Tile((id \
         6abc91eb-6d11-49ef-8276-cf058f310e08)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22b95f02-9cd6-4e5c-8343-02ce223d39c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2bca7d2-f6d3-4470-9f1b-c5895eae65da)(content(Whitespace\" \
         \"))))(Tile((id \
         bb994086-b340-4036-83fe-76df2c0f4d2d)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56e5b2bc-4fd1-46a3-be87-e942c4dfad19)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1ee491a1-23a0-4a55-aa26-835055191b29)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         17f219c2-9141-4437-8e99-5147040b3db4)(content(Whitespace\" \
         \"))))(Tile((id \
         e8017355-0be8-4187-a194-5532ed879150)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8627790e-f0de-4849-aede-43003a8e05a7)(content(Whitespace\" \
         \"))))(Tile((id \
         1eb2d39c-3266-475c-be98-1af3f6bb28ed)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         207a5a03-0a29-4f28-b491-11622fcbbb4d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e4e0aed5-022a-4b15-979e-7a8a0e46984d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e42c1bf-0ebc-4f7d-8f08-840467a69b57)(content(Whitespace\"\\n\"))))(Secondary((id \
         37911273-4fec-45c3-991a-65dd87d7a9c9)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         bcc57978-37e6-47eb-81e3-3766ad277644)(content(Whitespace\"\\n\"))))(Tile((id \
         07212ded-7746-47fc-88b2-aabb3cc311da)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1896a07a-0239-4097-960c-b46680449171)(content(Whitespace\" \
         \"))))(Tile((id \
         5a3fab1a-e8ce-4d95-a41e-9e4b5964b039)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9b62db08-f19c-4557-962f-d1d9c7355392)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3fb6af5-00a9-41c6-92ca-0a861f1e71eb)(content(Whitespace\" \
         \"))))(Tile((id f3a3fd1a-fe38-4d3e-ac2c-d3d1927bd467)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         93646319-feec-4665-90c8-5f4891044a95)(content(Whitespace\" \
         \"))))(Tile((id \
         58b90024-43ce-44e9-98a9-3fbffcd14cf5)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a1c401e0-440b-4f0a-b0c9-a2860ee3f00c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ce1dfa7f-4c84-43b3-b6fe-decada8939fd)(content(Whitespace\"\\n\"))))(Tile((id \
         0e04f57d-ded0-45d4-b651-8ea7a41c9812)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3bffb5d-0934-40e3-8900-6588e7c6d5fa)(content(Whitespace\" \
         \"))))(Tile((id \
         31bab43c-31e9-4eee-b746-a32fecbb294e)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a7a5db6-0499-434d-a16d-5ee82b6877f7)(content(Whitespace\" \
         \")))))((Secondary((id \
         e9e3ee62-89bb-4539-a015-223d0d496085)(content(Whitespace\" \
         \"))))(Tile((id \
         a8a9ac8f-97c1-40b5-b8af-34eaaa2bf94f)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ae9a83b-69f2-452f-82a0-8230de96dd72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         86b396c8-3de1-4ff1-8428-808cd0f0792f)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea60aef9-acad-4328-af11-633ec57c5b2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95b155ff-2420-448b-8b09-eaa87ba33810)(content(Whitespace\" \
         \"))))(Tile((id \
         c2a367c9-9bef-4f48-be67-a0ea13431359)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         de7c2f1a-15b4-4ed3-a8ba-e419bea895e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         da8fbbb1-16d4-463c-b918-b813238e20f1)(content(Whitespace\"\\n\"))))(Tile((id \
         3ec2a18d-4872-4844-b70d-d562f38589a2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ccf48144-d916-4931-9621-52fc9b99d14e)(content(Whitespace\" \
         \"))))(Tile((id \
         3f0cf536-821b-4507-ab8b-7a36ee136943)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aa234133-e5af-42a4-9429-156659aac5b8)(content(Whitespace\" \
         \")))))((Secondary((id \
         433ffe1c-bd0c-4835-8197-dd7ef2ee1387)(content(Whitespace\" \
         \"))))(Tile((id \
         3c54e37c-7066-4786-91dc-2ebe20e76d8a)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea0e73f0-e73e-41a5-b546-6340c6769652)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a59bcd36-4879-437c-93a7-bb1dff0eecfa)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19936eca-bd2c-4cf8-b232-63f1d5fafed3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8792bded-df58-4884-8a61-d09780d3991b)(content(Whitespace\" \
         \"))))(Tile((id \
         8cad49c7-4f5b-44f3-a047-5e2019a3c181)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         53900d33-d34e-4e39-85f2-23fedca54e94)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33e363cf-d501-4758-a029-d5b01f940bbc)(content(Whitespace\"\\n\"))))(Tile((id \
         d52def59-fa18-4ed9-8a61-952842fd022b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a07b9753-f23c-4c13-aeff-a987f1337a3b)(content(Whitespace\" \
         \"))))(Tile((id \
         d2083d22-8449-46e0-b179-233ba0aa61c6)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2db3a382-ec68-467d-bb32-534448982ef6)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d5e619a-bfa4-48c7-94c5-d511e66f52e2)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b0e333-6a86-4d09-be62-bf97770da864)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2160649-500b-4e75-ac3e-71d65dbc1786)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c0334b2-480a-4eb1-955f-aa29ce581e23)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74014858-1330-4dc6-af6a-fffdd4598f12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         337d8ab5-5903-4a74-8c89-c782e24062bd)(content(Whitespace\" \
         \"))))(Tile((id \
         f80f0c20-6227-40a7-838d-f6d081736047)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8af539cd-311b-4816-8d0f-a5c358c97403)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2ef2aaed-cb69-4270-b110-230e744ee141)(content(Whitespace\"\\n\"))))(Tile((id \
         c667fa15-6807-4076-8b37-2a45dd42455a)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf3c7703-7363-4533-b4b6-b6378c834a91)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         689ecdc1-4344-4763-bd34-cda0af3c7d53)(content(Whitespace\"\\n\"))))(Secondary((id \
         64b96b6f-0db2-4d33-864f-b52630301fd4)(content(Whitespace\"\\n\"))))(Tile((id \
         607b3e7f-84fd-4a86-a63e-87180345393b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         25bec063-6e28-4b02-9590-0b17644240f6)(content(Whitespace\"\\n\"))))(Tile((id \
         ed1e442f-d2e9-421c-a2a1-70667eca09a2)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a84fc97d-80a8-4ab1-a9db-33134df4f020)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3f1df9d-2cbe-4ee5-aafd-3b03e091b7fd)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         95e92772-d56f-4ecb-9dfa-3290899e43dd)(content(Whitespace\"\\n\"))))(Tile((id \
         facfff0c-4fd9-450a-a35e-41146b9df465)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcb27cdf-c089-46c1-9d82-2b319ebf8457)(content(Whitespace\" \
         \"))))(Tile((id cfdf8d2d-f6d0-4810-b186-56932911eada)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         031423ce-c65a-43ba-ad21-5c3951e6ced0)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         13acbb0e-24ac-40bb-90a9-ea3e1a91f10b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         93712fd2-6602-47b6-ab31-96a305bfab25)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         576c5ba4-78a1-4e81-9ccc-04cc37d9f144)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c2325a1-bd8d-4fcd-a6c1-c5e8ebe3b079)(content(Whitespace\"\\n\"))))(Tile((id \
         3e3c703e-7f56-43bc-a270-ad972df3eadd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         edca1d9e-830a-4798-89cc-c357b9a7cc36)(content(Whitespace\"\\n\"))))(Tile((id \
         607873b2-1226-47d0-b531-183b6e7a130c)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a194f0f-cbb2-40c7-ae20-1c373fafb7e9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ed804eb3-5109-48af-9107-0e98a376ba91)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         251b4583-d47d-47d6-932f-276b9ec0436e)(content(Whitespace\"\\n\"))))(Tile((id \
         1c42ea6a-dad7-466d-a3cb-71e091ee8b3b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         210f39b3-28a4-4d26-92e1-8fb9d3c7b2fa)(content(Whitespace\" \
         \"))))(Tile((id 3881f96f-3645-466c-beec-adf9995f937d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6de17d9-6bfc-47a6-81f6-efac197bdd8e)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f07b35a0-4aff-4d62-bddd-67ce277a6119)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9299a456-6647-45e8-80f7-2d0945391e34)(content(Whitespace\" \
         \"))))(Tile((id \
         98dfdb61-02a8-46ff-aba3-78c3dd006feb)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1cf8e178-7835-4220-93a7-7b5404a29503)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c82012dd-2082-4951-accc-16a3226fb625)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         107244e7-7482-47b7-adec-facd183a0306)(content(Whitespace\"\\n\"))))(Secondary((id \
         416b7e7a-28a2-4669-9d4e-9d239748cccc)(content(Whitespace\"\\n\"))))(Tile((id \
         330d01a2-48dc-4f75-8f6d-e0ab54bc1eb2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c8af20d9-7403-4bc0-9629-752eb47eff5d)(content(Whitespace\"\\n\"))))(Tile((id \
         64b5f4f8-4b15-4c70-95e4-0329813bf0a2)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         174d81a9-4447-45b4-93b7-a1e4eb430d4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ae9912d4-a12f-4425-a7c8-6ff86836af7d)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4b8b13be-bc55-40c8-8f2d-1a4b74b9007e)(content(Whitespace\"\\n\"))))(Tile((id \
         685da724-750e-4858-8264-06c77f7f7d8e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0d6fd29-690b-416b-a96f-c091c9aeb8a9)(content(Whitespace\" \
         \"))))(Tile((id \
         24e89dfa-e6fe-4fd4-b932-06232f4990f0)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e163d63e-ae5c-4feb-9408-d8eb08a915bf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c6ce7d76-8b99-42c5-a87b-191c8ab0a715)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         525dd462-e43e-4b5c-943c-995c43a7ac2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f86beba5-51da-433c-819e-22450d37a8f0)(content(Whitespace\"\\n\"))))(Tile((id \
         bf4ddb80-5692-4738-a029-d819dd61c46c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ccba803f-5d32-48d4-ac68-7afd206831e8)(content(Whitespace\"\\n\"))))(Tile((id \
         d3c9b92d-5353-4222-95e2-d440a34921e1)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4537c16b-31dc-4358-9646-33233db58a4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         423fd27d-1d1e-4d26-a5f5-ce92ce8712ff)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45f1a455-dc61-447e-b3a5-6aba56cb5516)(content(Whitespace\"\\n\"))))(Tile((id \
         796428cd-3dfd-4cd8-b439-526833f774ec)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2adf181-5777-4a5f-9ba6-41386d1cc979)(content(Whitespace\" \
         \"))))(Tile((id 35c149ce-d8ca-4643-bd16-f23085495014)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         325707b8-c2ea-4ecb-a7d3-54159e3d94d8)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2647cabf-6244-45bf-adbb-e5a6db234f9a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bb0c31e0-8703-457a-8034-5f93aca1fa4d)(content(Whitespace\"\\n\")))))";
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
