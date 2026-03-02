let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         676ea1ef-a3eb-4bff-8356-d041cfc03358)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         1d1841c9-a5f0-43e9-bcf5-0600dc5bbedc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8dad0b2-a404-4bf1-8efc-125984c656e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c89a4f70-efe9-40a0-87ae-b4f9676700d6)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         d4d3e423-c493-4b2d-9701-6a31a4ecdb8d)(content(Whitespace\"\\n\"))))(Tile((id \
         3561f291-539f-434f-ba16-f7709bcf6225)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83fa206e-11b8-47e4-9b03-212e52ea6999)(content(Whitespace\" \
         \"))))(Tile((id \
         5f6ab89f-550b-4b6f-accb-3a85e19fa024)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         33851ba0-639a-4004-a166-26aafe4acde7)(content(Whitespace\" \
         \")))))((Secondary((id \
         8766d82f-d364-46ed-87ec-2de89327364c)(content(Whitespace\" \
         \"))))(Tile((id 1c9063dd-0db5-4b98-a884-09648302dd22)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a5e95dae-7eb2-4caa-9802-b6a7bcdeb013)(content(Whitespace\" \
         \"))))(Tile((id \
         dcabc745-0744-41bd-807f-668d32908b1a)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         de4a947f-2ff4-4a5f-a21c-22977ce0ed01)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         017489b2-fac0-4308-a8fd-f999890d2fe5)(content(Whitespace\"\\n\"))))(Tile((id \
         bcb2ab3e-bd6e-46db-bd36-b147fb1d513a)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0fa6c36-d89f-4071-bf2a-300b5729bccc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9798a2d7-55fc-4b9a-91c5-107da49a55c5)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1eb5c8fe-a53a-41dd-a1f7-6ae683dfbead)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31add0e7-b333-4fa2-a45f-8095349d0de0)(content(Whitespace\" \
         \"))))(Tile((id \
         43492ff2-e890-4415-90a4-737bff7f90ea)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2eca9e08-853e-4a89-9159-48ea45d9a812)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2aa88d7-f9e9-4a26-97ff-74d7c581d720)(content(Whitespace\" \
         \"))))(Tile((id \
         fbb89da3-6bc4-4b08-8290-64024328f423)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3eae734d-7a3a-498c-a37a-062267d8bd1e)(content(Whitespace\" \
         \"))))(Tile((id \
         bf0ce77a-d773-41be-98b7-c187d3adcce6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f4c64a2-175b-4e09-a260-ecdfd7918b17)(content(Whitespace\" \
         \"))))(Tile((id \
         5d69349f-7cc3-4ae4-b365-081307e85094)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1b93416-7d26-4e48-a2d7-b70e14e271f4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         35b01a40-7d04-4452-93b8-0208ac8bc9e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         97888f70-61d5-4ca7-bf22-e5362187251b)(content(Whitespace\"\\n\"))))(Secondary((id \
         287d092b-064a-470e-ad62-7832f75f7620)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         6d7d947f-a0ce-4748-b673-f2db2df50a11)(content(Whitespace\"\\n\"))))(Tile((id \
         0c1d9884-b94f-4f03-b81b-85ddbe8f9191)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2233a000-3162-4678-8f00-90e9e17f0277)(content(Whitespace\" \
         \"))))(Tile((id \
         5cbaa4bc-7e10-4af2-85ba-07dfe91bf5d4)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a1cd2b61-f067-43b2-9eb8-03bac21884a0)(content(Whitespace\" \
         \")))))((Secondary((id \
         be3c902d-7939-4dc0-9f2b-d94000e2e5fb)(content(Whitespace\" \
         \"))))(Tile((id 027bd30e-ac4d-43cc-b7a4-0d23f829769e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         87e0c4fd-9422-4742-8989-03647d0a11a2)(content(Whitespace\" \
         \"))))(Tile((id \
         c8abd5fe-641c-4d54-a8a3-40539e2f2589)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a06be90e-043f-43b9-ac2b-92b5d8033c64)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2b3777bb-aba7-41e0-bbe2-9e874fb97291)(content(Whitespace\"\\n\"))))(Tile((id \
         bdeb7211-2047-4926-a81d-4296353a1c9d)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ddec9a4-f531-4649-bdeb-9ca6634dc073)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         31c2f099-25e3-42ff-b6c8-b41200382b5b)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2621a70b-9920-4d05-80ba-643c8f3ef572)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4742059-0442-4d86-813d-4b9393fed27b)(content(Whitespace\" \
         \"))))(Tile((id \
         e5f74038-3849-4041-a852-1fb3e74fb4e4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         edc95469-782a-4a64-bd79-398bb3c981a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10da39cf-020c-4bf1-aa88-becdc207760e)(content(Whitespace\" \
         \"))))(Tile((id \
         ebbdbe31-34dc-4c6b-9649-4d8a1f71883d)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b459ff05-268d-4b57-b343-0bb88f515f7b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         243a57ee-d6c3-4602-afc6-4f4ca3fabe3e)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         becfc99e-38f4-4dcd-a11c-228122610631)(content(Whitespace\" \
         \"))))(Tile((id \
         43e12c43-d96f-4861-b83f-410d1a77c950)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6e9681f-c120-42fd-a02f-22074ab4f002)(content(Whitespace\" \
         \"))))(Tile((id \
         d0e4f7f1-b096-42da-8311-31b5946690c9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70bca7e8-8265-44b6-9380-ff7dd72484eb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8ca9b913-bea2-44e8-a8ef-db496678d55d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6923179a-2e6a-4313-8fe6-74c8290593bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         701cbb45-9027-46fc-8a0d-9fd388847deb)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         06e2e16b-ddba-4d85-84db-cc0f9a3b7909)(content(Whitespace\"\\n\"))))(Tile((id \
         86a56c66-91e8-40fc-ab71-5a02b96f72ed)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         79d67182-dfe2-4f9e-abe8-52900fb6414a)(content(Whitespace\" \
         \"))))(Tile((id \
         4a4f9a22-4638-474e-9001-695c2161578a)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         64891be5-b94f-44e5-9ce7-043a7bbc6c84)(content(Whitespace\" \
         \")))))((Secondary((id \
         93f34a62-4d95-4bec-93a4-d2042897dbd9)(content(Whitespace\" \
         \"))))(Tile((id e75bfc70-fe62-428b-ab21-386700108cd7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2a3a37b7-3949-4d12-97fa-3a178ba5f716)(content(Whitespace\" \
         \"))))(Tile((id \
         b059363c-61bb-42b6-9f5d-c59c5b158886)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c8b803a9-fa2d-477c-b5b3-27ff2cfc76e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ef474005-2ba2-45ef-82b3-349c8e723adb)(content(Whitespace\"\\n\"))))(Tile((id \
         ec4c9341-88e4-44a1-b4a3-e4c18b9909ff)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c94248ab-ec90-42d5-8634-71418b536176)(content(Whitespace\" \
         \"))))(Tile((id \
         7c39a8f2-4265-49a0-ba22-fda6c40c7602)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         519ca1fa-0e51-4355-86b7-2edf4148e2c4)(content(Whitespace\" \
         \")))))((Secondary((id \
         9e68f5a5-b53c-46a8-a6b5-b36918b46199)(content(Whitespace\" \
         \"))))(Tile((id \
         585e8b4d-86f5-4496-9d67-bb31f87f1d7a)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         03ee94c6-bbd5-4fee-9730-812a3016a505)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d163e26-616e-44d9-9ab1-f5217edf7fac)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eea49119-826f-447b-9068-e7b7c9aebb98)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4dea9dc-4711-434a-9b3d-8ca70ad03e8b)(content(Whitespace\" \
         \"))))(Tile((id \
         7eed0411-6f73-40cf-9479-553690f73cb5)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88a2b84c-7b09-422e-9c1c-aa66cc4181e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         28c68652-2f35-45c5-ad06-ba56e619a1f6)(content(Whitespace\"\\n\"))))(Tile((id \
         6444f1c0-9d96-4397-9ff8-94bfcb778bfc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55d7250c-8b12-4026-8453-0849d98e6905)(content(Whitespace\" \
         \"))))(Tile((id \
         58dfb91c-5dc3-4273-aa82-a29d4a70b9ea)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0cdfeaad-8f72-4050-97a3-4c6a5ab3aeeb)(content(Whitespace\" \
         \")))))((Secondary((id \
         7a570bb0-80d9-45d5-aaaa-d2609fa8905b)(content(Whitespace\" \
         \"))))(Tile((id \
         b28c1fdc-88b4-4086-b5b5-1c28ab8a0939)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebe890c1-e8eb-4d40-88da-9ad8d985bca6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c81936f-04aa-4067-9874-c1fcb275d1ef)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70b95e7d-a854-428f-bde6-7ae0cc852a6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0885b587-0cf0-422c-afae-ada940dfef3c)(content(Whitespace\" \
         \"))))(Tile((id \
         fa85581f-419c-42d2-a055-78233d608090)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4f7e3702-b900-4dfa-85d8-0db10cfd03ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b9950b6c-4917-4e40-928b-6ac1638dc6b3)(content(Whitespace\"\\n\"))))(Tile((id \
         773d37a1-e242-4468-a68a-b94a758d015c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ec135a96-20fa-4a2a-b950-464844097d13)(content(Whitespace\" \
         \"))))(Tile((id \
         1a7237db-e924-46be-9b33-329b335f6fe4)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3f72c165-82da-41d5-bc5e-48d401087a6c)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ce4d173-d8a2-46ff-8787-951d23ca592d)(content(Whitespace\" \
         \"))))(Tile((id \
         18d81d88-2041-4a72-abf7-6af6bd8b8b1c)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2a684d5-b7c2-488c-9ad9-3a163cc00000)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ebc0e352-efc5-42c7-91fa-986016b3ccf8)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca4ddc90-bded-45ff-b635-4b4dd6830f47)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         833a4828-75e6-41fd-ba55-ea205136d5ef)(content(Whitespace\" \
         \"))))(Tile((id \
         0492b864-97fd-4331-9a93-16e3e9371904)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         30917eb7-741e-47ee-80f1-b9a154e91bd2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4d09d7cf-a0c7-4c2f-9f58-c2832ad10a4b)(content(Whitespace\"\\n\"))))(Tile((id \
         f2e91416-eea2-4fda-b75f-58042591da7e)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0302a2e3-7196-446a-98bd-746341ceb69b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1de9d18c-aa49-4834-a57b-770c34bf3ece)(content(Whitespace\"\\n\"))))(Secondary((id \
         d39ad5b6-57cd-45a5-9c23-8170c58aa000)(content(Whitespace\"\\n\"))))(Tile((id \
         1f8e26d9-ce4c-42e6-a7ef-f47c0725e80b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         addda28f-d094-4818-aa85-53f8f4cdcdee)(content(Whitespace\"\\n\"))))(Tile((id \
         9d310a28-10bd-486c-9e69-562b1b4b6a5b)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3bffb2d-fe25-4fd6-92a6-1de5d5bc90da)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3caf7b3-0e6e-410d-bc50-8550789af2f8)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fc619d22-9ed3-4817-869f-9a28117ea185)(content(Whitespace\"\\n\"))))(Tile((id \
         78767846-d528-46e5-b4f4-f210ca669546)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74d06522-86d5-40e3-a8f2-bca1fe51579a)(content(Whitespace\" \
         \"))))(Tile((id f9f89f8e-fed9-4c3c-af12-20a00c56dd82)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4fa4e5a6-5393-457a-ab6a-ccfd2ebce2ea)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb50ae56-c138-4c6d-9b12-3e18a32ad80b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         878dc3a5-336f-4d54-bcbc-4b4c7a9bc807)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6079c10-4b3c-486c-9df9-448c0a1fff23)(content(Whitespace\"\\n\"))))(Secondary((id \
         20f9df39-05e2-4c5a-90a8-3c29ab8c8837)(content(Whitespace\"\\n\"))))(Tile((id \
         9129473d-44cc-49ec-ad64-e405eb3a503a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e9994307-d424-4261-9275-0f0c8e1196f7)(content(Whitespace\"\\n\"))))(Tile((id \
         036130a9-5143-427c-a500-ef30dae6ec8a)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         211956c1-8f40-4734-b317-71895b637130)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c93fb9f0-de3b-40b1-be03-f0c10884f5ec)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         226e29af-3410-4909-a5ec-f08c5f1a93ee)(content(Whitespace\"\\n\"))))(Tile((id \
         bfc9e15d-8b3d-4b8f-91ab-8e09eb6ea160)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7cafd7e-1e80-4e2a-bb8a-9ea6141288a9)(content(Whitespace\" \
         \"))))(Tile((id 14c5d0ce-56db-4a4e-9635-88254c2fb1bd)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b93f3d1-f04d-46fe-8d51-342ab5b57f5a)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a76a04d7-42df-48a6-8448-1496d2849b94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dbdec524-7b4d-443b-8bb4-1dae346f37e4)(content(Whitespace\" \
         \"))))(Tile((id \
         ce27c313-7d82-4db1-88bc-cc210074bdc6)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         94ee0f17-e2ab-4a65-955f-c07b959129b6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a995c4a2-91fd-4360-9f1a-bbe8d61c68ed)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         559d5b26-4cf2-49e5-9668-f872b4ee5f58)(content(Whitespace\"\\n\"))))(Secondary((id \
         6314ca30-8806-4d68-a399-483b3074f944)(content(Whitespace\"\\n\"))))(Tile((id \
         0e8c60c1-f65a-4357-8b23-1aab9b9938d9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         295ecfa5-4f19-4c35-8361-4c153f0b8cfd)(content(Whitespace\"\\n\"))))(Tile((id \
         b8b42062-c888-4de2-b636-b5e28f2fa4fe)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9f1c99d-1c18-4998-9637-c7f4570971de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         43a2a198-a6ea-4259-b312-432f4827ce47)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d4c88477-d6af-426a-890f-77622251c76e)(content(Whitespace\"\\n\"))))(Tile((id \
         fa10bcd4-5972-46ff-a534-0cb996f183dd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e529f5b-5558-48fb-a9a7-af8bea6a0c99)(content(Whitespace\" \
         \"))))(Tile((id \
         ff9d08d1-9508-49d2-ab97-1c580a7b22a6)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96877d3c-93ef-4194-90ea-ec49f9167015)(content(Whitespace\"\\n\")))))))))(Tile((id \
         04a4de1b-b96c-4f2a-8145-54f374990dcf)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         099f085f-c563-4885-8f70-2f1e0d7b1c88)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e3d1f73-0cf5-4b45-b1b0-d9af06b9c0a2)(content(Whitespace\"\\n\"))))(Tile((id \
         dcb3d5c6-830b-4560-bc3d-6e5ed58bab1c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         59555f36-39b8-4833-a2f1-1de65181d6f8)(content(Whitespace\"\\n\"))))(Tile((id \
         c58876ee-908b-483d-b819-87659be47a80)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22c087a5-0697-4ffa-ab71-3713555a871f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a26c0c0-e42e-4ea6-8686-18ca203c0818)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         02965a71-aaa5-434b-8885-5222d66b3227)(content(Whitespace\"\\n\"))))(Tile((id \
         b0f61bc6-a896-46c4-ad48-39da1b5af74d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96970c60-1e9e-4b40-9c0c-4be02b7ed0fb)(content(Whitespace\" \
         \"))))(Tile((id deb43374-6483-4cea-9f48-1c3b3672c13d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a8290a31-ba50-4e1a-8708-ad7406f2218f)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c196d249-c77f-4a82-9b76-669faa3d5f3a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d882680e-cbba-4d7a-b6b9-765824bcbe5e)(content(Whitespace\"\\n\")))))";
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
