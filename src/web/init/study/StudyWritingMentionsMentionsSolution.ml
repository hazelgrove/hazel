let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         5d6104d1-0330-43c0-b433-0171012dd4dd)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         71a58ee2-cceb-4221-a4c9-f67158626670)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dbdec84-fab4-4871-96e9-191a9808dd06)(content(Whitespace\"\\n\"))))(Secondary((id \
         db1da954-01fc-469d-ad0a-80e3f2c90297)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         67f65f09-e1fc-4f28-95ac-71305005a3c9)(content(Whitespace\"\\n\"))))(Tile((id \
         452d97b8-87ad-4a5b-aaf9-2eafc7fefbf5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bd9c0d4c-74e3-46b2-88c5-33638c96cfc2)(content(Whitespace\" \
         \"))))(Tile((id \
         a6c36a71-efbf-4f2f-81ef-95562a3e682b)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4cee5f40-e6e7-4acf-b6cc-4a07fca3bba9)(content(Whitespace\" \
         \")))))((Secondary((id \
         85598434-c028-42bb-8d63-98b49a5a8d3f)(content(Whitespace\" \
         \"))))(Tile((id 31719da7-3305-4ff5-942e-2f4be6caa07c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2605ad27-2132-4184-807f-5365a89f8765)(content(Whitespace\" \
         \"))))(Tile((id \
         99bf092f-e795-4312-834f-ce636cc8944f)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3cd5908d-a471-4256-bc8d-105be4dee06b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         505d61b0-9cf5-4eb9-8fdb-894933d6d3b5)(content(Whitespace\"\\n\"))))(Tile((id \
         e5975ee2-b80d-4afa-9bf2-d73f0e5ae42e)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7925d0b3-3b1b-4631-ab65-bf6df200b4d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29eb8d3a-ce86-40c3-8e66-f536bde987a8)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68ace5b6-dc29-4999-8629-1194851f01ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         872cd263-eb51-4bb4-8291-fc0d18fc4fa6)(content(Whitespace\" \
         \"))))(Tile((id \
         bd10639f-1e07-4911-97ab-6498fe6d3884)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7dff2d85-382d-439a-a501-218945044ade)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         640e62ac-9c2c-441b-b249-1ebc1884e5b9)(content(Whitespace\" \
         \"))))(Tile((id \
         2eaa9342-351e-4de7-9cca-9671240812f8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         95659400-11f5-4263-9f16-eb28d9b1ac9b)(content(Whitespace\" \
         \"))))(Tile((id \
         51071a5a-981c-40d6-b5c1-30ea114c8da9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96d79c52-08cc-4b48-8a71-f38d9bd1dc6a)(content(Whitespace\" \
         \"))))(Tile((id \
         4e732ce3-a7d3-4bf5-9f28-ba8ea0e0fd63)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         90920228-d954-465b-a0d4-c6376fe79227)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8809efed-d9fe-4d1a-9780-a3c29152786c)(content(Whitespace\"\\n\"))))(Secondary((id \
         f422358d-4019-40e2-be4d-ab483ed0d864)(content(Whitespace\"\\n\"))))(Secondary((id \
         08c7105b-abea-4a9b-a835-229eec3eb449)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         aa77329c-1196-4c84-8886-2008caf860bb)(content(Whitespace\"\\n\"))))(Tile((id \
         888255b8-fab3-4bad-85be-012978c31764)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a64948c-b775-475d-9f92-5117e1c5e73d)(content(Whitespace\" \
         \"))))(Tile((id \
         c695aed0-ab16-4daa-8443-5f7af02f52f2)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8ec3377-5f8a-40e5-a32f-c0a41bf8a046)(content(Whitespace\" \
         \")))))((Secondary((id \
         093d4fd5-0875-4174-91c8-dd2838b34416)(content(Whitespace\" \
         \"))))(Tile((id 33bdd854-54fb-46e6-b5f6-e8562df91ff3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1454cda3-f25b-4204-b382-33faa8da4590)(content(Whitespace\" \
         \"))))(Tile((id \
         ca935fb7-70b8-4f1d-b44e-55eb4d4b61bf)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         368ea653-90f8-4928-af43-6481bfc6ab3a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c5860bb-6e09-4331-b731-d420adde256d)(content(Whitespace\"\\n\"))))(Tile((id \
         434b3fee-17a5-4ffe-b957-a24bd780eda2)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         db91f472-925f-4a1a-9f12-7da7b9493bd6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0cf03e77-d4be-453b-a2ae-e127f6f1ac3d)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         977eec74-d1e2-4bba-92ab-973d089e830b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb978c19-4939-49c8-a567-0f2036d2922a)(content(Whitespace\" \
         \"))))(Tile((id \
         255b96e1-58d2-42c4-a5d4-3bd9c613327a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a79a19fc-525d-463d-b8fa-c9c4540d962e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d66dd69-b3c9-478e-bd3b-b75f39666827)(content(Whitespace\" \
         \"))))(Tile((id \
         a465c202-7c03-4b18-9fdb-07bdb1ee34c4)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3dfc6e5-d6ba-4b40-ab3d-c62b2f18c3c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef7fd8e3-5779-46d5-8027-37c928d43076)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a3665844-128b-4e6c-bab5-c091051851aa)(content(Whitespace\" \
         \"))))(Tile((id \
         7fc16651-b8eb-4d70-8c5c-9f66d14c7696)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67e8d15e-e855-48c9-b93a-e9c475b3ac57)(content(Whitespace\" \
         \"))))(Tile((id \
         5cb9d2ca-fc04-4404-b82a-9dab4d9a6f1a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8afdc803-0eb9-4e6f-a33b-656d6bc7adbd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b238de57-879e-439e-9eba-f2a39c5dec5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4562f318-3153-43c8-9ac0-04dc0795d0a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbe8aa48-474b-4c0f-94ac-497bd156e21d)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         78d8afb2-4a26-4556-9ca6-ad31a1689b43)(content(Whitespace\"\\n\"))))(Tile((id \
         14a07e75-8142-4228-9c09-b3dc02d1e815)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee939624-1650-4434-8bec-e544886f340e)(content(Whitespace\" \
         \"))))(Tile((id \
         500270b9-2f59-4169-933e-5d39e8cbd9dd)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ec6e5651-5350-46bd-a9db-1691a023e742)(content(Whitespace\" \
         \")))))((Secondary((id \
         335d0605-06f0-45f1-8ee6-8f16dc49164e)(content(Whitespace\" \
         \"))))(Tile((id bf73eb3c-f6a6-4dcf-afc2-d903fb23b390)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         019ebcf2-40ac-4f84-9511-c0cfe3fc7bc1)(content(Whitespace\" \
         \"))))(Tile((id \
         402e917a-e1c0-4876-80de-f394b0548548)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1deed889-68da-458c-88c0-6b4cd65a6ea0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6a93de9d-3a1e-4dc3-a2cd-09ab8ef485f9)(content(Whitespace\"\\n\"))))(Tile((id \
         d7ed0934-443b-47a3-9442-4fd97fe9931b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4b5b8934-e704-4210-b635-eef44e9f89d0)(content(Whitespace\" \
         \"))))(Tile((id \
         7f59895f-e680-460c-bc35-8666db7045a3)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52f2d1c0-f7a4-4d21-8975-38145042c3d4)(content(Whitespace\" \
         \")))))((Secondary((id \
         b473c41a-0afa-48a2-8521-d15f773bd08c)(content(Whitespace\" \
         \"))))(Tile((id \
         374d9201-93bb-49f1-9f73-1393825ae456)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d0b1342-c3bd-4e77-8134-6d2353312991)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         765bc8b8-4fd9-4665-9603-976bdb4a5ba3)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c825ff7f-287e-471e-a79a-78c745f4f222)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d920a4c5-ab24-4634-92bb-a726219d2650)(content(Whitespace\" \
         \"))))(Tile((id \
         e4ed0d86-b387-4ea9-aaa6-fe54d14397e3)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ffe0c0cf-ed26-479f-901e-0c344843a195)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d4685a95-2b29-4ee8-98d0-833fc48980ee)(content(Whitespace\"\\n\"))))(Tile((id \
         bd6db6c6-9abb-4a62-948e-872a3538ed9f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         acf40dac-12a8-4f90-ba87-211bddbe6780)(content(Whitespace\" \
         \"))))(Tile((id \
         1ab4818e-e870-4b3b-bc75-a4a07d4e8adc)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         42c2130c-462d-4b20-bbe4-b669334132c8)(content(Whitespace\" \
         \")))))((Secondary((id \
         9e95e046-d6a7-4f30-8ba7-d456ad440057)(content(Whitespace\" \
         \"))))(Tile((id \
         09459b05-5d9e-4e30-b9e8-f7f9771db472)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b64d510-d3f7-4167-a883-efe2849e17ff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         962c6762-6326-4baa-9829-beb8daf5104f)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c51fb7b9-a2f9-441e-a72c-093134448d2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0b92261-4bcb-474d-9470-51a8f846378a)(content(Whitespace\" \
         \"))))(Tile((id \
         0a840c47-9dbf-48bc-b45a-d429848f3b4e)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a879c2ec-ae70-4542-8be3-e23386f67645)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08941596-df90-4cb1-8f49-f5f5da16c9dd)(content(Whitespace\"\\n\"))))(Tile((id \
         48a8e317-8b95-4636-ae5d-ed23ad15bed9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3ca7a951-ce92-4161-b39e-1e1fb31ff6cb)(content(Whitespace\" \
         \"))))(Tile((id \
         1584dbd3-5b46-43e2-9460-b42496de2499)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f96915ee-c020-4942-828f-165c24d93ea8)(content(Whitespace\" \
         \")))))((Secondary((id \
         193f5e5d-e3c9-437f-ab0c-81cf80907856)(content(Whitespace\" \
         \"))))(Tile((id \
         7c035ffc-bf0e-48d3-ab20-129c6cd9ab24)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7b0b96f-e2ab-4e0b-80ce-b5dc97340774)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7fb8d5a2-0070-4e72-a500-78cb47883e79)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84b57eb0-1cfc-4137-afdc-ce6cc66f0eea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba10e1ca-60dd-4bd9-a782-ecee9052ddcf)(content(Whitespace\" \
         \"))))(Tile((id \
         92706b18-ff2a-4ceb-bb91-95fc2a4693c7)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         66c09d6f-b16a-4f5d-9e83-56a65f87b0bb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b85f3dc-7b58-46e5-bd7a-4be33f681551)(content(Whitespace\"\\n\"))))(Tile((id \
         8bcba9ec-d101-4b78-8657-7b136ab48058)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6affe9f3-c402-4ef4-852f-b7a2857df258)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         21af996c-d22d-4827-8afe-6c5c27037cd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2a6e8a1-eb54-4bf7-bb27-d1a55bb0b728)(content(Whitespace\"\\n\"))))(Tile((id \
         7569682e-1190-4277-b100-2166ba65fe04)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6d2e7248-fce0-409c-a0bc-485b2762d22e)(content(Whitespace\"\\n\"))))(Tile((id \
         39288963-3c08-46af-ae34-92a4d2291112)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e20a603-f4ad-4e9a-abaa-d28b2f6224e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c6f99fb9-1db1-4bcf-b6cf-351767521d82)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7c1011f2-8f49-4605-b66c-0ca805ce8de5)(content(Whitespace\"\\n\"))))(Tile((id \
         99edc051-1713-4624-a226-343c48063453)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d56c4c92-e532-4e51-9178-cc233804b5da)(content(Whitespace\" \
         \"))))(Tile((id cdea8057-0b7a-4c3f-a9fc-2ddb657f2bb0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f87c3e0-fd8a-4193-a3b5-0fa8e68578c8)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d4b3ca4-549b-42b8-bb7b-cc1e85aa8c54)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5844e3e6-e7d4-440a-b4cc-675b3ce22027)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2fe0d31-59ea-40db-8b33-3edad52bf815)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca34d2dc-06ab-44bf-a052-882fbd4d2ac1)(content(Whitespace\"\\n\"))))(Tile((id \
         f08de854-a4c0-45f2-a9a2-e3415d1cdd11)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7924b783-a3d3-4478-86b8-7af68150654a)(content(Whitespace\"\\n\"))))(Tile((id \
         27fa9247-f05c-4b77-8b37-4f25771194cb)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eba94ba3-7d20-4bb3-a042-8042f40c0e26)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a02ccb9c-a397-4411-a322-63652c039bdb)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         58534030-59ef-4c73-b448-89ee835cd631)(content(Whitespace\"\\n\"))))(Tile((id \
         0d3b640b-f2af-41b7-910d-3b4b86cad7f9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61866b5a-2862-4ff9-abd5-cff10ad485cd)(content(Whitespace\" \
         \"))))(Tile((id a192bd0b-27a4-41af-a569-fc5dba4d1060)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6182e700-52f2-42bd-bd37-67fb1b30527f)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8554a835-154d-4206-aad4-a176cc21ff3a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ef892c8-21e3-48e1-948f-94485efd6888)(content(Whitespace\" \
         \"))))(Tile((id \
         89f1db49-d1a4-4dfa-8407-f7c6440bfe32)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6d35beb7-7640-4c07-9b2f-e31381a0771e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cdbf2fa8-207e-4e29-a420-cb4129ee7adc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fe8127f-638d-4fb8-9573-b539db088ccb)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6eb308b-333e-4dfa-b52c-48eb2545c7df)(content(Whitespace\"\\n\"))))(Tile((id \
         da45cfa3-cff0-4e42-aa57-ae0c5f03daa7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ae711117-0843-4c49-8ed0-b2706bd4e435)(content(Whitespace\"\\n\"))))(Tile((id \
         66246ef9-602d-4d53-8ad1-f8d51b94b9af)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         10e36202-0c40-4efb-a02d-e67b0c6717df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7eee051e-1a74-415f-97ed-8dccda3f2043)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5fa8f41f-bec5-423a-9efb-d9be8672ac3b)(content(Whitespace\"\\n\"))))(Tile((id \
         9bf90708-e36f-4954-a419-e84717fb774c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         777909e1-058e-4bc1-921a-c1078b436e1a)(content(Whitespace\" \
         \"))))(Tile((id \
         aca33ac9-88a9-4f7e-b1ce-d9ae02303fec)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         975b332f-7f56-422f-9452-829567707ec3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c673d4a4-5a1d-4b26-b0a5-55c0d8b01b6e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4427118-7560-4932-a1c5-255a4bac0b51)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f864209-2731-4ca7-a2bb-87b323c3f62e)(content(Whitespace\"\\n\"))))(Tile((id \
         582acce5-9b31-4756-b614-7fe95d4d2c70)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         037febc0-f3a4-4737-9398-7773fbe59f81)(content(Whitespace\"\\n\"))))(Tile((id \
         37873b0d-c8bb-4e3f-92cf-632379d026c8)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7d41f63-963f-48fd-954a-cd92cec49254)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1f10d4d5-21dd-4292-b1a3-0505c27f385e)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         69c0f13e-e0a1-4189-b02d-81768f386419)(content(Whitespace\"\\n\"))))(Tile((id \
         38ed09f1-515a-422f-87c2-4b5274e2cd87)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89134e41-25af-4842-9cf9-764565b3aad6)(content(Whitespace\" \
         \"))))(Tile((id 77ba2a0b-c833-49f8-84ef-05dad535367f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5258c116-a48a-454e-82f7-d67e7746e676)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a2db38f3-9f84-4d61-84fa-e6d2c9c19f70)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3b8f5aca-8f4c-432a-a1f7-1cd3b9385f6b)(content(Whitespace\"\\n\")))))";
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
