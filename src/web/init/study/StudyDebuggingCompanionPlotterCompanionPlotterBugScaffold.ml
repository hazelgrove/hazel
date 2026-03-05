let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / debugging / companion-plotter / companion-plotter-bug-scaffold",
    {
      segment =
        "((Secondary((id \
         9a1af2da-7fa8-4e7f-88ef-6d93b38d4a0e)(content(Comment\"# Companion \
         Planting Grid #\"))))(Secondary((id \
         2948c206-8a45-48d8-a970-2b7f09f7bf78)(content(Whitespace\"\\n\"))))(Secondary((id \
         b83ab127-68fc-49e2-a8c3-0f3053ffc806)(content(Comment\"# Plants \
         affect their neighbors #\"))))(Secondary((id \
         9605005d-c267-4405-b460-825fca1bce05)(content(Whitespace\"\\n\"))))(Secondary((id \
         7647890c-c808-4924-bdfd-39bb459f9250)(content(Whitespace\"\\n\"))))(Secondary((id \
         9124d714-3b8b-42e0-a449-d6e5b5e5e6de)(content(Comment\"# A crop is \
         represented by its emoji #\"))))(Secondary((id \
         c7ef0bfd-1d85-4b1b-ab90-e58b953cd69d)(content(Whitespace\"\\n\"))))(Tile((id \
         cf87168e-b727-4bb0-8285-15c325531fa6)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         88c2de26-98d4-456f-b361-aff8434dc4dd)(content(Whitespace\" \
         \"))))(Tile((id \
         41ce6761-b235-4b88-b636-d3bc9ab4dc02)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4d038b0b-0873-4e06-ae82-2c5f05eabd3a)(content(Whitespace\" \
         \")))))((Secondary((id \
         0b5ff36e-24c4-4cda-8d56-820b2e2608e2)(content(Whitespace\" \
         \"))))(Tile((id \
         a2682376-8f27-474b-ad94-fc6b31e0da50)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         14d0bc9d-d74e-42cf-bc87-fb45e5074bf1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9b0bc29f-1f15-4d17-8224-104d94f4d6ff)(content(Whitespace\"\\n\"))))(Tile((id \
         5ab31aed-d10a-4c5d-bd81-d228febaf603)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a9a0457-3b59-45d0-bf64-4435fb86c86c)(content(Whitespace\" \
         \"))))(Tile((id \
         0e4c6098-d169-4d33-95ee-2553bc3d03b3)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         92b20392-e5dd-4dea-85b2-b9571c74fe38)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d57083f-8c67-4854-92f9-044c81c4bd23)(content(Whitespace\" \
         \"))))(Tile((id \
         b0ef35fc-16f9-4f01-8588-f5451766a07c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b63f41a3-adf2-4761-8ba4-c79c388b5eef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9ea8bc9c-b1c7-43ac-bebe-1727e17b706d)(content(Whitespace\"\\n\"))))(Tile((id \
         299ecf34-9431-43b8-b73e-a20570a69b77)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4463a9f3-6869-49ae-af35-ccecf79bce57)(content(Whitespace\" \
         \"))))(Tile((id \
         05804ff2-b47b-440c-aa46-d4578fab4d03)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f172abcf-fbb2-4fcb-a51e-6d1f209d10a1)(content(Whitespace\" \
         \")))))((Secondary((id \
         dbfc5c22-0309-457f-b43d-bfd984694bf0)(content(Whitespace\" \
         \"))))(Tile((id \
         05535df6-829f-48f0-b20a-d30d2b1fc1a9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9e897c57-a23a-429b-b953-42d20f5f62bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6e4bd0ca-3b71-4ce3-96cb-00b0dcbafd61)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6be2636-d0f1-478d-bf2f-b8acc1258749)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e86a005-5126-498f-8c12-e796247a53c3)(content(Comment\"# Health is \
         0-100, affected by neighboring plants #\"))))(Secondary((id \
         0009e871-594e-4d11-9090-4b66b4045523)(content(Whitespace\"\\n\"))))(Tile((id \
         cb918545-7a71-4ce9-881a-1e9485ef9ba5)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b52f682-a89d-4e1e-ae90-a5aca654605d)(content(Whitespace\" \
         \"))))(Tile((id \
         06ea2f06-2591-4cf1-9ed4-018b0d05b893)(label(Health))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         21a9352e-e3e7-4ec2-8985-388758f136bb)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e195d97-3a81-444f-b963-f32ac20df7ca)(content(Whitespace\" \
         \"))))(Tile((id \
         f67bf1e7-5074-4bd8-b036-c775559bfc3b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d8edef93-fd30-4dc5-8eda-3dcf09b73988)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6d4a63c1-ae5b-422f-93a6-a7d132a260b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca7f7d93-2b31-4731-8e24-f15101977857)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb5b0c1c-3d11-4aa8-bfc7-1097135a6769)(content(Comment\"# A cell in \
         the garden grid #\"))))(Secondary((id \
         6a83eb86-82a2-4f7e-94e7-dd23551feba0)(content(Whitespace\"\\n\"))))(Tile((id \
         f636a92c-cb51-4f22-84d5-e347a731253d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f267ea90-eba1-4a3f-9d3a-68021229df41)(content(Whitespace\" \
         \"))))(Tile((id \
         08e721a9-71ef-4e8e-a1a6-5d8fc5f96590)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3221e974-7c3f-41fd-aace-2da32d4a2da5)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff5cc878-3660-4f8c-84c8-6c79d69b456a)(content(Whitespace\" \
         \"))))(Tile((id \
         2ec84569-750b-4901-a3a7-1adf89420acf)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         9e0c92ee-e053-4ab5-b42a-643148df40bd)(content(Whitespace\"\\n\"))))(Tile((id \
         bd3ce6de-d3c1-45e5-b5ee-90add1df0517)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fa765026-d253-4a9b-8795-f17d59bc81f7)(content(Whitespace\" \
         \"))))(Tile((id \
         5c02c360-447e-4f55-bacf-d4bed8f57be9)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8384ad25-8d28-4bf6-98c7-fc03b18c8774)(content(Whitespace\" \
         \"))))(Tile((id \
         137ced6e-badc-4798-89e6-3e35865fe69b)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         249a9850-eb37-4e2e-a2af-175b72fe5497)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         921165ad-7a47-4f0d-b7af-db18c2b13282)(content(Whitespace\"\\n\"))))(Tile((id \
         8a6c6a2c-1aba-4924-8bf5-4804bc60480b)(label(health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d050e70a-0078-4f4c-a34d-1d0c923ddc7c)(content(Whitespace\" \
         \"))))(Tile((id \
         6ee7673a-f089-432f-9b52-7545f88c56a3)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e388bb9a-dd78-4649-9479-9f96d1ede386)(content(Whitespace\" \
         \"))))(Tile((id \
         400e4fdd-26d3-44d0-9489-a216374b4f68)(label(Health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5307e0d6-6927-4973-8364-e22340eafbba)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9e495db7-8a14-481f-b4e3-6f02efc56121)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5f65550-1417-495c-990b-70ee1cc6d9c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         974065d4-b376-419a-9af3-e3a025b433a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3867fdf-2dcf-4ff7-915c-84a6ba9caa47)(content(Comment\"# The garden \
         field is a grid of cells #\"))))(Secondary((id \
         1a951f94-0daa-4593-932f-5b319d1cf611)(content(Whitespace\"\\n\"))))(Tile((id \
         3b0fe769-175f-43c3-b82a-23a0b6cd3193)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         75bf3b1a-4c82-471e-9a7b-2b343633a9e5)(content(Whitespace\" \
         \"))))(Tile((id \
         1dc216c9-b786-48c8-8f93-366643821e9c)(label(Field))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         fe032708-235e-4fbf-a78e-34e1b97fbea0)(content(Whitespace\" \
         \")))))((Secondary((id \
         560aed1f-08da-4594-ad3b-11ca3face982)(content(Whitespace\" \
         \"))))(Tile((id 1ac6f17f-0636-4391-8e57-1251f17adc83)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         0241a5b1-c07c-4ade-a049-b70522e49af6)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7a4b771d-eb72-405d-8f86-30b188b9e2da)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         06dee697-4eab-48c0-aad2-5d04e8a7f367)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b279e113-6074-420e-ad36-65fc4e753f02)(content(Whitespace\"\\n\"))))(Secondary((id \
         4774f13f-6453-45f1-8215-825ae659b5c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         45985e1c-efb5-4773-b072-ca74024d1bd4)(content(Comment\"# Companion \
         effects between crops #\"))))(Secondary((id \
         b778fa42-3135-4697-a7fc-1ac18f41985e)(content(Whitespace\"\\n\"))))(Tile((id \
         c3ace6d7-0641-430a-8310-9263a622fccf)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         67a9181c-d5e7-448e-b22d-01394413c10c)(content(Whitespace\" \
         \"))))(Tile((id \
         95aade43-2f34-4834-9fcc-f35790b5237a)(label(Effect))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         9109bc04-4566-4dd7-93df-3436fddc2c82)(content(Whitespace\" \
         \")))))((Secondary((id \
         d368ecd8-e9f4-4868-a816-9c4e564f4033)(content(Whitespace\"\\n\"))))(Tile((id \
         d0e35ec6-e66c-4d60-a792-eb2037e319b5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f3c027da-ebbe-44e0-b861-d3dc4779ed89)(content(Whitespace\" \
         \"))))(Tile((id \
         ce8d5737-f8c0-434e-8c30-3759db39949e)(label(Beneficial))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fe21e96f-8ea8-4eb4-b303-3d94d1acdb99)(content(Whitespace\" \
         \"))))(Secondary((id \
         774ddfda-dfb5-4363-8caf-39a816bb5099)(content(Whitespace\" \
         \"))))(Secondary((id \
         c6c68fea-bbd8-48b0-ab75-bbff8da6e794)(content(Whitespace\" \
         \"))))(Secondary((id \
         d17b59c4-97fe-4918-925c-c9122389cea6)(content(Comment\"# Companions \
         boost each other #\"))))(Secondary((id \
         aaa1c140-59b3-470d-a936-f5ac642a02bd)(content(Whitespace\"\\n\"))))(Tile((id \
         4f69d837-80e4-49a1-afb3-89303b0bedc6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55b61600-064f-49db-967b-b6f6435b6add)(content(Whitespace\" \
         \"))))(Tile((id \
         2894cdec-1536-4bea-8678-33a1eb2734c6)(label(Harmful))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7b67ddc6-fa6c-4e39-8569-709ed4c89157)(content(Whitespace\" \
         \"))))(Secondary((id \
         ec600fdd-4e4e-4442-bf22-931b9519a93c)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbe7c343-dac4-408d-a349-132c2bfbf97e)(content(Whitespace\" \
         \"))))(Secondary((id \
         2325f2ab-8a56-4d4a-b5b1-0e0bbb458dd5)(content(Whitespace\" \
         \"))))(Secondary((id \
         a7d3a451-83aa-4b8c-97c9-adbf65c9b806)(content(Whitespace\" \
         \"))))(Secondary((id \
         92051aaf-32d2-4e68-816b-1be23472198d)(content(Whitespace\" \
         \"))))(Secondary((id \
         ef26bf8c-57d7-49be-a938-4908c4147eff)(content(Comment\"# Rivals \
         suppress each other #\"))))(Secondary((id \
         d3a04df7-c704-4669-970b-cb458d2d5aab)(content(Whitespace\"\\n\"))))(Tile((id \
         d2896d96-70c3-45ff-a014-0448189a7773)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ec793f52-b639-446f-87de-1742788e76f7)(content(Whitespace\" \
         \"))))(Tile((id \
         82548455-073c-40c6-a773-10e448d693b2)(label(Neutral))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f1d3d341-1c3f-49d2-ab1f-40d292f9d53b)(content(Whitespace\" \
         \"))))(Secondary((id \
         fde211d3-fb65-4c8a-8027-0c1e85bfd8fc)(content(Whitespace\" \
         \"))))(Secondary((id \
         76ea4493-7b8e-41aa-af1e-d71a9069c0dc)(content(Whitespace\" \
         \"))))(Secondary((id \
         49e58e6c-ef5e-4b2e-8e5b-f9f54aeae35d)(content(Whitespace\" \
         \"))))(Secondary((id \
         d0ce7640-25aa-46c2-a780-0416049f6863)(content(Whitespace\" \
         \"))))(Secondary((id \
         3947e4b1-5dbc-48ee-99a2-86090edb773e)(content(Whitespace\" \
         \"))))(Secondary((id \
         67c176f6-6324-4f0e-b72b-397f39f7fdc4)(content(Comment\"# No \
         interaction #\"))))(Secondary((id \
         051ee563-4abb-4489-b2a0-c415f8fcf5f7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         08f9b4db-22c2-4ed9-8a55-53d3be78f53a)(content(Whitespace\"\\n\"))))(Secondary((id \
         35d8afa5-d81e-4903-b014-44b6228cdc4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4bdd675-a696-4935-8629-928f24fbbd35)(content(Comment\"# The garden \
         state #\"))))(Secondary((id \
         74d2fdb7-a4c8-4584-8b83-237f3d938af0)(content(Whitespace\"\\n\"))))(Tile((id \
         f37831e9-089a-4d99-9c83-60c44cefff97)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f4daa437-5b9e-4216-800e-df4b38b5a16b)(content(Whitespace\" \
         \"))))(Tile((id \
         ab7b6059-ea90-4ed5-80a0-62504e9d8789)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         de278e64-420f-4861-a4bd-20c15fc91ba5)(content(Whitespace\" \
         \")))))((Secondary((id \
         df3c71cf-d7b5-42cb-b39a-77b865221fe5)(content(Whitespace\" \
         \"))))(Tile((id \
         f7a6f29d-b282-4723-8c61-2414c526dacc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         78a8af36-6c94-440a-a829-49a7c2225ed9)(content(Whitespace\"\\n\"))))(Tile((id \
         33c9fb5c-7606-4ec1-8de6-a37bf4dffb13)(label(field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e108c265-15bf-4ec9-b03b-9c2ec0b6b39e)(content(Whitespace\" \
         \"))))(Tile((id \
         2f242b80-e311-4e4d-909c-1a2f4f343caa)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         40dbe562-7619-4f5b-9423-56c984450375)(content(Whitespace\" \
         \"))))(Tile((id \
         d728afaa-2a02-4266-bde1-f512c7fcfd58)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a9e4812d-4007-496a-a690-1aacbec92c58)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         69fe6395-40eb-46ae-b27f-95a4bccb010b)(content(Whitespace\"\\n\"))))(Tile((id \
         ab846acf-deaf-4983-910f-d9a240f674d1)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bf7a339a-62eb-4233-99c6-86b33b7c9dc8)(content(Whitespace\" \
         \"))))(Tile((id \
         bb15217c-a2e5-4343-bd03-7ec0ab0286ff)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d32afc65-a5b6-4432-aa49-537da61bc22d)(content(Whitespace\" \
         \"))))(Tile((id \
         c6fc4e3d-4ee7-4161-a723-7e629139a7f0)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a88a0a75-ba02-464c-b0c8-20e48ec75768)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15be3150-e9c0-4064-862d-ebf793afd523)(content(Whitespace\"\\n\"))))(Tile((id \
         24458653-d40c-4b7c-b492-73968e9482e4)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fce525c1-67d2-4c2e-8763-d2d64ebe3b6a)(content(Whitespace\" \
         \"))))(Tile((id \
         d823ab18-a435-4832-904d-35504fcf9ddf)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         29b49044-4f92-4722-8ad1-98696fdcf989)(content(Whitespace\" \
         \"))))(Tile((id 9baa6b67-c9a5-43b5-af62-8412261fe60e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6ad84748-976f-4682-ba43-c18e42b52040)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9f2e78f8-3777-41cf-8c73-9d2c952bc3eb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a4206f8d-2fba-4728-bd1b-07c9acb02774)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         48b656f7-c38f-4a67-b6e7-285ba5d0dd32)(content(Whitespace\"\\n\"))))(Secondary((id \
         91d2b924-2810-482c-9587-c3e151eafec8)(content(Whitespace\"\\n\"))))(Secondary((id \
         af98ece0-681e-41a6-900e-66237b13bb50)(content(Comment\"# Actions the \
         gardener can take #\"))))(Secondary((id \
         f30cb0c0-4cfb-40b8-81bc-d4d22c08e92b)(content(Whitespace\"\\n\"))))(Tile((id \
         a1d6d3c4-ffb2-4a40-8b33-e2a312baad27)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93ddc8f0-c6b8-4b1c-b240-c99c75cb57c9)(content(Whitespace\" \
         \"))))(Tile((id \
         5c45749a-23b2-40da-9b71-96fc1154c142)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         28bb10e0-6ace-4d9b-9e17-b8246fc8489c)(content(Whitespace\" \
         \")))))((Secondary((id \
         d87c3fe2-0842-4e02-89a3-ec8515791d70)(content(Whitespace\"\\n\"))))(Tile((id \
         01e3b978-fb48-4f03-be44-f43e00cea177)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         67bd8f76-4d0f-4a49-96a8-667d01fb5829)(content(Whitespace\" \
         \"))))(Tile((id \
         24709008-2461-48a5-8f4f-156e2da66291)(label(PlantCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9f4c2759-b14d-4340-85c9-c144af98c725)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         fe6a7f63-0130-4d5d-a712-cd9f2692253d)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d679a3a7-59db-468a-80ef-2dae994ccc6f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f6aa5602-b3b6-4f93-9e23-c45a5769383f)(content(Whitespace\" \
         \"))))(Tile((id \
         113d6cc3-9210-46a3-9233-1622e37bd1f6)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d411411d-74c1-454e-a16b-4d8da78c3dc7)(content(Whitespace\" \
         \"))))(Secondary((id \
         2f057167-fd7b-4bec-8435-caa53a326c80)(content(Whitespace\" \
         \"))))(Secondary((id \
         ad658a00-e857-4b5e-9b80-a5c368407917)(content(Whitespace\" \
         \"))))(Secondary((id \
         3173127e-2c49-4808-8451-4d36b0c62c4d)(content(Whitespace\" \
         \"))))(Secondary((id \
         e6318579-2e6c-4f12-8793-64945a94220d)(content(Whitespace\" \
         \"))))(Secondary((id \
         594e0742-8349-47dd-8c2a-18e01548c05a)(content(Whitespace\" \
         \"))))(Secondary((id \
         6be508c2-29de-495d-927b-c144e4a43c1c)(content(Comment\"# Plant \
         current seed at position #\"))))(Secondary((id \
         931ab822-b4c6-4be8-9b92-12a6424d4cff)(content(Whitespace\"\\n\"))))(Tile((id \
         b28008c4-6d95-41bd-aa08-a672b37caaf0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7136d609-d833-4a50-bbac-d0d50ccd3d9b)(content(Whitespace\" \
         \"))))(Tile((id \
         a1bb7e6e-e518-4e44-9f37-3eb6e8b66190)(label(HarvestCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         08f735e9-16c1-42ac-9bf1-48767b94e0a4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a03b2f92-20f1-4b39-b2d0-1cea0ab0edcd)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         59cf4c49-40a3-459b-9566-decd23cb12dd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         82dc199c-4bdb-4d93-a225-2cbae4bcbc68)(content(Whitespace\" \
         \"))))(Tile((id \
         122a999b-62b9-40a4-8dbb-37c6ba289c57)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         46be928f-5815-4027-a3ab-504348edd42a)(content(Whitespace\" \
         \"))))(Secondary((id \
         741b3309-b803-4486-ac99-5c30b0e664d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         8de87e01-5291-4115-9733-f25331699c5f)(content(Whitespace\" \
         \"))))(Secondary((id \
         07bb01c7-d50c-4b22-9d32-628e3f73df02)(content(Whitespace\" \
         \"))))(Secondary((id \
         dddad486-167f-4113-b0f4-0d3063b2e11a)(content(Comment\"# Remove a \
         crop #\"))))(Secondary((id \
         ed7769c9-140a-4f00-aae6-b962c4638771)(content(Whitespace\"\\n\"))))(Tile((id \
         6535769d-f2da-4c95-b8c8-65245bcedd7a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         33b1b65a-7e23-43bf-8985-ac6a1f34c735)(content(Whitespace\" \
         \"))))(Tile((id \
         9a1ae369-11a9-45d4-9878-dffaac95f347)(label(CalculateHealth))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         80993304-1a5f-46bd-9267-bb9bbd9f08a9)(content(Whitespace\" \
         \"))))(Secondary((id \
         62b4f339-759e-4ac3-a49c-acf24e06558c)(content(Whitespace\" \
         \"))))(Secondary((id \
         61450830-1d7a-45ad-93f6-dd2f5d5f3182)(content(Whitespace\" \
         \"))))(Secondary((id \
         8f6f1e0f-610e-4ca7-aabf-65d9d68362ac)(content(Whitespace\" \
         \"))))(Secondary((id \
         10e80c14-f31f-4cad-ba06-b4a13d750a80)(content(Whitespace\" \
         \"))))(Secondary((id \
         c3a97520-29cc-45a5-bbd0-53c247df0b2f)(content(Whitespace\" \
         \"))))(Secondary((id \
         adc7a8d3-4ed3-4f80-a504-11f9885f94b4)(content(Whitespace\" \
         \"))))(Secondary((id \
         c7141217-f428-4d3e-a408-d590f3cb997d)(content(Whitespace\" \
         \"))))(Secondary((id \
         f83537d4-51d6-47be-a8de-6fbcfa952dee)(content(Whitespace\" \
         \"))))(Secondary((id \
         5da781b9-150c-493a-a363-41c31d3ddfb6)(content(Whitespace\" \
         \"))))(Secondary((id \
         f289a23d-83e7-41f1-88e4-567aaf34d682)(content(Comment\"# Recalculate \
         all health based on neighbors #\"))))(Secondary((id \
         d195e84c-929c-4f1b-ad93-5ad72a8a8b35)(content(Whitespace\"\\n\"))))(Tile((id \
         a81cd5f7-3bc3-44aa-9b3b-8f228b595f7d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3693deb0-13af-4fed-93b1-7ea129b304de)(content(Whitespace\" \
         \"))))(Tile((id \
         5d5595a2-c4ba-426d-86b3-6f14a84bf978)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c7f19f1a-439f-415c-a355-efe8fde48fb8)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6570b4ae-c714-49dc-b47a-0af5bcfc9049)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e2224436-5f7c-4dee-b718-ece120e07b9c)(content(Whitespace\" \
         \"))))(Secondary((id \
         f0d8914e-548f-4ba9-a213-18e49ea3753e)(content(Whitespace\" \
         \"))))(Secondary((id \
         11e40bb8-2210-4979-b7b2-db0de40668ab)(content(Whitespace\" \
         \"))))(Secondary((id \
         b9aa45e3-da15-43c7-9500-d68b827ce15e)(content(Whitespace\" \
         \"))))(Secondary((id \
         756bcbb9-333d-4d92-8537-0bdf53af654f)(content(Whitespace\" \
         \"))))(Secondary((id \
         f8351231-1a7f-400e-847b-e92ba953f02b)(content(Whitespace\" \
         \"))))(Secondary((id \
         32154d72-0cbe-46f2-a7fc-8d09b52b2ac6)(content(Whitespace\" \
         \"))))(Secondary((id \
         512ef966-32c1-4edf-b4cd-d9ea84c2f940)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce57c762-b08e-404f-9ebf-db3e5db585d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         cdc2b39b-8459-45d5-9d6d-c049830b9df2)(content(Whitespace\" \
         \"))))(Secondary((id \
         30fe6601-2d24-400e-8316-716b3dfbef2b)(content(Comment\"# Choose a \
         seed from inventory #\"))))(Secondary((id \
         25e1fd8b-ea0d-4ff9-947b-a531d27c5fc4)(content(Whitespace\"\\n\"))))(Tile((id \
         1cde75b9-6381-4e1e-a0aa-3b19d8a0099f)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1a7fcc5e-096e-4c3f-9b7d-09055f22bdd9)(content(Whitespace\" \
         \"))))(Tile((id \
         537df636-cede-4de8-b41b-1891697dc121)(label(WaterAll))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         71d1d80e-e10b-498f-be0e-cd90057593eb)(content(Whitespace\" \
         \"))))(Secondary((id \
         0ff23834-4100-4944-ab07-7ded78028863)(content(Whitespace\" \
         \"))))(Secondary((id \
         5104e02a-d42a-49fe-a4aa-2ccfa0c6a6c9)(content(Whitespace\" \
         \"))))(Secondary((id \
         572af805-84aa-4df4-ac53-a420b78250bd)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1ca91c4-b8a5-4e92-a11d-6d53152229ef)(content(Whitespace\" \
         \"))))(Secondary((id \
         5f975050-bf7a-45f5-ab54-733e15ea87e8)(content(Whitespace\" \
         \"))))(Secondary((id \
         9fa1e933-4791-4e16-86b3-36367ffe9664)(content(Whitespace\" \
         \"))))(Secondary((id \
         7cb320d8-e9cb-49fb-bb45-55b0abe3b2b4)(content(Whitespace\" \
         \"))))(Secondary((id \
         78dec1f0-617c-43db-82fa-c35bfd1669f7)(content(Whitespace\" \
         \"))))(Secondary((id \
         f3606131-2811-4d4e-ad2e-dadc7b218cf9)(content(Whitespace\" \
         \"))))(Secondary((id \
         4a979b50-99bc-4a68-9b91-3e82e1520cbb)(content(Whitespace\" \
         \"))))(Secondary((id \
         76a0c6db-ab11-4563-994c-32dd2b4fb274)(content(Whitespace\" \
         \"))))(Secondary((id \
         b83a04ff-42e6-4742-b9bf-b42c716ae802)(content(Whitespace\" \
         \"))))(Secondary((id \
         bcb1ee9f-81d7-4121-b47a-4da6c6f08fbb)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e638b4d-3bb6-4403-b6c1-a2f091cb480d)(content(Whitespace\" \
         \"))))(Secondary((id \
         d93c02fc-8848-429d-b8f1-7dd27e8318a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         f9a093a8-77c1-440f-88eb-8f792bac1cb2)(content(Whitespace\" \
         \"))))(Secondary((id \
         04687855-7edb-4715-8e68-54966350e159)(content(Comment\"# Boost all \
         health by 5 #\"))))(Secondary((id \
         7d05a159-af06-40ed-9410-f6f61aff1861)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         85eefd78-19ca-487f-ad5c-4fd046bb7ff7)(content(Whitespace\"\\n\"))))(Secondary((id \
         c87ebb0c-7529-4c39-8f95-688984bdaa7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b5e4817-b978-4cb4-9df7-697050c372f2)(content(Comment\"# Utility: \
         clamp value between 0 and 100 #\"))))(Secondary((id \
         7bb9b930-7c47-4931-8af9-94addcb0f276)(content(Whitespace\"\\n\"))))(Tile((id \
         f8a01868-2eba-4dea-8a39-16c28f5b932d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a0506d4a-2967-4640-a414-d673b81a36c9)(content(Whitespace\" \
         \"))))(Tile((id \
         c13f77da-6ccc-4f10-8730-34f4587bcaca)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         93967c1f-fc33-4ad6-bc26-7cb12d32b319)(content(Whitespace\" \
         \"))))(Tile((id \
         fccd2360-d18d-477a-83b3-07fe5e237937)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3907860a-6a90-49dc-9c1e-9ac27ec1bd49)(content(Whitespace\" \
         \"))))(Tile((id \
         e3e75efa-d910-41db-8d7e-ec69ab64e96d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ed9a4c30-57c5-4dc4-ac07-e09cffd510de)(content(Whitespace\" \
         \"))))(Tile((id \
         9e4f27d6-2aee-490d-b829-1fdefecccfe8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         401a1917-e640-486a-af5c-fcd1b6c45e31)(content(Whitespace\" \
         \"))))(Tile((id \
         af4381c9-0532-4176-9493-47a2c5b6e90a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b58a5b6c-3653-4b68-8885-a8fba40ec2d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         45ff36a8-98ea-4a76-bbae-2e23c1f2c87b)(content(Whitespace\"\\n\"))))(Tile((id \
         a2c94aa5-7d97-40e0-8c39-27d60b1a5a80)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         eaf0da4d-84af-4388-ac69-80d164bdfc0f)(content(Whitespace\" \
         \"))))(Tile((id \
         c1a71d81-290c-4835-bbd7-d64ca505e6ad)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eea5740c-646c-4f44-bc19-22bcd99af8db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d8bc129d-1351-4a0a-bf7a-d4cc33f0fb1d)(content(Whitespace\"\\n\"))))(Tile((id \
         c2a77780-fc36-4ba9-8b22-5e830a33789d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         905eb057-728b-42b9-9827-f3e0448a8586)(content(Whitespace\" \
         \"))))(Tile((id \
         b5168199-92c6-4479-ac9f-eae7ba87228e)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af9e53cb-391c-4f4f-a02f-9d4062da2bc3)(content(Whitespace\" \
         \"))))(Tile((id \
         aeb5724d-edd0-433f-ba4c-8768947c6f29)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e648c155-1a7f-4f83-a06a-0969b5618d15)(content(Whitespace\" \
         \"))))(Tile((id \
         e7c82ef9-e860-4c7c-963f-00be41cbf162)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         04f07524-eb37-495c-ad0d-849dea073397)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e03adf4-ad91-494f-9c17-5d2827315047)(content(Whitespace\" \
         \"))))(Tile((id \
         e36903ad-ebdc-4c57-98ea-eac22f07627a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32eecac5-3567-47e2-8b33-115a1006e91d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         099300c6-2e32-4c51-acc7-104a54997895)(content(Whitespace\" \
         \"))))(Tile((id 67986a78-3cd5-4a5f-a990-72a7520d5192)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         373f37bc-33c5-46e0-9efb-de6ebf0c40bf)(content(Whitespace\" \
         \"))))(Tile((id \
         33a326e3-cb0a-4a6d-87ae-fd0c54f4d58d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b49074df-6c47-495e-b2b9-bd40007c34c8)(content(Whitespace\" \
         \"))))(Tile((id \
         dc2ea017-b859-4a92-b6f0-25139b1a0469)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87ff3ba4-4b64-446a-8d9d-e01444da1b65)(content(Whitespace\" \
         \"))))(Tile((id \
         dfd0f164-6edb-4b75-ba3d-18db311edd7b)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ccedec9a-2033-4a03-942a-4b24d4804306)(content(Whitespace\" \
         \")))))((Secondary((id \
         3ae256da-a517-4f0c-a871-4141883c5020)(content(Whitespace\" \
         \"))))(Tile((id \
         96598299-202d-4dd7-8c6f-2bf8c64511f1)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6fc5b280-421e-40b9-964c-cf977b362b72)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cbc4940e-556d-4901-b46e-4f0e4a07d689)(content(Whitespace\" \
         \"))))(Tile((id \
         4d808299-9ac5-401e-9676-aef60bb7e496)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1eb395a-661e-4b22-8982-c9e4fc2f51cf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e4b2e569-e181-4f9e-a132-61d5b280ba93)(content(Whitespace\"\\n\"))))(Secondary((id \
         dac64f24-b806-47f3-b424-e1a84ee3fe46)(content(Whitespace\"\\n\"))))(Secondary((id \
         b082002c-1cc4-4b9d-a9f7-47aa458944bc)(content(Comment\"# Create an \
         empty cell #\"))))(Secondary((id \
         ad7523ae-67e8-4c65-97c6-38026de4d8b7)(content(Whitespace\"\\n\"))))(Tile((id \
         9879df89-e13c-4250-823f-203197f8205b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c9c4ec97-4947-422e-aabc-c03908fdfa44)(content(Whitespace\" \
         \"))))(Tile((id \
         08b12496-1cdd-4ea0-8fff-d8c70ecb70aa)(label(emptyCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ff895f53-3123-4da5-932a-f85d55f6a801)(content(Whitespace\" \
         \"))))(Tile((id \
         a8ac02d8-3566-4fd3-87e3-06fc410ddb31)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         799ec0a5-91f1-4ca9-b8df-4c9199eeac2e)(content(Whitespace\" \
         \"))))(Tile((id \
         edee821f-1178-4bbd-bfc3-afadfdf68306)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         697238d8-5a60-4884-b590-5113e30706bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         8a1ce648-a498-4965-bf65-bc9b28ecceb3)(content(Whitespace\" \
         \"))))(Tile((id \
         02187d3c-111e-4872-bf35-75c662b3613c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bc2489de-f78d-4b62-b462-98c704583ea8)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43f8aa3d-277f-44d3-8ce5-a173c989ceb0)(content(Whitespace\" \
         \"))))(Tile((id \
         387fcbea-55e2-4ea3-83f9-5839d93cfcc3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f7d8627-3651-4135-b670-f30b58923a9b)(content(Whitespace\" \
         \"))))(Tile((id \
         f32c45ba-29a5-4738-b3c5-c3e451645fb0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         137e73f5-8bae-4f30-bf2f-3524943d578f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b43a90d8-436c-4d80-b569-a8f6d0000701)(content(Whitespace\" \
         \"))))(Tile((id \
         43a333d4-82cd-46b0-9fc7-5e653b417bb9)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         009a0b0d-fd2e-4525-8989-e8f34a7beed7)(content(Whitespace\" \
         \"))))(Tile((id \
         3e6f42f5-8b3a-4ec5-ab89-f4ca81286950)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c3b6c56-e4c9-4da5-a4f9-95ae4e144e39)(content(Whitespace\" \
         \"))))(Tile((id \
         69957486-e4bc-4a3f-a5fb-06f81a4277bd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b3a1053c-5338-467d-a76e-f6dfb2194190)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d3243073-71aa-4b8c-830d-80706c91fc98)(content(Whitespace\"\\n\"))))(Secondary((id \
         23d89cb4-a0d6-478a-a200-40295e4b2f08)(content(Whitespace\"\\n\"))))(Secondary((id \
         73f6c252-fb74-4b9d-aa93-0e0e6c134814)(content(Comment\"# Create a \
         cell with a crop at base health #\"))))(Secondary((id \
         46e1c0db-7fb9-421a-814a-59aaa5f0e230)(content(Whitespace\"\\n\"))))(Tile((id \
         15b11630-4cac-46df-a737-2bfba3e66555)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         079b95da-e6ba-4955-b0d5-40836e19066c)(content(Whitespace\" \
         \"))))(Tile((id \
         2228ecd2-1f80-479b-8f86-e30817a381dc)(label(makeCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5db0d6d2-9fcd-487f-b96c-a15b8b3b4ec1)(content(Whitespace\" \
         \"))))(Tile((id \
         b30a5e52-bc9c-4d9e-bb71-9f0095dc23da)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         906e8a7d-9547-4d10-a700-8b7854f35850)(content(Whitespace\" \
         \"))))(Tile((id \
         0035e8f8-e872-423e-8c91-343ed6c722af)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         913b6f14-3861-43bb-aae9-7b0b33dca37a)(content(Whitespace\" \
         \"))))(Tile((id \
         ca8d9a9a-ce31-4079-9726-f7b1c847a17a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         67a5e9f7-63d0-4330-8bba-15fda9c34d5b)(content(Whitespace\" \
         \"))))(Tile((id \
         be7399fc-6fc7-47c2-94b3-016f839b3fe6)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aafb5f21-7919-4cbb-b2dd-8ca7cb59f823)(content(Whitespace\" \
         \")))))((Secondary((id \
         21f4932e-16b4-4bc1-85e8-2b67938353b8)(content(Whitespace\"\\n\"))))(Tile((id \
         3faea845-6192-4921-a1be-0fc5ce0349b6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b6be4aaf-8615-4041-9b17-35bc539f9d3c)(content(Whitespace\" \
         \"))))(Tile((id \
         a785856a-197c-41e2-b118-f749c7fa8dc5)(label(crop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1b44ebef-46b7-4757-9f7e-031d938f0d8b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         211eecec-6635-454d-b43a-deecc5ca5487)(content(Whitespace\" \
         \"))))(Tile((id \
         a6b4747d-dc38-431e-bc7f-648e01c488b7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         823aa7d3-98c4-4d4f-9047-28be9f2aa297)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9b80237-b4b9-4274-bd71-e333974a6880)(content(Whitespace\" \
         \"))))(Tile((id \
         69e017e6-8bca-4958-a250-3490fbe1d9fc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46a75c46-e3c8-443b-8d14-1f2725265079)(content(Whitespace\" \
         \"))))(Tile((id \
         af95ded9-cce4-4628-abf4-4f2e3c3b379a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9bea625-3a4c-41e5-85e7-942b810c3759)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16478bb0-c5da-4027-b290-2ce788f7bc98)(content(Whitespace\" \
         \"))))(Tile((id \
         466812ae-be1d-4820-8b9a-0877a7f795cc)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e12e4e89-f5fb-4354-9a82-874641494f0d)(content(Whitespace\" \
         \"))))(Tile((id \
         8ae9d0af-8769-4220-a7c1-ccece4d133be)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2844c5a3-f1e0-47c3-b43a-c56bd1b11b08)(content(Whitespace\" \
         \"))))(Tile((id \
         28465999-94bd-434c-bd4a-ca3c35a23c47)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5c2edae7-b541-4c4c-aaaf-75d08d2758b9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e0ff08c3-33d4-41ad-b9c6-b2d87eb8ac4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bca900a-ce29-4cb1-a546-b9cae9b3b3f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         81c6fdd1-7fab-405e-a760-71e596ea5134)(content(Comment\"# Determine \
         the companion effect between two crops #\"))))(Secondary((id \
         8c5dacf0-84e1-4fc0-8998-594f763c06c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fa5bbf2-7363-414e-9116-640da25d0ec8)(content(Comment\"# \
         \\240\\159\\140\\177 and \\240\\159\\140\\191 are companions \
         (Beneficial) #\"))))(Secondary((id \
         a3b22017-78ea-462f-ad44-131353d3da01)(content(Whitespace\"\\n\"))))(Secondary((id \
         34695c93-0358-4595-b880-e3a4895e8030)(content(Comment\"# \
         \\240\\159\\141\\132 and \\226\\152\\152\\239\\184\\143 are \
         companions (Beneficial) #\"))))(Secondary((id \
         95670c4b-2b62-446e-937f-39bf630b04e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         13e0632d-3f82-4da7-8cd0-bba8a8495967)(content(Comment\"# \
         \\240\\159\\140\\177 and \\240\\159\\141\\132 are rivals (Harmful) \
         #\"))))(Secondary((id \
         f51dd494-3bbb-4ccb-a9f6-ae1c8c649b21)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea0d5675-1e56-4585-a8b5-7ce6fccabe80)(content(Comment\"# Everything \
         else is Neutral #\"))))(Secondary((id \
         6291e8ef-f3b0-4f6e-9f26-ccc764847461)(content(Whitespace\"\\n\"))))(Tile((id \
         efc9bc05-f5f1-4fdb-99ef-ff059e6d1158)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7ca89ecf-4a97-461d-a32d-da026d1ade9a)(content(Whitespace\" \
         \"))))(Tile((id \
         578c4df0-6fdd-4c76-a374-fce05b0eadaa)(label(companionEffect))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82c72d31-e53b-4cf1-8abf-5cdde0df5d60)(content(Whitespace\" \
         \"))))(Tile((id \
         1801291d-27f2-4d78-a47d-5bddb4bd0cd8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         18fb5983-d37b-46e9-a926-b2ce94423f69)(content(Whitespace\" \
         \"))))(Tile((id \
         7e0d86e0-f748-485a-b280-fb7e430ee306)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         70114aaa-9d88-4702-bcc5-7c7527819631)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8a2730fd-afd5-418b-afa4-d82911b7c7ed)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41af9bd0-f032-40fc-96a5-2da51907a694)(content(Whitespace\" \
         \"))))(Tile((id \
         fdbe777c-5829-415d-891f-a6cd7909245e)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0ae03281-526a-490c-a2bd-e79dbaea6cef)(content(Whitespace\" \
         \"))))(Tile((id \
         2b4c90cd-052c-4774-b910-1700ccc8e6f2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c9b031cf-c0fb-4e24-8666-dc7a76d0c0c8)(content(Whitespace\" \
         \"))))(Tile((id \
         d9c36ec0-4cb6-4e7a-9c3f-e37759ff8e54)(label(Effect))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         95febf54-ee6c-49e8-8202-460e59db0339)(content(Whitespace\" \
         \")))))((Secondary((id \
         7d9d2170-2114-45b0-b24c-23d8bb3d4eaf)(content(Whitespace\"\\n\"))))(Tile((id \
         7caef608-9b49-428f-b20c-ecd2d95ff879)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         efcab13d-5cb0-4e00-abe9-255dfb4beeb8)(content(Whitespace\" \
         \"))))(Tile((id \
         0df81428-9d47-4399-a660-b463bcc73b14)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c60be2c0-1570-407c-8660-60b1902e6ded)(label(crop1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c7592bf6-1122-4c20-a716-fc73d22d3a7b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         404c90db-b3cb-4e02-805f-7101b69d5802)(content(Whitespace\" \
         \"))))(Tile((id \
         548dbea0-4399-4ae9-865e-1135d109c434)(label(crop2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         12f6c9b1-e7d8-41df-9276-71ea077da168)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb4ae4c9-fb1b-4ec4-85c4-4eef1850b92d)(content(Whitespace\"\\n\"))))(Tile((id \
         9d1410f4-3a65-4122-91ee-321e74cc0227)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee015542-9053-428e-8671-5c74976e7751)(content(Whitespace\" \
         \"))))(Tile((id \
         ca031b29-582c-4898-93c5-8c9df0a8bc88)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5266b0d5-665b-40ca-b0b4-471f6bcf35b2)(content(Whitespace\" \
         \"))))(Tile((id \
         e6adb332-187c-44fb-a958-61a2180ab15d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2f684c1-13e1-4089-b4e8-dd3fe8e927ff)(content(Whitespace\" \
         \"))))(Tile((id \
         297adbc4-adc2-4c61-9e30-6816b965cde8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b321ed06-c6af-45f6-bc72-2d093a404dc4)(content(Whitespace\" \
         \"))))(Tile((id \
         ae7fb99d-8d70-4791-a281-178b81dcb4da)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa50f92f-cd0f-4e92-b935-b071a286b0bf)(content(Whitespace\" \
         \"))))(Tile((id \
         c975c149-9c8c-41ce-8817-8c3972224da1)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41aafa05-7a34-4eff-b2fd-de69d4c12eec)(content(Whitespace\" \
         \"))))(Tile((id \
         eba4193e-55b0-4118-a027-c6ad31ed3871)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35fd5f47-5a11-4966-bad1-01699942030d)(content(Whitespace\" \
         \"))))(Tile((id \
         efb8091f-c0bb-4ef8-90fa-983f7b7ce7cb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         436de381-966c-4886-b0c6-f04a1e810e05)(content(Whitespace\" \
         \")))))((Secondary((id \
         d08ab890-5274-48ab-9fe7-aac861465b5a)(content(Whitespace\" \
         \"))))(Tile((id \
         84e3c2cc-97a2-46ba-b2ad-79ad369951d1)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19ff6f31-336a-4828-af0f-1d0e668cec15)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         04a1b43b-f80e-4220-936b-de6074973a4b)(content(Whitespace\" \
         \"))))(Tile((id 12dafc38-0421-41fb-a3f1-8387d7a650ad)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         880d6884-6d1c-4f30-b3af-7eb2405f1267)(content(Whitespace\" \
         \"))))(Tile((id \
         fe30cbc5-f902-4c58-8428-7bddace28861)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf3d7c6c-33a3-43dd-a81a-22702d91fe15)(content(Whitespace\" \
         \"))))(Tile((id \
         f9e2d5aa-b84b-4c65-b7de-645fdd59687b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         baccd8e3-6a11-4927-b3cb-fe26b4c0d9bf)(content(Whitespace\" \
         \"))))(Tile((id \
         587ebd10-e278-45b5-b035-eb18cac8605e)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1745b99c-2b82-4c44-946e-f0d5587c6117)(content(Whitespace\" \
         \")))))((Secondary((id \
         fcadb873-3bad-4637-82b3-60374fb46762)(content(Whitespace\" \
         \"))))(Tile((id \
         7ae9856f-2b5f-41de-8d76-899fffd52e06)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         593c875d-297e-4f5a-b1e6-0993fd3bde61)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b9082ae8-29fa-455f-b09c-87a57019e328)(content(Whitespace\" \
         \"))))(Tile((id 65b0b225-c089-436a-887d-576835628c1c)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         b7832488-c2c5-4826-a0ae-dd7a9f05df7c)(content(Whitespace\" \
         \"))))(Tile((id \
         620d4201-821a-484a-814c-c48565074fdc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         abf88f25-dbcd-4dbb-aff6-b0ec22d33586)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6f4bc0dd-fe19-4f09-ac3a-0c8a3e877fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         1fe39f38-bcb7-478f-aa24-2c707003aaf8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f986c34-2bfb-4817-95e8-4a264f7a2639)(content(Whitespace\" \
         \"))))(Tile((id \
         07b8ab30-64f6-4fbc-9ca0-44a17462b084)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6af7cd01-1a89-4796-9cc6-4a577f81a7f1)(content(Whitespace\" \
         \"))))(Tile((id \
         fee22cd1-b6c9-46ea-bbba-57f88d9d8275)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77fe6a97-b7fa-49e5-a1ea-b389246c2a31)(content(Whitespace\" \
         \"))))(Tile((id \
         60e82462-c10c-47a5-886b-89ef622f885f)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92d050ce-8b2c-43c6-8421-881a92624844)(content(Whitespace\" \
         \"))))(Tile((id \
         ddb5cfe3-8117-4454-9f85-508c59a07c56)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cf33cdd-0632-46f4-b1ff-3fcd08912185)(content(Whitespace\" \
         \"))))(Tile((id \
         7a3ef4e6-a496-4031-ab6c-ed3245813cd0)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3f935226-f8ff-4d8f-a39a-68dd96da0b4f)(content(Whitespace\" \
         \"))))(Tile((id \
         b425b792-bd37-4882-b3dd-71bb640d21a1)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a24c27ce-93ce-42a1-ba8b-ac27dc10c4d9)(content(Whitespace\"\\n\"))))(Tile((id \
         60ae5506-68e6-4d67-8566-3f969740d81b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         41f2c78a-aa97-45de-8707-f4700137953c)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41c1de18-7e0f-4e5d-8788-71446c9343fe)(content(Whitespace\" \
         \"))))(Tile((id \
         7e0dcb97-96dc-43e6-ae00-6370e7204430)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6fcb46b-8f8a-4b90-b642-6e5e658331e8)(content(Whitespace\" \
         \"))))(Tile((id \
         25aa8e5f-18f4-4446-92be-2a4df5e84f69)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd335c8c-bbc5-48b3-91b0-7697222b1084)(content(Whitespace\" \
         \"))))(Tile((id \
         258c0cfd-a4cf-4056-8916-3fd82f28f2c0)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a870f47-e49f-4612-a06f-f8dcf8a0a83a)(content(Whitespace\" \
         \"))))(Tile((id \
         188710b6-1f41-450f-a9ae-b91ca9147959)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7560c586-ccd3-461e-90b8-5c4c276a8dca)(content(Whitespace\" \
         \"))))(Tile((id \
         127d7473-c467-4ac2-93c4-1205e23f22a2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b1f705a-0ff2-4929-8314-3ff8b2275be7)(content(Whitespace\" \
         \"))))(Tile((id \
         b791eb99-d256-4d17-9166-57c11c8f49e6)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45bf8f41-49f0-4425-9d0d-f190a2f33075)(content(Whitespace\" \
         \")))))((Secondary((id \
         c176dec4-e7f0-46fc-8def-18d89daf208d)(content(Whitespace\" \
         \"))))(Tile((id \
         b3910f90-ff25-4166-894a-34dfa315d3a4)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb69d64d-e240-4e88-9a53-8e2bd2b5da5e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5f1d5850-e919-4d5f-8bcc-3be3fcc44b37)(content(Whitespace\" \
         \"))))(Tile((id fe51658b-daed-48ea-84fc-d6a0ffa15b3f)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         dcde6ad6-10c4-40af-97db-59b1314e304c)(content(Whitespace\" \
         \"))))(Tile((id \
         ab03ca01-cedc-4583-b722-0ac7b169dc8c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8cfcf095-68fa-48b6-8d07-3497fa0a7fd3)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7da57b9b-93c1-47e4-be00-bd2ca75273d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0358b142-d1b3-40ae-bdd7-b60f73961c43)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         baca6677-57dc-4299-b4fc-f14e1c08d6ea)(content(Whitespace\" \
         \"))))(Tile((id \
         7a7fdf5f-5a01-4421-8595-060b8b335801)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee306374-71ca-4428-8bb6-822d1e8dafc7)(content(Whitespace\" \
         \"))))(Tile((id \
         d30bc3ed-4aec-4fd4-9900-e2b7e313a140)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2684325-4061-4b24-8312-76e55c79cae4)(content(Whitespace\" \
         \"))))(Tile((id \
         5c32655c-4cf2-474c-ade1-ba9f57777975)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fffc3c9a-8174-495f-b183-12b1fe971321)(content(Whitespace\" \
         \"))))(Tile((id \
         2d3fc34a-6056-40e5-95f7-ec89175d46df)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d525781-127d-439b-9ca6-cec11bfc2cfb)(content(Whitespace\" \
         \"))))(Tile((id \
         ca7508ba-d5f6-44ff-bb2c-dc60d0db067c)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         798590dc-a442-41a1-a0fb-156cd26a5b75)(content(Whitespace\" \
         \"))))(Tile((id \
         cb521e61-794d-4edc-be00-0b2aac72c368)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f44c42f-606f-490f-a706-c44889d9fe28)(content(Whitespace\"\\n\"))))(Tile((id \
         cb997f0c-2455-4412-91a0-84196b9853f1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f9031960-1bd4-497b-8ec6-1834fba3778c)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21157153-808a-41ba-b022-7a1fa53d1207)(content(Whitespace\" \
         \"))))(Tile((id \
         a4c87823-6a34-44a4-a9bf-64094c306db9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fec34d5-c882-46e2-9afc-22e612c66ac8)(content(Whitespace\" \
         \"))))(Tile((id \
         68c2b239-8bc1-4ecf-93bf-e3c0dacfcb54)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         68e9945d-30a9-4850-a695-46653535e200)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b36edd-6848-4df7-95c8-397d7562a5b4)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a6ab24a-4300-4c44-ad3b-777cf7fad9a9)(content(Whitespace\" \
         \"))))(Tile((id \
         8ec82da9-4434-468b-aa98-dc85ae8688cc)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         37e97f87-4194-4c00-9c26-17ec0868081d)(content(Whitespace\" \
         \"))))(Tile((id \
         226482e8-e1de-47a3-aa35-66478ee10f44)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51886a19-162e-464b-b198-a0706275bb98)(content(Whitespace\" \
         \"))))(Tile((id \
         0ce45bce-5e90-4cc1-a457-56a4709a324f)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d3b0ddf1-6382-4946-9369-1cd9eb4f19f8)(content(Whitespace\" \
         \")))))((Secondary((id \
         66b31d83-1f5d-4118-a07b-33c30b5e604a)(content(Whitespace\" \
         \"))))(Tile((id \
         ee01f0c8-14ff-4e0e-8735-a5367e3ef2fb)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d2506d4-5ef7-48ab-ad69-f4a3643aed14)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         36fa9ddd-8812-4e66-b957-c01453baa539)(content(Whitespace\" \
         \"))))(Tile((id 6ac6e3b4-6d42-4c68-be6e-7f3f1de665e8)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         16a69dcc-2672-4954-a754-069ff490fb76)(content(Whitespace\" \
         \"))))(Tile((id \
         2ec488f9-3a36-42cb-b4f9-a9c8b75105c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         19eae338-8713-4080-9e21-e18ae8a7783a)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c70bac49-97ba-4974-8ccd-1a0748a46c10)(content(Whitespace\" \
         \"))))(Tile((id \
         b4ce751c-ee46-4a01-aefa-98d65fe8056c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7bc072d-6d06-4bbd-a319-8fe13235f472)(content(Whitespace\" \
         \"))))(Tile((id \
         7a7f21b8-1c33-4d4d-9cb4-5fb3c341ec2c)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         924d13a1-8cc3-46f4-90a3-def19eeb4652)(content(Whitespace\" \
         \"))))(Tile((id \
         e87d4d3f-4b1e-4673-a321-3005674f7864)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22f3bab4-cab1-4d81-a73a-9d801c42e566)(content(Whitespace\" \
         \"))))(Tile((id \
         a9628251-6d44-4d6a-9a50-b8a390b902e5)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         591a6e67-b8de-46ff-b910-d8e94ffcb43a)(content(Whitespace\" \
         \"))))(Tile((id \
         1b49b81e-c066-4097-a33e-34ddf9d4b88d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd6d206f-8f9b-4de1-8c5a-003e9639058e)(content(Whitespace\" \
         \"))))(Tile((id \
         2e03ca86-ec78-49a1-8291-c881d4dc0014)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         375c6b42-1dc4-4acd-ba5f-a5b1018ab901)(content(Whitespace\" \
         \"))))(Tile((id \
         7fe80e74-7f16-43ce-a1f5-aed7235db842)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c6ded14-7a4b-4af6-aef6-44b2342d831a)(content(Whitespace\"\\n\"))))(Tile((id \
         f59c51bf-b0c4-4a68-a3ce-463a514abb80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e49f52b6-78b1-4c58-ad9f-51ae1688db1a)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1b58d0de-e04e-4aab-9ba4-717eac43cbbb)(content(Whitespace\" \
         \"))))(Tile((id \
         788c991d-6cde-43d1-abba-003d13d9c8aa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         101d73c2-d0e4-4ff6-9bcd-1cf1167c8bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         3d8bf159-7822-4731-aded-ee2f8a06d390)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6191b558-c8b8-47e1-b9f4-cefc1a272ec8)(content(Whitespace\" \
         \"))))(Tile((id \
         2aefe4d6-94cb-4ae6-9c35-487c032512f2)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7aa06e5d-b131-4d26-b4ae-3d12e1afd1fe)(content(Whitespace\" \
         \"))))(Tile((id \
         af482a95-25af-40eb-9ed3-9bfe0fc62318)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f071a86-f056-40a3-b92f-fde18e056697)(content(Whitespace\" \
         \"))))(Tile((id \
         248bb5cd-6777-4ee1-b993-cd44fe19ba71)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c6cfe2c-802b-4959-9033-6e70cead4296)(content(Whitespace\" \
         \"))))(Tile((id \
         0048a5fc-1553-4c39-8fa8-20e12edc13e9)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bed30610-d1f5-42fd-a657-45e08b1d0e54)(content(Whitespace\" \
         \")))))((Secondary((id \
         df9fcd9c-f443-4eb8-b03a-5e634701bc62)(content(Whitespace\" \
         \"))))(Tile((id \
         c9a06dbb-5a2c-40c1-a6b9-44c8daef82a3)(label(Harmful))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b3c95d8-66d7-4c17-99c4-74245ffabd69)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4869e097-5f74-49e8-88ed-bd00eb6a3fb5)(content(Whitespace\" \
         \"))))(Tile((id \
         b9de9073-f4f6-40e5-863c-6fa01c84a5f0)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1384d58-7cdb-4a1c-bade-002348ed3354)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f82cbe09-892a-4357-bb1b-158f157066a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         c25a64e4-dd4b-4cb6-96da-158318686f49)(content(Whitespace\"\\n\"))))(Secondary((id \
         909572f9-7c40-4b7b-ae36-a4506873066a)(content(Comment\"# Convert \
         effect to health modifier #\"))))(Secondary((id \
         e09dcb9d-cfd1-4e48-8ed6-533e86e65ec9)(content(Whitespace\"\\n\"))))(Secondary((id \
         861285c1-ef58-4d84-825f-66f81af3e175)(content(Comment\"# Beneficial: \
         +10, Harmful: -10, Neutral: 0 #\"))))(Secondary((id \
         cc621231-1fe0-43a9-b785-fe5d4b87b146)(content(Whitespace\"\\n\"))))(Tile((id \
         76475a97-f0b3-4fd4-9701-9dc30a707357)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea13e44f-c212-4140-81b4-a52767abcba1)(content(Whitespace\" \
         \"))))(Tile((id \
         cc062bf9-b26c-4c88-ac0b-ffccba6f6332)(label(effectToModifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f4929b0b-ca81-4f8f-8c23-bdd79bdf198e)(content(Whitespace\" \
         \"))))(Tile((id \
         5811c424-8caa-41df-a953-c1b6b63cf5e1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fe5320d2-9a5a-42e4-9b07-a2055a9f94f1)(content(Whitespace\" \
         \"))))(Tile((id \
         0c03baae-c044-4725-97bd-784add058b44)(label(Effect))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         af382e76-8c44-4031-aeb1-e27ac3469869)(content(Whitespace\" \
         \"))))(Tile((id \
         a9eaaa75-b172-4e3b-8219-3e4bbceacef8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9de5da11-4d6e-49b1-9238-4279bace479d)(content(Whitespace\" \
         \"))))(Tile((id \
         f3219858-e891-4f44-a58c-b77afd0b4487)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5c309d05-196b-4578-b574-e79c3112d81b)(content(Whitespace\" \
         \")))))((Secondary((id \
         4392e0fd-584c-4aaa-bda2-f5bfbb26a717)(content(Whitespace\"\\n\"))))(Tile((id \
         69ce49e6-8c47-429b-abfd-95e612c6ba1b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b09968ba-29c2-457c-a989-f1cfd3978f5c)(content(Whitespace\" \
         \"))))(Tile((id \
         34f911cf-4456-4bb1-a2dd-f7cbe6f635b7)(label(effect))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         443e5fb6-000f-405f-9579-3c83b78ea883)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7ef10cb4-a627-496a-8e9f-1fc6ccb0d9df)(content(Whitespace\"\\n\"))))(Tile((id \
         b231be88-aa81-4a44-91a9-bfcf1af31fd3)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         074cc8f5-f0ff-4607-a9b7-e0bc85d685c2)(content(Whitespace\" \
         \"))))(Tile((id \
         0acc0244-4f6c-4796-9961-1d3a49d0aeae)(label(effect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         65b0bcd4-b641-4dc9-bc98-996d8d4482fa)(content(Whitespace\"\\n\"))))(Tile((id \
         7b036c0d-9b24-4273-83f1-fd327126a85a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e97087c7-519a-4f58-8712-b3d821787d72)(content(Whitespace\" \
         \"))))(Tile((id \
         c5637464-183d-4d92-ac8b-253f7e8ee66d)(label(Beneficial))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9c955507-0a3b-4e7b-a5cb-99e53e6b887d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         82f58f8a-e175-439b-99f0-9bc5e0484a4a)(content(Whitespace\" \
         \"))))(Tile((id \
         4153891d-ddde-4588-b31d-c1ec290b79f0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ced02371-e784-4161-9a22-a7903ea38a64)(content(Whitespace\"\\n\"))))(Tile((id \
         152d24fd-e71f-4f4a-aaea-14799765cc47)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8244d3da-dbfd-454c-a09b-23587394ecb1)(content(Whitespace\" \
         \"))))(Tile((id \
         8ce91aad-56d4-4b8d-8d12-123e5cbf490f)(label(Harmful))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ad9d6a15-327b-4952-800e-061812d28ff9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fb5a9ce6-a068-4af6-b8de-90c6357d74f8)(content(Whitespace\" \
         \"))))(Tile((id \
         82c4a1eb-bf45-4916-8974-4552d41ab3e5)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69991498-bf5c-4723-bdf6-0fff761e451f)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad6b86e3-3259-44e9-8062-b57c50a08d08)(content(Whitespace\"\\n\"))))(Tile((id \
         fa5b12ca-40fe-428f-a08f-e77825da4d88)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bdfa4883-4ac8-484b-96ce-9145934425e4)(content(Whitespace\" \
         \"))))(Tile((id \
         a1580ea7-a4f8-40f9-9b43-af75b6c98e59)(label(Neutral))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bf872cee-0f27-47b4-8133-613937c4d732)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c3aef7a4-c137-4a8c-bcda-d94d7abccb0b)(content(Whitespace\" \
         \"))))(Tile((id \
         f68e0abb-abfd-46d2-9d3c-f72a6f373f15)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f005ca67-08fe-4a05-90dc-596965d8f8b1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee518688-5808-40fc-b191-fca7b2ed408b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cb30950d-9a8e-4e59-a8f2-57885c56b415)(content(Whitespace\"\\n\"))))(Secondary((id \
         c653d5e3-731d-4f1d-b7c0-1658fe34a7d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f8120a7-2333-497a-90f4-653656448bb8)(content(Comment\"# Get cell at \
         position, or empty if out of bounds #\"))))(Secondary((id \
         a362b188-8ba8-458d-9c75-135db604d4a9)(content(Whitespace\"\\n\"))))(Tile((id \
         189db5c5-aa90-4665-9794-6c17388a418e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0a97ec1c-7c9d-4ae7-b95d-5ab7d920986d)(content(Whitespace\" \
         \"))))(Tile((id \
         9d3b1e97-9653-4040-86bb-6c62c739fc74)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6dd57421-ca05-48f0-9796-567cade0db0d)(content(Whitespace\" \
         \"))))(Tile((id \
         a291754f-59e5-4802-a367-b63722d9321c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         45941ee4-4825-4dde-a615-a0e4ccaa6633)(content(Whitespace\" \
         \"))))(Tile((id \
         21ab8ce1-9823-4b57-9c9b-03d6a439409e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e0b5b18e-b61b-4096-807a-62d6cc06485f)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         68499023-87b5-49f7-88f6-ea30c8ac77a6)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d390bcce-e2cb-4e5f-ad2f-572ed3134922)(content(Whitespace\" \
         \"))))(Tile((id \
         c81e8a6f-32b1-4d3d-a066-d1b48b02132c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0eb23e40-8094-442f-b410-96187d92fc8c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f552ea6e-fdfe-493d-8c12-dc541b6659a4)(content(Whitespace\" \
         \"))))(Tile((id \
         521be2a2-3791-4d35-b5e0-ff3050161325)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4c8bdeaa-5570-4bdb-af65-9d51c08b3ca3)(content(Whitespace\" \
         \"))))(Tile((id \
         2bfd2244-8ebd-4a3b-b9c0-649bc87ac402)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ed4ca6bc-c83c-4370-a121-ca2df5157093)(content(Whitespace\" \
         \"))))(Tile((id \
         43a0b4d4-6fde-4a53-9f79-5632ad7a7f7a)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         32baf11d-39e9-437f-9893-1a9a9413ea9c)(content(Whitespace\" \
         \")))))((Secondary((id \
         c28133ce-1c28-4661-bdf4-a608ace4f02f)(content(Whitespace\"\\n\"))))(Tile((id \
         35046ce1-1d4f-4f67-af26-60c47ddbe0c3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b4fe49eb-b006-4b69-b9e9-6e8db534c075)(content(Whitespace\" \
         \"))))(Tile((id \
         295ac48b-92b8-452f-910a-e7b907443cd4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         775418a7-f6df-4c6c-aebf-52cd09595b49)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ef991bbe-5039-46b2-b95d-a83ff9f9be73)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7095bfac-be02-460e-9089-52a823ebccad)(content(Whitespace\" \
         \"))))(Tile((id \
         14840932-3a83-47b9-83dd-fbc7a8c047f2)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         03814a92-74cf-4094-9e0c-74efa90dd345)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         262bc0ff-9a47-480f-b3ad-c5d0d5e97156)(content(Whitespace\" \
         \"))))(Tile((id \
         c1635b16-f171-4175-bdc0-6a7752996d3f)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f92ab1c1-af71-4eec-8393-e7b31ec72e06)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         86de01ca-b7bd-49a8-afe2-75c1e752e848)(content(Whitespace\"\\n\"))))(Tile((id \
         e33daf22-5bb7-42a9-9b94-b1ab87b9448d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f65a7f4-7aa7-4235-aa67-da4c35e5f089)(content(Whitespace\" \
         \"))))(Tile((id \
         da0d78cb-264a-4241-9fa9-f3681133dca1)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ae2de745-14ba-4281-bab8-fcb0475e5eb6)(content(Whitespace\" \
         \"))))(Tile((id \
         e0583cbe-8c06-4caf-8176-b09cc2dffa45)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         352f8c43-babd-4400-b15e-0921f4169c11)(content(Whitespace\" \
         \"))))(Tile((id \
         5774316b-0711-4dd3-b5c1-f12f08b6c348)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         761233f1-eb00-4aec-88bb-f8f1545d3132)(content(Whitespace\" \
         \"))))(Tile((id \
         c63e31d6-4ec2-443e-a292-58702aa36035)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4ff0240-3863-4130-a2e9-c204869d24bf)(content(Whitespace\" \
         \"))))(Tile((id \
         80ae26ac-88cd-4d27-a10e-c676281a1102)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba44e76f-a70b-4541-bbe9-a89c55e5ddbe)(content(Whitespace\" \
         \"))))(Tile((id \
         7327d28a-d1cf-4f03-8a6e-2f677f5dae3b)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b210274b-34ed-4cb1-801b-8300a3af0c1b)(content(Whitespace\" \
         \"))))(Tile((id \
         3a890286-408c-418e-9b73-872ddd0ff678)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b72bcfc6-ea63-4365-9067-301e556104bf)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4a9d51a-b91b-4b9f-afae-797edcca25eb)(content(Whitespace\" \
         \"))))(Tile((id \
         9c7c220b-aed0-413b-9307-cbbce1cb7d1d)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2dc18a60-2a85-48b9-8199-7d35a46468dd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8b2ba9c8-c5d1-4686-9746-3133259b2a1b)(content(Whitespace\" \
         \"))))(Tile((id 17dc21b2-d7fd-4216-a582-cba4e69280da)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         1d01f4a6-c8d9-43c4-93dc-95d27c5eb89b)(content(Whitespace\" \
         \"))))(Tile((id \
         552aef82-215b-4288-b195-cdc248dcb88c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9323cb4-28cc-4b00-ac3e-f1cb42f75cbf)(content(Whitespace\" \
         \"))))(Tile((id \
         6f60b7cd-a58e-489d-8954-23c6b4b4135b)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71116fcb-3480-4dfe-9421-3c246bf47cfc)(content(Whitespace\" \
         \"))))(Tile((id \
         132fe78c-84ee-4a93-a58a-a8408e374d25)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         193e7195-d238-45cc-809c-29d7f5edffea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db7f889e-f61e-43ec-a09b-346fff9d004a)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9bbdbe3c-2093-4322-8b3c-48782dca45f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         4ec7fcc3-6276-430a-a129-16758f8413ab)(content(Whitespace\" \
         \"))))(Tile((id \
         dc954849-0058-435f-924e-9dedb1c22375)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         20d1f38a-ffc4-417c-b6d9-2e7ca2bde4aa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         adc0ec5d-1b55-4a70-91e5-9bba9b72472d)(content(Whitespace\"\\n\"))))(Tile((id \
         3fa1b69b-8b3e-4fde-b56f-1b1f799812df)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b2d6b336-dea0-4367-a2e0-471ebf1814e1)(content(Whitespace\" \
         \"))))(Tile((id \
         392e50b8-67f1-4d84-a7ce-8f0ed778333f)(label(rowData))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3715d593-f296-420f-b9a6-e35fd2048bdd)(content(Whitespace\" \
         \")))))((Secondary((id \
         a3c43cdc-e357-4eda-9ac1-a93aa462f01c)(content(Whitespace\" \
         \"))))(Tile((id \
         dd6f772c-b25f-43e8-ba1e-2291286ce4dd)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a783f228-bbe2-4e99-9af5-508575c68fdf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9710cbca-4c9c-4960-8f36-604279d3ead7)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60fcd1c2-7da2-46c6-aae6-8654cc85bd50)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b9e4f6f-72fb-44e3-846d-94359d1ebac3)(content(Whitespace\" \
         \"))))(Tile((id \
         850b54d3-13b6-4daf-94d5-2da10c073824)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d1e5ecf6-c453-462b-9aca-efa2e7c71b89)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         003feb31-15c2-4d81-b2e3-86a0554b608e)(content(Whitespace\"\\n\"))))(Tile((id \
         ef069050-5f23-4c7b-b59c-4143422c0572)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         522aef74-9b38-4e42-94c6-6fa288e11437)(content(Whitespace\" \
         \"))))(Tile((id \
         55e9efbe-2290-4e4f-9b30-a9d900fe0fec)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd0f8b0c-6772-46a6-8575-e0aa32e8d0c6)(content(Whitespace\" \
         \"))))(Tile((id \
         0a92d92d-bbc2-43e3-930f-6b30a0812ae9)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3083a37-4106-4102-a70a-cf787b5d62c5)(content(Whitespace\" \
         \"))))(Tile((id \
         ff1ed69d-2548-47e3-be3d-b168447cb364)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df1b197a-b8f6-462c-bc8a-4cdab1449bbc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4354e7e5-e4c4-4988-ab1c-7204a7ff4b71)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0a7a8ce2-a6f6-40a6-9f11-5257db07969f)(content(Whitespace\" \
         \")))))((Secondary((id \
         c1c702c0-a65c-42ea-a4d4-d9ae64d12533)(content(Whitespace\" \
         \"))))(Tile((id \
         fd05e5e7-1007-48d7-a77f-a1c88596d52f)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         311d55be-b42a-4d4a-8c0b-a9e2620e0af1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f8bcee33-0d45-4064-af20-e97398ade15f)(content(Whitespace\" \
         \"))))(Tile((id \
         66261da7-7ed5-46d9-8a20-be9f90a8d41c)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce1a3c52-3e04-4e83-8cbb-7f2d0d1a8805)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         01aafce2-3cf9-4b72-bedc-b31329c2a6bb)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8cf0bc14-1b52-492a-b082-36bed3cec766)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ecf6112-b076-4169-91dc-93bd05c227ca)(content(Whitespace\" \
         \"))))(Tile((id \
         b84e53d3-582e-43c2-8c8f-a59a4bae74fc)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         480a4431-354f-44ac-9987-4760b283185e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1cdb59c9-2789-475a-adbb-6493aa575ba2)(content(Whitespace\"\\n\"))))(Secondary((id \
         33513aa5-447c-4357-8123-ae71db59a779)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb0beb3f-b4bb-45c0-8ebb-535e548a7028)(content(Comment\"# Set cell at \
         position #\"))))(Secondary((id \
         4a0e8e70-c521-45e8-abb3-c9144dfca3a2)(content(Whitespace\"\\n\"))))(Tile((id \
         0b787ae5-9e5b-4161-adb8-ac01dbcb4461)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da37e93f-ad7d-4302-9712-d3a4af60f3e1)(content(Whitespace\" \
         \"))))(Tile((id \
         5723fb51-a0a3-484d-81bc-6dd3674a31d1)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4a6081b4-abe8-4654-b857-9b89b241419b)(content(Whitespace\" \
         \"))))(Tile((id \
         9f8a8981-7aee-4442-bd5d-3ea75b54ae5c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9c70ddf8-d30e-45b8-8da4-49653e4fa6b1)(content(Whitespace\" \
         \"))))(Tile((id \
         42b51989-4bcd-4480-8e84-794707fd5de6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8e0d9f44-4568-4b55-a32d-c615170e0e7d)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3ffa43d6-d028-453b-bdec-5314c6c7b861)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7bf2613c-fe96-4da9-9f54-e04bf55153eb)(content(Whitespace\" \
         \"))))(Tile((id \
         369314a1-d8d6-4d91-8f42-b9bd4de90a67)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         766b0600-6f3b-4a4e-803a-0522d3a89486)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         33ffd077-336c-4131-bc69-88eed0dfb0ff)(content(Whitespace\" \
         \"))))(Tile((id \
         8d531ffb-6db3-4afb-939a-2d737d62785b)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6ba96011-9e61-47dd-8ebd-5d73910df515)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bbf3333b-eead-478c-8629-814befd36a50)(content(Whitespace\" \
         \"))))(Tile((id \
         b7dc91ec-8d16-4f9f-9047-11f196b1c4c6)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         266033df-6340-4777-b9c1-3a1ab002580a)(content(Whitespace\" \
         \"))))(Tile((id \
         8fb63fd2-9be5-49fd-8d28-e04a0e494798)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         07d27176-b10e-4f5a-a204-3e00e59bb0cf)(content(Whitespace\" \
         \"))))(Tile((id \
         c9f61d68-f62e-4997-ae1f-090e98ed04df)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b1f976d1-0eb6-45ec-89e9-b6b4687c2180)(content(Whitespace\" \
         \")))))((Secondary((id \
         c31aae4a-43a5-4af5-8b30-f184f470a4ba)(content(Whitespace\"\\n\"))))(Tile((id \
         e618acdf-8784-4e83-bcde-144d46f3dd51)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         21ca032e-b4dd-4e5b-bd45-05e2811e3422)(content(Whitespace\" \
         \"))))(Tile((id \
         6e485883-b9b9-4e70-8ed2-52e098feb8e8)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         1acc9f89-4a98-47d4-acfb-c8f44e14088b)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         408f4a09-20a9-481f-aa03-00b5ff9ef5c1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b67346fe-898e-4271-82a5-a1fc1860edb9)(content(Whitespace\" \
         \"))))(Tile((id \
         156ee4f8-3d4f-4c6a-965e-49a4c0126e71)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         37a6bb13-2723-4285-9609-74cec9482f74)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         61af723a-736b-4248-b582-98fdcd433718)(content(Whitespace\" \
         \"))))(Tile((id \
         9a471912-fe8c-4c75-a852-7f840ab373f7)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         12bbe6cd-055f-4c99-acae-5452044deff1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         df9be315-5c8b-4d4e-bb2e-c26c26f4c35f)(content(Whitespace\" \
         \"))))(Tile((id \
         276ecdd3-f1c6-47bb-8235-a5571c1f53e8)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ea031971-eeae-4992-80f3-875e1b9a30ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         69b54f2c-cc12-4f47-bfbb-f35dfb3b8ba8)(content(Whitespace\"\\n\"))))(Tile((id \
         5f4348ec-12d3-4a4a-8b90-9a125432174f)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44be50ac-5f1a-49f7-a373-4eeeee7ff544)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f8e2764b-97bc-4bcd-9c25-b525d16efef2)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cb36f80-35b0-4c69-933c-4037bccf36e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c92592c1-1998-412c-8641-67f3cc3dcbd5)(content(Whitespace\" \
         \"))))(Tile((id aff72b64-4c3d-4510-b294-20fc855944eb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         41606852-7090-40da-b226-8908c1d6dab1)(content(Whitespace\" \
         \"))))(Tile((id \
         a9d87d42-fe14-4291-8c41-750ebbd37209)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         687ccdb7-787b-404b-9036-e67cc55fbbfe)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8da5d3bd-4476-4688-823d-f936050620e5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1550aaa1-e3bb-49e9-b273-f0be973e2df9)(content(Whitespace\" \
         \"))))(Tile((id \
         34f566be-fae0-48b9-bdec-d116c4b7e6cd)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         066b90d3-ad7e-415c-ad27-2d8e351003d5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4756353c-5b0b-4e62-9a94-0b8d230b5ba4)(content(Whitespace\"\\n\"))))(Tile((id \
         951a344f-cb24-4a3b-ba4c-e4f7c54898ba)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         de973dca-79c7-4ea8-b428-a67819b4d898)(content(Whitespace\" \
         \"))))(Tile((id \
         29738f02-d22a-40e0-a82b-49ad2bed5427)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ed95b3ae-8edf-4a39-87bf-d8dc76b313a6)(content(Whitespace\" \
         \"))))(Tile((id \
         8ad1b7f5-56f3-43f3-8843-495d5d61ec7c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         259b43ea-fecc-4fc9-9bd8-0be0a9526977)(content(Whitespace\" \
         \"))))(Tile((id \
         fd1dbc4e-8152-46f4-9113-d65fc705d6bc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14bcccce-e684-4375-9b7a-4c48166d5697)(content(Whitespace\"\\n\")))))((Secondary((id \
         d5ceea39-855a-49ec-b049-3309f7f6c7df)(content(Whitespace\" \
         \"))))(Tile((id \
         6c093b5a-3a51-47cc-bf38-689873fa81a9)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         10e51286-58fe-40e8-8554-d8713607910e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0303b5e2-8adf-4096-9f9e-1b5e84416c1e)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cb361e2-69d0-41c0-b6b7-c8b8acc3a12d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1dcf0098-89d1-45cf-ad13-0112c5704972)(content(Whitespace\" \
         \"))))(Tile((id 80acfe04-d49d-46fb-8288-ae176bcd6e4b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a47c6c3c-da53-45ad-9cd5-80e5425b8c30)(content(Whitespace\" \
         \"))))(Tile((id \
         9e17dab0-b74b-48ef-b5c5-593821292c4a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bdc303fd-a8f7-4454-9bf7-14010087cfc3)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e0bbce35-7621-4f39-9152-07d37c6f798e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4e824dd3-6738-45a4-80d4-2b9d7397c74f)(content(Whitespace\" \
         \"))))(Tile((id \
         8415f348-d88b-4a50-b6f3-e6eab6cffabf)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b0d2e5ad-5835-45ee-99c5-20b1be5f2c20)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7da7adc8-3ebc-4ab1-a9db-7f42c0d121aa)(content(Whitespace\"\\n\"))))(Tile((id \
         4daf4ccb-1d2e-4781-84bb-46cbdf275212)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4c736a98-70c7-4aaf-8a98-dac47ef1d0f9)(content(Whitespace\" \
         \"))))(Tile((id \
         c6f23f4d-e1f5-4f73-85fa-07cd3b2855e2)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         588a48b5-6688-4b36-913d-ff5ab2968f2f)(content(Whitespace\" \
         \"))))(Tile((id \
         2e499d41-5835-4e72-acaa-8f76d5cd8282)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         030db24b-f8a6-43dd-90dc-8b5b7028d42c)(content(Whitespace\" \
         \"))))(Tile((id \
         6a350bcb-1536-4395-895d-2b0638c22da6)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b380c8a8-d3d2-411b-ac9d-b0a8cc191f9d)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb3ad1af-6fbd-411a-8f6e-37e04737cb83)(content(Whitespace\" \
         \"))))(Tile((id \
         b47fc97a-4ce8-4af1-b0ca-d7d1799ee8fa)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f95dcad-a0c6-484d-88f9-5f7bccd21fd0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         638f5b52-4dc6-4850-af34-5bb0bc8f7c29)(content(Whitespace\" \
         \"))))(Tile((id \
         0769f69e-f538-4df4-a09c-e7a31a11da46)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         00630029-b32f-4b8c-8389-809bafb553f0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         218c9c8b-0d9e-4af0-a3c2-8cbfdbf58121)(content(Whitespace\" \
         \"))))(Tile((id \
         acc9c150-4ff1-4915-8014-58aec3dc7cd0)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7bd4fb28-65e2-4e4d-91ca-34189eede52d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fba65a2a-37b1-410b-81c1-745f71aca290)(content(Whitespace\"\\n\"))))(Secondary((id \
         c59ae130-9437-498f-9d2b-0e332ea4bac2)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab275856-0c23-435e-bb1d-ece664d89276)(content(Comment\"# Get all \
         orthogonal neighbors of a position #\"))))(Secondary((id \
         9e68ec2e-e658-4144-b710-194eabd4820f)(content(Whitespace\"\\n\"))))(Secondary((id \
         037db916-161b-423b-9e1f-de511115a114)(content(Comment\"# Returns \
         cells above, below, left, and right #\"))))(Secondary((id \
         e91a6282-5716-4085-834d-6faf0ccd13bd)(content(Whitespace\"\\n\"))))(Tile((id \
         73de9eee-82e8-4f1c-be22-147b32df43fc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fa245407-9d53-414a-add5-5fce5e083355)(content(Whitespace\" \
         \"))))(Tile((id \
         65237d21-c0e0-4b38-aaf1-34803216f767)(label(getNeighborCells))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2873477c-d60f-4bdd-a07b-aa8ad315d794)(content(Whitespace\" \
         \"))))(Tile((id \
         15c05372-6ec5-476e-a2f7-f9260180f81d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6acd1c50-eabf-435d-ae98-5c262c7a1568)(content(Whitespace\" \
         \"))))(Tile((id \
         8ec688d3-c130-4778-8def-9f7591e9f3a5)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         85b58f07-8d4e-48c8-9b56-a9b84e5e0bf5)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3afa7736-4f1d-44be-9ce2-ee3fc9481bb3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         37b31f83-49ee-4e6e-8d30-d3ba2b4a6ed7)(content(Whitespace\" \
         \"))))(Tile((id \
         b14d4a98-897d-4dde-ad0b-12e6eb1f5a71)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         94f786c9-c8ad-4ad8-8d35-c89f9f971279)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         168aecfe-2721-4a36-a349-0f00e19ec4fd)(content(Whitespace\" \
         \"))))(Tile((id \
         2d19701f-c777-43c5-80e4-7351ca7a157c)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8523b7b6-05cf-4eea-8133-f9d7fd4286d3)(content(Whitespace\" \
         \"))))(Tile((id \
         1242a4ef-3469-40df-9b44-687d451be8d1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4de16a2-6d5b-4082-9a4f-eb59acbba503)(content(Whitespace\" \
         \"))))(Tile((id 457dc191-8b86-4587-bb2d-d376431dd089)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         26b6763d-942b-48f9-b2de-ca79e16321cb)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e95590d5-169c-4ef6-9217-9f7746109c36)(content(Whitespace\" \
         \")))))((Secondary((id \
         53cea2ca-5fd6-4a3f-b568-b72988ccc122)(content(Whitespace\"\\n\"))))(Tile((id \
         9c49916c-15dc-4b6b-8897-dcd79851890c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         97cf9a64-6648-4b31-97ac-cd3805edf47d)(content(Whitespace\" \
         \"))))(Tile((id \
         08b22604-4b3b-4428-a2d7-67c54ada95d9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5a38f68f-3251-4731-9d2c-8d8c8af967f8)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f01596e8-59a7-4985-902b-c42c66c9ca1c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c42775b5-568b-4ff3-8a79-74e240c1139f)(content(Whitespace\" \
         \"))))(Tile((id \
         1850616c-7308-40bd-bd54-09f5ddc4da00)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         be208b24-bea6-4910-9988-390b8d43b8fb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         475323c9-6d37-48c0-956b-21ec2665f8b9)(content(Whitespace\" \
         \"))))(Tile((id \
         f173ed9d-44fb-40b5-ba3f-a49e5398c400)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5b46d13a-82b1-4fa8-b2c8-1df5b6aba606)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5475e10a-5a11-478f-b364-65b76e16f7d2)(content(Whitespace\"\\n\"))))(Tile((id \
         26e40776-26d6-4c43-a203-fa7b44788f77)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         347a5f32-73fe-41d4-944e-69caff5510c2)(content(Whitespace\"\\n\"))))(Tile((id \
         d62b1650-0aed-4877-a418-2a641e97654d)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c59aebcd-9c04-4a38-80c2-520af7d925fd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         657cccec-5ee2-442c-9313-c4bf92502db0)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0acf5d51-344e-43d7-997e-5230baf0619b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f87eaed7-2c2c-4245-bd35-8b43188e7304)(content(Whitespace\" \
         \"))))(Tile((id \
         a50a82dd-427e-48a9-9684-549eeacbfcba)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dbd4883a-ec20-4367-88ae-862d09a085f4)(content(Whitespace\" \
         \"))))(Tile((id \
         cb7b3208-03c3-4c29-9dac-409077029bf8)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f969330c-5713-4524-a5a1-369bffa5e7a3)(content(Whitespace\" \
         \"))))(Tile((id \
         93a365c8-a180-4b7d-8888-b2d60f360a4f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b4021874-ad9c-4364-a658-4a705937f5b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb407242-d2d1-424e-93e7-2da87d360ff5)(content(Whitespace\" \
         \"))))(Tile((id \
         ffa05f5a-4807-4736-a6c6-ab2606712039)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0b6a5729-f685-4b53-9998-f689e9e2d4b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         022f4690-65f7-49be-a49c-39ac14da8644)(content(Whitespace\" \
         \"))))(Secondary((id \
         5bf04490-4f74-45ab-a992-a79edea0738c)(content(Whitespace\" \
         \"))))(Secondary((id \
         9ff74dc6-1e38-4acc-b5b0-c5126929376d)(content(Comment\"# Above \
         #\"))))(Secondary((id \
         c9f48b20-4461-49e4-a8e8-4f1c7416a700)(content(Whitespace\"\\n\"))))(Tile((id \
         47c329ab-e1b1-46c3-834e-25998e511c3c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c205896-d498-4a5b-8f6b-3cb67eabfe64)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df14d450-9e07-4377-b7c0-b09f8d1845d0)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff687642-d2c2-411e-9b4e-d7d875eb748b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95b0d744-da5a-4ed3-9caf-2c8c6b0218db)(content(Whitespace\" \
         \"))))(Tile((id \
         9a6fdeae-ec76-4805-9f9e-2dc511437503)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         280c3be1-ae53-481d-ab45-f1db1c60e497)(content(Whitespace\" \
         \"))))(Tile((id \
         5648569f-fd6c-4760-bc8c-b65e846bee74)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01c1c2d9-e7dd-4959-a985-22e0b7017ed3)(content(Whitespace\" \
         \"))))(Tile((id \
         4d096472-82f1-4b38-887f-c0fc126e15ff)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e09e5540-deb4-4555-b267-c8785ef26fd0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2f0e152-a437-431b-884d-3351a35e9ab8)(content(Whitespace\" \
         \"))))(Tile((id \
         3b6e28cb-a798-41a8-b687-cdf4da27e91d)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bf52549f-0f3c-4897-a35a-b089bce1cac2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e41e823-bf21-4a07-ac12-6f95a793d30c)(content(Whitespace\" \
         \"))))(Secondary((id \
         1be6ba86-d54f-4e56-ac84-fb31073d91a5)(content(Whitespace\" \
         \"))))(Secondary((id \
         f9343270-51d2-47c0-ac6a-0fd3d507a102)(content(Comment\"# Below \
         #\"))))(Secondary((id \
         a7f11e55-fefa-4f47-a051-11c4d8b23361)(content(Whitespace\"\\n\"))))(Tile((id \
         fdc5900c-3e3b-43a4-a1c8-80172acbb284)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fddb9d9-2c39-4512-9f1b-af43ad1b1486)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         00a8a489-8929-4933-9f78-c274cbf6ca65)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4959e987-703d-4125-be50-4ec7f809c029)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5958c34-201a-4eee-8343-78f7e416f3ef)(content(Whitespace\" \
         \"))))(Tile((id \
         65e195c6-ba3e-481f-a4bf-9b052826426b)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7cff1bfd-efe6-442f-949b-cd7c33de02d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         312611a5-00ea-4e68-8f15-9781e6b3b977)(content(Whitespace\" \
         \"))))(Tile((id \
         161e0882-1d2f-4058-b435-1e0cbaebec6c)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e570a362-d17d-4d79-bd4d-a01dd0ed1cdb)(content(Whitespace\" \
         \"))))(Tile((id \
         60e187d4-e0cb-4c3a-b107-6150904916ce)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         956a013a-85d5-46c6-9e24-944e08f42448)(content(Whitespace\" \
         \"))))(Tile((id \
         026c8f82-a157-46d3-97c0-390a8ed1861f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         52a81a07-53ca-42b6-8dea-919a36e62ffb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da3bd675-de48-44d3-9588-1d905191f7f8)(content(Whitespace\" \
         \"))))(Secondary((id \
         df0df5ce-777a-43e0-89c6-a02c5ad6d228)(content(Whitespace\" \
         \"))))(Secondary((id \
         29b794e5-8136-422b-82e8-293cffeed855)(content(Comment\"# Left \
         #\"))))(Secondary((id \
         665c778c-d4c3-46f1-a052-920621054d41)(content(Whitespace\"\\n\"))))(Tile((id \
         70fde00e-edf2-4357-aceb-3bbd0ba960a9)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7655ed10-517b-4cca-8ead-6e22787db744)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6069ec95-96ee-4f20-8c34-9936476ba117)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4a30f8a-5428-4ee0-8b90-542f971698b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0517b358-4f87-4693-8ad8-70aeb71d8a8c)(content(Whitespace\" \
         \"))))(Tile((id \
         6cde0ace-bbb1-4ee4-b013-63010ee4cf0f)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e1ef43a-a1c1-438f-9d8f-d093217f82a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dbba31a-852f-463e-b261-eca45b065e2a)(content(Whitespace\" \
         \"))))(Tile((id \
         0759dae8-242a-4104-9bc2-3c81ce9f31cb)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         db115590-80eb-4349-b76a-4759e6d07f9e)(content(Whitespace\" \
         \"))))(Tile((id \
         9079f8d6-9f56-43e9-8d31-dfea33f87752)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         285260dd-f8c5-4877-bafc-dfa40f613df0)(content(Whitespace\" \
         \"))))(Tile((id \
         3fe907b8-34d5-4955-873f-e89a55ca1f5a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fbfc77bc-356e-41b8-8e2d-483b9be8ca70)(content(Whitespace\" \
         \"))))(Secondary((id \
         75d5d5b5-9d5f-4dad-b0cc-d5ff2c9b6a0d)(content(Whitespace\" \
         \"))))(Secondary((id \
         c9bd39bc-4578-4c6e-a423-90fd4fa6dca0)(content(Whitespace\" \
         \"))))(Secondary((id \
         89bb3b13-741c-4ea1-815c-125e6aa35edb)(content(Comment\"# Right \
         #\"))))(Secondary((id \
         9c6fc6fc-35a1-42f0-bd36-23ee72302a76)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c91197a4-8c86-486e-bf5e-78198de6037a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f897954c-e8e8-4165-9f3e-d8c231a4c155)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f8088f9-807e-4aba-9c5e-0c20a69927cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4a8b21a-cc94-4bd8-bce0-6b9b4bf7d4f0)(content(Comment\"# Calculate \
         total health modifier from all neighbors #\"))))(Secondary((id \
         2321769e-25fe-4cec-b6a5-cd49c9f88aaf)(content(Whitespace\"\\n\"))))(Secondary((id \
         bda9da92-868d-43b2-af43-d0a5e1e38998)(content(Comment\"# This \
         function computes the companion effect for each neighbor \
         #\"))))(Secondary((id \
         8bb0ceb8-d3dd-4b60-bd7d-66b6c78758d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         de69ef90-02bc-436f-af19-4b8bbcf4ea46)(content(Comment\"# and sums up \
         the modifiers #\"))))(Secondary((id \
         e39871cd-e141-4f53-8e9a-4c67bf5951e0)(content(Whitespace\"\\n\"))))(Tile((id \
         464a7768-a060-4bee-a146-ccf630c2f1d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7d6382bc-8c11-42ce-b4f0-a8d087282200)(content(Whitespace\" \
         \"))))(Tile((id \
         49549d98-7037-4897-9af3-6b33b3bdbd5b)(label(neighborModifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d2c8ee43-186f-4a41-8aaa-737c228c1779)(content(Whitespace\" \
         \"))))(Tile((id \
         5659edfd-8f13-44d1-a743-dae627a81305)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9af3f806-e863-4fa9-a039-85cf8a0320a2)(content(Whitespace\" \
         \"))))(Tile((id \
         20a32123-7ad3-4c34-9f3e-9279e8cdeb61)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7e85013a-0852-43fb-9274-bbe14685f516)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a9d65638-7418-4b71-ba51-bace44a6627a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1a8b317d-f6fb-4409-95a9-7e92ee2c2f2a)(content(Whitespace\" \
         \"))))(Tile((id \
         c46d6066-65cd-4533-ae47-888aef0fe0cc)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9b20d82e-bcc2-4793-8e04-ee80a352ff63)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a2f1165-925b-4e39-a454-c005f9022f0f)(content(Whitespace\" \
         \"))))(Tile((id \
         017e74fe-2319-421e-a895-2f95c7fc5f91)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a80a6e11-0f45-4d05-bc9e-8a3803a8620f)(content(Whitespace\" \
         \"))))(Tile((id \
         84e25cbb-582c-458a-97b9-e56d1efe9811)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73fbd972-bac6-4158-aa55-65ea12c57a0c)(content(Whitespace\" \
         \"))))(Tile((id \
         c61cfbee-0033-497e-aabf-2cb2c0e3d9c9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b313daa8-460e-49db-a236-f46d4f0da558)(content(Whitespace\" \
         \")))))((Secondary((id \
         5f54bf98-862e-43ee-a199-c5e60f1435d3)(content(Whitespace\"\\n\"))))(Tile((id \
         61813a5e-a2d4-4fb7-a553-0d6a7f7ce5c5)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3cd60a57-8b37-4a3b-a70b-206f55bbc0e5)(content(Whitespace\" \
         \"))))(Tile((id \
         8e5f6765-151f-4fd6-ab82-a14f2627544c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         fa7f3574-143d-4f9f-859c-3032995bc491)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a13a8bc4-35b5-42ff-8f66-0998fb009a28)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         46abf3d4-db8d-4c24-9087-b6f1f725c837)(content(Whitespace\" \
         \"))))(Tile((id \
         249800fc-8a30-4f59-bbfb-410f060babf1)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5bda7ae4-5186-4b0b-a2a2-7c87b249afb9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         03520e64-aca6-49b3-956b-e37bb9a1f254)(content(Whitespace\" \
         \"))))(Tile((id \
         0b2fa7d6-c6bf-490b-aa2b-cdeddf793094)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0f1e34bb-a05f-4db9-8c91-d4f454ffa2e4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a40e7759-4b3e-43fa-9043-e4e12d20db46)(content(Whitespace\"\\n\"))))(Tile((id \
         f0c47ecc-c35d-4d44-8547-30c9ecde0f01)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         50f34255-5e4a-4ad7-b052-7c79e4f08dc0)(content(Whitespace\" \
         \"))))(Tile((id \
         da6144fa-828d-4e79-bb8f-c7064abc321c)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         518c04a3-fa59-4602-8a14-8b213b0a31c5)(content(Whitespace\" \
         \")))))((Secondary((id \
         3c951e07-f011-49e4-9847-4a1bc11f706a)(content(Whitespace\" \
         \"))))(Tile((id \
         9c3d9488-a493-45e2-b386-7ed7a28429f7)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66d70235-4ece-46ce-b688-58e12f025aec)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2053e08-5090-4d7b-81c5-aaaf6295ce41)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         458dbaf6-52c4-4b3b-9972-8ff8074493df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb2d9787-7410-4c93-bd30-4047097c2521)(content(Whitespace\" \
         \"))))(Tile((id \
         8b7922e6-d2cf-4744-8c61-503c97f2c542)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecd0cdab-c44e-45e7-8217-c8c249434435)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51a9c03b-8ade-442f-a16d-43ff240745c6)(content(Whitespace\" \
         \"))))(Tile((id \
         e713414f-25a2-42df-aad9-5d712269edaa)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         51ce9945-176c-4b9e-8612-0629a9d957f1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c999b12e-ac9e-495b-a3c1-3eaa5a735a1c)(content(Whitespace\"\\n\"))))(Tile((id \
         1212011f-0ddf-4bf1-9e87-7d3ac786b87b)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9cdea944-cbf7-4bd7-b422-58d8dbed7258)(content(Whitespace\" \
         \"))))(Tile((id \
         6149c16b-5052-4bbd-897a-426e7289f125)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a715bb3-fd1c-463e-b03f-665a875a1119)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7b1e7060-d7de-42c2-be35-8960f180edc2)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7dfddea7-0955-4aa7-b9a2-4c44086c8690)(content(Whitespace\" \
         \"))))(Tile((id \
         32db2e1d-feba-4ef9-b344-2cfb068276df)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91cd64e5-53e3-4ad2-82ef-3b7efa5aebe0)(content(Whitespace\" \
         \"))))(Tile((id \
         da93661f-e82b-4248-bde2-f6e9e54adac5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ab3ba32-f42b-4123-9a1c-9828d5d2e115)(content(Whitespace\" \
         \")))))((Secondary((id \
         74465aba-138f-4fd3-8617-5ad2808a9dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         9147d9c0-74a2-4e11-8213-5fe183bf3e16)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a1b9019-8043-466f-910c-087231317f96)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4457305a-4458-4477-abb5-5f75e3be4ec8)(content(Whitespace\"\\n\"))))(Tile((id \
         57a27b6b-7d17-4cc5-b786-41faf83a6c0b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         031d9914-f617-4b24-930f-c560475678ac)(content(Whitespace\" \
         \"))))(Tile((id \
         69c1520b-d693-4788-9259-5e8a4c3a89bf)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e4f9d47f-10a1-458a-a667-034ddd75c52b)(content(Whitespace\" \
         \")))))((Secondary((id \
         ea583591-9463-4831-bb4e-d685566e9724)(content(Whitespace\" \
         \"))))(Tile((id \
         5140653f-e091-4bec-aec5-41f9ac215993)(label(getNeighborCells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         032bc07a-db70-4ef5-a387-8716e8868c43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88c67bd1-b4d6-4c55-840f-559835e13ed2)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61b6ef03-568f-4de2-8904-2833311bd0d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f68350e-bb05-4415-8fa3-467a9d610d30)(content(Whitespace\" \
         \"))))(Tile((id \
         a13a56db-54c2-4f6e-a31f-62bf4a142555)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         061a5cc6-f646-4193-8e06-bbed9c444e65)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0912873-8375-4b86-925e-15bda947d14c)(content(Whitespace\" \
         \"))))(Tile((id \
         d718b4a2-af5c-4377-95fc-b32bbe8ec6cf)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         696a87a9-b3d3-4c5d-a08f-244305f829d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cf52828c-f66c-4be2-b37a-7732ac4499ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a936b0e-6197-4b47-88a8-5c008b258b50)(content(Comment\"# For each \
         neighbor, calculate the companion effect #\"))))(Secondary((id \
         b689cdb5-e5a3-4977-a210-73f20e346f8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a60e920-0497-409d-a4a5-f758a270799f)(content(Comment\"# Effect \
         should be between the current cell's crop and the neighbor's crop \
         #\"))))(Secondary((id \
         84837aa3-b20e-4005-b865-f1555e741eb0)(content(Whitespace\"\\n\"))))(Tile((id \
         9aeaa48e-1eb7-489b-9df7-a1fdec97e52e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e5f89173-6075-4e6b-9836-d12fa32a3a68)(content(Whitespace\" \
         \"))))(Tile((id \
         678a08bd-168b-4d61-8a0c-d975f9b7cf66)(label(effects))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1311e9fa-8d1c-41bf-936e-a9f1a1dd3b67)(content(Whitespace\" \
         \")))))((Secondary((id \
         de7d1da4-03af-46ff-8ced-69584db0b8ce)(content(Whitespace\" \
         \"))))(Tile((id \
         3665a388-9779-4670-934e-f44af6c12afc)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e9b1943-a9c8-401f-a8b4-34dda9362f88)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f6d66b14-c71d-4bd5-a88a-5260f7bf998c)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         141fb9eb-26e4-4bde-805a-d6199afd61b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6fce307-1add-48c6-8979-07a393041bbd)(content(Whitespace\" \
         \"))))(Tile((id 476a5810-ccd4-4bf1-9912-fcee4922107b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d711dd8c-b0bc-4fc7-af7a-efb1f4e87fcf)(content(Whitespace\" \
         \"))))(Tile((id \
         493c8a86-274f-4e4c-890a-c8c61aadcf67)(label(neighbor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52fe246e-4570-4fc2-a284-b3dafcbebe8b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         903c156a-6fa9-4264-b57c-a8780fb549c7)(content(Whitespace\"\\n\"))))(Tile((id \
         0b966e2c-7679-4d15-a385-77258155469d)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c41241b-2fce-485a-b85b-3047052a914b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42bb5c4d-a007-42bc-b9c9-16d410fc456b)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0631ca1f-f539-4f48-a77f-752e5a069d1e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         18705047-1ad5-4399-b926-a94fc256c284)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31b0237c-833f-45b2-9118-8f3fc68de527)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e815c8bb-28b2-4450-818f-7abb77c4009e)(content(Whitespace\" \
         \"))))(Tile((id \
         70e9ece5-c5ff-4954-a04d-a87829c7e5fe)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc7b40d4-4693-4403-8232-c75248d157e5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c2363f57-a626-44b1-87f8-0b44a4979da2)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a19c994c-01d5-4472-8470-05303c1276ed)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         620ff85f-23dc-4ebe-ab22-9fcaff0ea80b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de18b250-0b9b-4e05-8720-efeb273c4b83)(content(Whitespace\"\\n\"))))(Tile((id \
         a53212e0-9a13-43ae-a9b5-fdc61f2a583a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55a3f58a-18c5-44d9-bccb-19aeea949a1c)(content(Whitespace\" \
         \"))))(Tile((id \
         feb5c5a6-8b33-4bdf-8959-2df653dac2fe)(label(modifiers))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bad9e175-3d69-4463-8f82-140dcb56200d)(content(Whitespace\" \
         \")))))((Secondary((id \
         e59f30db-04a8-45bd-a47e-eed2b6a29dd7)(content(Whitespace\" \
         \"))))(Tile((id \
         c746d24b-fb12-45e5-ae57-1493fab1b001)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f6b0c45-d52b-46a3-9623-94760f65ca34)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7995c919-1e6a-417e-a677-26f02e17a623)(label(effects))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fd9ae03-9c71-420e-8eb2-da9e6f583cf9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1ea577a-b062-4d26-b3c4-6491f6941ca3)(content(Whitespace\" \
         \"))))(Tile((id \
         7463ac84-65d9-4dfd-bc97-ddf90480e835)(label(effectToModifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6280091d-e3f2-4fee-bf2d-f99e8df0061d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         48f9c81d-f856-4e99-8ea7-4216fef5629c)(content(Whitespace\"\\n\"))))(Tile((id \
         1c1f4554-1ad3-41d7-979e-99d429f7a31c)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c20a9fca-434a-4e70-ac2c-0bcfcf091a4a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a4dc20fd-c124-47b2-a566-44a873606d41)(label(modifiers))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d82560d7-5296-4a26-ad9f-fe2ff09a0a1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d3a1f12-e291-4914-ab18-3d45f890097b)(content(Whitespace\" \
         \"))))(Tile((id cf0b5ad5-fa7e-4c2a-8a1e-baf0e3f79683)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         49e8190e-5595-4b8f-8dd4-6dbd8d8ded8c)(content(Whitespace\" \
         \"))))(Tile((id \
         9f9ce5ac-9b14-4bee-9762-de41447df6a5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a862c648-44db-4cf4-a507-48f1558f41b0)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         12561025-cb43-44a7-86d7-0878f333d408)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         15d39012-ea9a-46af-871a-cab76ea5fb6c)(content(Whitespace\" \
         \"))))(Tile((id \
         35bc08d3-3495-40c4-99b6-caa41da04269)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         06cc969f-5195-4c5e-a9a9-948a683e1f0d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bbb50a6c-6c70-4a83-81df-0fea642ee84e)(content(Whitespace\" \
         \"))))(Tile((id \
         78e3e845-887b-4a9d-bf78-6b43d20e86ce)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0c917d0c-0d36-41e3-b7cf-2920485889cb)(content(Whitespace\" \
         \"))))(Tile((id \
         ce64dd04-2c19-4a9d-85f3-d1cfca9628bc)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9b3bdcb-8f9b-41c0-8812-d1ec197f8283)(content(Whitespace\" \
         \"))))(Tile((id \
         b7cbb12c-6064-4d43-921f-7e5fb8243bde)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc2f24c9-2b09-4983-b8ea-bac4cbadf049)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05341098-0f58-4023-92dd-bfd7b26a240a)(content(Whitespace\" \
         \"))))(Tile((id \
         26b184b4-69b9-4a41-af50-7fc63532940d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c3f87ef6-f83e-45f4-a318-a5c0738550b6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         590d29ef-4d06-4116-932b-4b5f4d62b9d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         f34ecc21-863d-4c41-8485-f83bef8c68d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0efa48e-d63e-481e-9b94-c1b6f87d9682)(content(Comment\"# Recalculate \
         health for a single cell based on neighbors #\"))))(Secondary((id \
         f0c33f0f-304a-4788-99af-7cd855b21aff)(content(Whitespace\"\\n\"))))(Secondary((id \
         861caa28-b633-4858-a363-07febf7a81dd)(content(Comment\"# Base health \
         is 50, modified by neighbor effects #\"))))(Secondary((id \
         d920ed78-8c3e-4674-9c0f-732bb9a27136)(content(Whitespace\"\\n\"))))(Tile((id \
         4a1403ab-8474-4670-ac7f-e9d31e0e12a8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2cd356ac-1346-4549-a8ee-e91be5e20143)(content(Whitespace\" \
         \"))))(Tile((id \
         8ef04093-cea9-4a74-8632-d6b2666fe4f7)(label(recalculateCellHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         93644641-8565-43ce-b1b2-7a7dcba3c753)(content(Whitespace\" \
         \"))))(Tile((id \
         0a52f394-97dc-4ed6-95f0-1c46f1f329a7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9e6a6ad8-2138-401c-a960-6f82075fac62)(content(Whitespace\" \
         \"))))(Tile((id \
         9fd4be39-bf16-4d68-9334-a7738737ce43)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9226eec6-db2d-48a1-8b70-d73097ee0977)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ab1f136d-3b46-4812-82e6-804d1880692f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0bdd0ed3-ebaf-4c31-af85-1d2e260ed393)(content(Whitespace\" \
         \"))))(Tile((id \
         bc3ebc08-4f3d-4b6a-b717-1d3914953232)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         31439495-0e15-4b61-bd24-c3b2d89f4563)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         391d5ea4-15ed-43bb-b4bb-7375113c616a)(content(Whitespace\" \
         \"))))(Tile((id \
         0016e59c-ae60-4950-8ad8-73e9970710f6)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5ef6be71-d8c7-4b5b-b27d-78065ac98fc7)(content(Whitespace\" \
         \"))))(Tile((id \
         bb46ad3a-925d-4424-9bf2-1e357f86959d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         934c0d15-5135-4a11-a8dc-eab17f18f1de)(content(Whitespace\" \
         \"))))(Tile((id \
         89a4fea7-da2c-4e14-8255-b92fcbfacd28)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c6f145fa-8036-4f64-a670-7341becb721d)(content(Whitespace\" \
         \")))))((Secondary((id \
         d1b17c80-40d7-42d8-b923-0d1fcb9f1136)(content(Whitespace\"\\n\"))))(Tile((id \
         7f11209d-474c-4d05-82a6-5c3e17b47d96)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0b83a4f2-40a7-4af5-893c-dc7d3d965bac)(content(Whitespace\" \
         \"))))(Tile((id \
         e7d990b5-f7c6-4942-b807-5765333df0d3)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5c354366-7816-4047-ba99-eaa5c81be4a2)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1e502fdf-4bbc-42a3-a996-ca00b53ca20a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         62477c1a-6eba-40fe-831e-3305d8608f80)(content(Whitespace\" \
         \"))))(Tile((id \
         7f661791-3fe5-4cae-b7c9-07735e6a2c7f)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c4780834-7d40-4e51-b430-9b15cccf669e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         66b850c3-ae1c-4080-9a26-ea5d732612e8)(content(Whitespace\" \
         \"))))(Tile((id \
         d5d59202-ae09-4d5f-83cb-dd7ae7720508)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         85fecaa8-bb80-478d-8210-3dc7760aabc5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e802abb-807c-4435-b782-a0bb93b67d3b)(content(Whitespace\"\\n\"))))(Tile((id \
         44220c33-0f0e-48b1-a5a3-d04c3658a04a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         11dba88d-a534-4ec1-93c0-ecaf35fd2132)(content(Whitespace\" \
         \"))))(Tile((id \
         e506f9c0-6905-4bcd-9d12-cfb43730d79e)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0668f4ff-4dca-4e52-b9b2-1a5d43bbd340)(content(Whitespace\" \
         \")))))((Secondary((id \
         46ce871a-f793-47ea-b839-15ffcd3f32b4)(content(Whitespace\" \
         \"))))(Tile((id \
         148d7270-99a5-4e2d-80fd-6f6254f2c3ec)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43c07765-bd80-4b09-a584-02226eec9b1f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1a9d7211-8eb7-4260-8e49-6fe88d0da67f)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         117c6067-19a0-42f4-ba18-2b58a028c21e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a23423cd-490a-4de0-bb58-3002945ef647)(content(Whitespace\" \
         \"))))(Tile((id \
         7ed5fe44-d90c-4a48-b022-214f1fbeb4be)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4fa4c17-e13e-4a15-8495-05e9bae1b25b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         334745e1-d1a2-48fc-b007-f98deea0030b)(content(Whitespace\" \
         \"))))(Tile((id \
         a7f7f33d-cd20-4eee-a11a-5a05d86ed703)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e0a57f61-eb37-4024-87c1-fc392cff2fa4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bf7b1941-07b0-41a4-9b1a-81f51ea40ae0)(content(Whitespace\"\\n\"))))(Tile((id \
         10790158-f00f-427f-a0ef-bef82755fbbe)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         466a2ede-09f1-4bf1-bfb0-d661a77c64e4)(content(Whitespace\" \
         \"))))(Tile((id \
         8693a5d1-3df8-4dcc-96b5-aad5834f0f88)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d1e14df-19ea-4e1e-8dc3-2acd053517dc)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c8c51a6d-5bb3-4d29-aff2-6ad6f5fb0ea4)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a43b6e2d-1df8-4547-b762-850039e13d40)(content(Whitespace\" \
         \"))))(Tile((id \
         c8097ce7-2ac7-4f86-ac6d-5bdd122fc4b0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3364d168-2bd4-44f9-80e1-680fdc80f8b1)(content(Whitespace\" \
         \"))))(Tile((id \
         f4765fd5-c252-43a6-b04a-e879b6826525)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4592debf-4509-4f4e-9f48-f961aa0311ef)(content(Whitespace\" \
         \")))))((Secondary((id \
         3509a914-5294-4b68-8a4e-974cc8587dc5)(content(Whitespace\" \
         \"))))(Tile((id \
         8a122ea1-5d72-4710-9f7a-f8e61e05dc43)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e7263546-b82a-4656-a13d-f241548f0ae2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b2da43c4-c17e-47c5-843c-44dd893afd6f)(content(Whitespace\"\\n\"))))(Tile((id \
         8934a73c-f307-47d3-9e57-441696662a07)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d323e89c-5a2b-498c-8726-0d39274b2e11)(content(Whitespace\" \
         \"))))(Tile((id \
         f1f53ab1-627b-4af5-83ed-863ec6799e53)(label(modifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c749951c-c1a8-4e59-bed1-69d589b0a163)(content(Whitespace\" \
         \")))))((Secondary((id \
         507d134c-8077-4ac7-93c4-db74ffd79346)(content(Whitespace\" \
         \"))))(Tile((id \
         34e0f8bb-8159-4605-b377-9fb0df0a40d1)(label(neighborModifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0dd44996-7534-4ecb-b7a1-923f2393b17a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aae0fd3a-0914-467b-ac8b-cd05f1d076b7)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9600bc89-ed4e-4d16-98f5-96aa47e094df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93c59d5b-be32-4774-8314-9efe7483e5d1)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8af473-91bb-49dd-a153-7b1557b9579e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76194180-f177-4876-a8b9-fda8e1fabb52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9b19d5a-0761-47c2-9c72-9a62867e89f6)(content(Whitespace\" \
         \"))))(Tile((id \
         b70b8888-36ec-4710-9bcc-ff52ff6a03e3)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d306b588-94a3-4dd4-885a-19bda6b6b1ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3801e42f-176c-47c7-8a37-2367c6399085)(content(Whitespace\"\\n\"))))(Tile((id \
         080737be-6167-4daa-8508-7c1186bf6751)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1e227d7-ab39-4b12-a727-aaf69bd34593)(content(Whitespace\" \
         \"))))(Tile((id \
         eb253008-88e8-48e8-9081-739f67b33b98)(label(baseHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         382e716e-baf9-4de3-9eaa-0822d195c1f8)(content(Whitespace\" \
         \")))))((Secondary((id \
         59125fd9-bf9f-4848-9654-17b6b2ddc6a2)(content(Whitespace\" \
         \"))))(Tile((id \
         7ebf0fb8-0e0a-41da-9bff-cc9a8567cd39)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         721edcaa-c528-454d-b160-093d3c874680)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3e701cc7-1ee8-47e5-a872-36a931d891db)(content(Whitespace\"\\n\"))))(Tile((id \
         c77e5f17-321a-47d8-b883-9ba4a4ea3b61)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f52e8fe7-3f23-46f5-9078-60bfc1a9bd8a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         161b94c1-b207-4963-b2ab-95c4b4aa7802)(content(Whitespace\" \
         \"))))(Tile((id \
         c0a03c3e-7e0a-446f-8256-1f336c9f6002)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77def901-23e5-410a-b126-24c0b040caec)(content(Whitespace\" \
         \"))))(Tile((id \
         da9d566a-ea0c-45a8-a42c-ff4d81d7345d)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4dafec74-a17d-4295-94e8-9ca7b4fc3a9e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7259fdcf-fb96-4f3c-b32f-786fe9c8d9e0)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0762ab9-6290-44ba-8ec0-ae420d25a088)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38a65250-6973-4c04-b690-417a2819e67e)(content(Whitespace\" \
         \"))))(Tile((id \
         de3bcd0b-dfdb-4ef2-870a-6bd0fc7d9e70)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e51d53c-7cd6-4f95-b07b-092114639570)(content(Whitespace\" \
         \"))))(Tile((id \
         82a3d68a-306a-48e5-80b3-b1ee2ec6e231)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23978cbb-cf6c-4a69-8d6f-d96c2cffb2f7)(content(Whitespace\" \
         \"))))(Tile((id \
         d2ffd448-b7ef-436c-a4a0-5fdb0a46c82e)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80d3f38b-4815-479f-85ed-c09f1ae18fe0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ec861b62-0d0e-442f-9d80-374e0e50ce15)(label(baseHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf633e86-c2fc-4669-9a38-3823cbb56b44)(content(Whitespace\" \
         \"))))(Tile((id \
         0f2fe339-01e8-4c64-823e-5ae4ea5b3718)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85a6935b-5649-482b-b3b0-2c7c7b3d9ca3)(content(Whitespace\" \
         \"))))(Tile((id \
         d118b9d9-df94-4db8-b860-910a88d2e830)(label(modifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8307b387-9854-438d-b663-1833287c8621)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         331c8bf0-1be0-4a34-b8e6-f0829e8a8e09)(content(Whitespace\"\\n\"))))(Secondary((id \
         95457e92-bd3a-4e2e-8e2a-44c0ab684b38)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b43c133-31ba-498b-8664-bdeb7e0030a4)(content(Comment\"# Recalculate \
         health for entire field #\"))))(Secondary((id \
         1841b681-1474-419f-b647-3cbd51090285)(content(Whitespace\"\\n\"))))(Tile((id \
         e23e0fe8-903f-4e5d-b991-3a80673e5b41)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3ec70027-5bb6-4672-88c7-b02abda3f731)(content(Whitespace\" \
         \"))))(Tile((id \
         5af0fe1b-e587-4ec5-890f-9bb11b4801a5)(label(recalculateAllHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2aa4670a-158e-491e-8d41-2ee88287c611)(content(Whitespace\" \
         \"))))(Tile((id \
         9a8e62f0-3b32-4174-8c95-d328ac82c874)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9096b178-1717-41f8-83ff-1965a142ccbf)(content(Whitespace\" \
         \"))))(Tile((id \
         9cc46563-2e8b-4da3-a2d7-0109c14d465f)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e60dd699-31e5-40fe-a60e-b96f6a81601c)(content(Whitespace\" \
         \"))))(Tile((id \
         124ed2bd-2965-4d6b-927d-7539fee015bd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0556b5a1-1a94-43f4-9b6a-90aa3442e0cb)(content(Whitespace\" \
         \"))))(Tile((id \
         a4174a7f-7c26-41aa-948a-a125fef8ef75)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f40f6f7d-0b7d-4262-870d-91438fa58b78)(content(Whitespace\" \
         \")))))((Secondary((id \
         4f10b7d7-8055-450f-82b4-4e80713d2770)(content(Whitespace\"\\n\"))))(Tile((id \
         da42f34b-a80e-42a4-9252-c3e235df1c0b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d2fb6daf-f4cf-4c18-8a2a-c806578830c7)(content(Whitespace\" \
         \"))))(Tile((id \
         7f1f16b5-1348-47a4-adb3-64dd4e5f0724)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         876b06fa-641a-4464-858b-2397693b7e80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         89665dbb-ae46-4e00-8f5e-605fb71a682f)(content(Whitespace\"\\n\"))))(Tile((id \
         d8c5032e-5477-4017-8b32-f86f634dc2a1)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d024892-9fcb-4fa7-a962-2671bfdd0df4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         90c33c19-cd8d-4c5a-b473-44e31c531951)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         beae00e0-fb7d-4e70-9a84-7a76c37f75bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7528baad-4e19-4907-867f-69f94d499301)(content(Whitespace\" \
         \"))))(Tile((id 1bebbe79-bebd-4ff7-8605-f010ca8d1de6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7f973d11-72b4-4f00-9e74-33e362a70fc8)(content(Whitespace\" \
         \"))))(Tile((id \
         9514a1e3-b9ad-4d3c-9576-9457283c97a9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3a842600-8a69-489c-82bf-d7ddad0bca34)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         943b5c30-b799-4694-b9e0-14f5d88b2994)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         08a091cf-a3db-4567-9a18-20484096ad8c)(content(Whitespace\" \
         \"))))(Tile((id \
         0510be97-8781-436d-aa4a-f4e6f413681f)(label(rowData))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         87933178-0e27-45dc-8136-bdf45f344e30)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aec44b04-b0ac-4c08-a8a5-d5e7a93c4b43)(content(Whitespace\"\\n\"))))(Tile((id \
         202f9874-9b83-4754-9946-2892ae48d6fa)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         914ec1f8-5e75-49a5-babb-2cb142cdfc62)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2e02ef84-fa43-47bd-b5e6-d3dbf6868ef6)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5312eb4e-9b16-4adf-ab74-89a0ae21bd3d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         891f5f45-79cb-4420-9f7d-2a5ace9e9c6d)(content(Whitespace\" \
         \"))))(Tile((id b2bf5ff0-8d72-4a46-91b5-e6f04b42a7fe)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0e44dcbe-038c-449c-841b-5e6d27d352b1)(content(Whitespace\" \
         \"))))(Tile((id \
         d46a2d75-be8d-4859-80eb-99c22845ff5b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         87b26c6c-8eed-401f-8f54-5dc91df5015f)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         92f9af5c-9020-4eb5-a1e5-1dfdee936855)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a2ffc96b-6e03-4855-98de-29905eb09d6b)(content(Whitespace\" \
         \"))))(Tile((id \
         02b8bd0f-6854-4b5e-8dd8-880b398b87eb)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         66c4480d-9d93-47fc-b1e6-bf02e7644636)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         55624c47-cf68-441b-98dd-55c970b6ec2d)(content(Whitespace\"\\n\"))))(Tile((id \
         a01d9531-f688-4a1e-af62-da232be3119c)(label(recalculateCellHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3348671-8cf4-4611-9896-20ca72f51e69)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f0862fb-e017-47fc-8060-3290bd00b4ce)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08b01d96-203e-481e-a83f-0c6fe51a01e3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4da11d13-afdf-4cd8-b554-e5b4e7286f83)(content(Whitespace\" \
         \"))))(Tile((id \
         bb7079f9-8d00-4368-8b52-887a2ba6b050)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ab4052c-87e7-4560-aa6b-e28a457519a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06e59665-490f-45ad-a2b4-a83264b9ddd1)(content(Whitespace\" \
         \"))))(Tile((id \
         3af10870-2b85-405c-a762-9880e01e1fd5)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b81c99f2-2fb6-4248-857e-f4d0b71285bd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         77f1010f-80e9-4d40-b961-e9b392d39fd8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b7912950-f54c-48e2-b8a5-a43692550cc3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7d2c8f7-f483-481b-a58e-a57eb7043553)(content(Whitespace\"\\n\"))))(Secondary((id \
         0210f615-2e0a-48c1-bcf9-40699d9d6f66)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0c308a1-4213-4f08-ab56-e06a41e197b8)(content(Comment\"# Create \
         initial empty 3x3 garden #\"))))(Secondary((id \
         44750c22-4b76-4b15-a5e0-2c9ac633c74e)(content(Whitespace\"\\n\"))))(Tile((id \
         afed55ed-b288-49f1-b688-95848c82d0f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13729bc6-202e-4453-8751-a7946d6144a1)(content(Whitespace\" \
         \"))))(Tile((id \
         17933e0c-b037-4779-8a98-b64c17df1991)(label(emptyField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e7b17634-9979-494c-bf82-91d055d58623)(content(Whitespace\" \
         \"))))(Tile((id \
         330a98bd-d604-4423-9e91-bc9bf9832ed0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2a4bd31e-e824-4db1-8a35-c3763bafe09d)(content(Whitespace\" \
         \"))))(Tile((id \
         b5c8a7ea-c665-4d05-b076-d407e58886db)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         044ec602-3e64-4be2-b8ef-92cd2a48b753)(content(Whitespace\" \
         \")))))((Secondary((id \
         9282d3d5-8eeb-40ea-8850-8756f0d6837c)(content(Whitespace\"\\n\"))))(Tile((id \
         f0d86cc8-9db2-4028-aeb3-d31311e79c5d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0a5474be-5125-4324-afda-608f7d40e58b)(content(Whitespace\"\\n\"))))(Tile((id \
         31b953ae-a7e4-4295-a70a-484c3550b690)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a0b6eedb-577a-400a-bb53-bbe7a8b511bc)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2f1fc1c-0a7c-4804-a6ad-4085ba8c557c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c080473f-49e3-4ba1-80a7-bcce32a0b928)(content(Whitespace\" \
         \"))))(Tile((id \
         16bfccf0-1dcc-4967-9292-12b38f7aaaab)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34ebc638-c651-4300-8b02-7ce6e9fe1616)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c37c513e-8753-4bc0-bd33-ece0bac18540)(content(Whitespace\" \
         \"))))(Tile((id \
         81a91fa8-b36a-4e81-ae95-24010adc9f8f)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2ed37ea4-3609-451e-9c76-910d52aa9c59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3044001f-07e1-4c6f-b63c-ffb3d4484872)(content(Whitespace\"\\n\"))))(Tile((id \
         1a6d04b6-d26a-44a9-89b9-78927c038ea8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a39863ec-f311-4af2-b6cb-1342601fb6c2)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c21e6d0-9282-44d6-9af7-ee82ab0ccc4f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f9a5560-b131-4917-b491-dda240007a83)(content(Whitespace\" \
         \"))))(Tile((id \
         157a644f-b9dd-412d-b449-a2c56a01cb4c)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4ee0d87b-dafd-4932-a200-1589404283da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2451059d-291b-42cc-bcbc-f0af72518635)(content(Whitespace\" \
         \"))))(Tile((id \
         ce5930fa-a630-417d-ac92-930506b06977)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         83dabd1b-291a-4bf1-8446-857ed5e31a38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48713d79-6174-4d9f-b01d-3c82095c0cff)(content(Whitespace\"\\n\"))))(Tile((id \
         e2a6d9ce-abdf-4247-aaa8-49f9f38a66c9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3b59a81c-414c-4ee1-b608-b8d0da6209fd)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         51b7f5e3-6acf-4c65-b756-cc0937f2453f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e420d99e-5fb1-419c-9beb-c0b0da632ba3)(content(Whitespace\" \
         \"))))(Tile((id \
         e40d7db0-3d5f-4a6d-bf3b-61fdd9ff9a17)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9f403dc-f51b-467f-a013-b3e66e25c107)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         058e8447-cc8d-4e64-8975-349f56856092)(content(Whitespace\" \
         \"))))(Tile((id \
         a07bc012-3ec6-4817-9fd1-3d0c733125aa)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a9f3fed-0b73-4ef4-b1d1-e391a297ef82)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fa7e85dc-d3cf-4a44-9a3f-912fc3a7be42)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a2f22669-97fc-4a19-b865-9d33b378e5ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         850c676a-ad98-498c-b6b5-0cc9950238ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f0863f6-3bd8-4a49-9866-1ed98817ae6e)(content(Comment\"# Initial \
         model state #\"))))(Secondary((id \
         c9f3b8d9-739b-4b58-a2f1-6704ee10c030)(content(Whitespace\"\\n\"))))(Tile((id \
         123e56db-3916-40b9-8e44-b36097c11493)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         490206fd-d3c7-4172-8fe6-e71caa0ca2a6)(content(Whitespace\" \
         \"))))(Tile((id \
         67df6cf6-acf6-4a1d-ad60-308f4981241a)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bca9a19f-e68c-4652-b96b-cf6d8c3cf9d8)(content(Whitespace\" \
         \"))))(Tile((id \
         9b7b3447-6ea6-4412-8c3e-623f7e518b4e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ec7ed8ad-cb08-418d-9ab5-5c83a63b7f1f)(content(Whitespace\" \
         \"))))(Tile((id \
         5d54b660-c7a6-4721-a98b-24494b9165d4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         653d3611-9f98-482e-92d4-9ca94f8abf1e)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f87d930-1086-42e3-acf4-42960dd2215c)(content(Whitespace\" \
         \"))))(Tile((id \
         20ff689c-6869-4d54-af46-5c5dff340a1a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a4a14aeb-32b2-4625-8e08-520ad4597b9d)(content(Whitespace\"\\n\"))))(Tile((id \
         38a0dd98-a65e-4fd8-ba09-911ca209d64b)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d56a6f67-1972-456c-87d7-5e6d137edd7c)(content(Whitespace\" \
         \"))))(Tile((id \
         1cec86b5-38f5-4b5b-8ecb-7c6498cdafdd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88ac0995-daa0-4826-9058-e7a351cb1858)(content(Whitespace\" \
         \"))))(Tile((id \
         75a87c65-6f3d-4c8d-a54f-0b4bf50e1ba6)(label(emptyField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a618fe4d-c7e0-46de-807e-dae450e93c08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72a40753-776b-4987-9408-82438d7f5383)(content(Whitespace\"\\n\"))))(Tile((id \
         d9911398-a2c7-495f-a0c7-7537384ebc18)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42ca3d02-be0e-4cb2-92ee-e9ecd74aee1e)(content(Whitespace\" \
         \"))))(Tile((id \
         eb4d8928-dd6a-4ab2-8dab-68cbcc7bf354)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         186f7b5a-e408-466e-9c96-e935571f6a06)(content(Whitespace\" \
         \"))))(Tile((id \
         8e822e9a-b782-4e2f-9f84-c4b2e490d67b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         abeac3df-89c8-4330-9763-409d246b479b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea182042-9128-4d43-a500-b235f3e2ffcc)(content(Whitespace\"\\n\"))))(Tile((id \
         a0039ecd-3409-4aac-ba2a-03eece162aab)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c247a92-c079-41ee-8f56-ebce012255ab)(content(Whitespace\" \
         \"))))(Tile((id \
         70c29f84-7841-4e3d-9050-a2bb208b9f1a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c5050ae-9b7f-4c7f-9632-2400b17ed475)(content(Whitespace\" \
         \"))))(Tile((id e6d57ae4-72ef-4ee9-995b-b65297c69310)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a36b18d1-3728-4a54-95a4-26cc314f3da1)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         258263e1-1442-421b-8c70-ae34d8d5eab4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a781ada3-7498-44be-b7b0-7b6bb9a80b5a)(content(Whitespace\" \
         \"))))(Tile((id \
         f631fc15-9a6d-4fc1-a2dc-4c2312f87e76)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c25894ac-6099-40cc-b992-7313c83b2992)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df2ca4ea-1e1e-47ef-ab96-955a78c4164d)(content(Whitespace\" \
         \"))))(Tile((id \
         6fc05509-55f8-4976-ba83-71623a4b4240)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c818dfed-2379-4ded-821f-cff2d1c1d35b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d89156e4-505f-46dc-87a5-14383f24ed2b)(content(Whitespace\" \
         \"))))(Tile((id \
         85e678f1-7f3a-4db2-9baa-f2e3d465fe65)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8699f44-26b7-4e27-bd2e-312133a693d5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4342bd7b-51c4-495b-ab9c-9586c845eceb)(content(Whitespace\" \
         \"))))(Tile((id \
         75aba10a-d3f9-44f4-a231-f77a44d13b88)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         04ea6887-d9ea-41af-bb46-14e09cd40093)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bede883b-91b0-4692-a549-7f5178d95610)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f223a918-9c18-407f-aa89-59b832eceb4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4f810a8-b455-47d1-9fc4-e8997ad35536)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7a0a29a-0abc-4b69-95e3-dc980a7c1c86)(content(Comment\"# Apply an \
         action to the model #\"))))(Secondary((id \
         a0921471-8319-41f2-8ca9-0fa855668ed7)(content(Whitespace\"\\n\"))))(Tile((id \
         068cac6e-eeff-48cd-9d8a-83c02d488402)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd90a487-c299-4512-b261-4643eea9b3d1)(content(Whitespace\" \
         \"))))(Tile((id \
         1f9d78ba-4e89-4661-8d93-bad2a1af2dbd)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c7419d9f-1549-44d0-ae00-4c0db714ad34)(content(Whitespace\" \
         \"))))(Tile((id \
         d6929e75-eddd-4609-ae82-73367a1a8d20)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         58e0760e-8d26-46b7-bde5-84ce09015ada)(content(Whitespace\" \
         \"))))(Tile((id \
         279173c2-a9dd-42d6-afc1-2b30e60e722c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         bebecdc7-6015-4a6e-bba3-f5a0ccc91d04)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         907cd199-3612-4474-a7da-a02250cd6efa)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55062522-1491-47fd-b3b7-79c6bab6d8dc)(content(Whitespace\" \
         \"))))(Tile((id \
         07cbbb52-025d-4362-a007-12c3d9d369c1)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         bb7d2314-523e-4ad7-8dff-758d47a773ac)(content(Whitespace\" \
         \"))))(Tile((id \
         eebda367-4991-40f1-97ec-cc27e784d066)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         35862327-081c-4d8e-9727-0b1e8a5338f0)(content(Whitespace\" \
         \"))))(Tile((id \
         39f2c1e9-07b5-42f6-97f8-b02a22ca4aef)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         10f39369-48f3-4c00-ae9b-c902f52a36f8)(content(Whitespace\" \
         \")))))((Secondary((id \
         ab01dd39-d2d8-4914-8184-2b2d8570034e)(content(Whitespace\"\\n\"))))(Tile((id \
         cac0a04e-86d1-4eb8-90a7-4ca201bf7b2d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ca5900d4-8af8-495c-a6fc-804ea3586fd6)(content(Whitespace\" \
         \"))))(Tile((id \
         7f8db542-f4c6-4604-917d-eb52add6eb17)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4a7bcd93-d510-46bd-9c49-ded365ae3c67)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b97b86d4-0af0-4e76-9299-ccf8c89cb287)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         899f7386-01c9-4838-a5f7-3c999ef920c4)(content(Whitespace\" \
         \"))))(Tile((id \
         808139e5-78d1-4e5e-939f-e2de43d11247)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7757bb88-25cd-491b-928d-b33e3946c19a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         637c3757-b87d-40f0-a5af-23fac0f21fe3)(content(Whitespace\"\\n\"))))(Tile((id \
         5d0374ee-c587-47d3-a867-a6259218fd36)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5e0b7057-89a3-4ba6-9244-e80df94d4a94)(content(Whitespace\" \
         \"))))(Tile((id \
         5f1d2d85-4d3e-49af-9f4e-fa7d49d8c194)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92dfb496-2dd3-41ce-b32b-d4d789c674c3)(content(Whitespace\"\\n\"))))(Tile((id \
         0bfe15b5-e89a-4fc0-9372-59b1f5c1e1ad)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2d5e5ceb-1ce0-412d-b53e-ccd7e60489e5)(content(Whitespace\" \
         \"))))(Tile((id \
         d095be46-cc8e-4979-a8a6-cf4575fe8f9d)(label(PlantCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f11143a-2c57-4613-adca-0c29e2bcee00)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         1876a1b1-5643-44d8-ac39-ff2d97021f13)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1be22d0b-0c64-4c4c-b87f-4ae552886bfe)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2e169863-710d-4179-95e3-35a0e4ba0480)(content(Whitespace\" \
         \"))))(Tile((id \
         27d9fe2b-9d2a-4de5-afed-0dc0ff5b1d14)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         27fc0c5a-33af-40af-a701-6bf649d32dfa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a783672-1c0b-430b-a9a0-5a3d1e3a2377)(content(Whitespace\"\\n\"))))(Tile((id \
         2544ffaa-6aa4-4aaa-bce1-07cd247f3456)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         70fc7c0a-e9b6-430a-80dd-432d9eb276c9)(content(Whitespace\" \
         \"))))(Tile((id \
         34c9ca91-08c5-435f-85cb-790f3a07432e)(label(currentCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eea53c9f-5be5-4327-a0d2-119cae223346)(content(Whitespace\" \
         \")))))((Secondary((id \
         d6228bd0-5f45-4d37-bf9a-85eab3bf0d77)(content(Whitespace\" \
         \"))))(Tile((id \
         619e2994-9772-4eb0-8960-b7bee5039e5e)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa37d4a1-a56a-4606-a40b-537d25515365)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5059c952-137e-4230-87fd-1bea91c86021)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         183c59d3-e8db-40c8-802e-9f41bfe7dcbb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0cc40ce0-ce78-462f-834d-770df12b1319)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd9b692c-2670-4102-bc67-426c23ce55a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8f9e479-d397-428c-b0d8-3e149bc586f6)(content(Whitespace\" \
         \"))))(Tile((id \
         85fa1e63-3ccc-4f03-afd5-52e0b7d20a26)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ada1e52d-e232-4b3b-8e45-668a5b47ba52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e948afc-0f31-44be-85ad-adf3cad227c9)(content(Whitespace\" \
         \"))))(Tile((id \
         0a6b88e7-03a8-4ba2-855b-d392e0cb3092)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eabb523a-3eaf-4da4-9c6c-eb57cd140ef5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         142b0ce7-740a-40b4-a489-14e1b8d0a4f4)(content(Whitespace\"\\n\"))))(Tile((id \
         566e41dd-021c-4272-986c-29929390ad1f)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24b0e6d5-3c4b-4d95-b3e0-d795f5fd31d3)(content(Whitespace\" \
         \"))))(Tile((id \
         ffbf4f4f-0369-4710-a88a-739e13f5b2f3)(label(currentCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd713107-d0c2-4f65-a1c9-b6f73ba52222)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         69d7c6c0-e012-4c22-87d2-a046c181247a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c526822-f317-481c-a583-a974848537d9)(content(Whitespace\" \
         \"))))(Tile((id \
         cec1af72-4428-4df7-96d1-42237211ae35)(label(!=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db94de2a-b56b-4c43-9e4e-c64ecbd1c685)(content(Whitespace\" \
         \"))))(Tile((id \
         126cf14f-dec5-4ac2-9622-15f08f044cd6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2cd32f84-ddcb-4732-b924-8033691c75be)(content(Whitespace\" \
         \")))))((Secondary((id \
         43e66155-0adf-4757-a2ca-512a3fa48358)(content(Whitespace\" \
         \"))))(Tile((id \
         097820bd-7f81-46eb-92e3-af396ecc1dbd)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62398afe-d8c2-430c-ba43-94a0527abb71)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c4d0b789-c6ec-4966-a536-c584fe653ea3)(content(Whitespace\"\\n\"))))(Tile((id \
         c7ac590a-8636-4a0d-bc64-0ae9429f9ad1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         425bdf8b-f830-403a-95d3-f95caaaa6a7e)(content(Whitespace\" \
         \"))))(Tile((id \
         61845b72-dbdd-4c22-ab0c-ad137c0634c7)(label(newCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         92b49b9c-01c6-444d-be75-bd09637caef2)(content(Whitespace\" \
         \")))))((Secondary((id \
         76672219-5c2a-4dc1-b68a-475a40d7e014)(content(Whitespace\" \
         \"))))(Tile((id \
         3e684e75-29b3-4b55-8178-02dd09911546)(label(makeCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c433992-2620-49a3-ae34-28bc4c31dd84)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f28c66a-57b8-470e-95d0-221effaf4578)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e35ee7e1-02fe-4fa9-a570-621602897922)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a628baed-ab82-408c-a57e-f58519c7e23b)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2cd67e71-2e47-41dc-9f92-0866bfb7c948)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b84d8880-51b8-4075-9aa4-89c51eac7fc0)(content(Whitespace\"\\n\"))))(Tile((id \
         9bcc1f51-034e-46a4-be60-8f651192e6c1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2e4c4067-e406-4fcb-96bb-7408afaed05d)(content(Whitespace\" \
         \"))))(Tile((id \
         38a7f956-e702-4285-8ef3-2052f6bae1fa)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7dc10443-b12c-4dbb-82fd-3203dacc702a)(content(Whitespace\" \
         \")))))((Secondary((id \
         0cc6c0e5-1f36-4ecf-953f-c9d9cc170b45)(content(Whitespace\" \
         \"))))(Tile((id \
         1e898390-5032-497d-92d2-6d2b61a8be91)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dec50524-972b-4fe2-8cb3-f7653e8802b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ec8054f-8929-4207-97f2-ffe7f6b4118a)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         132f21f0-0630-4a16-98f9-c0134711aaa3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6ae3f3d3-9a5a-47bc-a4d5-12b5584159e5)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce2be6c8-1b09-4a33-8a7b-e053765b96ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01f010a0-effe-4443-939e-47bef2b05ba6)(content(Whitespace\" \
         \"))))(Tile((id \
         c8c68813-93fb-4623-b6ff-37dac818524c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84298971-d0eb-431e-a48f-635c31819d0c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         abe080f7-9d80-41cb-91d6-12cfccca6c3a)(content(Whitespace\" \
         \"))))(Tile((id \
         662f40e7-1767-4812-88a0-d955e7750dac)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         644beca5-971a-49fc-a54c-9cae21c57ac3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cd71ebe-f0f2-4c80-9928-79547c32ea4b)(content(Whitespace\" \
         \"))))(Tile((id \
         040952a8-5e26-4c7f-804f-5b09b86a10bc)(label(newCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         69c275b7-7413-4733-aed9-169175c68b63)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b95b373-93ff-4f67-95d4-0dba9eed7d16)(content(Whitespace\"\\n\"))))(Tile((id \
         984bd9c5-5c63-4f7c-87f5-9ccd0f7ab68f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b8495c0b-263a-4502-a020-ce8ca379c9ec)(content(Whitespace\"\\n\"))))(Tile((id \
         0affaaa9-949d-45f7-8492-08f1d8869ae0)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77ab6c04-5e62-4595-9f30-988969784936)(content(Whitespace\" \
         \"))))(Tile((id \
         0ff652da-04ad-44b3-a536-baa2e121bb9f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4daeab96-8b2e-447e-b4a3-1532459a4e8c)(content(Whitespace\" \
         \"))))(Tile((id \
         72e32401-7f0d-44af-826b-d24b5954f6e1)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df173e59-997c-4b0c-a964-88e028702fb1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddc4844b-6732-4c8c-9b05-d76a97598608)(content(Whitespace\"\\n\"))))(Tile((id \
         24510d89-80b5-411e-9b99-ef219834035a)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fddf3fc3-3244-40e1-909b-5cf4cb8bc206)(content(Whitespace\" \
         \"))))(Tile((id \
         509ffed1-5058-4c63-ac4d-fef382fcdbc1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d088c7dd-1c9c-4fd1-98ce-7b217c8aa886)(content(Whitespace\" \
         \"))))(Tile((id \
         4c02be39-5256-4ba1-a669-a879f59ae423)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8eec990c-e811-4af2-a96c-bc80e7bdb4cb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1b57fbc6-29e2-48be-b712-0d4e3f55907b)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7b0674f-fe74-4121-ab62-b7443edd83f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5776e191-cdea-4738-8602-5a52639b1c36)(content(Whitespace\"\\n\"))))(Tile((id \
         23bad39d-df36-422d-a42d-ae4b6f6f68b8)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14470ad4-c321-44ea-b2bc-4594b3229ec3)(content(Whitespace\" \
         \"))))(Tile((id \
         e431c343-481e-4acf-be41-f8dbb382d05a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d0c8d92-a959-434d-81c7-adaec776bd55)(content(Whitespace\" \
         \"))))(Tile((id \
         538afdda-f34c-4a23-96c8-80b101ca6db4)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7516811b-c9d2-4190-a178-a0e98a3b6deb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6a7bfa65-33a6-4f2e-bd9d-2da4f44f92c7)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b26f068a-3c13-49f0-923e-28c109be6aea)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         05a7f67f-f0e5-4a79-9c52-d2462a549891)(content(Whitespace\"\\n\"))))(Tile((id \
         884b675b-6fe3-458d-9438-e7a5d799fdd4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f714a20b-8b12-4482-a08b-a421abe8be1c)(content(Whitespace\" \
         \"))))(Tile((id \
         f1d7e9d1-1b25-4f39-8dcd-3ea6f12eb286)(label(HarvestCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         32e52baf-94e0-494f-acb2-52ee2262c8c0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         201a4d18-21b4-49f3-b74a-9d7e0a345248)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fb352f40-7a42-4a04-b27b-d836a7555119)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         913d8ef4-cdda-4852-946f-ef3a0ca0f55f)(content(Whitespace\" \
         \"))))(Tile((id \
         0f6f965c-d550-428a-858a-8f7d8098fd5a)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9a7c0e44-ce18-4157-8bea-1a1b6dcb66c9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ede4f29-16e9-4ea4-b63f-70f6851a629e)(content(Whitespace\"\\n\"))))(Tile((id \
         713f7a1f-778f-42ed-9801-e0db1f319d61)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa49982a-64b8-4bc3-949f-088d2ba4e2e0)(content(Whitespace\" \
         \"))))(Tile((id \
         a638177d-ba4c-4f62-8aec-1b1641317286)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e64ab333-eaa8-46a4-b8ea-dcd4763e4e67)(content(Whitespace\" \
         \")))))((Secondary((id \
         fce921ff-8516-4971-a360-c12b26e4c26a)(content(Whitespace\" \
         \"))))(Tile((id \
         eb189daa-bd2a-4dfd-beef-9085643a7235)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b8c4c1e-2bff-4d5e-b5a7-09c2c94ddbad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a616ac40-29a9-4c70-8c8a-cc57584d2866)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c9ea0e3-713e-4a09-a815-d01eb87b3cdf)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2396816e-8123-4e18-a2aa-c76cfc127da3)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae2e1737-b559-45bd-9e53-e6446998f7ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfbd38c0-4bda-43d1-aa1b-341755bec7ce)(content(Whitespace\" \
         \"))))(Tile((id \
         b5a41e35-3305-4921-a56b-6f26cdba40f9)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79f57fe9-39ba-49bc-89a9-a2faf31e9e7c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea56c01a-fb1a-4f90-aac4-f81302e558fd)(content(Whitespace\" \
         \"))))(Tile((id \
         477428a0-a2ba-4d26-ad84-1b8b1dee0308)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd9957fe-4d8c-425b-8517-93a3e9b96038)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         599dcd36-4502-49c2-9b23-0f059d58e12f)(content(Whitespace\" \
         \"))))(Tile((id \
         68fa3155-8941-4d90-9d69-36cfeb412d30)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         62bbdffc-7589-454a-b37e-77bfa6511792)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92fdf1aa-2094-451d-86e1-c5fc8a6f95cc)(content(Whitespace\"\\n\"))))(Tile((id \
         62c561cd-27f2-4ea2-b853-6261c9b6e5b1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5ff52254-b2fa-4f36-8889-5cff5bc0766e)(content(Whitespace\"\\n\"))))(Tile((id \
         0e634eb2-8c1d-4815-b478-df45d6d09037)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3e284ae-9d82-42da-beb4-e25c95301742)(content(Whitespace\" \
         \"))))(Tile((id \
         c433e4ee-8b89-4d32-b484-e8dfcedd0259)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb07d38b-f5c9-4f23-ae50-1d80628bf2a1)(content(Whitespace\" \
         \"))))(Tile((id \
         00702840-abba-4ab4-8344-ec530ec0afee)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         324a129c-0bc0-4f6c-af45-afbc533be0b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         960d09ec-eed4-4dde-bd29-348e056c5b19)(content(Whitespace\"\\n\"))))(Tile((id \
         61689a33-239f-474b-bfa7-18517d49888b)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b62bf0ae-e4ba-40ef-a47d-8387a8465f6e)(content(Whitespace\" \
         \"))))(Tile((id \
         0dd5306c-0f3a-4c6d-99cd-8b02f9916963)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c7fc76b-97f0-4b50-a3bf-61acf8d51f25)(content(Whitespace\" \
         \"))))(Tile((id \
         a997ddcd-d3d6-4a3e-80e7-39a6bb9c94cc)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fa94de3-384e-4eb2-bce8-1a9ef97c75f9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8aaf826c-5b61-4277-bcef-9f8f20a7d043)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ceaf46a-8f8a-43f4-9209-fb796cd5c9cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e6eb687-e556-42ae-b8bf-13839dc17afd)(content(Whitespace\"\\n\"))))(Tile((id \
         cfb7d5e9-6627-48c4-a854-3f9e718d60c7)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d825986d-a31f-471a-a3d4-4a2943539c70)(content(Whitespace\" \
         \"))))(Tile((id \
         0c1dbd9d-0a32-443e-9f2d-9bcefd799b7b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8b002a3-6238-4ce0-9815-88cfe3a20d5c)(content(Whitespace\" \
         \"))))(Tile((id \
         5df2f187-6f0c-4a4b-8a17-35d2ae40b875)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2adde46-fe2c-4850-b712-3c5b1afba716)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a1c50d9a-7ffb-4356-9f31-bb72a6fe059e)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd40ae38-98d1-4930-a04a-9d0d075e3d63)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c44f443e-4dc5-4caa-9f9a-61b864e30671)(content(Whitespace\"\\n\"))))(Tile((id \
         ffef4a0a-077a-42b7-adac-c63fb6c7215b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c97adad2-cd45-4c86-99f4-34576d27daaf)(content(Whitespace\" \
         \"))))(Tile((id \
         92839b1d-eb00-4365-a8b8-a682915a2c1e)(label(CalculateHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         160ec16c-db5f-4ae4-a737-7b32a68847ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         54e7ecfa-21dc-4ba7-ad6d-feb16aa90e2a)(content(Whitespace\"\\n\"))))(Tile((id \
         b4090c8a-12ef-4918-ada7-db3cbbbdda90)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5443eea-2e1a-4cd9-8163-56e2d17f3831)(content(Whitespace\" \
         \"))))(Tile((id \
         6cf01d86-87d3-4911-b6df-d4dd58897df4)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6ea9a4a2-0e6a-4540-ab8d-a5bbb9e7eb63)(content(Whitespace\" \
         \")))))((Secondary((id \
         305b406f-ea00-4b7a-84e5-aa6da08e48d2)(content(Whitespace\" \
         \"))))(Tile((id \
         a589f741-b193-47de-8ed6-66a4bf9b5312)(label(recalculateAllHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d56bcc8-eabb-4193-88dd-88732c5bc6b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ad805f6-2245-4f4e-b1ee-19b966ed359e)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c3fd1c5-53b9-413f-ac90-e959c187f38f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b53005c0-c11e-4e45-87c1-c6e20a117337)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a184baa2-24b4-48b6-87b4-a7f078ba4ba9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fcf1ac4d-a30b-4bc3-acbe-e096bbc4cfa6)(content(Whitespace\"\\n\"))))(Tile((id \
         db9127fd-5628-4cea-9a6f-f69bcd78f54b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         53ea88a7-4104-4b67-a937-570f9abfc128)(content(Whitespace\"\\n\"))))(Tile((id \
         b491918f-1328-4fda-847a-a5ed97e74488)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8fc0649e-b4d0-431a-9e29-16c4ab499ab4)(content(Whitespace\" \
         \"))))(Tile((id \
         d50b999b-d3e9-4575-be88-614390dd7549)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f478a95-2547-4374-8ffd-8283726b4d83)(content(Whitespace\" \
         \"))))(Tile((id \
         a6d8f8c9-5fd6-4115-be11-4747804e5b85)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd79b11e-cd90-4c05-b976-bc68bc96e9a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fbd36a7-6ae1-46b8-88be-85b67defe779)(content(Whitespace\"\\n\"))))(Tile((id \
         abcad4f1-cfe1-4419-8634-690f5341ec1a)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         695a9943-be8a-4f2d-b10b-5e34d25708e0)(content(Whitespace\" \
         \"))))(Tile((id \
         4bcbe4b2-aeff-415a-a1a5-34dbefd2ec59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2def8eb-a0fb-4508-b876-cb683ab0c1b8)(content(Whitespace\" \
         \"))))(Tile((id \
         23aa747c-ee33-44b5-8766-a86883c40275)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a841a34b-9ba7-4f32-bdef-aafb1f9f5c0e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         281aa859-862b-48af-9e51-9300dfbfe373)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2472acd-f35e-4c62-923f-50df9ccdaa4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d3f74c7-9736-4902-86d1-df9e86fdf367)(content(Whitespace\"\\n\"))))(Tile((id \
         0290f739-4338-47da-a4bb-418284272327)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6fbe52da-641f-49c9-ba8c-266e6f0118b7)(content(Whitespace\" \
         \"))))(Tile((id \
         d8b20fd2-9d68-4c7e-8ee9-d9950aa94d8c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fff2b5fe-2cb2-4148-9e1e-44d4eaeeb6c5)(content(Whitespace\" \
         \"))))(Tile((id \
         9c0320bd-3a98-4795-9d56-e8d59722ab20)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b6edc90-0c0e-4d8d-9851-73069e54a5a7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2f9c8301-798d-4f11-98c2-9e32f814ad33)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dbe35de8-8ec9-488c-9d39-06819339f95e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4658291c-1621-4640-a12e-cd5243dca8cd)(content(Whitespace\"\\n\"))))(Tile((id \
         cfd8d334-4e88-465f-b263-d7a8e2c10253)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         59dad58a-cd77-41a4-afd3-f167962e15d1)(content(Whitespace\" \
         \"))))(Tile((id \
         95c13267-22f7-461a-a9b3-1f866ee67a15)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8c847522-2d37-4c48-b3f3-20ee90deffc2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a442f92d-41b0-4bb8-b551-f2f9a278f93b)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         fc40f1cc-9e44-49ee-a89a-5ab61f3665bc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d27849b4-9f67-4cae-a4d4-1f25e59a15b3)(content(Whitespace\"\\n\"))))(Tile((id \
         b43295ad-139f-4443-87a3-02690d28f51c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4e54234e-f5c2-4ef6-9fe0-b065cfc62816)(content(Whitespace\"\\n\"))))(Tile((id \
         3fa38106-0c82-4209-9318-f3d39c6ab216)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b4d51db-c89a-4d47-acfc-4a70171388a7)(content(Whitespace\" \
         \"))))(Tile((id \
         f4a2d166-ca20-4604-8c88-a3df3c4021a5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6ee2abb-0698-4dac-90ca-9e84d0d75adf)(content(Whitespace\" \
         \"))))(Tile((id \
         052989c5-60ec-41b8-8b30-008e6e020cd4)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d4d9318-383f-4f11-9fe4-46a53e71e1f1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         de97889b-d787-415c-bc95-6c3726ffc755)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b265bd88-5385-4333-9873-b75297798c04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45177f0e-fb91-437b-aba5-192bd19726a1)(content(Whitespace\"\\n\"))))(Tile((id \
         f2c2dd43-2ef4-4a0c-b2f6-8783969dd272)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         009bbb68-141b-401c-a34b-58b7e072e60e)(content(Whitespace\" \
         \"))))(Tile((id \
         15f7c2a4-c6ea-4797-8117-0087444c4dfc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2209aee4-e2ce-4684-b1f1-2e1ffc5cbdf4)(content(Whitespace\" \
         \"))))(Tile((id \
         9b89f496-a3bb-4c29-9022-da41f745851a)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd1c46d8-aab0-406c-98d1-b885f96d9e91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         90792c4d-4c49-4d89-b402-96e9ee91e659)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9615453-9f20-4835-92e7-e7b6ea1faa47)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0b1d9335-330e-4706-bb6d-eebe0b722976)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b546f62f-4e6b-40a2-8ada-d72ef9d9b121)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b8e6f18-506e-48b4-8f29-752e45d7ced2)(content(Whitespace\" \
         \"))))(Tile((id \
         1280d91a-24f9-4d5e-8a95-113f1293bba2)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e8906403-0e00-4a7c-ba1e-b9de360338d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a833941a-5595-4784-9609-27f6453a348e)(content(Whitespace\"\\n\"))))(Tile((id \
         7cf7e5da-2eba-43bc-992f-dc9abdd861d4)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3acd3d65-f7f3-4582-80ed-dcde508451d1)(content(Whitespace\" \
         \"))))(Tile((id \
         8767043c-2007-4647-8b8c-d2348eba7ecf)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b116b80a-263c-4843-b5fc-6f9f8125e9bb)(content(Whitespace\" \
         \"))))(Tile((id \
         0d4ba119-099d-4c49-8fcd-eee069484308)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f988fb3-c6b9-4d93-8f03-bfdda2ffdece)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         672c5310-c218-4d95-b905-b448a49860d3)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3a6c810-3517-4d7b-88fd-1584e7542a65)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1b31ecc2-4c3e-4f85-a528-72ea2741f3e9)(content(Whitespace\"\\n\"))))(Tile((id \
         63a10871-b4cf-47a1-a014-b62d9addbd08)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7ddf25c1-23a9-4825-af42-3d1e6397d04c)(content(Whitespace\" \
         \"))))(Tile((id \
         a1a2e3d9-e64d-4f82-b4fd-0fc183708089)(label(WaterAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d86cdb2f-56e6-433e-9d47-0819d2e23342)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ebcc0d4e-9060-4054-8c67-14a4cb54d691)(content(Whitespace\"\\n\"))))(Tile((id \
         4524d449-2852-4416-b458-ddb8d08e1f25)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4cec2b63-5048-4b90-b22b-9962b9546328)(content(Whitespace\" \
         \"))))(Tile((id \
         71cd8c42-186c-4c77-8756-3244f2b768af)(label(wateredField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         02a433a1-950b-4a8b-99cf-a2ca96de46a6)(content(Whitespace\" \
         \")))))((Secondary((id \
         196ebe82-a177-4c65-b77a-350c2a02c545)(content(Whitespace\" \
         \"))))(Tile((id \
         0502fe1d-43f3-471a-9044-05fe5c5b0a4e)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d00195b8-9895-439c-a7e4-89f9421a99ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7033b822-8f19-4914-8f1e-3d2fbe14a6d1)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         669a0a1e-6a1a-4a59-a837-bc278dd2d2f1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         944b3a2c-e00a-4ee3-a657-64d261cf6562)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1117b955-b169-4b9e-abca-73ae890fbdc1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47fe7b2d-be29-481e-b0e0-a9fd17357177)(content(Whitespace\" \
         \"))))(Tile((id 877f6d6b-c472-478a-870d-a0b9d5ee569d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ec470e88-3797-4a76-8f6e-03157aaa530d)(content(Whitespace\" \
         \"))))(Tile((id \
         c3a90afd-32e1-4890-9bc8-bc07ca54b6a0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3bb96634-e859-4094-9dc3-4352d21ffe4d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         068fd54b-bee5-4b79-b96b-5304e860abda)(content(Whitespace\"\\n\"))))(Tile((id \
         26136d8a-f0a4-4208-8ecf-eb499d892cac)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f55834b-5716-44cc-8f2d-e8ebeae384e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b02ba7c1-a682-47c1-886b-7054af6df957)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6da2c58-ea16-4bc7-99a9-670068fb12cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b265997-2621-4258-bf68-85194948162c)(content(Whitespace\" \
         \"))))(Tile((id ecec44b0-6b7c-4e9c-b699-78b9cca1ce37)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6322c02d-5858-4d05-9247-e53b6ceda181)(content(Whitespace\" \
         \"))))(Tile((id \
         cb18f6a5-35f3-41b0-983c-c66f5d782131)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3a698fed-8dcf-479f-8115-bbc3ea9055ef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d1c3af2e-a945-4933-be5e-c6e8e4658301)(content(Whitespace\"\\n\"))))(Tile((id \
         7fbaf521-464f-4c2e-b134-e3ce0c4b65ca)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d8b538cc-f1ff-4619-b0fa-6a3c0d7d35d5)(content(Whitespace\" \
         \"))))(Tile((id \
         b129a4b4-daab-4e2c-9291-594bf486f7e9)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76c3bdff-27b6-4a46-b8f0-d1254ed9e751)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ad872312-9826-4be8-82f1-bea4ca306c55)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02ce7369-4879-4056-ae0e-6382ce23baa0)(content(Whitespace\" \
         \"))))(Tile((id \
         5b4fa668-a995-4a7d-98ae-af73fdf551da)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc0f137e-e7df-475d-acd1-4f67a5318e93)(content(Whitespace\" \
         \"))))(Tile((id \
         d78ed335-f71c-469a-ba98-95cf93aca178)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92a16b86-8091-4773-a221-d9debf5909b9)(content(Whitespace\" \
         \")))))((Secondary((id \
         c597ec85-0f0e-4692-aa97-09feb87d3f94)(content(Whitespace\" \
         \"))))(Tile((id \
         d1cefdec-a715-491d-9b82-a0e9c577e314)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74ddd867-72fb-470e-8fd5-fa8816b8d9a7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dcca1c27-b42c-4275-8057-7baf8dbaa940)(content(Whitespace\" \
         \"))))(Tile((id \
         ba43a590-968b-4389-a402-156f86467a12)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e34d0711-faf9-4966-8222-533671f50704)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ee33ce6-0930-4162-a812-cc294e726cdb)(content(Whitespace\" \
         \"))))(Tile((id \
         2c207c3f-bf95-45dc-a35e-b2596a2d23ed)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03f87602-cdfe-4cdb-8e77-678715935817)(content(Whitespace\" \
         \"))))(Tile((id \
         9f6f0462-d13a-4fc5-8c3a-9acc5b0d5bf0)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2595ce2b-0318-41fc-8c75-6685a680ba3a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9b58da3d-54b4-48ae-82a8-3fb228671a60)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbf2cec5-0b65-4307-97bd-05e231545d10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3f6d717-e099-4caa-9bfb-62793a7966be)(content(Whitespace\" \
         \"))))(Tile((id \
         19932e68-fb2a-4696-b868-71c34fd72ff6)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         30712410-43ed-45af-a3b3-9fd61564f41e)(content(Whitespace\" \
         \"))))(Tile((id \
         7b5decf9-5b4d-42fd-82a1-fff5aaaf4377)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99d0d06b-d859-4370-8ace-f117048b0695)(content(Whitespace\" \
         \"))))(Tile((id \
         70546042-006c-4685-947e-cc1318963660)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb4732cd-b3f3-4d24-a622-fa6cf487e671)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4c08d3e-3674-41f8-8966-f2e5709e6b5a)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d1ee7a1-04dc-4a28-8fed-d356e5c3a01e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         290db99f-7335-4bd9-a689-80a9c6532d10)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34c364b4-5ebf-470d-b133-d5fae6d9f9ef)(content(Whitespace\" \
         \"))))(Tile((id \
         35594c58-2127-49a3-93d2-23bdb8875ba8)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         352659c8-0ffb-44a3-ad73-36ed2de1306d)(content(Whitespace\" \
         \"))))(Tile((id \
         37c6f974-3eae-4a9e-8b8d-0ebbc34e8d0b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3fd98686-f9b8-482e-af17-747a3c46b7a8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7e88836a-cffa-4b68-8f6d-6af50350e697)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9aa7389a-4c99-4f56-b77c-d15a2b8f8b78)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         86a9d4d2-2b94-42db-a591-ca4e81df84bd)(content(Whitespace\"\\n\"))))(Tile((id \
         c8dae80a-1da8-49c0-a34f-db2e7f726779)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         14f8bd28-f6cd-4216-819d-0a96a7d2e641)(content(Whitespace\"\\n\"))))(Tile((id \
         79ce6f00-9ab0-42cb-b5d5-f65cce084d71)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8404fe6b-485e-46dd-98f0-09aa163c0c3c)(content(Whitespace\" \
         \"))))(Tile((id \
         b9ddca19-59d5-49c5-8f6a-ec3d7d1c02a7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0affdb4-9035-4890-a4c1-3a5ea6d8572a)(content(Whitespace\" \
         \"))))(Tile((id \
         d970b72f-3bf0-4714-8c78-f1441b0ce541)(label(wateredField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a2ad4cf-10b0-4da9-aa80-6dc581548471)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1c25cfb-9f87-4592-b8ff-353926caaedc)(content(Whitespace\"\\n\"))))(Tile((id \
         588e3685-4512-42a9-9385-70d2221a1923)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d2d7b599-2ae5-4c5d-801e-0ecc5c4073b8)(content(Whitespace\" \
         \"))))(Tile((id \
         97e808ef-4ccb-47b5-be25-8abefc526ad0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         194db62f-1d84-414b-a407-1f3f4fd38360)(content(Whitespace\" \
         \"))))(Tile((id \
         87eea356-1091-40b3-86dc-699ce6f764ae)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5ed401a-7172-4828-bdaa-593ee2676b62)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         48b9a6d1-1c68-494d-8df0-5594a418dceb)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02100e7f-d14d-4c00-9c51-038bf6222af8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48a0fd7c-fc22-4421-8df7-ea1b46cb0a4e)(content(Whitespace\"\\n\"))))(Tile((id \
         a7315a19-b8c9-4b99-a563-4a58ee932df3)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62ab6752-a079-43b3-83ea-db760bdb1656)(content(Whitespace\" \
         \"))))(Tile((id \
         a95ba08d-392e-4414-9a99-7835d10447a2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd24a348-818a-4d89-9f04-ce519a201879)(content(Whitespace\" \
         \"))))(Tile((id \
         16c4c1a9-fb1e-4e02-abe6-71fa9ced8c76)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a175cc04-a3be-404e-b5e0-fee48bd226b2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d9ae50c7-7ae2-49ea-9390-71d3c15bbae0)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a300b6d-fe09-4198-a65f-2b8fa3303a3d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fba4f441-7b83-48c2-b418-a75045e11184)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6ce58667-db0c-4010-8a15-7bb5887bea90)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         132dd5de-2372-4ce6-ba2b-d4be670b14fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e289e30-79c2-4039-85b5-00dbd4f74a3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         78c680df-9591-467f-82da-fa8645aa0b18)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         32b8b3da-493b-42fb-96c2-ebc19449cdaf)(content(Whitespace\"\\n\"))))(Tile((id \
         5d9981ff-0fde-4165-9298-bd4cce1528e4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a614eed0-cc3f-43cf-a4fe-8238dad9a7cb)(content(Whitespace\" \
         \"))))(Tile((id \
         beb825b0-6dff-40b3-af8f-5b80336aeff4)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3011b637-cfaf-4bf3-88fc-a24440b58736)(content(Whitespace\" \
         \"))))(Tile((id \
         c19fe6ea-04c9-4b0e-92cf-37d1ad9370b4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1b988bba-61c1-47b4-9d0a-ff9b0bbbd498)(content(Whitespace\" \
         \"))))(Tile((id \
         826ea3ef-5e2f-4a43-8ace-c9a52612ae88)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         81ebfda0-983a-4504-a1d6-796fe2ff820c)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cd00cdd4-e1f3-444a-975a-dff984cf32d5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         033fa66a-8a01-4807-906b-f8c466cdcb66)(content(Whitespace\" \
         \"))))(Tile((id 5274682b-0fb2-4738-8fb3-4c85a0f2c271)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         664850e0-bce3-4e2d-b57d-af8e22c56c59)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         b80eb56b-9724-45d3-8492-f7c954656f3c)(content(Whitespace\" \
         \"))))(Tile((id \
         a7cc1eae-09c9-4cd1-96d9-16907330a90a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2303d8a9-129f-49fb-b27a-21a7b05c314d)(content(Whitespace\" \
         \"))))(Tile((id \
         3d440da1-3635-41b0-896c-ce67c6d2a0b0)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         82f08717-8e0b-4b6e-bda6-cc6765189b91)(content(Whitespace\" \
         \")))))((Secondary((id \
         af73d026-0796-4720-89dc-18080e6f093b)(content(Whitespace\"\\n\"))))(Tile((id \
         c4ff97ec-7726-48d0-93db-1a80fa1283c0)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f59989b3-8aff-4a83-95c2-b9114b8725f3)(content(Whitespace\" \
         \"))))(Tile((id \
         319f2cd8-b8d8-457b-8b78-cea45fbbf08a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2f2a1e9a-a0e5-4892-a281-0c700a018b5c)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         acd04daf-4cfd-4618-b4d8-457fab28c610)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5e1f6dd8-65db-4b4d-906b-755699d84cfb)(content(Whitespace\" \
         \"))))(Tile((id \
         227afa11-75ca-41c1-8d59-33fef205dc75)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8a85cf33-be45-4005-84d6-c241d440af3a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f38f385b-0c90-4f39-962e-8b5a48aa95f5)(content(Whitespace\" \
         \"))))(Tile((id \
         a116c511-f638-4d48-9e9a-e0cf363cab14)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5e1b68fd-0fc7-4042-9dd2-70ac4098c7c5)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b886740a-0333-4b3f-a35f-79e8300ab2aa)(content(Whitespace\" \
         \"))))(Tile((id a4b241c7-091c-4904-9b26-730b0de232d7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         262ae0cc-5092-4313-b2ce-af70d36703a0)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         49c3fd93-a57e-4b4e-a060-eaf72389f2f5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         832281cb-affb-486a-a56e-f3d601b87fe6)(content(Whitespace\"\\n\"))))(Tile((id \
         72b0f4ef-3f5e-4122-bc00-4adf22ad74df)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0057b154-6ca9-476a-b00b-7c6bfa72e488)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         368e437b-33e5-4344-bc33-e04d6c0f6a22)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         529b5f67-beff-4d3c-890c-2f3351d80c18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1854be8a-b422-4afb-863c-ebed47ee4a69)(content(Whitespace\" \
         \"))))(Tile((id \
         161ee613-940a-4fa0-8b9d-ebe5bb72029e)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e304d378-3754-46d4-b6e1-018b817d104d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d669c65-ebb5-43a4-9750-3e8963a98311)(content(Whitespace\" \
         \"))))(Tile((id \
         16ae554e-9f4d-466c-9306-fbdf60d1494a)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e372ffac-a612-4b96-9da1-3e2090248689)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6220832b-d851-455e-a2ba-ab94c537d8c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         61eb748c-1853-46ad-8fe2-6b2af3f1b1e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0b53046-bd46-47b9-854e-c09596f82f61)(content(Comment\"# Helper to \
         get health at a position #\"))))(Secondary((id \
         b4f023e1-0d34-43c8-8f6b-d82af2f99495)(content(Whitespace\"\\n\"))))(Tile((id \
         6901b753-ff68-45db-8df0-59bc9497d4d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         35cea60f-927f-488b-92b6-2624ad6126c3)(content(Whitespace\" \
         \"))))(Tile((id \
         e933c1c2-c0f5-4bf6-8885-162cc48e29f7)(label(healthAt))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f4119648-e0f5-4b82-9e40-d5da010121de)(content(Whitespace\" \
         \"))))(Tile((id \
         acbbb0d8-d7f1-4eb9-81a8-33ae491f810f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         063fe4c1-b540-4bd3-902b-726ce51dcbab)(content(Whitespace\" \
         \"))))(Tile((id \
         85834c4f-9271-4eae-a051-f5ea2fce8079)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         94047e12-2a93-435c-bc43-ba7f82ad75d1)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a8bc3489-e134-42d9-b846-60a7e2a9dd9f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28290e92-2664-4421-9946-9fcaf7f6af46)(content(Whitespace\" \
         \"))))(Tile((id \
         c7ee60d5-7dd4-481f-83bd-e538c3e5998b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a11c22b9-cb42-4487-b618-0cf4bd210405)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         111f3456-3c4f-4c75-960b-d62e5fdb3a15)(content(Whitespace\" \
         \"))))(Tile((id \
         5f9070a6-c0fd-4b13-857d-d8a85cf437b9)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         382dd180-2434-4739-8685-570f0a3c367f)(content(Whitespace\" \
         \"))))(Tile((id \
         c394ad63-9e25-4079-a974-94f48c199303)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ed15c73-2fc9-4ea7-8bbf-5f66c5e310a1)(content(Whitespace\" \
         \"))))(Tile((id \
         9a69721a-d8e1-44f2-8706-fc03d5b01996)(label(Health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         10a7ea01-26ad-4ed6-99b8-7a3d51579f0c)(content(Whitespace\" \
         \")))))((Secondary((id \
         d19c35f9-5b8a-49ac-bde3-96d3957e2f60)(content(Whitespace\"\\n\"))))(Tile((id \
         a7a85ed4-a1cb-439f-b326-02955589150d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1700214c-2d8b-4480-b950-5b3b5f0ef1f6)(content(Whitespace\" \
         \"))))(Tile((id \
         3afc2997-bc82-438c-8b77-ba7c2f980ff7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bb9d45e8-4781-4169-879c-908c0b5a90d4)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9fe77e3e-13ed-44c3-88f0-384d18c3e7c2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ad3c6a23-cdaf-4e01-8c46-9f4d1dc5335a)(content(Whitespace\" \
         \"))))(Tile((id \
         8f8e4ef4-3fe2-4743-9f94-95fd73529141)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5d527bed-9e49-4d71-8eed-1b46140f4310)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3e5b8cce-9c02-4931-a237-0f0d4d300757)(content(Whitespace\" \
         \"))))(Tile((id \
         02ad57e7-4df6-4abf-ba67-2805be53de86)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4b2e32a4-72c7-44f3-b58a-991d31efd4c1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ec8191b5-2286-4e22-9e31-01e7128004eb)(content(Whitespace\"\\n\"))))(Tile((id \
         8a5a3046-ca82-46d7-a438-d03a49f6d9c9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f769b0da-64d9-4cf0-8007-1870e47b17c9)(content(Whitespace\" \
         \"))))(Tile((id \
         00d6e2f0-9853-467f-a1d3-33db44ca00c7)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e7b6320b-d8f0-49d1-b01f-fc4f6cd76b18)(content(Whitespace\" \
         \")))))((Secondary((id \
         975b56bd-80f6-48d0-a193-5fb146e65dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         098130fd-649b-41c7-8140-51e120330604)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a688994-eac7-4bd6-ae2b-240d97b3d364)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58b855ee-b551-4c79-8c75-b23fdc580b08)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd503fb8-adf2-45b3-ae1a-adaf1410d84f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         80001622-bc7e-4ab8-bf03-43d60ff5de6c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a410d834-342a-4585-9f12-6f86d829ff54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1945aca6-793c-43aa-93d1-e90edfe7a309)(content(Whitespace\" \
         \"))))(Tile((id \
         0a90b607-c9da-4a69-b3d9-e76d0de25468)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         264bd1dd-3913-4be3-a6bc-bdf47a996f7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64bd904a-833d-4fca-a9e1-8ff5c9623684)(content(Whitespace\" \
         \"))))(Tile((id \
         1c31a3b1-e588-4145-892f-8eb39c37ceb6)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         72d60e6c-2999-4405-bf7d-afdd22636f02)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34aae84b-d255-4388-be18-0b7b02ba9989)(content(Whitespace\"\\n\"))))(Tile((id \
         40ea7f03-5380-46ea-89ee-6e00d319509a)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbb7b1ec-039e-4ea3-b2a8-55816afe8bb1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         41782e34-53a7-427a-80e5-85ebc5121da3)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00dbde77-64dc-4770-af8e-0d681ab31c26)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         22bdb5d2-4e4e-4bd0-85cc-043f04ccf55d)(content(Whitespace\"\\n\"))))(Secondary((id \
         2488f9de-fda7-438c-b4ed-a9a2d7597f4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e4abe84-7729-4160-9917-26b82f8348f1)(content(Comment\"# Helper to \
         get crop at a position #\"))))(Secondary((id \
         175ddc0c-5223-4279-be1d-9f9da1d579a4)(content(Whitespace\"\\n\"))))(Tile((id \
         92bb9c3d-f24c-4290-a8e1-f9885736b8f1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         454160cb-530b-4e74-aecc-2e7acc733c29)(content(Whitespace\" \
         \"))))(Tile((id \
         9bbf5c3b-e866-4070-88ad-134355b3933a)(label(cropAt))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         766ae0a7-a06e-4dad-b05e-7c38f0922797)(content(Whitespace\" \
         \"))))(Tile((id \
         bf364c73-0232-4782-8feb-2941669eafc7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8bb9d21-8b49-41a7-8539-fa35d1c582d0)(content(Whitespace\" \
         \"))))(Tile((id \
         e058aced-53e7-493b-acd3-96897f0807b4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ea2dcb6e-f443-4da6-a1d7-1eba6d14da17)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7c0b6308-b59a-4f22-9606-a02802d35c48)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b30b119f-b32a-42fe-9526-cb4137393f64)(content(Whitespace\" \
         \"))))(Tile((id \
         44534c2d-9423-4094-99e3-457dfec0d602)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c374dca6-92ad-4acb-8987-4c14134f7431)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         14d5e9c1-421c-4638-ae1c-62e77479edcb)(content(Whitespace\" \
         \"))))(Tile((id \
         34e7fb21-4163-4aff-817b-2c5e9a8cca9e)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         51ddd010-6039-407f-9a72-66748d1575ad)(content(Whitespace\" \
         \"))))(Tile((id \
         d3869339-2b29-4d21-aaf7-e2d48f21b773)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bfdd82d7-48ae-428f-8734-fbb5ec515a2d)(content(Whitespace\" \
         \"))))(Tile((id \
         93e993c5-0dde-41c2-abc7-5d75dad4d515)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3b1fb32d-3040-40c1-aa04-e7383e386927)(content(Whitespace\" \
         \")))))((Secondary((id \
         01e73e2c-5023-44f6-8e4f-82ec8449b95f)(content(Whitespace\"\\n\"))))(Tile((id \
         37e4031f-59d2-4c44-a539-c2cbfc36dee3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         00f88214-2b4a-429e-b5d3-a901bd876559)(content(Whitespace\" \
         \"))))(Tile((id \
         d7bf759e-9013-43e0-95d1-7f64190aff5b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4d2e4b33-1c15-43a8-8e9e-c27b3b341b16)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f7b90166-11f6-4e41-9a52-82567181b1d9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b42d9f8c-11ae-4d90-9db1-fbea9e4f4fbf)(content(Whitespace\" \
         \"))))(Tile((id \
         08af2bff-4256-4f21-9a68-48beabe7b817)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ce81b42c-0e16-4325-9f9b-ca894292d565)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3fc4b73e-1c4d-483c-b647-5fafd1f33f71)(content(Whitespace\" \
         \"))))(Tile((id \
         d1cd62c1-f24a-4e88-9e4e-303590144146)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1c02045d-142c-495b-bdea-ddb9b4742a24)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ee2383f6-1042-4e9e-b700-8390505378f2)(content(Whitespace\"\\n\"))))(Tile((id \
         fbc03efd-f981-48ac-b046-45d3487e526f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         733a1a61-252d-4418-bc41-f9151e5fad74)(content(Whitespace\" \
         \"))))(Tile((id \
         0e2b9795-9e84-4be6-a07d-68bc55787e96)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         78c418f7-9a70-439b-a2c1-417f7320848b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a2b4c6ca-6a22-4bbf-8751-fa0dc0e7691b)(content(Whitespace\" \
         \"))))(Tile((id \
         006e561f-7838-47c4-a14d-142561b360b6)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c9d7d03-9a06-4ae1-8016-2972af91940f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf83a3a4-6914-4781-ba96-3543e6eaa10f)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca527c91-abb4-4e62-a1d7-84a86e3882bd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d8f8a656-b298-4c5b-a1dd-f2ae11098d26)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92cc9cf4-8409-43ee-8957-0227cbcae081)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ccdf159-f642-4425-8e29-4af3cd5cafdc)(content(Whitespace\" \
         \"))))(Tile((id \
         999c5257-e1aa-4311-986f-f0f535afc1b4)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9036bc1e-eedd-428e-98b2-18ed7ea654bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a7baff3-4be2-46ce-861c-9ae6b7fb2937)(content(Whitespace\" \
         \"))))(Tile((id \
         eef4fc82-19a7-4757-ab16-baa024c68900)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f25219bd-c5a4-444f-bf90-4cdb56f34e31)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f14b03b9-14e5-4fa2-ac6e-6522265c9d96)(content(Whitespace\"\\n\"))))(Tile((id \
         3f8d4f0b-fffe-41fa-80e9-d4fbb83bbb6f)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9884d146-2046-4454-aeff-341955d4a233)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9e28542f-d957-434b-9d18-387bdb4e5a98)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3fa34ea4-e1be-4cb6-8ee4-9c4877f30f49)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         31f48413-406b-4adc-bfd9-eaaec333e788)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b318227-2e19-45b5-8d92-b836a6d4478d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4dfb3afa-b5bb-47b1-9363-072b919ca29f)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         16213565-1cb5-4767-ba1e-daec4a161933)(content(Whitespace\"\\n\"))))(Secondary((id \
         548da3fd-0233-447b-b7df-5b0f95312dbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e1c3c24-862c-4f3b-aea5-dc3418b90b36)(content(Comment\"# Basic \
         planting #\"))))(Secondary((id \
         b78c0d22-8d40-467a-b27b-614d147e8a01)(content(Whitespace\"\\n\"))))(Tile((id \
         a93eb69e-3adb-4875-ac5f-ea166b9fc49b)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0ce03c7d-7912-4b43-bbde-9c7ea22e54da)(content(Whitespace\" \
         \"))))(Tile((id a1d001f2-8eca-4471-bf4a-e86f9deeefc1)(label(\"\\\"can \
         plant a crop\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a38842a-9e0d-4915-97f0-a491cee88878)(content(Whitespace\"\\n\")))))((Secondary((id \
         a9041b68-91af-4032-87b8-d8edac67defd)(content(Whitespace\"\\n\"))))(Tile((id \
         fccee5ef-838b-4c66-ada4-51793133953f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         36612bab-a996-43b6-8409-2daae8587863)(content(Whitespace\" \
         \"))))(Tile((id \
         20b23c7e-d86a-49c1-8b9b-ae1a71ae19f9)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0482caa7-692b-4af5-907e-34ad13dea8cf)(content(Whitespace\" \
         \")))))((Secondary((id \
         46ae8b4d-695f-428e-9841-d4d9debea67e)(content(Whitespace\" \
         \"))))(Tile((id \
         e5f58e9f-a28e-4ac0-9bf6-b5007a80e905)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         094e9feb-5f10-48b9-83c1-015840196fc9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         899f01d7-0bf5-439b-a649-f489923b2b98)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e5ca9b9-93f9-4161-9040-b1a709c6d496)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cb21caa-9d24-4cc6-befd-ae8bb31616f6)(content(Whitespace\" \
         \"))))(Tile((id \
         7ff1e459-c6d9-45a3-9710-5f6052942fa6)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45a48409-c9c2-4a02-a560-23047b6c6793)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67b8997c-918c-4e16-81ae-8dbb6680b96a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f7c90de-63c0-4c03-a3d8-85a370367786)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27eea232-3b47-4315-8e83-8dcd3bd69763)(content(Whitespace\" \
         \"))))(Tile((id \
         2409716d-378e-4812-9378-63ac72066cb7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f8fe0144-7bba-44d7-8dfa-19af0c76b743)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         242da296-13a6-4d5b-ad3c-e7560b606d6c)(content(Whitespace\"\\n\"))))(Tile((id \
         29ff2c7d-dd55-4622-a97d-c15f6b5e010a)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed8cd857-90ac-49f9-8bfd-0e9b90c7dac9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4f791bc8-bb49-433f-81ff-248f5b1ddc21)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62bf81c4-941e-402b-9ae5-ac777bcb9900)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41d50627-5c8d-468a-956a-02ddd37423c1)(content(Whitespace\" \
         \"))))(Tile((id \
         65e78ad3-7271-46a7-a278-0525b1a9c3ab)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eba2f1c3-c4da-43ab-b064-44f855e1d075)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b39110b4-40ac-4131-9a95-61f1d9345203)(content(Whitespace\" \
         \"))))(Tile((id \
         2d04418d-cf63-4fa4-bf5d-95ffc477d607)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfb85606-6f6f-492b-9251-25f328094f33)(content(Whitespace\" \
         \"))))(Tile((id \
         e1c1ebb8-6b39-4447-a9b5-393f590d18ab)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca355260-bf4c-467d-a713-63da3a9dffd3)(content(Whitespace\" \
         \"))))(Tile((id \
         58926ce5-5902-455e-a4e5-0262753e6e0e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e388611c-0388-49cc-8fac-43b3cd4b825d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         03966d58-40b2-4aa6-971a-39621fa4fcfb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48deeb71-81fa-48b8-bca1-c6561d5bffbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         87161f38-6013-4c01-8b27-19669a86a2fb)(content(Whitespace\"\\n\"))))(Tile((id \
         11d1e780-07e2-466b-85cd-56cf9058436d)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         06b214bf-488b-402b-80a8-cea1123a424c)(content(Whitespace\" \
         \"))))(Tile((id 74cdf6a2-b69e-4b45-a708-4021bc95d241)(label(\"\\\"new \
         crop starts at base health\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bc4739a0-51cd-435a-89a7-07f100d7ddfc)(content(Whitespace\"\\n\")))))((Secondary((id \
         1bffc129-3106-46db-ac8d-4c8d28233989)(content(Whitespace\"\\n\"))))(Tile((id \
         f28fc84e-d059-40b6-a078-f36f37624226)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9809628b-6743-4602-bb16-f190954f8d02)(content(Whitespace\" \
         \"))))(Tile((id \
         a209d820-b9c2-4109-b865-a57a2592aeb8)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         06e9685a-bd5d-4454-b215-2132887758cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         76ebb40c-ff05-4f1d-b8b5-652fe132f938)(content(Whitespace\" \
         \"))))(Tile((id \
         e5b92af5-84fb-4edb-8594-885631264aea)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41ecb226-f728-460c-ac1f-120cc142d832)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         544738cb-ebba-4a83-9211-de35fda3fee2)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e9ee26f-b69f-4eb0-818e-8059d58e47a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2477a106-99ce-4f95-9e4f-9c3be862e210)(content(Whitespace\" \
         \"))))(Tile((id \
         1c75f05f-4ca2-4ede-b1cb-d0f73a460cce)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6819582-7955-443b-aefd-cb450d3c2851)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2095f3d6-3377-4966-9a62-db6d032b3555)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49a12c4a-cea4-4689-a3ad-492e6b9532a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58a02bb2-b95e-4727-a5c0-2dcff1436d65)(content(Whitespace\" \
         \"))))(Tile((id \
         1507e694-6173-4051-a89c-af996733a84d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         93fa4c04-4993-4047-aff1-09b4ffe499fd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3000c548-e68d-42ce-a03a-b39f4a72fa2f)(content(Whitespace\"\\n\"))))(Tile((id \
         32c09333-569c-4f5b-a02e-9a0b98c52d70)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         401196ba-ae36-44da-b2f9-e8b4e99d2d92)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         171df75a-760a-4782-98e5-317c370fd87a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3290f9fa-178a-480a-af70-3b00a27d4038)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a271c80-22ca-4ea0-94a8-4507ffaf9a6a)(content(Whitespace\" \
         \"))))(Tile((id \
         1f2b4299-44c0-48de-9844-255f35c6d3d1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1c58191-b142-4c02-a701-b64b656e3bcc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70af3cdd-c57f-44e7-8f21-2fea157aa331)(content(Whitespace\" \
         \"))))(Tile((id \
         b1f478d2-04f5-4ef6-8b45-928df24f6a07)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b5256f9c-29ac-45d7-aa89-521783b2c5f5)(content(Whitespace\" \
         \"))))(Tile((id \
         cc934565-07dd-4c7b-8bf6-6c80af55fa72)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         275305d3-1e14-4184-aef7-eab0a0cd65a2)(content(Whitespace\" \
         \"))))(Tile((id \
         92e8262e-7292-45cd-a160-e1698af95b77)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cacf3afb-2855-4ca6-881e-536a1607e27d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b2bc4340-f739-4336-9ace-d385a8339375)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfd556c9-1cd2-4513-b7e7-87710cf6ef49)(content(Whitespace\"\\n\"))))(Secondary((id \
         88f5b701-5e23-4123-b3d7-fc46f528aab1)(content(Whitespace\"\\n\"))))(Tile((id \
         38aee566-875c-4b1e-83aa-b6241f97c737)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5df4612-312b-4d9a-83fc-23837fca87c0)(content(Whitespace\" \
         \"))))(Tile((id \
         40503aad-6eac-4fc6-a9f3-b8230e019d31)(label(\"\\\"cannot plant on \
         occupied cell\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         595094a4-e3d2-4f6a-bf94-56062eaf2d1b)(content(Whitespace\"\\n\")))))((Secondary((id \
         f01334f7-c032-4dca-a516-5c012d57feb9)(content(Whitespace\"\\n\"))))(Tile((id \
         7b81cf01-820d-43bd-a4e7-2bc1bd6e88ff)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ebd8da1c-2f57-41c9-bade-8bdb1b4367b9)(content(Whitespace\" \
         \"))))(Tile((id \
         40c5a627-c60c-48fb-99a6-a6046c55e71e)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd9772cf-4197-4c7d-b49c-cfc1e07d1c7e)(content(Whitespace\" \
         \")))))((Secondary((id \
         633549c6-3334-4e00-93ad-02ce5b35d70d)(content(Whitespace\" \
         \"))))(Tile((id \
         336e8f00-5914-4a2b-9d9b-33f887856661)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1c1845d-3aec-4a1f-bb0c-c80629023d72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db13f988-2a81-4211-b3ed-c8fb6d024b1b)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ca24d8c-a845-4a3b-a1cb-98689bfa3390)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b42cc034-a2e0-4f6b-9df3-7e9d0c11db6e)(content(Whitespace\" \
         \"))))(Tile((id c6c24790-6f45-4b02-ac9f-18b4be42c3c8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b0e5360-a334-4f6c-b72e-acf92a6e7fb7)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23f37556-6865-46db-be7c-7631d92150dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58ded336-b519-4865-8495-d9582d025be9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5a9ca39-0582-4933-b4f4-2566a832f408)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37b0e4d9-de4c-4094-afdd-2285b8499881)(content(Whitespace\" \
         \"))))(Tile((id \
         77afd950-1abf-4deb-af50-05fe8d8737bb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5f4297ec-b7a3-49ff-a68b-31174c68fc76)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f09b3ba-001d-418a-b150-cf32b1de9829)(content(Whitespace\" \
         \"))))(Tile((id \
         7747b797-35d3-47c9-b64c-6d00f368e02d)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2850b5e0-4fe4-40d5-8b23-d7b726d65616)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e6fcbe9-28ef-4c81-b83e-27c6408fa5db)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         67814973-0359-4544-9371-181827f53a19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0ac09a3-6b98-47fd-852b-3bc4c2795f77)(content(Whitespace\" \
         \"))))(Tile((id \
         3f65a0f0-39f9-44d9-b2ac-1759433bc281)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e70438bd-77ad-4be2-9706-49c56d347450)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a7d97eb-cb69-4877-9106-1119bba92b10)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da183eae-da59-4371-9726-4afb5ab23f9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2a6eb1e-b6c9-401c-9c55-2f2a94bb4d50)(content(Whitespace\" \
         \"))))(Tile((id \
         edf20194-3576-4c15-8ccd-2924c931c0d4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         3a394d51-7095-476e-939a-3afe29bc944a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a39fe933-8666-43a6-958a-474bc836ea96)(content(Whitespace\"\\n\"))))(Tile((id \
         6c896fc9-b900-4959-a8cd-acc28d142463)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6fe66cf6-883d-4d6a-aabe-84c75799db3a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         30ff1780-0c48-4dda-a6f2-7ab6416ca5d6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c24c595d-5ab1-4658-972e-47b029b90ea9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb8808fa-20b2-43db-af4f-3785f665400a)(content(Whitespace\" \
         \"))))(Tile((id \
         2eb42f4a-cd9a-45a6-a704-81f5fc9f4591)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb2ba353-9dd3-409d-a6d3-66b8444cb124)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2861d24d-e909-45c0-abf1-e731d5537170)(content(Whitespace\" \
         \"))))(Tile((id \
         6e96c51b-611d-442d-b3bd-b5da1c337619)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         121c7d24-0569-4463-b1d2-23c51bd12880)(content(Whitespace\" \
         \"))))(Tile((id \
         60d24215-ee51-4c9f-a1a4-54a174906ae7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4e3d95f-38ef-4d80-bf83-01dd4539a0ab)(content(Whitespace\" \
         \"))))(Tile((id \
         af00b0b5-f965-4372-b4a4-d7e4326e6f00)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21dabfc2-fca4-45af-ac2c-0d12337702c5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c78f884f-2021-449c-9667-0fc1b88086c1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af3e19d1-18e4-4237-81b0-1818a932914c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c0bfcc7-c8cb-4e07-addc-6e078c3a31f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         37d3678d-a2a8-4370-97d0-c2caefcf61bb)(content(Comment\"# Seed \
         selection #\"))))(Secondary((id \
         6b63c203-3642-46d0-8560-06627a38869a)(content(Whitespace\"\\n\"))))(Tile((id \
         4623e7cb-10a5-4e5c-8891-c633684ab301)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48cac0ef-2308-4953-bb06-60bea9e6a1cc)(content(Whitespace\" \
         \"))))(Tile((id \
         a991d7c7-a112-4b69-8342-1fc11a366f36)(label(\"\\\"select different \
         seed\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77a3d862-6559-45a0-9155-697872602671)(content(Whitespace\"\\n\")))))((Secondary((id \
         4f1b91b1-25a1-4183-b2b6-4732b96eaf98)(content(Whitespace\"\\n\"))))(Tile((id \
         1ab320ff-1868-44d9-b5c1-fccc73eb3b0c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8bec0d35-fb15-4321-9915-e4f59cfc3600)(content(Whitespace\" \
         \"))))(Tile((id \
         a904ced2-1359-421b-a999-c583909f71a4)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d1a178a1-f411-4fb7-9582-d4952b07b273)(content(Whitespace\" \
         \")))))((Secondary((id \
         c96a98b4-c1ad-467a-a225-25c84a840858)(content(Whitespace\" \
         \"))))(Tile((id \
         91266b93-d68d-48bd-b729-51f0fce455af)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c3ebb575-8867-402e-8ed0-0a9ab711a890)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         699d398b-79af-4ee0-bc3b-7bec1a27b08f)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72cf1e4e-4ecf-47e8-9cdf-f797dfc0aab1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d950f914-0d30-4ad1-ab3b-6766f7951a31)(content(Whitespace\" \
         \"))))(Tile((id 1f7dc816-5d6e-498c-b9ac-b570c29d7b9a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d09402e-99ba-4def-a743-cc340204f4b7)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14f99c57-040a-4401-a294-ef0bf8ceeaf5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         697416e4-c90d-4f7d-a358-fe74f264675c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bc7c582a-f8ed-4099-b2be-db1bf6048617)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b87f0e00-b11c-45ac-ba02-ddad29288f84)(content(Whitespace\" \
         \"))))(Tile((id \
         fa7d308c-1248-4a4f-b9b2-1306afac028b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58a52dd0-e966-40e5-83c7-e12778b4ace1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fe668b27-83ed-486c-9fe0-11ca191d3c73)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         090140e6-ec7d-4424-9434-92b58d316a90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a7fc9ab-0da7-4396-aee1-62cdde178605)(content(Whitespace\" \
         \"))))(Tile((id \
         c5fa262e-5630-482e-959f-56a1692de6b6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         6e5806d1-87d7-4d0e-bcbe-f4d5dd3d7237)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ac8535a-8395-4ed3-86e4-51bc93ea719e)(content(Whitespace\"\\n\"))))(Tile((id \
         41f8a382-d3e4-47b5-b808-029622888c21)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a77fbf5-f717-4a8c-8cb2-366239c74d1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9db23db1-de9b-4fac-8139-b0a73ce4631a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6bbfb32-4b8f-4d34-90dd-44333ecbb115)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7edf69d-b726-4220-86ff-e9626c87dd2f)(content(Whitespace\" \
         \"))))(Tile((id \
         9835dfee-6aff-466f-87a4-2bba55550a57)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         165fdae6-712d-4f20-aa86-e6d50b4e2fea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6768a145-9d7d-4bc1-9406-36a1a9ca31f8)(content(Whitespace\" \
         \"))))(Tile((id \
         c67a24d2-308b-4ec9-af97-9844962fbc73)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         53d1e948-c53e-48ba-98c1-43fce12b0d24)(content(Whitespace\" \
         \"))))(Tile((id \
         c993b04c-5732-4da9-951e-cd93ddc5bda3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a3c19ad-7bd6-4381-870a-44a7d6797389)(content(Whitespace\" \
         \"))))(Tile((id \
         2642e8fb-8cfe-4b7d-8133-33e67cb24c19)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6a8ee912-d1d4-4925-98f2-e68c27e6c20a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c29e7435-2c27-4f8a-8092-e63bbf801c01)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da83b416-9764-4d74-816a-17c434b54eb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         436b568f-5088-40c4-8767-e33294e2a8ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e42dba0-4e66-4de5-9393-5dd3d47dc952)(content(Comment\"# Harvesting \
         #\"))))(Secondary((id \
         bc07a492-2bf1-4fb6-abeb-9fd6b03d4207)(content(Whitespace\"\\n\"))))(Tile((id \
         8c57943f-4d3e-4ed4-9356-d25dac1fddf7)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         95c05671-4b28-46be-b2c4-a3b09850bff1)(content(Whitespace\" \
         \"))))(Tile((id 1d50012c-355d-4aa8-99a8-3d1ce61e4c6a)(label(\"\\\"can \
         harvest a crop\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f16aa61b-70c4-4ec2-acc7-7a64405f149e)(content(Whitespace\"\\n\")))))((Secondary((id \
         01c6cea3-b705-436b-b2ef-a1fd20e85344)(content(Whitespace\"\\n\"))))(Tile((id \
         0047c776-cfec-4fa6-a6ca-871eea0c1496)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         20402a49-bd6e-4837-a562-563d2108a930)(content(Whitespace\" \
         \"))))(Tile((id \
         dc4adf19-5986-41be-bdeb-6b603ad6b757)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         87524987-812d-4341-bffc-22bee50dcc75)(content(Whitespace\" \
         \")))))((Secondary((id \
         4ed0a8ab-f53a-496b-b01c-10e44c8a6593)(content(Whitespace\" \
         \"))))(Tile((id \
         719cec7d-3659-4bbf-8c09-d2d1b1642fcf)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52613b15-7988-40d7-8c88-76b47236790a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b81fc4d8-974b-4ed4-b7ea-65216adcd21c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4067bb73-0875-404e-92ce-96182269ebae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d66da93-92dc-4878-9721-1f8f786bb371)(content(Whitespace\" \
         \"))))(Tile((id ca65e391-f798-4d1a-bfe4-4a2344422db0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9845f209-c136-4dcd-8c30-e60400e7a3e1)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77aebfd3-0718-49d6-bf77-f76dc788fdc4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         27f2ef02-41c2-4e3b-b2b6-d8756433d340)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5339eef2-eb65-4fa4-a68d-0d6243c022dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b084853-9a85-4b09-adc7-e382172e1fad)(content(Whitespace\" \
         \"))))(Tile((id \
         c75ce851-79e8-47c2-b760-bdbc0854e5e6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         17f0542b-c6eb-4930-a587-907a76e72be7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1816c87-bbeb-4f4e-b70c-dc4df4ba5a15)(content(Whitespace\" \
         \"))))(Tile((id \
         d245ed51-ee74-446e-8f5e-53625f4c8cc1)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8b85205-3d88-490e-9cb0-d61893d64faf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         93c0be30-0465-4df4-b473-92eaf55305cd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32802625-1eaa-4e5e-bbaa-25a1adb47687)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e5b864e-f21f-4c6d-a416-a8aca41f8bda)(content(Whitespace\" \
         \"))))(Tile((id \
         e2b7686e-8335-43f7-9fb4-e1fa207c677e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         112a1d68-5bdb-4b25-a9a4-4850c6f7be69)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         883a2a11-b92f-4e1a-b967-c27fc21558be)(content(Whitespace\"\\n\"))))(Tile((id \
         2bd7ff0b-739b-4999-9e8a-42c39aeee151)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d278cc4-1770-4567-b3be-df13d8328730)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         275a207c-46c9-4d69-8d72-3ed112dcd1d6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87a9d57e-d7f9-4ecd-8990-855a1adde889)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08f72874-16f5-4be8-9adf-1aa7bb8612ae)(content(Whitespace\" \
         \"))))(Tile((id \
         a98db4f9-b706-4f99-9ec2-98da7ca04895)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8560be3a-c1ee-45b4-a26e-18531e5106da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         134a79b9-c7cd-42c8-9c86-47d649000b9f)(content(Whitespace\" \
         \"))))(Tile((id \
         56fbf247-f003-4de9-9eff-b9df7261a7e9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2da6e487-ac74-4840-82c2-1251a170d212)(content(Whitespace\" \
         \"))))(Tile((id \
         d843c9c0-7fa8-4dc3-aa5f-107e55549c2a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60e193cc-2e59-497a-a05c-9b60bf041b7d)(content(Whitespace\" \
         \"))))(Tile((id \
         ef118e2f-f92e-4486-bbb2-02d98d8b7825)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff9d03cb-8825-4a00-bf32-1410cbe32e69)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5476bcb0-3d5d-49df-a0b8-e33d54aceaf8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01a72956-13bd-4280-bf1e-407e21619528)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5bb75ed-e7bc-4896-a9aa-314804f3a1d8)(content(Whitespace\"\\n\"))))(Tile((id \
         9ef3daef-b0e0-40f7-aa9b-91bb8afc97b1)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         37eb06b1-1bd8-47af-a655-e6b3661cddff)(content(Whitespace\" \
         \"))))(Tile((id \
         ac0ba5fa-2f3e-4787-93e1-16e80febb854)(label(\"\\\"harvested cell has \
         0 health\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c773cbe5-1184-4e95-9bec-37cde0286956)(content(Whitespace\"\\n\")))))((Secondary((id \
         77a1058f-b045-4792-849e-801dffd1f6cd)(content(Whitespace\"\\n\"))))(Tile((id \
         3f57b90a-4d76-4058-af90-5311bf055e27)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         887b89f6-474d-4cbb-958b-248f4bec9ed0)(content(Whitespace\" \
         \"))))(Tile((id \
         1f1a8802-5ac8-4d8e-ace7-1711404c47a7)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24ac84b6-7e7c-4c4a-a598-827c07051577)(content(Whitespace\" \
         \")))))((Secondary((id \
         53517f48-4508-4bff-bf53-bb25ba3f33ac)(content(Whitespace\" \
         \"))))(Tile((id \
         3ba29454-399b-48e4-9575-e7a4f8a0876c)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39594afd-768c-48b9-9b5d-1bcdf84a7d6a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         83440f53-930d-4b7f-83f7-e96ac4457d7b)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6af36b7-a345-42bf-ae50-157e64fb5837)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         744a1172-d9fb-4b31-892e-de11eaa73b96)(content(Whitespace\" \
         \"))))(Tile((id 0605d446-2ac9-435e-bd76-7c9896c44357)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b645ad76-d8c4-4551-bb59-4b3de5ed85bb)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8113c4d4-579c-4ba8-9a66-20ae8e334773)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         61c9d0cb-ed05-4302-8b73-035bbedf29d9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8badb509-f4e7-4181-a77b-1ed9dd462eac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb560660-3f1e-4b2a-98df-0fadfd36682f)(content(Whitespace\" \
         \"))))(Tile((id \
         5d550df6-3aa0-4e51-8d75-078192c8f469)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8ec05aed-b70a-45e3-833c-f27c8d342521)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b956729d-9004-4f26-bffe-18f7fac03cd5)(content(Whitespace\" \
         \"))))(Tile((id \
         c59f13e6-c4b7-433a-ad60-f459892bfc61)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         913de39d-62ad-465b-92cf-6b7552094c3d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87b3d515-b563-435e-bee7-daec0f134d8a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b8e6753-2599-479e-b172-6a6f7b58d3c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12781a44-10fa-434f-a895-513dcbed95b3)(content(Whitespace\" \
         \"))))(Tile((id \
         8fb51ed4-6eb5-4142-82ea-d2cfb3164359)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         7ac0938d-bd6f-4299-9d41-87aacd56b114)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d00488bc-1442-479c-b49c-f1376a11a7fb)(content(Whitespace\"\\n\"))))(Tile((id \
         f6e70e85-a921-41d6-9e1a-0d01920f112a)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f9df97cf-dd61-4430-8b12-35b0d80fae07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aa18f491-26e3-49cb-9edd-1d1b145b20bd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d894dc4-0699-4fb1-8461-108882174cd6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d914132-fd18-4384-8c52-0969f57fd93c)(content(Whitespace\" \
         \"))))(Tile((id \
         ada3b541-ec53-450e-8a12-b4d5e3db87c9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7c91256-d8b1-47c9-bd2e-55af8ef385af)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b480cf9-f5d2-4445-a520-3f0908a12ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         7106ac16-1186-4489-ad23-cd1219337b59)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         99358e97-e0a3-48ce-addd-f34222d08c42)(content(Whitespace\" \
         \"))))(Tile((id \
         2143ec48-f463-4751-930c-09bdcf10b3f8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e889492-289c-4289-a5ba-8bdadd0f5a88)(content(Whitespace\" \
         \"))))(Tile((id \
         338e2ff2-748f-479e-b4f2-e7a3a886d28c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ca28fdf-959c-46a1-81b4-2d4b4d41fcb3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f43dec6c-0d79-4f6f-a60e-86ef24ac24cb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2db995f-68eb-4c3c-932d-175be6bc0edd)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb6bd512-e090-48ac-a4e0-147c6e747c39)(content(Whitespace\"\\n\"))))(Secondary((id \
         914fee03-be7b-408c-b2bb-da375c017d04)(content(Comment\"# Companion \
         effects - Beneficial #\"))))(Secondary((id \
         e25f86ee-05b7-47cd-8dfc-5c2c3e5bc587)(content(Whitespace\"\\n\"))))(Tile((id \
         995e2805-dd40-4746-ad97-b81310ab3b6f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0bf72eb9-11a1-4934-a28d-504ff1a25a8f)(content(Whitespace\" \
         \"))))(Tile((id \
         3254eff4-d00a-4f80-9ea1-525dd20e4e9f)(label(\"\\\"\\240\\159\\140\\177 \
         and \\240\\159\\140\\191 are companions\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad8ff04d-7be5-48f9-9ad3-05244fd3926b)(content(Whitespace\"\\n\")))))((Secondary((id \
         0f0190d9-aac4-4797-95ab-80f378796c45)(content(Whitespace\"\\n\"))))(Tile((id \
         cafc3bc6-53db-4679-b4c7-f3e62dae883f)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bb177f0-ee41-4d9b-ae6a-964e437a9707)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ffb3efb5-cc79-4703-9964-885459b259f6)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0db6e3b-4dbe-40b1-b809-bc6aade750f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a709dd24-968a-43a1-b989-d03d66e269ac)(content(Whitespace\" \
         \"))))(Tile((id \
         1c1d631a-3ec9-40db-ab44-16d97a87d1ba)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8a0462bb-cfac-4e21-b5a0-27d62419f198)(content(Whitespace\" \
         \"))))(Tile((id \
         a62ea2a9-f9e3-498a-8970-a3bf266d705b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         222034b0-fe61-4102-969b-1a98afef3d74)(content(Whitespace\" \
         \"))))(Tile((id \
         3d2f0de2-f439-4f5f-82fc-1e456c630a6f)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f0a5940-b610-47a2-9a94-ab9904508daf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         aedcb868-067a-45dd-81f4-cce159b6109d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d465025-b468-4378-a7b7-9a6a32065133)(content(Whitespace\"\\n\"))))(Secondary((id \
         f121a8b6-5856-43d2-b067-4147113acc82)(content(Whitespace\"\\n\"))))(Tile((id \
         0627f0c9-690e-4b2c-b27e-e7b57e896207)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         325957fd-4036-4e86-ad58-34217933067a)(content(Whitespace\" \
         \"))))(Tile((id \
         0af849c3-5df5-46cd-bbd7-1f4cbcc306b8)(label(\"\\\"\\240\\159\\141\\132 \
         and \\226\\152\\152\\239\\184\\143 are companions\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d834ea2e-f66b-4497-b434-91c7b99a748b)(content(Whitespace\"\\n\")))))((Secondary((id \
         5f94042a-86c0-422c-869d-775c6a38f099)(content(Whitespace\"\\n\"))))(Tile((id \
         2aa1a70a-3813-4a6a-bf06-cc8543f457af)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07d2c24c-9487-450f-9874-54f2c333672e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b6c1365-ad0b-492e-adc7-c8262207d42d)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a1be3fc-e836-4d28-a04c-2a5e42f45e31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af6b0d3a-59ef-46c7-97ce-364324c8488a)(content(Whitespace\" \
         \"))))(Tile((id \
         3024b8b7-4b24-4371-a425-0b61cf6dab85)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         27466fc6-cfcd-4523-8f81-e7d96e8684c2)(content(Whitespace\" \
         \"))))(Tile((id \
         d83734d0-6be5-4991-929c-4b64ce0a0d14)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f862e09-b0f4-411f-be84-7809bfe6ea9d)(content(Whitespace\" \
         \"))))(Tile((id \
         d4695802-4495-49d7-9e6c-408b27bfc875)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f57e3091-f59f-426a-9e65-3688534a0a0f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         53fae801-5b1d-459e-b3d8-fc0c782596df)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4eb580fb-13ee-462f-b560-0074380f23ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         16dea722-549d-4acc-bd19-4ab9b771a9eb)(content(Whitespace\"\\n\"))))(Tile((id \
         3936bbfa-e6e0-47eb-9849-b2975c70ca70)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f903e6b8-f8c4-4249-a990-12d020c00b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         533b913e-f940-4fe3-bd9f-1862ca88e1bc)(label(\"\\\"\\240\\159\\140\\177 \
         next to \\240\\159\\140\\191 gains health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd9cda94-46bd-4f70-88a3-9cf35c24b034)(content(Whitespace\"\\n\")))))((Secondary((id \
         3808d4c5-c29b-4c33-b5fc-19e084078295)(content(Whitespace\"\\n\"))))(Tile((id \
         237cab77-4abc-46d8-a6a6-4b35826a3173)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c856f671-2169-4bba-a11e-f1e098116f23)(content(Whitespace\" \
         \"))))(Tile((id \
         ecbab672-0b2f-47ee-bac1-6690a91f989a)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cf1efc7e-a300-4beb-bb9a-bbdd022f45c8)(content(Whitespace\" \
         \")))))((Secondary((id \
         17f918f2-0e5e-4e7f-a8a9-2fc807d4ec2c)(content(Whitespace\" \
         \"))))(Tile((id \
         b5e30374-8140-4079-8aae-ee0f9e76d3cc)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4793c6a2-94bf-4f9b-8d71-08d1bf0e1832)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         52d15369-eebc-4325-92de-ec998e1e0e56)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c904a648-686c-4398-98e1-b31be5798e44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e55af2a1-4d4a-45e7-afb7-fead58ea02e5)(content(Whitespace\" \
         \"))))(Tile((id 5998f904-0ee1-4953-a2bb-77d6d1b297e9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dcbc9725-d672-4f6c-a77e-842e57b31dea)(content(Whitespace\"\\n\"))))(Tile((id \
         470be8b5-44c8-456a-9dd9-56c409cdfbe9)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a7fe918-5880-40c9-804b-66af6eca9bae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         373e9c1f-392e-4822-a27a-47c7279e721e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58ef237e-d077-4bb1-82cc-f499d30bc7a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed908bdf-ad0d-4218-882f-fc481f3d30fa)(content(Whitespace\" \
         \"))))(Tile((id \
         b24a9c32-07e7-4ea6-89a6-f20aa12898e6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e9a41c85-b947-4ea7-ba92-33f29d970dc8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be58d559-fb1c-49b0-be89-9f0a4d6b0a0a)(content(Whitespace\" \
         \"))))(Secondary((id \
         bf030006-c1ed-4323-b194-4e87ea651684)(content(Whitespace\" \
         \"))))(Secondary((id \
         aa85a0fe-1be9-464a-9fe1-75dcad1e2be7)(content(Whitespace\" \
         \"))))(Secondary((id \
         b213fc83-a5a2-41d1-916d-a871ab877ff8)(content(Whitespace\" \
         \"))))(Secondary((id \
         44115898-b0c1-4d5b-b21d-0020fbaec2cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e2872b4-c805-44d9-9f64-7fe068128c85)(content(Whitespace\" \
         \"))))(Secondary((id \
         c9f34f09-9c30-4724-823d-84c642c3d018)(content(Whitespace\" \
         \"))))(Secondary((id \
         a3640219-9285-4f07-bf6c-cd19e0d3816f)(content(Whitespace\" \
         \"))))(Secondary((id \
         3985c33e-eb16-4584-a535-4333dd9053c7)(content(Whitespace\" \
         \"))))(Secondary((id \
         5f29331a-cbcd-4e6c-82f0-5b4ac5dcf055)(content(Whitespace\" \
         \"))))(Secondary((id \
         807914fe-4e7d-42df-bca1-1025607931c0)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e920585-f4b3-47ad-b81b-84c8fc1af015)(content(Whitespace\" \
         \"))))(Secondary((id \
         7d98dea2-a879-43d9-be0f-0abe4ddccfc4)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d1e4474-988c-49d4-a76b-bfa4beac894b)(content(Whitespace\" \
         \"))))(Secondary((id \
         2ce9726d-823f-45d4-bfcc-b5fddab63e37)(content(Comment\"# \
         \\240\\159\\140\\177 at (0,0) #\"))))(Secondary((id \
         34a7e63b-9f62-46c1-8732-9330e6643de5)(content(Whitespace\"\\n\"))))(Tile((id \
         b2fedf07-00a5-41c5-ad8a-925836aa7cdc)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0714169-bc00-42fe-99a9-f25fd92dad5d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         69fd75a9-a63a-45a9-a38c-1934ed5abbba)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c9d80b75-4aff-47e1-92c0-f34114d0ed28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdf07620-9e50-40be-a067-fd728ee3b375)(content(Whitespace\"\\n\"))))(Tile((id \
         4a53cf97-51fe-485e-a77f-4da69d0e2492)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15ffafb1-a34b-4a87-b219-620391f1a5e4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c90958c-00c7-482f-8068-4e07d486815a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13e48693-bb45-4736-baee-dc72b76d2587)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         941ddec1-b7c7-4528-a709-f93ae0a25ec6)(content(Whitespace\" \
         \"))))(Tile((id \
         4b23131f-ec84-4b94-b640-d98300449be7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7a6b1b13-bccd-4e08-afb5-f5d7d7d37a70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91536c1c-da18-4093-8e77-e5249c7e7eb4)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e53a5a5-d2db-464d-94df-702530c242d5)(content(Whitespace\" \
         \"))))(Secondary((id \
         66ff8388-f6cb-486b-b404-a9c1a49c93dd)(content(Whitespace\" \
         \"))))(Secondary((id \
         b8f4d011-1bf0-4b7e-b79d-dfbb85c465b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         dcc975d3-c1ae-4e84-9d8a-8fe201ce1353)(content(Whitespace\" \
         \"))))(Secondary((id \
         8777927f-4a49-4908-83b9-e25429457c9f)(content(Whitespace\" \
         \"))))(Secondary((id \
         4401cafd-637b-4a39-a476-5f8510496c20)(content(Whitespace\" \
         \"))))(Secondary((id \
         3fc971b9-9afe-410a-aea2-bff27536c872)(content(Whitespace\" \
         \"))))(Secondary((id \
         c4d41146-fab7-4c38-9800-97ea5bf36c84)(content(Whitespace\" \
         \"))))(Secondary((id \
         4dfd5903-0af6-4f66-b2dc-80c7c1e497b5)(content(Whitespace\" \
         \"))))(Secondary((id \
         76e737c7-5a0f-4fb3-b0d3-e530451d0ebe)(content(Whitespace\" \
         \"))))(Secondary((id \
         16f67613-24e9-4fe3-a9ea-2040b5c738a3)(content(Whitespace\" \
         \"))))(Secondary((id \
         9ea5921d-d55b-47ef-8af7-374aa2b67a41)(content(Whitespace\" \
         \"))))(Secondary((id \
         f0df92fd-62e2-4066-a597-5e4ab9be2726)(content(Whitespace\" \
         \"))))(Secondary((id \
         87465d55-0f38-43aa-b9d5-74c9cfb78b64)(content(Comment\"# \
         \\240\\159\\140\\191 at (0,1) #\"))))(Secondary((id \
         681ddb9a-d289-48c6-ad1c-7a063b5288f5)(content(Whitespace\"\\n\"))))(Tile((id \
         5742fc18-e9a0-4b05-8326-d5af4aab922a)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8b5712c-21f9-423c-9fe1-3fa2b37a771c)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         ad33f26d-605b-4f7a-ba04-707aeb3746d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         52d6918f-f13b-4c86-b849-fd0a0b3c7fed)(content(Whitespace\"\\n\"))))(Tile((id \
         ad35be94-754a-42bc-8b55-3c6eb3757d8f)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ee11d16-066a-4c32-9674-dc4ba0f8edaf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ff48e6c-7716-40e9-b81a-ef2393d56b59)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84ab41d5-4d09-4131-8efd-95e765c1e251)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de1a2bde-f10a-417c-b9f0-8cdde8bbf592)(content(Whitespace\" \
         \"))))(Tile((id \
         3c3a8df5-885a-482c-873b-cfe74c130442)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c919106-3d03-4029-844f-addfe1b3a77c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d84421f6-485a-4bc2-84c3-ba4d50d03a17)(content(Whitespace\" \
         \"))))(Tile((id \
         aac879ac-b558-4259-81d5-1c21a5751c6e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         28928e91-42b0-469b-90cd-996b3812fa73)(content(Whitespace\" \
         \"))))(Tile((id \
         33c67a9b-8167-45eb-8888-7ba610745226)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8600ed7-1dc8-41c3-8eb0-ecfc04358295)(content(Whitespace\" \
         \"))))(Tile((id \
         59374233-7edf-4c82-922d-6cdb715296b4)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9cdb9e49-3593-4a2b-bfd8-305ac6097a9a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         58876c59-fb02-4c20-842f-6eb51da99f53)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba597553-01db-4820-9b78-a3f12571dcab)(content(Whitespace\"\\n\"))))(Secondary((id \
         e81c73b5-968e-4bf8-8a5c-8b5b133f2050)(content(Whitespace\"\\n\"))))(Tile((id \
         41c1b2f4-d226-4a84-ab90-5af7c687407e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         45694340-93b4-4e4d-b19a-8b25cf4a50b5)(content(Whitespace\" \
         \"))))(Tile((id \
         2f3ebac8-4465-491c-8c65-d2dd7018a5a5)(label(\"\\\"\\240\\159\\140\\191 \
         next to \\240\\159\\140\\177 gains health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61bcf20d-bef9-4432-89e0-01dfe9830971)(content(Whitespace\"\\n\")))))((Secondary((id \
         2fd57883-d69b-49cf-a4ea-47666aadf9ea)(content(Whitespace\"\\n\"))))(Tile((id \
         7a48f591-572e-4cb6-aaa6-90f9f71e991f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a745f684-8680-4b78-9c33-93e70bad5735)(content(Whitespace\" \
         \"))))(Tile((id \
         7a580de5-42c8-48e4-a6b4-e5c2da05cfbd)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         26bcde8d-efda-4cae-b320-c5fbf3a6121f)(content(Whitespace\" \
         \")))))((Secondary((id \
         c7f0e47c-ffe1-43bc-9ece-0cdc22590e57)(content(Whitespace\" \
         \"))))(Tile((id \
         a033a8df-54db-4c56-962c-4cd4797cd0c5)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f00a382-617f-4acd-893e-3a0c100f1aee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d3be4d12-b4b1-4569-a7cb-c3d3e4dab3d1)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98d51829-5b64-47f8-b9f8-0fb7b1b89fbd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af6cf0cd-6c8e-4bb0-a36c-16742560471b)(content(Whitespace\" \
         \"))))(Tile((id 4eba9772-f57e-4078-8300-840b45a87eb3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6fcdd7d0-8f75-4205-9cc8-95c6e2c90c30)(content(Whitespace\"\\n\"))))(Tile((id \
         bd7dee03-5afc-40ac-8e35-111607b3e18a)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b49d7590-f0bd-4a57-96ea-eef025c28be0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         442eec4b-437f-43f6-aeee-9ea1bfcea185)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9bcf273-101b-4334-850b-e1eaba69c4f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16521c19-eb5d-44b3-9020-a21498e2cec4)(content(Whitespace\" \
         \"))))(Tile((id \
         23cca365-a0fa-4d36-a103-58c5bc51d1ea)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         25aa8a92-1d84-44b5-a4d7-e8e3ee67cb2a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1586386-f802-4a38-96bd-1514d80d28f8)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb58bb3d-17b7-4187-b880-5dae2d8949f3)(content(Whitespace\" \
         \"))))(Secondary((id \
         e77813c2-8745-4b38-9ebb-8c8ce132986b)(content(Whitespace\" \
         \"))))(Secondary((id \
         ca9510bd-3009-4066-ae5f-a726c486ad5d)(content(Whitespace\" \
         \"))))(Secondary((id \
         43620219-4a2a-40fb-b5ad-1497db977934)(content(Whitespace\" \
         \"))))(Secondary((id \
         d809ce9d-c6c6-4f9a-9915-b9a89c172e24)(content(Whitespace\" \
         \"))))(Secondary((id \
         c0160539-97a6-4b18-8e58-cf3b9b677dab)(content(Whitespace\" \
         \"))))(Secondary((id \
         3ceb8c41-0cdb-4a62-a023-8bf794b3a6aa)(content(Whitespace\" \
         \"))))(Secondary((id \
         540061cf-3ea7-4a3d-92e9-fec49abbe008)(content(Whitespace\" \
         \"))))(Secondary((id \
         9219aa29-f49a-4b34-b173-522fd6d292dd)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe8544b2-2f6b-472e-976e-1d34739d5589)(content(Whitespace\" \
         \"))))(Secondary((id \
         361b2112-e5a0-46f9-81f1-d3db412cfd44)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e48869f-605e-42b5-bba6-e38b732198f2)(content(Whitespace\" \
         \"))))(Secondary((id \
         3783b789-18e0-4b5b-bc85-d35c1e0abee3)(content(Whitespace\" \
         \"))))(Secondary((id \
         efd8dd78-c80e-4283-ac94-c2af4c11edab)(content(Comment\"# \
         \\240\\159\\140\\177 #\"))))(Secondary((id \
         5beb0580-1a8d-40c2-936b-dc9fef34a0d2)(content(Whitespace\"\\n\"))))(Tile((id \
         91191204-004a-4372-a977-2575ecd13ffe)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2419840b-e1f1-4aa5-a3b1-ca419e0e35a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4a3d9fbb-fd29-4b7b-8c81-c0ddbd332861)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9e299ac5-e85c-470f-9bd4-383b0e7532b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9283fe6e-df55-4580-8de5-a1922fae2a25)(content(Whitespace\"\\n\"))))(Tile((id \
         6d39c852-b03a-48ed-a944-b538e72315e5)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c155bf0-2a5e-4739-b01d-63f6d15cafb7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c58d20bf-f348-4d21-bfcd-2b8ceef84ca1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe58cc93-2fc8-4ff6-9c29-28715fb7cd59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c8ac931-7db3-432d-9839-6b4c02a83180)(content(Whitespace\" \
         \"))))(Tile((id \
         aeb6e84f-f519-41c2-b745-a729c28222fb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         98958c20-0909-4840-ab68-f7d6697aa3f5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ba0b46f-a4f8-4b42-8537-06cb604555b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         c01d9a74-cef9-4258-b19f-c4370e5c4965)(content(Whitespace\" \
         \"))))(Secondary((id \
         84e35ccd-fccf-4786-a472-3b8dce20ff35)(content(Whitespace\" \
         \"))))(Secondary((id \
         f8a920a6-6745-46b3-b3be-49782e7ddeef)(content(Whitespace\" \
         \"))))(Secondary((id \
         b1ad1446-5eb7-4b51-bbdf-77ae648f9841)(content(Whitespace\" \
         \"))))(Secondary((id \
         9176018e-2314-43b6-90cf-8d126c4f0586)(content(Whitespace\" \
         \"))))(Secondary((id \
         2f7a531b-e877-4450-8202-9fe437e438f7)(content(Whitespace\" \
         \"))))(Secondary((id \
         254902ab-41b7-44b1-8bb8-4b1a046f6c84)(content(Whitespace\" \
         \"))))(Secondary((id \
         52e9e88d-7cac-47e9-bbcd-299be45bae6e)(content(Whitespace\" \
         \"))))(Secondary((id \
         363d14ad-5881-48ef-8de2-32ed4bd0c5f0)(content(Whitespace\" \
         \"))))(Secondary((id \
         3b1ac7b7-f20b-49dc-9954-b9f3b30a04b8)(content(Whitespace\" \
         \"))))(Secondary((id \
         2b188689-720f-4c6c-b603-74cf449bfb1b)(content(Whitespace\" \
         \"))))(Secondary((id \
         34266b67-7072-4dda-b4b1-b0bd997b7659)(content(Whitespace\" \
         \"))))(Secondary((id \
         f7b13254-9ab4-4982-8795-b057c83be6f4)(content(Whitespace\" \
         \"))))(Secondary((id \
         b9366cd2-ffa9-432a-9824-28dc901dc7ae)(content(Comment\"# \
         \\240\\159\\140\\191 #\"))))(Secondary((id \
         3761f994-4dc4-4624-bfb5-3db5ca92b4a1)(content(Whitespace\"\\n\"))))(Tile((id \
         ac29ece0-e3b2-4c81-8df0-eecdd008c174)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23ad9220-b9ac-4b2c-8354-14b4c283f412)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         d2d35de0-bd56-47d9-ad96-6eaf507bbfff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3cdcf7c3-0da1-4d0b-96cc-62798adec384)(content(Whitespace\"\\n\"))))(Tile((id \
         edb31dc9-4d87-47d1-987b-3fec2b1e349a)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7d1956e-22f3-4f2e-9522-851986745a4e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4ddb99d-a0b6-4632-bcad-2b047698dff7)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07837837-ab4d-4cfa-88b0-4f91361860c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0127152a-a56d-49c6-90e5-f976aec3145d)(content(Whitespace\" \
         \"))))(Tile((id \
         8039e157-194e-40cb-8a79-eec91d9eb208)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40c787b5-2ad5-48d7-9891-018baa9658c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49e44191-45c0-49a8-b7be-2d6718b64414)(content(Whitespace\" \
         \"))))(Tile((id \
         b3311852-796f-4bc3-8ceb-b50a70a874d9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c2ac0eee-d12a-4a68-bdc8-cff7f864c7fc)(content(Whitespace\" \
         \"))))(Tile((id \
         160e4a74-ccf0-478d-b112-2e0cf7bff37b)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09427647-0e2c-46de-b697-c8554c430e54)(content(Whitespace\" \
         \"))))(Tile((id \
         7454925d-9e95-4ef5-b0e7-dd101039d5ce)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         118ca9ef-336a-4146-b825-cede3bd9cc34)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e472608e-7969-476f-a657-187ad0176a50)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         325a29a9-0f1e-4bc6-9957-03be684aab79)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c733c33-3f22-4552-8afd-3853cba43f4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         135fe233-af93-4d82-91f3-5ccb9746f17d)(content(Comment\"# Companion \
         effects - Harmful #\"))))(Secondary((id \
         8fb49213-8d1e-4745-98dc-f30a4c6da9ac)(content(Whitespace\"\\n\"))))(Tile((id \
         efdfe24d-3a48-42a6-adbf-ab51147f87f2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9d9e0ade-10b8-4a45-9b7c-db14ec5a7273)(content(Whitespace\" \
         \"))))(Tile((id \
         b9419b2c-1382-4741-a471-8e4360570e88)(label(\"\\\"\\240\\159\\140\\177 \
         and \\240\\159\\141\\132 are rivals\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19970cb8-2493-47bd-a457-32bf3fa16a7c)(content(Whitespace\"\\n\")))))((Secondary((id \
         44050f4c-d8df-4f99-8707-298704ddd524)(content(Whitespace\"\\n\"))))(Tile((id \
         1cf682bb-5f61-4031-8098-c7faf2ab7361)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccf51eaa-902a-4d0a-8233-c6e42329d164)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e1bc951-0376-4a52-a7c3-c469f8a18d83)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fdb7382-8b44-48c8-ad37-67e3328eef1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e02a464-bec2-46ee-9b7d-5186b72947a9)(content(Whitespace\" \
         \"))))(Tile((id \
         810de1da-d9a0-48ea-b040-79915f37d1bf)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         363687c3-44b6-480f-baf4-666475da3cb8)(content(Whitespace\" \
         \"))))(Tile((id \
         24016f45-7389-4af3-87f4-aeadbb5bcda0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f516b2e-4527-401e-9041-7fe4cfb3d80b)(content(Whitespace\" \
         \"))))(Tile((id \
         fc2f5206-f3d7-4849-bd3b-be7c28e45465)(label(Harmful))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef680706-79b1-42b1-967b-96cb6c4f2e5f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bd24ba78-7a98-40b4-b29b-992d04d23b7c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec1e59a0-f33d-431f-869c-e44d6fe002dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         aae77fcf-979f-4665-b0a8-c81120eb91b9)(content(Whitespace\"\\n\"))))(Tile((id \
         29921ce7-e5fb-4c6e-9d3d-e910ab5002b2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f42a459-130f-43c1-b682-07fc04fcdc97)(content(Whitespace\" \
         \"))))(Tile((id \
         79a3887d-db36-44df-86cc-bf6d572b9d16)(label(\"\\\"\\240\\159\\140\\177 \
         next to \\240\\159\\141\\132 loses health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         477606ed-c018-427e-a1f5-bb7ef08868b6)(content(Whitespace\"\\n\")))))((Secondary((id \
         eb85f9d2-c5f3-4769-80c3-49d7915aed50)(content(Whitespace\"\\n\"))))(Tile((id \
         0dfb244f-bce7-461d-953f-a223be846f50)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         414d6fb4-443a-4cb6-b177-6eaec6c4b891)(content(Whitespace\" \
         \"))))(Tile((id \
         19c4c2d5-e7d2-4e78-8852-12621c87fe31)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0b2e5ae7-c449-4e68-bae7-36eeee845d8b)(content(Whitespace\" \
         \")))))((Secondary((id \
         c711a169-67ce-4278-a1cc-de3b02c5aab8)(content(Whitespace\" \
         \"))))(Tile((id \
         2003736d-9e1f-4951-a2d5-1d304d6494b7)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a0bdac92-3ad6-4855-84b4-e6865e282c40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef52acd2-0c87-427d-aecd-d4f77cabf13c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbde05f9-02de-49c7-a29b-52e285135960)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22e583e0-3798-4227-b48a-3adb5f039def)(content(Whitespace\" \
         \"))))(Tile((id e6514260-bebf-44f3-b69f-408cce205d45)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e9a80009-0a03-41bf-8e43-ebeb5705336c)(content(Whitespace\"\\n\"))))(Tile((id \
         5dc847f4-7e39-4bd0-ab78-60c59aba925b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb29f3f3-6804-4179-ae55-a0389f3ef1fa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67ee380b-9af1-4076-af3c-46b05090a78a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d82fb23f-61e0-42f6-af73-ea548375b1cb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         327debfb-ce8e-40c7-a00f-4cd3f5b4388c)(content(Whitespace\" \
         \"))))(Tile((id \
         69fd6ed8-1e3b-4644-905d-7f3e1acdd32d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a0f419f1-a552-4643-8a9c-309db0d0bc0c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d164e6f-946a-41f0-b12f-206333fd55c0)(content(Whitespace\" \
         \"))))(Secondary((id \
         83790048-509d-4427-bf45-d495622806fb)(content(Whitespace\" \
         \"))))(Secondary((id \
         621a6b60-a43f-4630-aa62-71b4e1bccdf3)(content(Whitespace\" \
         \"))))(Secondary((id \
         c333ec70-d964-47b6-9a3e-a77efc2cac62)(content(Whitespace\" \
         \"))))(Secondary((id \
         e673aeaa-d34e-479c-8c42-60147197ceda)(content(Whitespace\" \
         \"))))(Secondary((id \
         3e6cbf43-3766-498f-8888-5b3d65db59d1)(content(Whitespace\" \
         \"))))(Secondary((id \
         3f9f76f7-e8b6-4980-840a-ae469c52e60d)(content(Whitespace\" \
         \"))))(Secondary((id \
         7fb0c845-956b-478b-9f9a-4728aaad75d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         c323d371-96a0-4a3a-ae87-8fc621984d09)(content(Whitespace\" \
         \"))))(Secondary((id \
         50335480-8aba-4363-aeaa-7086552eb9ba)(content(Whitespace\" \
         \"))))(Secondary((id \
         b1d2e75b-0e8f-414d-87cd-59ba83e2cbe3)(content(Whitespace\" \
         \"))))(Secondary((id \
         6816c695-54b9-447b-bcd3-2242a1e9e20b)(content(Whitespace\" \
         \"))))(Secondary((id \
         1ae413b0-8aa4-458f-b9d2-e60be6f451ab)(content(Whitespace\" \
         \"))))(Secondary((id \
         f92cb969-fc0e-4a8b-ae95-187a1eeb87d3)(content(Whitespace\" \
         \"))))(Secondary((id \
         fa934f9c-8249-44f4-840b-dee900ccbf48)(content(Comment\"# \
         \\240\\159\\140\\177 at center #\"))))(Secondary((id \
         96cc3472-2934-41b3-9ea9-40f0a96210aa)(content(Whitespace\"\\n\"))))(Tile((id \
         ea4383a2-1bda-4b1a-ab9b-2b0aca20b1d5)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4253cee8-7b75-4599-b854-c148b8fea4c5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         12672230-3a8d-40f5-9aa6-f90fcb2e3d83)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         68ae0798-1e36-4882-8dbe-43bfa5c53284)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fee0e44-ede6-4838-b716-5386d67d5d01)(content(Whitespace\"\\n\"))))(Tile((id \
         0ef43a65-b91b-467d-ad70-157949a68666)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad3c20d2-91d9-404b-9792-5fe25e76752e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3eb0c66-8077-4468-a3db-30b7f9d305b3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec93716d-54fd-45db-87ce-58cd5e946117)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3853bd86-cb6e-4e9a-b27a-15f431da5b4d)(content(Whitespace\" \
         \"))))(Tile((id \
         76f65d29-c79e-4b7d-bd5a-0953495a0ec4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3b59de79-a5b3-4656-83d4-e4d480496fe8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2e5b37e-a851-47f0-b676-583807bf3b73)(content(Whitespace\" \
         \"))))(Secondary((id \
         2a3796a7-578b-4c04-8bd6-08b351846538)(content(Whitespace\" \
         \"))))(Secondary((id \
         73746cd9-e9c6-4c0d-8c50-65ab0cdc95c6)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e4bd65a-b791-49e3-9661-8b0da5de64b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         b83b241d-ef21-498d-903b-dbaa187c4025)(content(Whitespace\" \
         \"))))(Secondary((id \
         53b72562-d31d-429b-9565-6abd2055e1d0)(content(Whitespace\" \
         \"))))(Secondary((id \
         b7123d6f-955b-41b7-86f8-11fd5fd5fc18)(content(Whitespace\" \
         \"))))(Secondary((id \
         f094abce-70de-47d4-9575-b56029bfe637)(content(Whitespace\" \
         \"))))(Secondary((id \
         42cb8b73-2d92-4290-892d-4b064ddb78e8)(content(Whitespace\" \
         \"))))(Secondary((id \
         09e96635-b61b-4b4f-a6b1-bf7da89a3a94)(content(Whitespace\" \
         \"))))(Secondary((id \
         ba455762-1c40-420e-bf81-ac5174150b05)(content(Whitespace\" \
         \"))))(Secondary((id \
         1bb5f941-8aa1-4f7c-bdf1-307653eb02fd)(content(Whitespace\" \
         \"))))(Secondary((id \
         c5f60517-1fff-4738-a950-d36593e8bbad)(content(Whitespace\" \
         \"))))(Secondary((id \
         db6714fe-610d-4a67-9413-37277b470222)(content(Whitespace\" \
         \"))))(Secondary((id \
         cf1a39aa-da2b-49c7-a88d-e504a492f963)(content(Comment\"# \
         \\240\\159\\141\\132 to the left #\"))))(Secondary((id \
         649125aa-2ff9-4908-aa9b-3d0add9e8443)(content(Whitespace\"\\n\"))))(Tile((id \
         e277a40c-f895-4efe-9e53-ca4a3e644830)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         efbdef4b-0a4a-48f5-9905-3920fe0392da)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         bd841f2b-8027-4dbd-9527-22414a4f8086)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         528f6a9a-ad58-4957-86fd-93377c3e3286)(content(Whitespace\"\\n\"))))(Tile((id \
         e3864261-a12e-473d-aee2-a42caf247b5c)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2706856-cea5-41ac-bda2-2915bbbe043a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3eb6f8b-37b7-4ca5-a1a6-65d4a2d9ab01)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1d27a32-272a-4949-be23-e76a26c7a1a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4747cae6-bb82-46fd-9498-a4d50495df38)(content(Whitespace\" \
         \"))))(Tile((id \
         65659964-565e-45d1-9602-d2d7f82033f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42666c36-573c-41fe-bfca-09badae86aa2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50953a70-caba-4e85-b3ca-df15c31f50fc)(content(Whitespace\" \
         \"))))(Tile((id \
         8d6426f5-bcd4-4759-b533-2104e8710de4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         16b7d001-24f4-48fc-800f-c6623c473f91)(content(Whitespace\" \
         \"))))(Tile((id \
         4d78e921-fa4c-49f4-820a-11342a2e7bbd)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb9713a5-ef1a-4cf4-ae39-5c046b503d4d)(content(Whitespace\" \
         \"))))(Tile((id \
         e870d58e-4872-4938-ac44-bbf4a6ffd343)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8aeef943-7f5b-4179-8543-859752d0860f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         faebfcfc-7f44-4806-afae-e4fea07333f9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         869c8ae1-2acb-467f-83b8-4db941e68963)(content(Whitespace\"\\n\"))))(Secondary((id \
         229a96ca-377b-462f-bef4-a1fc8fd08fd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8904ae47-234d-4535-bd9d-f315ec36cf17)(content(Comment\"# Neutral \
         effects #\"))))(Secondary((id \
         812a4346-d861-44a2-9f72-afc5863077d3)(content(Whitespace\"\\n\"))))(Tile((id \
         b44a327e-e9f1-49ae-af4c-27784ef84c40)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6205c438-869f-4423-ae1c-675b55afc837)(content(Whitespace\" \
         \"))))(Tile((id \
         bea25463-a2aa-4da3-ae36-20dee81d5abb)(label(\"\\\"same crop has no \
         effect\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5e007e7-0425-4178-a31c-089fa70244fc)(content(Whitespace\"\\n\")))))((Secondary((id \
         5c332dfd-13d4-46cf-9614-ecb5f6cc957f)(content(Whitespace\"\\n\"))))(Tile((id \
         21372534-eeb2-4391-9f00-92dba57a8e77)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d786a88c-097e-4c36-ab2a-93790f11cfb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9fa975a0-6995-452a-91b7-e950ad7298dd)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0087e66a-751c-44ce-bae3-75dee0ab43c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c485cbc-bae0-4947-89ae-5643afe5d1aa)(content(Whitespace\" \
         \"))))(Tile((id \
         6bf14764-5114-4f8b-9c40-7fcffd8f0c83)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         302d8fd5-6950-4dc9-93af-5dcc55df349e)(content(Whitespace\" \
         \"))))(Tile((id \
         36677c6d-fe5e-4edd-8125-ada284a957a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cb0f490-8741-4a0e-be6b-c5ad3b0b0d17)(content(Whitespace\" \
         \"))))(Tile((id \
         be31f403-b196-41e0-bcd5-25733e0c0531)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63200b9f-a9b3-46d6-b3bc-1bddd9d93db1)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5f8acf16-1ec0-4705-a85d-ae1731821b5f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a9eca56-89e8-4bb2-ab7e-3283a6dd83e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         6cf96690-12db-4404-9242-afe6d31e586d)(content(Whitespace\"\\n\"))))(Tile((id \
         07d3a515-0c6e-44c4-90fa-f3fbc4801fd6)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9d3b04ce-7d78-49f6-a9ea-a3be7ba134bc)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed92fec-32e3-4c2d-94cd-eece0152fbef)(label(\"\\\"unrelated crops are \
         neutral\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9004a148-0618-4625-96e5-0a7eb59063ab)(content(Whitespace\"\\n\")))))((Secondary((id \
         e9d23d81-abe9-4ad0-8053-340a36b19da7)(content(Whitespace\"\\n\"))))(Tile((id \
         2da4dd41-955a-404b-906f-19d47f808195)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42992541-ee07-45bd-a9b2-a3841ab011e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4b878a3-490d-4cc0-8100-d3d7b800c224)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a33995f-bfa9-4b53-9bad-09025f4a1519)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30e7a590-c1de-4fb0-9f8f-fc53c3255a2c)(content(Whitespace\" \
         \"))))(Tile((id \
         16936927-4a27-4f12-9945-03092c463635)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c017c51e-9d3d-4212-b523-0c32c78d9c88)(content(Whitespace\" \
         \"))))(Tile((id \
         31cddb10-63f9-4bd4-969d-ee8df27c6015)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f23aad53-b919-4ecb-b793-0f4c1c894dc9)(content(Whitespace\" \
         \"))))(Tile((id \
         0abddb08-4172-4f8b-9a0a-0bcbf8081431)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         72e81b2c-e61d-4dd4-a76c-30054819e55c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0893facc-768c-4595-8d04-2f591787f550)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8894a3da-48d1-4c5d-8d1c-0c851a483ded)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b786567-59e8-4047-a1df-c248ca343580)(content(Whitespace\"\\n\"))))(Secondary((id \
         f995e403-0645-4030-a5ed-9023fd743f0b)(content(Comment\"# Multiple \
         neighbors #\"))))(Secondary((id \
         4e64b500-836f-4272-8d7e-bdf2d108ff1c)(content(Whitespace\"\\n\"))))(Tile((id \
         acd56f8d-8bf9-450c-8b42-39717c2d9206)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         386ae868-ff8f-4807-b42b-ce72ad6f36cd)(content(Whitespace\" \
         \"))))(Tile((id \
         df3b8c58-b1f0-4279-96b1-519f7e3a3dff)(label(\"\\\"multiple companions \
         stack benefits\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c1c481fd-3d35-42a6-b963-f8a19fb51835)(content(Whitespace\"\\n\")))))((Secondary((id \
         69cebbbb-64d4-4b96-a9f1-b7cf2ff51a17)(content(Whitespace\"\\n\"))))(Tile((id \
         180ae8bd-21fc-4e0e-a050-98483753e899)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa1cec9c-b2d6-4e65-8878-d9d986c53fae)(content(Whitespace\" \
         \"))))(Tile((id \
         519f637a-d9fb-4fba-a142-bac31de5af9f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f5fff488-cbc8-448a-91aa-a87f4a2100bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         1db6c4e4-bf46-4b54-9ccf-9c61e7ee4067)(content(Whitespace\" \
         \"))))(Tile((id \
         d7c4af42-8f4a-48c4-a6c1-b4b69868aaa7)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3d53dba-dc51-41a1-b43a-0952bf5d5f5c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb3977db-f0c1-4726-81c2-ea9dcf79f37d)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         88f69908-3439-4c65-b699-b36a8df26be3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19d77347-0a8c-4830-861b-8401cd1a020d)(content(Whitespace\" \
         \"))))(Tile((id 170c55e0-22ac-45ec-9f59-7f02a71406ac)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c4b59856-5a14-4c99-a8e3-2d1b3fde0eef)(content(Whitespace\"\\n\"))))(Tile((id \
         102d0721-dee4-4e3b-a1af-db2996b3a79a)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81faaf80-743e-4404-b3bd-40ae94d7a7db)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a9a3f8ea-7269-4c64-9cf1-32ff05a897c9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f2d3a98-b233-4648-84d5-45181d3bc9dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ff2ee1b-24b0-46d3-8f6f-bf81e4df54df)(content(Whitespace\" \
         \"))))(Tile((id \
         12b1834e-0021-46c9-9c74-137d727a1330)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bbb60e73-5a60-4181-ab4a-d88a328dd3d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         925b1f87-dbc7-4a58-b913-295ee28d9781)(content(Whitespace\" \
         \"))))(Secondary((id \
         e942cc43-53b7-4216-80db-d7c3e843ff69)(content(Whitespace\" \
         \"))))(Secondary((id \
         48e11279-7967-4445-957e-ac954b2e7de1)(content(Whitespace\" \
         \"))))(Secondary((id \
         ffccc919-4951-4953-91db-de6f4b597844)(content(Whitespace\" \
         \"))))(Secondary((id \
         a468872f-b076-478a-bdf0-05412c9b12a7)(content(Whitespace\" \
         \"))))(Secondary((id \
         27ac4e89-e026-4799-b397-ea9bae61f8b9)(content(Whitespace\" \
         \"))))(Secondary((id \
         ced5f44c-2d90-43d6-86ee-b93160f07a39)(content(Whitespace\" \
         \"))))(Secondary((id \
         b1509849-3999-4e1e-a7cc-20b62f9d3427)(content(Whitespace\" \
         \"))))(Secondary((id \
         360ee54e-31a8-4e94-9fba-0e818eca12e7)(content(Whitespace\" \
         \"))))(Secondary((id \
         652e2c06-425f-4d2e-bd75-f92eb5aec0c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         3f4d2c3d-b4b5-45e9-801c-b80fdc7804fa)(content(Whitespace\" \
         \"))))(Secondary((id \
         020c17b6-f4be-43ce-b23c-ada776f72e02)(content(Whitespace\" \
         \"))))(Secondary((id \
         6075bd9d-c21e-4bbb-b004-8c53c96be5c5)(content(Whitespace\" \
         \"))))(Secondary((id \
         338d2dfb-e4fd-4266-bc1c-ec31fd14ec8a)(content(Whitespace\" \
         \"))))(Secondary((id \
         44e8e063-5de3-415d-9a9e-0771bb21d5b3)(content(Comment\"# \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         c7d73546-7c74-4c8b-b5c7-e0549f1326d2)(content(Whitespace\"\\n\"))))(Tile((id \
         a34fed39-c7e6-4c69-8cf4-b0863350f8f1)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e3fdabe-5a01-4e97-ab10-97515277716f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7ecc7dc1-529b-4c1e-83e6-623c1237f7ae)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1c56d355-4604-4d58-a7b9-de49ef359aac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7a57a36-548e-48eb-a58a-7bf8fd393300)(content(Whitespace\"\\n\"))))(Tile((id \
         6e45a279-4c30-44d8-b4b1-3c7e74d46d2e)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26863238-6f45-469a-8d17-ac061f9d414a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         98a510e0-f2d0-414c-957a-d494f50e1f3f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         520ffeed-1e27-4209-99e5-c7389a849d5f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd1e8685-4847-43b2-978e-4e4a7f7f2d17)(content(Whitespace\" \
         \"))))(Tile((id \
         fd2e2499-f4fd-42ba-9a70-d0bd11d58899)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0573be25-a3fc-4ca1-bbc2-d265cba6e5ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a54adcf-fc95-44c3-82e8-757d476dfd17)(content(Whitespace\" \
         \"))))(Secondary((id \
         ad74540e-d35a-47de-b0c1-ef0de6c2b40b)(content(Whitespace\" \
         \"))))(Secondary((id \
         b2245743-a6f9-4112-ae7a-acad14767e1a)(content(Whitespace\" \
         \"))))(Secondary((id \
         310fc4f8-94ab-4544-858e-1982b704e19d)(content(Whitespace\" \
         \"))))(Secondary((id \
         73ebffb5-c780-4916-8b8d-6b8feb83fc48)(content(Whitespace\" \
         \"))))(Secondary((id \
         2835493e-f3db-47de-a465-31b842508000)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce3d187a-15ea-48d7-a406-b3e861bf3bf9)(content(Whitespace\" \
         \"))))(Secondary((id \
         2a54446e-34ac-49f2-8a2f-e6b47a568dca)(content(Whitespace\" \
         \"))))(Secondary((id \
         6fe9725c-0e7d-4732-9f72-f812faf94e2b)(content(Whitespace\" \
         \"))))(Secondary((id \
         850ba97e-b269-483c-8182-dfddd2c81b8d)(content(Whitespace\" \
         \"))))(Secondary((id \
         29869f46-858e-4a5d-91f2-bab65bd40fe0)(content(Whitespace\" \
         \"))))(Secondary((id \
         ea2d0a4a-c83c-4f35-a7c4-9a294163464a)(content(Whitespace\" \
         \"))))(Secondary((id \
         bb16190b-9713-429d-b007-a239a7fdbf66)(content(Whitespace\" \
         \"))))(Secondary((id \
         bc73e1f7-b44d-4fae-b0fb-4e1f293ebb84)(content(Whitespace\" \
         \"))))(Secondary((id \
         aa6d4b21-26a8-4a8b-aa9c-5573d660c918)(content(Comment\"# \
         \\240\\159\\140\\191 above #\"))))(Secondary((id \
         64fa360f-5dd4-4f49-bcb1-b8367035d9d9)(content(Whitespace\"\\n\"))))(Tile((id \
         d94ff0aa-9495-4c48-97da-ae646714a095)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7c945a2-59c3-4df7-b439-a6262510f7b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         02a7f3c7-084e-4ab2-be48-c4e9e1d2957c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b6ea703-4200-4663-a8a6-f263f511bac1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ac8bd37-dac5-4eaf-a508-02e521e409af)(content(Whitespace\" \
         \"))))(Tile((id \
         cf38607c-c2bb-4d5c-887e-1338f5cb7387)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         130b0181-51b3-4aba-8ed3-1a169bba857d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e98aa41c-fb46-4fbf-a7b6-1137a04bbddb)(content(Whitespace\" \
         \"))))(Secondary((id \
         0f1bfb77-1699-45bf-949d-e0c3f50d638c)(content(Whitespace\" \
         \"))))(Secondary((id \
         69c4e1fd-61c8-4ae1-87e8-b93c95fa804c)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4621dd0-5b5d-46f6-9b32-77a80b889bbe)(content(Whitespace\" \
         \"))))(Secondary((id \
         e1207f8f-56b6-4182-8f17-eb8dfcd296f5)(content(Whitespace\" \
         \"))))(Secondary((id \
         02d62da6-3535-49e1-999f-711f8cd99004)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d30bce6-f426-4be8-a8e9-b331234bcbeb)(content(Whitespace\" \
         \"))))(Secondary((id \
         c19a30de-4902-4c8b-8942-249036865d85)(content(Whitespace\" \
         \"))))(Secondary((id \
         38632938-82ff-46ba-b9c1-f51811dc19ed)(content(Whitespace\" \
         \"))))(Secondary((id \
         7d0ed990-2d2f-48a6-afb5-25384e492bfa)(content(Whitespace\" \
         \"))))(Secondary((id \
         9301dde3-5611-47f6-bebe-c3c6495c541f)(content(Whitespace\" \
         \"))))(Secondary((id \
         cfaab38e-a904-4867-a604-d55ce56a948f)(content(Whitespace\" \
         \"))))(Secondary((id \
         0b858ea2-d8e7-411a-9e2d-2a1ad585c046)(content(Whitespace\" \
         \"))))(Secondary((id \
         6ac455ed-668b-46e6-91ed-aabfd2513dd7)(content(Whitespace\" \
         \"))))(Secondary((id \
         4dd22b22-fb6e-4439-82cc-7dd63704e169)(content(Comment\"# \
         \\240\\159\\140\\191 left #\"))))(Secondary((id \
         5479a06b-e763-4011-9357-43945939b109)(content(Whitespace\"\\n\"))))(Tile((id \
         d75e5d66-59fc-428a-8d38-03590a08ed2f)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d8b6b56-aa2a-4eeb-a689-a6b820697cfc)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         d305f598-df91-4648-be24-b596c12c55c7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4ef9c2b6-b588-4c03-9a53-ddb41da45ac0)(content(Whitespace\"\\n\"))))(Tile((id \
         e356fcb1-4d47-4d21-8222-b4d77353557e)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71ca1016-c3b7-4fcb-9724-ed52513bffdd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9dea9b0e-e56e-435b-8a1e-e39be5855825)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2bfa6f4-a8e5-4405-bc50-0839421e21e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de9947eb-2f0f-4f52-a382-1782aa42466d)(content(Whitespace\" \
         \"))))(Tile((id \
         51ac51aa-7197-4e57-a557-7f7107ba756d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3209f476-5d1b-4b17-95df-9bb22fba80ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1bf8bdd-4640-4880-88fc-96bc0331c253)(content(Whitespace\" \
         \"))))(Tile((id \
         6ebf264d-9a4c-461a-9fbf-0bd1fa71aaf9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         171bef9e-c1f6-4d1c-8440-b2455efb98e4)(content(Whitespace\" \
         \"))))(Tile((id \
         01146190-9242-46fb-bbbe-430ac012559e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d72c00d7-4211-4444-90fe-e7ec820dd66b)(content(Whitespace\" \
         \"))))(Tile((id \
         9ba3a854-0c22-492f-9825-d868149a4aca)(label(70))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         05ad0fb2-a5d4-4849-b3ad-5f1a5a7492ab)(content(Whitespace\" \
         \"))))(Secondary((id \
         f70b2f19-816c-4d68-9921-785edbc7806e)(content(Whitespace\" \
         \"))))(Secondary((id \
         27e98edc-c6cf-4b29-a5bc-be8ab4f59ed1)(content(Whitespace\" \
         \"))))(Secondary((id \
         f8fc7c18-30b6-4f79-8d57-9bca137ca53f)(content(Whitespace\" \
         \"))))(Secondary((id \
         fda94146-1c5f-4587-8d89-0b66ae922520)(content(Whitespace\" \
         \"))))(Secondary((id \
         96476dbe-0514-4413-8662-40db7ce6ae70)(content(Whitespace\" \
         \"))))(Secondary((id \
         6cfd4e49-217e-4d8f-b678-1fa8e9d2154a)(content(Whitespace\" \
         \"))))(Secondary((id \
         4696c202-fb6d-440e-b5fc-a3173d9512ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         2d381837-9e9f-4e5b-b3df-f1d5feb6d0c9)(content(Whitespace\" \
         \"))))(Secondary((id \
         538b298b-0703-4437-9ec7-1a5fac432335)(content(Comment\"# 50 + 10 + 10 \
         #\"))))(Secondary((id \
         a53d6e1c-8074-4ec2-8cfd-78e37cedd618)(content(Whitespace\"\\n\")))))))))(Tile((id \
         61620b1b-dd69-4e9c-85a0-2160f9c3169c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         938fafc2-dd26-41a9-8102-51d1d7298dce)(content(Whitespace\"\\n\"))))(Secondary((id \
         5671ba69-a602-4c0a-b2e2-b64064e9fa82)(content(Whitespace\"\\n\"))))(Tile((id \
         d402bd29-6963-4611-9353-75b72b60ae10)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b7ceb3e-cb08-4955-a64a-bfd7895c2e7a)(content(Whitespace\" \
         \"))))(Tile((id \
         1befe091-471d-4ae4-badf-a393d12301e9)(label(\"\\\"mixed neighbors \
         balance out\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         140ac404-c815-4a0f-b878-17370378cdc2)(content(Whitespace\"\\n\")))))((Secondary((id \
         dfdcf8c7-0526-4608-b915-480ff8e295c8)(content(Whitespace\"\\n\"))))(Tile((id \
         a04d8c8f-0e08-4030-b35c-fef2603d560d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         457bb13e-a75a-4ef4-adce-e288de23a461)(content(Whitespace\" \
         \"))))(Tile((id \
         bc64893a-8985-42c3-9a33-0b874cf82bff)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b835829f-6fcf-46a1-bee9-850fddc3537f)(content(Whitespace\" \
         \")))))((Secondary((id \
         95238aa6-8a54-4dbc-9285-d17355d3d95f)(content(Whitespace\" \
         \"))))(Tile((id \
         fa19bbc3-cc5f-4678-9943-484a1fc78562)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72318e86-77a7-4694-b4c9-f6fe9fe52906)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b1bee677-e4f4-4e1a-93fe-1dc179aac584)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b537d1a-4710-4128-b0bd-1e373afbc066)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e767c984-5153-4905-86a3-26de8980e72c)(content(Whitespace\" \
         \"))))(Tile((id 0ccccc7a-9933-4675-9bdf-890cafd7f644)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f6524c86-eec2-45fa-b654-ca2be9465764)(content(Whitespace\"\\n\"))))(Tile((id \
         3f38a742-adc3-42aa-b8f9-aafe80f6e2e7)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d06eccd-ea4e-4046-876e-4183d0a7138c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f8b36add-1b36-4622-a54d-674fbd85c246)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0bc37dbf-ceaf-4a8b-b9c8-d215f2090f99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51c38fc2-3bbf-4cf5-86e7-7d9cb7912bf4)(content(Whitespace\" \
         \"))))(Tile((id \
         77cb90a3-96d7-4d96-b7ec-a4bfda4a2d9e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b0beaf0f-5f1a-4939-a7a5-be343515cb18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01e89bd8-9f34-4aa2-a561-12b6686df2aa)(content(Whitespace\" \
         \"))))(Secondary((id \
         264e9a8d-68c6-43a2-8576-d638329bca51)(content(Whitespace\" \
         \"))))(Secondary((id \
         28386a42-0733-47ba-ade5-7c19bf5bdfcc)(content(Whitespace\" \
         \"))))(Secondary((id \
         46af7bad-7e59-4cd8-a97a-3938c287a2fc)(content(Whitespace\" \
         \"))))(Secondary((id \
         5d0983ca-e011-4dc9-bff6-f2c7c87e4551)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4cd471d-a1e1-40de-8992-d664d077d420)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e25a724-8a19-480e-9d3d-974d5796f64e)(content(Whitespace\" \
         \"))))(Secondary((id \
         660e9ba2-c6c2-44ce-b1a5-cc4225569e57)(content(Whitespace\" \
         \"))))(Secondary((id \
         16fd8a37-612a-40a6-ad92-f436b57d4e6b)(content(Whitespace\" \
         \"))))(Secondary((id \
         37b57830-112f-43a4-b13e-45d60e8934cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         236ef14a-dbf4-4203-9daa-0024f00bc460)(content(Whitespace\" \
         \"))))(Secondary((id \
         fa90aaa3-621d-4d9b-b701-e2f892fc0f19)(content(Whitespace\" \
         \"))))(Secondary((id \
         81e68265-893d-45f1-aa30-28c5abd9e05e)(content(Whitespace\" \
         \"))))(Secondary((id \
         290be85f-a05c-4ab6-80a6-3e0de486606e)(content(Whitespace\" \
         \"))))(Secondary((id \
         8d1dc6c1-00fa-434a-89cc-551d1b90ead9)(content(Comment\"# \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         67a5b3f1-9005-4567-b7e0-fc56e581938b)(content(Whitespace\"\\n\"))))(Tile((id \
         eb5416cf-af77-4a4a-992b-102974115616)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc236b7e-9093-4985-a6d3-85051877d9f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d646b2aa-8aa7-4f83-be1a-fe9655fdc074)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4f7aa8e3-392a-414b-959e-28d2a3f33f11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8488860-1ff0-4a39-9fe7-01e3f5b1ec23)(content(Whitespace\"\\n\"))))(Tile((id \
         90640584-bc07-4fd3-bd44-9736b9c6affc)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f799ead-54f5-4c0d-83d6-e763de2ff2aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         758fcc72-6f70-4022-bf64-b4fa249ea0a2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee25d0a3-8c00-4cca-bbb3-e658ee648058)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82508efe-f687-4965-a8ab-5e19bf085638)(content(Whitespace\" \
         \"))))(Tile((id \
         39b9f6ae-af96-4fa0-9fae-ec9e85f11ab9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ad07485a-e3f2-4ead-a838-dcde99215e5e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d80ad58e-ec85-441f-a170-9e202818bcf9)(content(Whitespace\" \
         \"))))(Secondary((id \
         cdbcd353-7d76-4afc-8099-7443f7fea44c)(content(Whitespace\" \
         \"))))(Secondary((id \
         6779c47f-5f5c-4172-b57d-b7c452fdbdfc)(content(Whitespace\" \
         \"))))(Secondary((id \
         d6264e94-231f-45e8-b687-1b77b4ddd005)(content(Whitespace\" \
         \"))))(Secondary((id \
         05cdef83-8725-40e5-be4d-bbe20c7bfbf9)(content(Whitespace\" \
         \"))))(Secondary((id \
         68d5d813-da0a-466b-b3ba-b8c360696010)(content(Whitespace\" \
         \"))))(Secondary((id \
         517388d2-dc17-4ca9-94c1-fa5f95e9227b)(content(Whitespace\" \
         \"))))(Secondary((id \
         1b287dd3-50ee-4732-a80d-953b916d96c6)(content(Whitespace\" \
         \"))))(Secondary((id \
         1638ea29-b9e1-42a1-93d7-309cdaf4c0bb)(content(Whitespace\" \
         \"))))(Secondary((id \
         a20382ac-e635-4a75-92e5-8000f75f699a)(content(Whitespace\" \
         \"))))(Secondary((id \
         828b81e5-cec4-46b9-ab9b-c6c6f294303f)(content(Whitespace\" \
         \"))))(Secondary((id \
         092c8850-5a1a-44b0-b4bb-2149d2b3af59)(content(Whitespace\" \
         \"))))(Secondary((id \
         89b8d39c-7e17-4bbd-b1e2-124a5ac35202)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e519380-ebb8-4c87-aff9-2178d16393df)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e2dcd98-80ab-4936-a458-7c10fb981549)(content(Comment\"# \
         \\240\\159\\140\\191 above: +10 #\"))))(Secondary((id \
         aeb4a72d-efb9-4f60-b8ed-4cf0f48e64f3)(content(Whitespace\"\\n\"))))(Tile((id \
         20aa3613-73ab-4d67-9131-9add4d4a8651)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8367be48-8136-4d60-966c-d4395f62f545)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d5276c7e-4674-42ab-bb36-9373e35d5823)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         53f79346-d70b-48d0-8561-e12b0bd7cd57)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54be885c-df36-4168-9d32-9cf166bff120)(content(Whitespace\"\\n\"))))(Tile((id \
         aa971dcd-8243-46ba-b6df-1f5b82f4736e)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         656b0155-082e-48c6-b7a0-01ed7aa363ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         15a5a7e7-b5e0-4e80-9f1a-13e5dde7a0cb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecaeaf18-b3f7-455f-b1a8-969b8d084ee5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fc986eb-4a01-4b42-bafe-21730acbbd0f)(content(Whitespace\" \
         \"))))(Tile((id \
         8f44230a-f440-43c0-baae-994c7ee9f853)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         86aa5bb5-3026-45cc-9b23-c5dc00e4716d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20210fd1-7770-4329-8775-a423884225e8)(content(Whitespace\" \
         \"))))(Secondary((id \
         d6b6f6c2-9312-42e6-9f35-956bf10e32cc)(content(Whitespace\" \
         \"))))(Secondary((id \
         7ac0c693-287d-42fd-812c-a8752ee7053b)(content(Whitespace\" \
         \"))))(Secondary((id \
         2539ff89-712a-4c16-bd75-458dbfdd96a7)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc7cecb2-0f99-42da-81c2-ae3d136fe328)(content(Whitespace\" \
         \"))))(Secondary((id \
         84b670e1-2d06-49fa-a258-9f1980cd7200)(content(Whitespace\" \
         \"))))(Secondary((id \
         6c4dc3ec-8727-4798-862a-0f8fc7fb0e4e)(content(Whitespace\" \
         \"))))(Secondary((id \
         1944d665-d614-4e74-a5dd-775d1b445d1b)(content(Whitespace\" \
         \"))))(Secondary((id \
         02d88ea2-b673-4921-9623-46ae8b8292b8)(content(Whitespace\" \
         \"))))(Secondary((id \
         9fc1fdc7-c613-44f0-8a90-f5b485c70768)(content(Whitespace\" \
         \"))))(Secondary((id \
         ec564a6a-e73d-470b-9196-96da625ca8ce)(content(Whitespace\" \
         \"))))(Secondary((id \
         c34c10ca-67c6-4873-a108-992a75c59f17)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb21c237-2009-468d-bdf5-64e1cc9fc083)(content(Whitespace\" \
         \"))))(Secondary((id \
         5ab91a9c-7008-48c6-a3d0-6b9e6d5a732d)(content(Whitespace\" \
         \"))))(Secondary((id \
         06d15064-2265-42ed-bbe0-87cfe5c02567)(content(Comment\"# \
         \\240\\159\\141\\132 left: -10 #\"))))(Secondary((id \
         a8ca5a63-a424-4b4f-97c9-0f07548d57ec)(content(Whitespace\"\\n\"))))(Tile((id \
         2f7a3c36-af15-4a96-b356-dc13e52dc2cf)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2b615b4-6cc6-46ee-aa62-bad270b4192c)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         0ade013f-7304-4311-8ae5-5aeb1ccb5d5a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         afbf0007-5308-4140-8789-3092e2aa9ee8)(content(Whitespace\"\\n\"))))(Tile((id \
         345a576c-d232-4cf2-9bf6-837b8d28f283)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         187ef0f6-f64a-4392-ab16-1d016f4d46f0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7d92bc9-1ae7-45e0-af14-1e9836483ba3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af71a83f-1316-41d2-a084-47ed29820b09)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a163c016-05c3-4eec-a74d-527091ebb21a)(content(Whitespace\" \
         \"))))(Tile((id \
         15b6dd77-5e25-4ec8-ae22-bf1af8364b8b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         687ae1ab-a02e-4f85-8a1c-19108eb3b991)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4db246b1-152b-496b-af5d-cfa4ddfb663c)(content(Whitespace\" \
         \"))))(Tile((id \
         619a57b3-3b5e-4af1-adb2-b4aaca06f622)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         611fb462-345c-4bb6-b326-8e1714d7c9d2)(content(Whitespace\" \
         \"))))(Tile((id \
         6b80678a-873a-419b-a3b5-e1b59a9d25ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1edb394-1c3a-4617-8dd0-9d05ecc835eb)(content(Whitespace\" \
         \"))))(Tile((id \
         9afc9d7f-340d-4dbf-b2ad-f5a39f27275f)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c16b02d-7c23-47c6-bf90-f14d23abfda1)(content(Whitespace\" \
         \"))))(Secondary((id \
         26266091-afe5-4eda-b727-d65e8cf7026d)(content(Whitespace\" \
         \"))))(Secondary((id \
         3211976e-31d0-48a7-8b33-4d59fa955661)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbafdc57-3466-4e0e-be48-bc725cb3f399)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1bd6d3f-36e3-4696-a907-30258a4a8781)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a7f7590-16f1-4b64-914f-f4ad671775ad)(content(Whitespace\" \
         \"))))(Secondary((id \
         0bf357db-1e8d-4faf-b8d7-c6cd9edf7e8c)(content(Whitespace\" \
         \"))))(Secondary((id \
         aced8e92-0b47-4255-bc73-0a0aa314b9d4)(content(Whitespace\" \
         \"))))(Secondary((id \
         23f90336-3b72-4962-9bd1-43925f7e668b)(content(Whitespace\" \
         \"))))(Secondary((id \
         09d53fba-2be5-413f-9218-3eea924ab68f)(content(Comment\"# 50 + 10 - 10 \
         = 50 #\"))))(Secondary((id \
         64e94444-31dc-4e22-bbb3-4eb082812b97)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ee754104-bcb3-4344-b856-352c9a17e36b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5be97851-ce83-43ae-abe6-4d774b261e7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         16377fa9-1e85-4d6f-a97b-15f4740460ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         4dccda59-98ca-45ec-a106-6eaaaa5c2c7e)(content(Comment\"# Demo: A \
         companion garden under moonlight #\"))))(Secondary((id \
         c46c6f96-48e3-4f1f-abb6-0e5d7d78e9a5)(content(Whitespace\"\\n\"))))(Tile((id \
         fe459110-00fe-4119-9f5d-6e78f1bf562b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         af7dcc98-8752-4ea3-a458-8c31bbe09fc4)(content(Whitespace\" \
         \"))))(Tile((id \
         5a759d92-09eb-49fd-97d0-5ca6ecb2adf3)(label(moonGarden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5333389c-e242-4428-b240-4eba92213d21)(content(Whitespace\" \
         \")))))((Secondary((id \
         d47f67e1-ba21-4626-9910-9357a51aa594)(content(Whitespace\" \
         \"))))(Tile((id \
         468a8679-7743-40fa-a9b3-4f74b1ee71ca)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31082987-e664-44b0-8cd6-b0ad28dad16d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f98a6e65-b39e-4028-96be-c4c0ccfb5008)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b91f37c-edcd-4feb-9c39-68d73d0721db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8ee600c-3b9f-436a-9fca-fdec55af9a0b)(content(Whitespace\" \
         \"))))(Tile((id 7c0205c9-1c1d-40ee-80c0-6a477d5a563c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         62f44c6c-ea8c-4fbb-874f-f45f2a136ca1)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0ed40a2-17de-40ca-9b6a-e3f2779811ba)(content(Comment\"# Plant \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         82963499-1056-4ece-add9-09a44fd23e8c)(content(Whitespace\"\\n\"))))(Tile((id \
         ac8abe74-dc49-4913-adf7-3c3524afeb16)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         518b0ce3-f400-410a-8eef-badb6ad74d48)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ab20130-4633-4825-ac46-26873c33c89f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb6e3f0c-a846-4148-9441-db686d2384b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c64b5472-5714-4daa-a1d2-52b76a0ee36e)(content(Whitespace\" \
         \"))))(Tile((id \
         5c562264-4969-4ed9-b205-59683c75bc3a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fec39e93-594a-4ee5-9f95-2858575c8785)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d1de0ae-8a02-4a75-b757-cd2648781452)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ccf3e90-325a-492a-b0f2-91b6380ad035)(content(Comment\"# Surround \
         with \\240\\159\\140\\191 companions #\"))))(Secondary((id \
         b0a419f3-bf0c-48fb-8464-2011f8edbf0f)(content(Whitespace\"\\n\"))))(Tile((id \
         2c38ea67-5701-4f1f-86d8-050574edd311)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0d3926f-ef2c-47de-bb65-64234c851af9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7191da85-1fb2-45a5-9f88-2360adff34e2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         216327f1-f2d1-4749-a73d-70bcc1ef4605)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17f69ce4-1eca-4a15-822a-a65f4dfd7c11)(content(Whitespace\"\\n\"))))(Tile((id \
         a58f5c3d-6f45-4813-8344-63ffae3ca1d8)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e62ffa9f-f729-4d55-a0e7-0fdf4ca29476)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1d446d6-e9a5-40c2-83d6-91b466977595)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ff0254d-3254-4bde-a795-1862b1e08ce7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5b4b3c8-3b9a-4306-8d90-c41b91f95652)(content(Whitespace\" \
         \"))))(Tile((id \
         9278dec8-3b95-4924-8ff8-f7dc2b15567d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         967582f4-6a03-4fdc-87f5-d4eb04479882)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea1ac69f-e28b-4a39-bd56-611020eef897)(content(Whitespace\"\\n\"))))(Tile((id \
         642b1071-2f1b-4bf0-b12c-9ffaf30f2465)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d474a45-7cf2-4ec3-b7c2-efa830168c99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b7ae15c-64d1-4929-a0cc-9ad1ef1064c0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98b14adc-cc88-4004-b5d2-20d5fa3248c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92b74387-73ff-4595-b071-13693b484f85)(content(Whitespace\" \
         \"))))(Tile((id \
         77449caa-dccb-4bbc-a4b9-dec7f8f6e0cc)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         890771f1-9ae2-425e-b7fd-7f7ba59c9928)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3323a634-06d9-4161-882f-04bef88c4602)(content(Whitespace\"\\n\"))))(Secondary((id \
         6825ea2d-535a-457e-b95c-ee431437a0ee)(content(Comment\"# Add some \
         \\226\\152\\152\\239\\184\\143 #\"))))(Secondary((id \
         50767a2c-0755-44c8-bade-d7e3a7e18270)(content(Whitespace\"\\n\"))))(Tile((id \
         e695d77a-bf94-4c30-9e25-eefbd1ef5391)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         585af692-b282-4081-a658-ce0be9f28f27)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         59471a5f-b21f-49ea-baf4-dd97fddbbce9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         885ade82-6814-4400-a98d-2a338d16e955)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b0d02cb-3aae-4318-b8b0-58afc4dd71d1)(content(Whitespace\"\\n\"))))(Tile((id \
         8ec2a51f-34c1-4b85-baa9-7acf291012b5)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c51e039-4767-49f2-93f4-796e882e6182)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         16d75d9f-3d43-4738-ab16-c478ffc5e6ad)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f0476f5-c080-40ff-91cd-f5bd3aebf147)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c785e3b4-1afa-4544-8170-52ff5bc9ac37)(content(Whitespace\" \
         \"))))(Tile((id \
         adf66a09-a57a-4553-9463-6aa453d1a4a4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a5d223bd-a677-41a9-8868-25eff05a61a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6194da5-b16d-48f1-a9d3-028464a525cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         869cd8c5-981b-4b40-b5d8-811664c32b92)(content(Comment\"# \
         \\240\\159\\141\\132 next to \\226\\152\\152\\239\\184\\143 for \
         companionship #\"))))(Secondary((id \
         ae484031-6905-4bf1-b67d-5c1136c776fd)(content(Whitespace\"\\n\"))))(Tile((id \
         e768b0f2-1f42-49c3-9968-9b90acc42e29)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21001970-8dd9-4adf-8714-36942b07995a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         826cb416-89e1-463a-bb68-5e52bd6f8738)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         12ed5316-2151-427b-90e2-0c11acfd5673)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21714859-1ee4-4d6f-a8de-cf5dfd817422)(content(Whitespace\"\\n\"))))(Tile((id \
         c1d034ce-5f33-4c1b-89c5-733969822acc)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7eca814-7036-4120-815d-8d5aad76571b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         13fc3e0e-9bdd-4e32-878c-8d5f97b1836b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16ea3b10-9b31-4657-813b-458b64367e89)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4617670-e254-49d7-8bb1-5f8858dccf85)(content(Whitespace\" \
         \"))))(Tile((id \
         467a47a4-6ee0-4ac8-bd93-83bde418d2f7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9f84ad81-0bb9-4673-b98e-be036b1dd7c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9dbc554-e261-40bd-821a-7dc8ef32318b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c11dd05d-a017-4c9a-8ee5-52da2ecc2dc7)(content(Comment\"# Calculate \
         how they affect each other #\"))))(Secondary((id \
         78d33eda-169c-4d9d-aa24-15204a5459ac)(content(Whitespace\"\\n\"))))(Tile((id \
         2b581571-bc43-4ae1-b1c1-9d531eaf7e4f)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96a30772-6d45-4447-8f4b-f0430f3aa546)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         6fda9b8b-58a9-441a-95ef-9b0edd3dcccb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d1f195cd-e258-48d5-853b-624d55e26d8e)(content(Whitespace\"\\n\"))))(Tile((id \
         de966178-c2de-48be-9ac7-6fb7c51febbb)(label(moonGarden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         66b58066-10cd-484e-ad35-d68fa9ea17fe)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Companion Planting Grid #\n\
         # Plants affect their neighbors #\n\n\
         # A crop is represented by its emoji #\n\
         type Crop = String in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         # Health is 0-100, affected by neighboring plants #\n\
         type Health = Int in\n\n\
         # A cell in the garden grid #\n\
         type Cell = (\n\
         crop = Crop,\n\
         health = Health\n\
         ) in\n\n\
         # The garden field is a grid of cells #\n\
         type Field = [[Cell]] in\n\n\
         # Companion effects between crops #\n\
         type Effect =\n\
         + Beneficial   # Companions boost each other #\n\
         + Harmful      # Rivals suppress each other #\n\
         + Neutral      # No interaction #\n\
         in\n\n\
         # The garden state #\n\
         type Model = (\n\
         field = Field,\n\
         currentSeed = Crop,\n\
         seedInventory = [Crop]\n\
         ) in\n\n\
         # Actions the gardener can take #\n\
         type Action =\n\
         + PlantCrop(Row, Col)      # Plant current seed at position #\n\
         + HarvestCrop(Row, Col)    # Remove a crop #\n\
         + CalculateHealth          # Recalculate all health based on \
         neighbors #\n\
         + SelectSeed(Int)          # Choose a seed from inventory #\n\
         + WaterAll                 # Boost all health by 5 #\n\
         in\n\n\
         # Utility: clamp value between 0 and 100 #\n\
         let clamp : Int -> Int =\n\
         fun x ->\n\
         if x < 0 then 0\n\
         else if x > 100 then 100\n\
         else x\n\
         in\n\n\
         # Create an empty cell #\n\
         let emptyCell : Cell = (crop = \"\", health = 0) in\n\n\
         # Create a cell with a crop at base health #\n\
         let makeCell : Crop -> Cell =\n\
         fun crop -> (crop = crop, health = 50)\n\
         in\n\n\
         # Determine the companion effect between two crops #\n\
         # \240\159\140\177 and \240\159\140\191 are companions (Beneficial) #\n\
         # \240\159\141\132 and \226\152\152\239\184\143 are companions \
         (Beneficial) #\n\
         # \240\159\140\177 and \240\159\141\132 are rivals (Harmful) #\n\
         # Everything else is Neutral #\n\
         let companionEffect : (Crop, Crop) -> Effect =\n\
         fun (crop1, crop2) ->\n\
         if crop1 == \"\" || crop2 == \"\" then Neutral\n\
         else if crop1 == crop2 then Neutral\n\
         else if (crop1 == \"\240\159\140\177\" && crop2 == \
         \"\240\159\140\191\") ||\n\
         (crop1 == \"\240\159\140\191\" && crop2 == \"\240\159\140\177\") then \
         Beneficial\n\
         else if (crop1 == \"\240\159\141\132\" && crop2 == \
         \"\226\152\152\239\184\143\") ||\n\
         (crop1 == \"\226\152\152\239\184\143\" && crop2 == \
         \"\240\159\141\132\") then Beneficial\n\
         else if (crop1 == \"\240\159\140\177\" && crop2 == \
         \"\240\159\141\132\") ||\n\
         (crop1 == \"\240\159\141\132\" && crop2 == \"\240\159\140\177\") then \
         Harmful\n\
         else Neutral\n\
         in\n\n\
         # Convert effect to health modifier #\n\
         # Beneficial: +10, Harmful: -10, Neutral: 0 #\n\
         let effectToModifier : Effect -> Int =\n\
         fun effect ->\n\
         case effect\n\
         | Beneficial => 10\n\
         | Harmful => -10\n\
         | Neutral => 0\n\
         end\n\
         in\n\n\
         # Get cell at position, or empty if out of bounds #\n\
         let getCell : (Field, Row, Col) -> Cell =\n\
         fun (field, row, col) ->\n\
         if row < 0 || col < 0 then emptyCell\n\
         else if row >= length(field) then emptyCell\n\
         else\n\
         let rowData = nth(field, row) in\n\
         if col >= length(rowData) then emptyCell\n\
         else nth(rowData, col)\n\
         in\n\n\
         # Set cell at position #\n\
         let setCell : (Field, Row, Col, Cell) -> Field =\n\
         fun (field, row, col, cell) ->\n\
         mapi(field, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) ->\n\
         if j == col then cell else c)\n\
         else r)\n\
         in\n\n\
         # Get all orthogonal neighbors of a position #\n\
         # Returns cells above, below, left, and right #\n\
         let getNeighborCells : (Field, Row, Col) -> [Cell] =\n\
         fun (field, row, col) ->\n\
         [\n\
         getCell(field, row - 1, col),  # Above #\n\
         getCell(field, row + 1, col),  # Below #\n\
         getCell(field, row, col - 1),  # Left #\n\
         getCell(field, row, col + 1)   # Right #\n\
         ]\n\
         in\n\n\
         # Calculate total health modifier from all neighbors #\n\
         # This function computes the companion effect for each neighbor #\n\
         # and sums up the modifiers #\n\
         let neighborModifier : (Field, Row, Col) -> Int =\n\
         fun (field, row, col) ->\n\
         let cell = getCell(field, row, col) in\n\
         if cell.crop == \"\" then 0\n\
         else\n\
         let neighbors = getNeighborCells(field, row, col) in\n\
         # For each neighbor, calculate the companion effect #\n\
         # Effect should be between the current cell's crop and the neighbor's \
         crop #\n\
         let effects = map(neighbors, fun neighbor ->\n\
         companionEffect(cell.crop, cell.crop)\n\
         ) in\n\
         let modifiers = map(effects, effectToModifier) in\n\
         fold_left(modifiers, fun (acc, m) -> acc + m, 0)\n\
         in\n\n\
         # Recalculate health for a single cell based on neighbors #\n\
         # Base health is 50, modified by neighbor effects #\n\
         let recalculateCellHealth : (Field, Row, Col) -> Cell =\n\
         fun (field, row, col) ->\n\
         let cell = getCell(field, row, col) in\n\
         if cell.crop == \"\" then emptyCell\n\
         else\n\
         let modifier = neighborModifier(field, row, col) in\n\
         let baseHealth = 50 in\n\
         (crop = cell.crop, health = clamp(baseHealth + modifier))\n\
         in\n\n\
         # Recalculate health for entire field #\n\
         let recalculateAllHealth : Field -> Field =\n\
         fun field ->\n\
         mapi(field, fun (row, rowData) ->\n\
         mapi(rowData, fun (col, _) ->\n\
         recalculateCellHealth(field, row, col)\n\
         )\n\
         )\n\
         in\n\n\
         # Create initial empty 3x3 garden #\n\
         let emptyField : Field =\n\
         [\n\
         [emptyCell, emptyCell, emptyCell],\n\
         [emptyCell, emptyCell, emptyCell],\n\
         [emptyCell, emptyCell, emptyCell]\n\
         ]\n\
         in\n\n\
         # Initial model state #\n\
         let init : Model = (\n\
         field = emptyField,\n\
         currentSeed = \"\240\159\140\177\",\n\
         seedInventory = [\"\240\159\140\177\", \"\240\159\140\191\", \
         \"\240\159\141\132\", \"\226\152\152\239\184\143\", \
         \"\240\159\140\184\"]\n\
         ) in\n\n\
         # Apply an action to the model #\n\
         let update : (Model, Action) -> Model =\n\
         fun (model, action) ->\n\
         case action\n\
         | PlantCrop(row, col) =>\n\
         let currentCell = getCell(model.field, row, col) in\n\
         if currentCell.crop != \"\" then model\n\
         else\n\
         let newCell = makeCell(model.currentSeed) in\n\
         let newField = setCell(model.field, row, col, newCell) in\n\
         (\n\
         field = newField,\n\
         currentSeed = model.currentSeed,\n\
         seedInventory = model.seedInventory\n\
         )\n\
         | HarvestCrop(row, col) =>\n\
         let newField = setCell(model.field, row, col, emptyCell) in\n\
         (\n\
         field = newField,\n\
         currentSeed = model.currentSeed,\n\
         seedInventory = model.seedInventory\n\
         )\n\
         | CalculateHealth =>\n\
         let newField = recalculateAllHealth(model.field) in\n\
         (\n\
         field = newField,\n\
         currentSeed = model.currentSeed,\n\
         seedInventory = model.seedInventory\n\
         )\n\
         | SelectSeed(idx) =>\n\
         (\n\
         field = model.field,\n\
         currentSeed = nth(model.seedInventory, idx),\n\
         seedInventory = model.seedInventory\n\
         )\n\
         | WaterAll =>\n\
         let wateredField = map(model.field, fun row ->\n\
         map(row, fun cell ->\n\
         if cell.crop == \"\" then cell\n\
         else (crop = cell.crop, health = clamp(cell.health + 5))\n\
         )\n\
         ) in\n\
         (\n\
         field = wateredField,\n\
         currentSeed = model.currentSeed,\n\
         seedInventory = model.seedInventory\n\
         )\n\
         end\n\
         in\n\n\
         # Run multiple actions in sequence #\n\
         let garden : (Model, [Action]) -> Model =\n\
         fun (model: Model, actions: [Action]) ->\n\
         fold_left(actions, update, model)\n\
         in\n\n\
         # Helper to get health at a position #\n\
         let healthAt : (Model, Row, Col) -> Health =\n\
         fun (model, row, col) ->\n\
         let cell = getCell(model.field, row, col) in\n\
         cell.health\n\
         in\n\n\
         # Helper to get crop at a position #\n\
         let cropAt : (Model, Row, Col) -> Crop =\n\
         fun (model, row, col) ->\n\
         let cell = getCell(model.field, row, col) in\n\
         cell.crop\n\
         in\n\n\
         # ===== TESTS ===== #\n\n\
         # Basic planting #\n\
         hint \"can plant a crop\"\n\
         test\n\
         let m = update(init, PlantCrop(0, 0)) in\n\
         cropAt(m, 0, 0) == \"\240\159\140\177\"\n\
         end;\n\n\
         hint \"new crop starts at base health\"\n\
         test\n\
         let m = update(init, PlantCrop(1, 1)) in\n\
         healthAt(m, 1, 1) == 50\n\
         end;\n\n\
         hint \"cannot plant on occupied cell\"\n\
         test\n\
         let m = garden(init, [PlantCrop(0, 0), SelectSeed(1), PlantCrop(0, \
         0)]) in\n\
         cropAt(m, 0, 0) == \"\240\159\140\177\"\n\
         end;\n\n\
         # Seed selection #\n\
         hint \"select different seed\"\n\
         test\n\
         let m = garden(init, [SelectSeed(1), PlantCrop(0, 0)]) in\n\
         cropAt(m, 0, 0) == \"\240\159\140\191\"\n\
         end;\n\n\
         # Harvesting #\n\
         hint \"can harvest a crop\"\n\
         test\n\
         let m = garden(init, [PlantCrop(0, 0), HarvestCrop(0, 0)]) in\n\
         cropAt(m, 0, 0) == \"\"\n\
         end;\n\n\
         hint \"harvested cell has 0 health\"\n\
         test\n\
         let m = garden(init, [PlantCrop(0, 0), HarvestCrop(0, 0)]) in\n\
         healthAt(m, 0, 0) == 0\n\
         end;\n\n\
         # Companion effects - Beneficial #\n\
         hint \"\240\159\140\177 and \240\159\140\191 are companions\"\n\
         test\n\
         companionEffect(\"\240\159\140\177\", \"\240\159\140\191\") == \
         Beneficial\n\
         end;\n\n\
         hint \"\240\159\141\132 and \226\152\152\239\184\143 are companions\"\n\
         test\n\
         companionEffect(\"\240\159\141\132\", \"\226\152\152\239\184\143\") \
         == Beneficial\n\
         end;\n\n\
         hint \"\240\159\140\177 next to \240\159\140\191 gains health\"\n\
         test\n\
         let m = garden(init, [\n\
         PlantCrop(0, 0),              # \240\159\140\177 at (0,0) #\n\
         SelectSeed(1),\n\
         PlantCrop(0, 1),              # \240\159\140\191 at (0,1) #\n\
         CalculateHealth\n\
         ]) in\n\
         healthAt(m, 0, 0) > 50\n\
         end;\n\n\
         hint \"\240\159\140\191 next to \240\159\140\177 gains health\"\n\
         test\n\
         let m = garden(init, [\n\
         PlantCrop(0, 0),              # \240\159\140\177 #\n\
         SelectSeed(1),\n\
         PlantCrop(0, 1),              # \240\159\140\191 #\n\
         CalculateHealth\n\
         ]) in\n\
         healthAt(m, 0, 1) > 50\n\
         end;\n\n\
         # Companion effects - Harmful #\n\
         hint \"\240\159\140\177 and \240\159\141\132 are rivals\"\n\
         test\n\
         companionEffect(\"\240\159\140\177\", \"\240\159\141\132\") == Harmful\n\
         end;\n\n\
         hint \"\240\159\140\177 next to \240\159\141\132 loses health\"\n\
         test\n\
         let m = garden(init, [\n\
         PlantCrop(1, 1),              # \240\159\140\177 at center #\n\
         SelectSeed(2),\n\
         PlantCrop(1, 0),              # \240\159\141\132 to the left #\n\
         CalculateHealth\n\
         ]) in\n\
         healthAt(m, 1, 1) < 50\n\
         end;\n\n\
         # Neutral effects #\n\
         hint \"same crop has no effect\"\n\
         test\n\
         companionEffect(\"\240\159\140\177\", \"\240\159\140\177\") == Neutral\n\
         end;\n\n\
         hint \"unrelated crops are neutral\"\n\
         test\n\
         companionEffect(\"\240\159\140\184\", \"\226\152\152\239\184\143\") \
         == Neutral\n\
         end;\n\n\
         # Multiple neighbors #\n\
         hint \"multiple companions stack benefits\"\n\
         test\n\
         let m = garden(init, [\n\
         PlantCrop(1, 1),              # \240\159\140\177 in center #\n\
         SelectSeed(1),\n\
         PlantCrop(0, 1),              # \240\159\140\191 above #\n\
         PlantCrop(1, 0),              # \240\159\140\191 left #\n\
         CalculateHealth\n\
         ]) in\n\
         healthAt(m, 1, 1) == 70         # 50 + 10 + 10 #\n\
         end;\n\n\
         hint \"mixed neighbors balance out\"\n\
         test\n\
         let m = garden(init, [\n\
         PlantCrop(1, 1),              # \240\159\140\177 in center #\n\
         SelectSeed(1),\n\
         PlantCrop(0, 1),              # \240\159\140\191 above: +10 #\n\
         SelectSeed(2),\n\
         PlantCrop(1, 0),              # \240\159\141\132 left: -10 #\n\
         CalculateHealth\n\
         ]) in\n\
         healthAt(m, 1, 1) == 50         # 50 + 10 - 10 = 50 #\n\
         end;\n\n\
         # Demo: A companion garden under moonlight #\n\
         let moonGarden = garden(init, [\n\
         # Plant \240\159\140\177 in center #\n\
         PlantCrop(1, 1),\n\
         # Surround with \240\159\140\191 companions #\n\
         SelectSeed(1),\n\
         PlantCrop(0, 1),\n\
         PlantCrop(1, 0),\n\
         # Add some \226\152\152\239\184\143 #\n\
         SelectSeed(3),\n\
         PlantCrop(2, 2),\n\
         # \240\159\141\132 next to \226\152\152\239\184\143 for companionship #\n\
         SelectSeed(2),\n\
         PlantCrop(2, 1),\n\
         # Calculate how they affect each other #\n\
         CalculateHealth\n\
         ]) in\n\
         moonGarden\n";
      refractors = "()";
    } )
