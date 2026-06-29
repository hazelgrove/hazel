let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / debugging / companion-plotter / companion-plotter-bug-scaffold",
    {
      segment =
        "((Secondary((id \
         a9f42d80-649e-4046-aec8-e4eb2199194e)(content(Comment\"# Companion \
         Planting Grid #\"))))(Secondary((id \
         a545a462-eef9-4db3-8a69-06919e11c543)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f7f1226-ea4f-4352-889f-7d3af82307d7)(content(Comment\"# Plants \
         affect their neighbors #\"))))(Secondary((id \
         e9e8efb2-422e-4e5e-aa29-44f6835bdb33)(content(Whitespace\"\\n\"))))(Secondary((id \
         58401d25-1f3b-4350-889b-a628ce79bd3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         68206ae5-bfaf-448e-85ac-e8a6205dbb22)(content(Comment\"# A crop is \
         represented by its emoji #\"))))(Secondary((id \
         2f9be437-9467-464d-b6d1-3d8360585f54)(content(Whitespace\"\\n\"))))(Tile((id \
         9e65c4af-1bd2-4c2b-a018-a5e197e5a133)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e1f5a545-cfa7-4473-a51c-33df1f16b104)(content(Whitespace\" \
         \"))))(Tile((id \
         d9dd497c-e354-4fc5-a783-abd15045e43c)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5d204d95-7fb0-4b55-bd4e-7b60c6e977d4)(content(Whitespace\" \
         \")))))((Secondary((id \
         8270716f-f4c5-4ed5-becc-2c38ef83b56b)(content(Whitespace\" \
         \"))))(Tile((id \
         db72ac6c-6c63-4c4e-92fa-a44e307da63d)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         12bb59c5-67b4-4cbe-897c-5125b67db892)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c3b0009-cfe0-45cd-874b-73cfeef33289)(content(Whitespace\"\\n\"))))(Tile((id \
         a17730a5-88d0-44b9-a4f7-0d25c80830d3)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8ac559bd-9c25-48f7-82ae-f34f8f3e8867)(content(Whitespace\" \
         \"))))(Tile((id \
         79443081-691b-41a2-b5ea-eebf86195087)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         65973d7d-07c7-44b8-98e1-141df02d904d)(content(Whitespace\" \
         \")))))((Secondary((id \
         4330829d-91e4-493d-9bfc-4ad653f7dd06)(content(Whitespace\" \
         \"))))(Tile((id \
         3246bad3-c0f9-4c67-8bef-e7dd51ccb90b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d35477b3-d179-4b5d-81fb-8b516af07842)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e918dbd4-6555-4337-88d0-8081c2b4451c)(content(Whitespace\"\\n\"))))(Tile((id \
         20155210-b572-4b1a-b837-8c1797af9684)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9c8c164f-bef0-469a-aec6-dc655691a3aa)(content(Whitespace\" \
         \"))))(Tile((id \
         695cb1c7-aa78-4878-b9d8-430d6ba8784f)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         51298c5f-63c8-4119-b8b6-0305bc135678)(content(Whitespace\" \
         \")))))((Secondary((id \
         b325abb9-61a0-4e1f-b426-f242a5afc340)(content(Whitespace\" \
         \"))))(Tile((id \
         a2fd4205-d40f-4abe-82e7-e9d76362b59c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2e66c555-58b0-4a87-a936-185ed4101736)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7957c846-096d-4da7-8a74-a7d38de37942)(content(Whitespace\"\\n\"))))(Secondary((id \
         2305cf80-3d0e-4323-aba9-1b6dfe3b83d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         107508fd-bfbe-48c8-a80d-64742822c090)(content(Comment\"# Health is \
         0-100, affected by neighboring plants #\"))))(Secondary((id \
         fc9d4546-a2a0-450d-924e-adab55200d57)(content(Whitespace\"\\n\"))))(Tile((id \
         9430a114-dfc4-4d9c-8942-e8071a9c4b84)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7010dd94-356c-4103-b623-af5cf404c29f)(content(Whitespace\" \
         \"))))(Tile((id \
         619dba12-464b-4856-85c9-efac8b59bca7)(label(Health))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c661e2b2-63f0-4b96-82bb-27305c7f98f6)(content(Whitespace\" \
         \")))))((Secondary((id \
         1dd0370d-0cef-497b-8f7f-37f80217c112)(content(Whitespace\" \
         \"))))(Tile((id \
         9e2de19c-7b72-4931-a9be-c9d26912391c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d6cc8eb5-b89b-4dc0-9dda-1e86de616823)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fbe6d83f-421c-4138-a4ad-4863d3b9222c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a8a6ca2-30e5-446e-b266-600246aaee20)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c964c1f-2827-43fa-9aaf-82b577bcba47)(content(Comment\"# A cell in \
         the garden grid #\"))))(Secondary((id \
         c07614c0-6883-485d-a051-144cf4f4130d)(content(Whitespace\"\\n\"))))(Tile((id \
         093db0b5-2edb-4ab7-8565-f94f2510b47b)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6aef5558-1769-4e27-8e67-ba92a56acf01)(content(Whitespace\" \
         \"))))(Tile((id \
         185d635f-b13b-4234-8bbd-b5d44f3fd970)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         377a7118-7c33-4b20-b135-de20ec1cb7a8)(content(Whitespace\" \
         \")))))((Secondary((id \
         83b23c82-d6c5-48b3-afe0-e1989bc27d3c)(content(Whitespace\" \
         \"))))(Tile((id \
         72f9deb9-642f-4dda-8b3f-25263aebb310)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         e923ce42-17f1-4dad-9d62-09e15a705da7)(content(Whitespace\"\\n\"))))(Tile((id \
         2a99eb6c-a400-4c0b-9fbf-b81e79a53867)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         48535f90-ee88-4930-8ed2-c027ba140801)(content(Whitespace\" \
         \"))))(Tile((id \
         679d72a6-321a-46e9-a444-9d322461259d)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         96b8b87e-6fe4-4b54-bfdb-08ac38ea6c9f)(content(Whitespace\" \
         \"))))(Tile((id \
         1787f810-cb93-46cf-94e5-1b25bc84cfea)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ed9f0f5a-ad79-4758-a352-5dfca086cc0d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3b4df16-059c-4163-ba7f-1d190382546d)(content(Whitespace\"\\n\"))))(Tile((id \
         05b8b75c-2515-400e-ae09-f0debfba25a7)(label(health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b21bf0c8-cb81-40e2-b2db-1891d48cd1c1)(content(Whitespace\" \
         \"))))(Tile((id \
         cd3030cf-2a65-4240-b444-55453b9a17db)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         338e90d8-7487-4c7d-b500-3a865a2afb02)(content(Whitespace\" \
         \"))))(Tile((id \
         eb58a1fa-0ec7-461a-92a5-b609405ebb71)(label(Health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         661b88fe-3922-4693-acc9-278b29cf6b67)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cdd98471-714d-46c6-8f8e-bd9bb71d3f38)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a0006e1-37a3-46bc-8c71-6d5f90a92226)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfb9f507-add3-4fcf-a750-070b8821d47a)(content(Whitespace\"\\n\"))))(Secondary((id \
         9aab917c-04d0-4803-aab5-c00dfe0ffc83)(content(Comment\"# The garden \
         field is a grid of cells #\"))))(Secondary((id \
         a41458e6-6ed6-4cd4-adf6-eb56eb9c4623)(content(Whitespace\"\\n\"))))(Tile((id \
         584b713e-39c0-4e38-a3a1-04211b7256e5)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a10fa6f-4f49-406f-9850-db52909c0503)(content(Whitespace\" \
         \"))))(Tile((id \
         92c5f630-f625-45b2-9989-c48134ac7f4c)(label(Field))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         7833e9c3-7261-428a-9149-b4aa2503a5fc)(content(Whitespace\" \
         \")))))((Secondary((id \
         ab4a51b6-9542-4fd1-ba3b-3b9e47ec5380)(content(Whitespace\" \
         \"))))(Tile((id dc5e99cc-a682-437e-bd54-897e98cf7b33)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         793b58d1-7a61-4756-81a2-f76bfac80887)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e96bb6e8-5239-4f98-b450-308e018cd6ea)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         8e713395-5b5d-4f0f-8c3c-5dcb5b76105a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7260c0d6-d53b-452b-86c7-9e20827456c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         42223523-5bfa-49d0-bc05-5fe69e1ae83f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0be59d87-2bd6-4abc-a812-053472a05113)(content(Comment\"# Companion \
         effects between crops #\"))))(Secondary((id \
         4e4b5d52-8e4d-4379-8dc2-a17b911d517b)(content(Whitespace\"\\n\"))))(Tile((id \
         e5231393-8941-41c9-8d37-50f3fe9adc1e)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a3289748-782f-43a4-8d67-88b3aad4c67a)(content(Whitespace\" \
         \"))))(Tile((id \
         43572dc4-e45c-4f33-8c73-f29db129a626)(label(Effect))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3873f271-af1c-45f1-92d5-256eb97ab859)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb63ea4e-d554-4d7e-9166-785ed042d04f)(content(Whitespace\"\\n\"))))(Tile((id \
         c17c1609-7ec1-497e-9f99-53bcba7a76ba)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8f6d9e56-b376-4fd1-9b4b-6337b56f6497)(content(Whitespace\" \
         \"))))(Tile((id \
         7e916184-5d92-412f-92ec-590b42c977d6)(label(Beneficial))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         267e6260-5da7-4ba8-ab2d-620ef2db3297)(content(Whitespace\" \
         \"))))(Secondary((id \
         20531fa5-9fd3-46c4-b7a8-e2f505cf3734)(content(Whitespace\" \
         \"))))(Secondary((id \
         ca39ab46-12b3-466f-8745-c7ce3c2a62c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         e1418891-8f5a-4721-9f40-ebe26ec7f23f)(content(Comment\"# Companions \
         boost each other #\"))))(Secondary((id \
         b6fd4494-ba01-47fd-9c3f-f3c1f6ed521e)(content(Whitespace\"\\n\"))))(Tile((id \
         5433e02a-2816-4034-bfcd-2a0a27b30da0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23c04213-2257-46c7-8cb3-4023d9d8ce26)(content(Whitespace\" \
         \"))))(Tile((id \
         52d9298f-88bc-4c26-81d6-80e93980c485)(label(Harmful))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2e358997-714f-42e0-800c-9f362ef34bb2)(content(Whitespace\" \
         \"))))(Secondary((id \
         ad865d56-abd8-4dc9-948b-99ce28b65e97)(content(Whitespace\" \
         \"))))(Secondary((id \
         4bb5f19a-908f-4b3a-9ed9-d5c45f3b427e)(content(Whitespace\" \
         \"))))(Secondary((id \
         96b1f24c-cf00-44dc-aace-215307b38110)(content(Whitespace\" \
         \"))))(Secondary((id \
         06babd02-76d2-4859-9172-ec45074fb8f6)(content(Whitespace\" \
         \"))))(Secondary((id \
         ed37a30e-9c0c-43d2-935c-60a20bbbadec)(content(Whitespace\" \
         \"))))(Secondary((id \
         70b99294-8b00-4c24-b7a2-fc78b4a09c10)(content(Comment\"# Rivals \
         suppress each other #\"))))(Secondary((id \
         e67ba56e-03d3-4539-92d7-63a05c7de02b)(content(Whitespace\"\\n\"))))(Tile((id \
         88c41506-e824-43a8-9358-8e9c8cb173a5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a5b45dec-7b21-433e-b0e7-94cb0bc4cbf2)(content(Whitespace\" \
         \"))))(Tile((id \
         6c2394b6-2463-4334-89e7-01056e455a19)(label(Neutral))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         03ead937-7068-4d51-bf3d-1bb0219cee70)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e6abfa6-9fa3-42ca-8920-944d3bd6e737)(content(Whitespace\" \
         \"))))(Secondary((id \
         5cb45136-bc35-4d27-8339-84a5ea1e2f27)(content(Whitespace\" \
         \"))))(Secondary((id \
         c8659af9-2a7d-44fe-908b-e99d55a708dd)(content(Whitespace\" \
         \"))))(Secondary((id \
         d93414b5-8dcc-48e5-aee4-1e2cba436639)(content(Whitespace\" \
         \"))))(Secondary((id \
         81863542-5937-4887-b3eb-da364cd362c9)(content(Whitespace\" \
         \"))))(Secondary((id \
         6104c3e7-32e7-4b01-9054-6636e8a47a49)(content(Comment\"# No \
         interaction #\"))))(Secondary((id \
         dafeab8b-fd92-40ce-a15a-730b4fecda26)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e8e41f48-1dcf-41af-8a13-e6110fd6e70f)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd95649e-ee75-4230-8028-0b8e193e0828)(content(Whitespace\"\\n\"))))(Secondary((id \
         18d0d69b-22cd-4345-a0f4-da451281fb9e)(content(Comment\"# The garden \
         state #\"))))(Secondary((id \
         9182f096-4087-4932-9b4c-134777e700cc)(content(Whitespace\"\\n\"))))(Tile((id \
         64eac376-80ab-419f-b255-18e4e6443f32)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bac47495-4589-4902-92d1-4ae0c98f5319)(content(Whitespace\" \
         \"))))(Tile((id \
         7543ffec-6495-424b-a4e9-26b62fe3b821)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4bb64c2a-fbab-4f9e-8a52-2578428a3ef6)(content(Whitespace\" \
         \")))))((Secondary((id \
         00ba6e5b-f5ec-4ca6-8f81-a2bdaf9397e2)(content(Whitespace\" \
         \"))))(Tile((id \
         7b7d5f6e-eb66-4dcd-9aa8-08f0a11cfe84)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         5b426830-4742-4ff8-bf64-2b9e4f2beec1)(content(Whitespace\"\\n\"))))(Tile((id \
         3f2b5e49-25c4-450e-88b2-0b5f4d69c016)(label(field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f59d2171-99e7-4bf1-81c7-983d1000b723)(content(Whitespace\" \
         \"))))(Tile((id \
         e8a041cd-838c-4049-83a4-d005dd78bd18)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         240132d1-cbea-499c-b0f5-b49b687aa5aa)(content(Whitespace\" \
         \"))))(Tile((id \
         54cf6832-04d6-434a-9806-0a425d15f7f4)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dbb1bfcc-81ba-4e22-bec6-3db00e5e84e3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d51a9681-6cf1-4b63-8949-a7779ad471fd)(content(Whitespace\"\\n\"))))(Tile((id \
         d938ee7a-6788-44ee-a81a-dd914e7e1e84)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         558f79f0-fa69-4ed1-8e11-046e0789ac79)(content(Whitespace\" \
         \"))))(Tile((id \
         3f40da93-c14c-439c-8bc2-345a258db399)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         81acc768-14cc-45e8-b080-8f4ab6bc2922)(content(Whitespace\" \
         \"))))(Tile((id \
         0d69f757-c070-4acc-bb31-29fac1836a47)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         20a958d4-9e32-4b62-84f8-2a3f4f779d9c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9f1c71fa-aeb8-4344-98fe-76458a9dc68e)(content(Whitespace\"\\n\"))))(Tile((id \
         a050750e-20cc-4bf6-95fe-c305c229730e)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         94dac612-a28e-4273-98dc-067b463279c3)(content(Whitespace\" \
         \"))))(Tile((id \
         2489e973-e1e6-4ea7-a590-bdcad849dd5c)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         061f337b-04be-449b-b361-15b7eacfda76)(content(Whitespace\" \
         \"))))(Tile((id f2dabff1-7cd9-4aaa-91ad-ee4c03102eee)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         1fa5a573-e739-4f9a-be79-c674efdc9d32)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         30d563b0-f813-436d-a6e7-9b53ef0e8ac5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0bd5c22c-8720-44d3-963c-18696bf742ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2a20818-cccd-4f58-b97f-d0f78f6074a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         426882b0-8d5f-4d58-80bb-5faae10ee5ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         b44a0297-ae2d-4b28-aa10-3c1728c1c8ff)(content(Comment\"# Actions the \
         gardener can take #\"))))(Secondary((id \
         de42000d-fc7e-4b9f-becb-5ed8d13016cc)(content(Whitespace\"\\n\"))))(Tile((id \
         dfc1db0a-9267-4f31-801e-3a957f794e14)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2b8852d3-a351-47e6-aa68-5a95f2c43212)(content(Whitespace\" \
         \"))))(Tile((id \
         b8057086-bd2c-4ff7-a0a8-bcf669b1fc02)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         0faf47e4-e7e5-494c-9ae0-c5fb5dcc6d0c)(content(Whitespace\" \
         \")))))((Secondary((id \
         5aa18504-29ba-47d0-bd6d-f525856eaab1)(content(Whitespace\"\\n\"))))(Tile((id \
         94b53406-afc7-40be-ba60-b6139c4cdd5b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bcea2ebe-e38d-474e-82cb-62219545752b)(content(Whitespace\" \
         \"))))(Tile((id \
         fb2b67ee-d652-438e-aa15-b167eae13e53)(label(PlantCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b45c448b-6935-404e-80b9-b52ff8ff4d33)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8ebd63cf-8b35-43e2-8446-0489db01e6f9)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e72b15ca-c76f-40fe-a4ae-c2bfb81cba7a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b1ee0e0-63ef-4d8b-817c-cfe506b94203)(content(Whitespace\" \
         \"))))(Tile((id \
         76bc7892-e830-44ad-b2d8-e82245950f07)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5e0c3b33-8251-4b26-9b20-42d69e49602b)(content(Whitespace\" \
         \"))))(Secondary((id \
         f03173d6-02c0-4ca7-9ed3-b1d139b80115)(content(Whitespace\" \
         \"))))(Secondary((id \
         fd802120-8001-44e0-b5ea-7817f36f770a)(content(Whitespace\" \
         \"))))(Secondary((id \
         c7dddb1f-8a2c-4b7e-b718-045d5965bc99)(content(Whitespace\" \
         \"))))(Secondary((id \
         846127da-4322-4a7d-a673-0b27056573ae)(content(Whitespace\" \
         \"))))(Secondary((id \
         e9027039-1d25-4dc9-b938-c12c19e39305)(content(Whitespace\" \
         \"))))(Secondary((id \
         5bac105c-cc7e-48ad-82a1-40cc197815c3)(content(Comment\"# Plant \
         current seed at position #\"))))(Secondary((id \
         2bd3381f-6101-4d5b-9573-327e89443d04)(content(Whitespace\"\\n\"))))(Tile((id \
         c35883c0-15f6-4e99-ba10-5c3f5ec171a5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a0fc1c3f-69d8-42c3-859f-0e70e4116443)(content(Whitespace\" \
         \"))))(Tile((id \
         091a39e7-9d05-49de-be2d-05d5a281836f)(label(HarvestCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f19828cf-32b9-48f5-9c5a-6ebf7f3e36f5)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b221ef70-90dc-400f-896f-feb42a3e8015)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2c352b66-3949-4b27-9937-6d439f0edd40)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3d99c3e-009c-4060-aac0-830ac3e1e816)(content(Whitespace\" \
         \"))))(Tile((id \
         a6ae1dc3-29ab-4bfa-8327-62c9eda8c751)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b0c6e1be-d1ea-486c-a37a-56e5476d086d)(content(Whitespace\" \
         \"))))(Secondary((id \
         a9f1654a-f5cc-4d27-b9a4-b785287b550f)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1c80dc2-b384-495e-8489-e3a8f52917cb)(content(Whitespace\" \
         \"))))(Secondary((id \
         d880b14d-6ed0-464f-ab71-8f66ddd78daa)(content(Whitespace\" \
         \"))))(Secondary((id \
         8498be40-f5de-48da-b3fb-e9f2f2e88bc8)(content(Comment\"# Remove a \
         crop #\"))))(Secondary((id \
         674503e6-bfb9-4501-867d-adb59d4f4690)(content(Whitespace\"\\n\"))))(Tile((id \
         c7d629aa-4664-45aa-b453-2e47c9fbf083)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a20bab96-2a3d-4df5-9970-0c905e9d736f)(content(Whitespace\" \
         \"))))(Tile((id \
         9911621d-bd4a-4244-aeb5-e30ae79f1f12)(label(CalculateHealth))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2487bc7e-639f-4c78-a419-63bd539d6c0b)(content(Whitespace\" \
         \"))))(Secondary((id \
         9a528963-387f-497c-9de5-6a71d24d852a)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e13017c-d87a-4f0a-9070-d744d2c9ff6c)(content(Whitespace\" \
         \"))))(Secondary((id \
         aece87f2-f20d-4fa7-b291-dedd58301840)(content(Whitespace\" \
         \"))))(Secondary((id \
         396b9206-516e-4962-b3ea-3f1676040e44)(content(Whitespace\" \
         \"))))(Secondary((id \
         d536dc5e-3d96-410e-99be-450f030bf083)(content(Whitespace\" \
         \"))))(Secondary((id \
         eb6a12ea-2c25-493a-9b41-176aac52c8e7)(content(Whitespace\" \
         \"))))(Secondary((id \
         8cd421f8-9301-44ed-96b6-9c06f7badc0c)(content(Whitespace\" \
         \"))))(Secondary((id \
         0db29a13-e129-4adc-9e1e-cfb87b5f861c)(content(Whitespace\" \
         \"))))(Secondary((id \
         33bf8187-8f27-4950-9219-b929347f3ced)(content(Whitespace\" \
         \"))))(Secondary((id \
         295c9547-5342-433a-a0f3-3afcaccb2d21)(content(Comment\"# Recalculate \
         all health based on neighbors #\"))))(Secondary((id \
         af52e7c9-3770-4edb-970c-d19da97cb941)(content(Whitespace\"\\n\"))))(Tile((id \
         253392a1-ee6b-40fb-8546-382adfb5ce3c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         43050b28-e73a-4cdd-b963-700b06531b70)(content(Whitespace\" \
         \"))))(Tile((id \
         2e7acfe6-8768-4999-9eb5-0f7c162f24e3)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d32a79ee-aa7c-42e8-b235-59b8df632382)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ca1d346e-8e4c-41f3-92af-f934de63d614)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         509122a4-42f6-4856-9a50-d31faf0aec2d)(content(Whitespace\" \
         \"))))(Secondary((id \
         cbdd763a-60b9-40fc-bbea-6363893b8197)(content(Whitespace\" \
         \"))))(Secondary((id \
         f9c35dcc-12ee-48e7-9e4e-acee060aa78e)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbd065f6-24d5-4724-95be-ef3075022dc9)(content(Whitespace\" \
         \"))))(Secondary((id \
         1de2a62a-d9ee-4a2f-970a-382afbef728e)(content(Whitespace\" \
         \"))))(Secondary((id \
         3f2757a0-ad40-4d25-b1ce-93dd5a2ae2fa)(content(Whitespace\" \
         \"))))(Secondary((id \
         492fade6-92c1-4561-a58d-de0ab57dc966)(content(Whitespace\" \
         \"))))(Secondary((id \
         eb18568a-f098-41a4-88a3-d328a5a73807)(content(Whitespace\" \
         \"))))(Secondary((id \
         1eb3f1d8-d484-4740-aa9f-cbcb2d3f73e3)(content(Whitespace\" \
         \"))))(Secondary((id \
         88b1c086-8b73-45c6-b692-3d768ac252ec)(content(Whitespace\" \
         \"))))(Secondary((id \
         8a95f160-9478-4679-81e7-07eff5b36fa6)(content(Comment\"# Choose a \
         seed from inventory #\"))))(Secondary((id \
         eac4c322-9a33-4c33-9de7-e672b89c99ca)(content(Whitespace\"\\n\"))))(Tile((id \
         419272f1-dd71-4db3-a627-b3aa4d7f01c4)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5eb4b6c5-2a61-4256-ba67-846268e6a880)(content(Whitespace\" \
         \"))))(Tile((id \
         cefef766-e01d-4d0f-93ad-dba6d54bc4f8)(label(WaterAll))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         614af0f1-a3f5-481f-93b7-7c146671e9a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         2cf4b2b0-1ea4-489d-9264-fbe57b266ee3)(content(Whitespace\" \
         \"))))(Secondary((id \
         d4036d7b-9efc-4bf5-b9a9-7112edf621b5)(content(Whitespace\" \
         \"))))(Secondary((id \
         46e37d50-3e5c-46c8-9074-ad9cb224f15c)(content(Whitespace\" \
         \"))))(Secondary((id \
         54ff0ec7-3737-47d6-93c5-726f1c3d36a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         8b7b9c4a-8abf-4e3e-9adc-7b55cbb4f37a)(content(Whitespace\" \
         \"))))(Secondary((id \
         9e478135-ae0a-4152-9d4d-57b627b21725)(content(Whitespace\" \
         \"))))(Secondary((id \
         4833439e-f97f-45c8-95f4-6c26cf4a6f26)(content(Whitespace\" \
         \"))))(Secondary((id \
         922f1bc2-d3ea-4fbe-b0f4-a52da77d42dc)(content(Whitespace\" \
         \"))))(Secondary((id \
         a5af76b2-23c9-4d5c-9f5e-16219cbef5ad)(content(Whitespace\" \
         \"))))(Secondary((id \
         e8744cb2-8f7f-4511-ba99-bac2e76feb45)(content(Whitespace\" \
         \"))))(Secondary((id \
         6c664da9-9d28-4364-ad01-8acca405ede4)(content(Whitespace\" \
         \"))))(Secondary((id \
         b220a9e7-cda1-491e-bfc9-d6c7e0af13a8)(content(Whitespace\" \
         \"))))(Secondary((id \
         883bbe2f-5253-4f0f-8638-152bd72ce188)(content(Whitespace\" \
         \"))))(Secondary((id \
         314adf55-a647-40a5-b3c6-2d9ed7d94db9)(content(Whitespace\" \
         \"))))(Secondary((id \
         5750ff7b-feb8-4100-a0d1-ce5fa648332d)(content(Whitespace\" \
         \"))))(Secondary((id \
         ea037bf5-2059-45b1-a803-32dfccd3cd66)(content(Whitespace\" \
         \"))))(Secondary((id \
         9b9bcade-8cb9-4d21-892b-06788c3cddcc)(content(Comment\"# Boost all \
         health by 5 #\"))))(Secondary((id \
         afa44659-3808-4456-bdcf-074ff23ed363)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf6c8e38-2b36-47e5-839a-2bfa544669cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         01578903-add9-4467-a3ef-15d02bace0a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         78f8fa76-03f1-4db7-bf26-4871238bc4aa)(content(Comment\"# Utility: \
         clamp value between 0 and 100 #\"))))(Secondary((id \
         d42d2c74-3642-4fbf-ad63-4067f7b266aa)(content(Whitespace\"\\n\"))))(Tile((id \
         3585a145-03b4-4e12-ba40-7afce4c70739)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b7033610-df3b-4a8a-bece-4c926c693466)(content(Whitespace\" \
         \"))))(Tile((id \
         e5989d89-092a-4ba3-9c54-11609a70d3f4)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7430fe10-4547-4d46-a236-e843aec8bb85)(content(Whitespace\" \
         \"))))(Tile((id \
         0ac64602-d445-4a2d-b1ea-6397c706bf56)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b30d6c76-5ef9-4017-a555-372597343bc3)(content(Whitespace\" \
         \"))))(Tile((id \
         64434b28-022f-4b4c-9082-2cb87c779999)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         437ba833-4fb8-4331-bb00-48fefe82aa95)(content(Whitespace\" \
         \"))))(Tile((id \
         5eec6013-bdf6-4626-861e-a6a94060a077)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4715b94e-5665-42d1-98dd-5693dfb7f6c1)(content(Whitespace\" \
         \"))))(Tile((id \
         46bb6ed0-062c-4fbc-a2d8-f29aeece0774)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c7de1fcf-b9df-4dd2-b797-b3460e6b6333)(content(Whitespace\" \
         \")))))((Secondary((id \
         1e7e11f3-4c82-495f-b769-6f4ad6c2608c)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f89921-1e36-492b-8a0a-29607ca91aa2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         88b51695-d942-4667-8bd7-30b1f7192a6c)(content(Whitespace\" \
         \"))))(Tile((id \
         88af184f-9c39-4572-beb0-d51f37543aaa)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d021562d-dcf7-42f7-9ed8-896a89c8ed6e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1deae020-a456-4db5-abf9-6eb0fe58213f)(content(Whitespace\"\\n\"))))(Tile((id \
         4d7a113e-e739-449c-a046-aa998d83f778)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2874dff9-d8b1-42cc-baaa-aa63789b2a43)(content(Whitespace\" \
         \"))))(Tile((id \
         d372a4f4-296d-4504-8c39-73ea25261163)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         58349286-af30-4d89-8fc5-bf5e1e3ea995)(content(Whitespace\" \
         \"))))(Tile((id \
         3dafbac3-06b5-4ead-9d51-007e8aec4af5)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         918c84af-596b-4feb-8c7e-25dac03f5d34)(content(Whitespace\" \
         \"))))(Tile((id \
         6c1ba2a5-da2c-406b-967c-1f4c4511b2fa)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc5ec8a9-eb21-43b1-833a-6e59d6540edd)(content(Whitespace\" \
         \")))))((Secondary((id \
         465e7045-38a9-4f01-a851-7695b6d681f5)(content(Whitespace\" \
         \"))))(Tile((id \
         e58e6894-e7a6-4596-b77e-91b7df2aeb3d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3764bbca-65a0-434e-a569-399855c1d0fa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         593ab063-2be5-4692-8ceb-ac9cbb807af2)(content(Whitespace\" \
         \"))))(Tile((id b68275ec-1903-4a26-addf-b490f8c75651)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         c9a29c13-9bb3-47fc-a156-f4c0d72f28a9)(content(Whitespace\" \
         \"))))(Tile((id \
         6b2353dd-3759-4a8d-b077-07cae2cf8bad)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3826995f-c14c-4024-bcf1-141bcab04f51)(content(Whitespace\" \
         \"))))(Tile((id \
         15eb3d6f-18b5-4584-850a-144f69fae8b8)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2654d99-fcf3-4364-ad9c-8b8e89d53585)(content(Whitespace\" \
         \"))))(Tile((id \
         c41bed50-0f5a-45c1-b320-fdf533bc909d)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         373a7708-0867-4aab-9ca5-4e28cd22c713)(content(Whitespace\" \
         \")))))((Secondary((id \
         9563300c-88fd-454f-815f-869475f7434d)(content(Whitespace\" \
         \"))))(Tile((id \
         b63555c4-f291-4481-a4bd-a8a385c8f39f)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e628fb60-e994-4208-b724-90bcfbe9846f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7cebcfcd-9d65-4b23-8d46-9c0381ac728e)(content(Whitespace\" \
         \"))))(Tile((id \
         c1deeb0d-29d2-4d0c-8fc0-3635a6b3480f)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f77957ad-feaa-4ac1-9a09-d4cb17cbea7e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         54f58451-e30b-478e-b358-c88ebbbd1d52)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1ab459c-3db4-4858-902f-fe8dc0dfcaf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         51f87438-506a-401e-a61c-e07df01483c6)(content(Comment\"# Create an \
         empty cell #\"))))(Secondary((id \
         20489997-8956-4d97-989d-85ca44daf3c1)(content(Whitespace\"\\n\"))))(Tile((id \
         1a33bf87-f336-4c90-96cb-35103d932f45)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1739a69a-a764-4644-bdb1-a51915c5340f)(content(Whitespace\" \
         \"))))(Tile((id \
         937e9e84-ba5f-4520-8aec-9c51cd54b4dd)(label(emptyCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef9b91b8-4254-4853-976e-46b34690653d)(content(Whitespace\" \
         \"))))(Tile((id \
         20bbdf92-b5cb-4902-96f4-daf4f0ed080f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6aad2592-c542-4325-9846-9c803d237d28)(content(Whitespace\" \
         \"))))(Tile((id \
         9734c767-ff9f-4761-9652-62beab07a894)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         592dbbca-4fb5-444b-9f2b-c02b56e60282)(content(Whitespace\" \
         \")))))((Secondary((id \
         6c300964-30ed-4806-a63d-89f0a991698a)(content(Whitespace\" \
         \"))))(Tile((id \
         ed49d9ca-52a9-4bb0-9969-5348df016e77)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f604218c-4e71-4bc2-95a1-329bb3528e0c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4918e9d-a04a-4194-b308-47dca9d0a128)(content(Whitespace\" \
         \"))))(Tile((id \
         d4a41e7a-62fc-4e06-83ba-8d7c3a430891)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51963662-155c-4837-984a-b3ce1b569337)(content(Whitespace\" \
         \"))))(Tile((id \
         6697d4a7-4f20-4421-8460-e581a087342d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbde7b33-bf72-4a9a-ad39-6151ddf696e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         772f0609-a4af-40d1-8a90-c67446077432)(content(Whitespace\" \
         \"))))(Tile((id \
         87890919-a418-4444-bc48-26ddd0631398)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f6f7c93-c2e4-41d9-bd4f-666aa87b9cfa)(content(Whitespace\" \
         \"))))(Tile((id \
         011fad79-fcfa-4530-87e4-af63234a5740)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8dc6cc5-983d-4e6a-a8b5-2ffeb0a1f8a1)(content(Whitespace\" \
         \"))))(Tile((id \
         d1d6079c-612d-45cb-844f-2472114ba17d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f990eec9-d5ee-46e8-b926-9e8efef55647)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         97551ffc-96b8-4d02-8f9c-39bcc9393146)(content(Whitespace\"\\n\"))))(Secondary((id \
         5703644b-2ed0-4376-87e1-87d75c3dd13b)(content(Whitespace\"\\n\"))))(Secondary((id \
         74099233-fafc-48e2-8e21-5033cacd171a)(content(Comment\"# Create a \
         cell with a crop at base health #\"))))(Secondary((id \
         dd12c0dd-d98c-405b-b900-aa64919bab71)(content(Whitespace\"\\n\"))))(Tile((id \
         5774f660-1c9b-4b92-8fd4-a674a6bba43a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f0a54b7d-2b2a-4f63-9792-d89e2a04dc25)(content(Whitespace\" \
         \"))))(Tile((id \
         b8db4b23-5b8b-41b5-98e4-e7b618abbaef)(label(makeCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9631c597-d620-4dac-9918-94fabbd82cc8)(content(Whitespace\" \
         \"))))(Tile((id \
         686f84d6-d7df-4887-ac6b-f4a8a884c3ff)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e9d79577-937e-4a16-9a9e-ffa5c929f2c4)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b53946-477b-493e-ae1f-7345bcf2cd8c)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f35b12ca-2116-4dae-8689-547d5c132438)(content(Whitespace\" \
         \"))))(Tile((id \
         ce0b5d63-6e94-4113-93b2-e6401232e429)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         74cfac25-616e-46c9-8f62-b0a5a4caa3e5)(content(Whitespace\" \
         \"))))(Tile((id \
         7c6ec0ad-7885-49db-99d4-35e52ec0cd1e)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6cf94944-73d7-4bc0-9dcc-961e7bba7ad2)(content(Whitespace\" \
         \")))))((Secondary((id \
         4e63548a-a04d-4c97-b138-6afdf8ea6aa4)(content(Whitespace\"\\n\"))))(Tile((id \
         ea29fe6a-f96f-4b9b-a99d-2ab5a8799443)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         41d1d98a-0206-4118-befe-380bc68695e7)(content(Whitespace\" \
         \"))))(Tile((id \
         388bca2d-232e-49d7-9f1d-49927ef8e786)(label(crop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a2b1b9ee-a587-424b-b99e-0be55a60936d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7cd4ac1a-482d-4d44-b8c2-0f54ce4bbdf1)(content(Whitespace\" \
         \"))))(Tile((id \
         a6cd7f85-d4ca-4096-ba7f-a6b07171a8e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         be18ca30-f74c-4dfe-ac04-064cae58e747)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6f43a9bb-abd3-4eff-96b7-5ef3ed0fda51)(content(Whitespace\" \
         \"))))(Tile((id \
         b72a87ef-81d5-45db-af0c-33a6faf56efb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c51ed73f-35ed-4212-a10a-72c889b602f7)(content(Whitespace\" \
         \"))))(Tile((id \
         9ccda69b-c59f-4445-b7c7-33deeeb0ae6e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         343cbecd-449d-4cd7-a4a9-2bd17dd522bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c7cfc89-4ff8-47a7-a5aa-d74fdf3db157)(content(Whitespace\" \
         \"))))(Tile((id \
         d3cd8eaa-e624-4414-b6af-4ff8132465ea)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e2cd291-3f03-4bc7-8e86-0b53fede0a6d)(content(Whitespace\" \
         \"))))(Tile((id \
         24819e9b-7d67-48a4-8a19-ac7ce9f05fbf)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         697dd405-461e-4fd2-b42c-5a9a00acfb4a)(content(Whitespace\" \
         \"))))(Tile((id \
         5e917b94-a327-4cd0-90f4-bc69b7571a54)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         59ac1aa4-c5fe-4138-ac07-4f159b18d5ec)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         94a22698-1fa7-4a3f-ad0f-db74e258e57d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f18a1b9b-13b4-4df2-8e3b-7d75ab16f6eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         87bf37a8-d0d5-4f92-b2f9-74c4b2b249d5)(content(Comment\"# Determine \
         the companion effect between two crops #\"))))(Secondary((id \
         43ef4b6e-3563-421f-a252-4d67f1f7c3f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         afd9b337-8292-4032-a7a7-129b1f145326)(content(Comment\"# \
         \\240\\159\\140\\177 and \\240\\159\\140\\191 are companions \
         (Beneficial) #\"))))(Secondary((id \
         5d44a92d-66a3-4739-abdb-f1132de73be9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ae32bd6-f02b-43ed-a62c-3c051174af6e)(content(Comment\"# \
         \\240\\159\\141\\132 and \\226\\152\\152\\239\\184\\143 are \
         companions (Beneficial) #\"))))(Secondary((id \
         1085cb07-3ec8-405b-8987-f1142adbe9a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0d911fc-6ab7-4a6b-80d7-e5d63cf4130e)(content(Comment\"# \
         \\240\\159\\140\\177 and \\240\\159\\141\\132 are rivals (Harmful) \
         #\"))))(Secondary((id \
         fa9dc195-e0cd-4589-897b-e26454acefe0)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f17e93c-0e49-4055-a548-87e12d47b667)(content(Comment\"# Everything \
         else is Neutral #\"))))(Secondary((id \
         f5a89330-3161-4744-bf85-81889f4dbfb2)(content(Whitespace\"\\n\"))))(Tile((id \
         444300f5-9131-4252-9ae6-c64bb5eaa70a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c17edd3-a769-467e-8d39-e66eb1004ea7)(content(Whitespace\" \
         \"))))(Tile((id \
         4c07bb41-05d1-46e6-80c5-b2e267cf3a49)(label(companionEffect))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         475263c9-8b7f-40e1-bf1d-18d19e3b950e)(content(Whitespace\" \
         \"))))(Tile((id \
         c676b91a-c4d1-4586-a657-e6b090e7285f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         53080193-9e4c-4884-b46a-fb201cdc2fce)(content(Whitespace\" \
         \"))))(Tile((id \
         89b88967-5bd3-486d-b3ea-157ff999e39e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         4833cd54-28cc-4c2f-ac55-e89e5691946b)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7eda9020-7d04-4198-bd09-40b0ce7941fd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         59a81e62-0e2e-4d91-b629-db9071ebadd8)(content(Whitespace\" \
         \"))))(Tile((id \
         73a74816-360f-44fc-bb37-0dc139d6b901)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         fb91b796-f777-4555-8437-c2d1a77cdbb9)(content(Whitespace\" \
         \"))))(Tile((id \
         b1736c87-f8d8-4ee9-aa4f-95718e1796df)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3ca78e19-222e-4cf6-b126-a87b2296c846)(content(Whitespace\" \
         \"))))(Tile((id \
         fab133bb-5c29-43fa-a858-f8bd48786ae2)(label(Effect))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         22d58428-53c0-44b9-ac6e-ed40488ffe20)(content(Whitespace\" \
         \")))))((Secondary((id \
         de0af9d3-5ab2-4a6b-8df6-88d1872913f2)(content(Whitespace\"\\n\"))))(Tile((id \
         bb3ed975-eaf6-4465-93a9-8c550be15663)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         17ca76be-cf9c-4056-af01-e03616b74c6a)(content(Whitespace\" \
         \"))))(Tile((id \
         c0b4dcc9-c723-4520-a260-0eddce5dd9ae)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7b7e8127-dfd4-4b99-a934-61f54deeb334)(label(crop1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0e9495e3-6a57-45d5-bc51-c356fd4f678b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         391d6329-b00b-44be-bed7-d3ccae63ec71)(content(Whitespace\" \
         \"))))(Tile((id \
         2aad2f24-86b5-4949-b5de-104e1d351712)(label(crop2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         97bc866e-9a1f-453a-885f-8bfd945e8404)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46a6a522-ffe1-4d2b-91c2-e2427dbf9bee)(content(Whitespace\"\\n\"))))(Tile((id \
         8796a6c6-e3a1-424f-b7bd-42ae9eaa169d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f28fd60c-2659-4541-97fa-075c0ff43189)(content(Whitespace\" \
         \"))))(Tile((id \
         68c59a8a-96f7-4b33-b459-56505aa72f4b)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9f5876e-cfbb-447f-b72a-78909855a0cb)(content(Whitespace\" \
         \"))))(Tile((id \
         75b31689-13cc-4213-94d7-cbfeb455d513)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86e9a9e3-71a8-4dbe-a5ac-15fc5b3afe07)(content(Whitespace\" \
         \"))))(Tile((id \
         9863a85f-0e2c-45c8-ade1-c76e8fd30973)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4febce29-a123-410a-82af-43d7bae45eab)(content(Whitespace\" \
         \"))))(Tile((id \
         6972b38b-cb49-4b06-9690-d1362336e288)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         046272a2-6afc-49cb-aa64-4abddfdaa142)(content(Whitespace\" \
         \"))))(Tile((id \
         7c0a5d48-a7eb-4179-8afb-70024235962d)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         079bec58-6eed-4d91-a84c-24df425721e4)(content(Whitespace\" \
         \"))))(Tile((id \
         d97dd130-0e59-4725-83bf-0b792267fc00)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cbaa584-5e1d-4c8a-a8d9-e7edbeaa1850)(content(Whitespace\" \
         \"))))(Tile((id \
         690b3729-04c9-4a0f-897a-a7edbd1e19a8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92cc19fa-5212-4164-97bd-9a124fab5186)(content(Whitespace\" \
         \")))))((Secondary((id \
         44a617a2-2efd-4410-8e79-094c436e5553)(content(Whitespace\" \
         \"))))(Tile((id \
         b22b13c5-d804-4cdc-af00-c6d88992555a)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5ed108f0-784c-4c71-9f8b-896dd3bc694d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7ec7099c-0570-4091-b102-873e1d377bb7)(content(Whitespace\" \
         \"))))(Tile((id 9375b408-d7a8-4cad-b6fd-f4097f19efbf)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         44f2a9bf-3624-4e21-afc8-627f75613ed4)(content(Whitespace\" \
         \"))))(Tile((id \
         46dd41d0-f9ab-43c9-bff3-d41abe60914c)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3f7b38f-da7e-4d22-bc72-33497e37be60)(content(Whitespace\" \
         \"))))(Tile((id \
         9be6bdfe-e23c-4ea2-b8fe-bbe76926bd95)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba022cad-f76a-4dd4-a1cb-4e593870aab4)(content(Whitespace\" \
         \"))))(Tile((id \
         3d65d8ad-dabc-4650-bd85-55143d532787)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ac25fe7-9c73-4259-9142-1b5e6f4a6203)(content(Whitespace\" \
         \")))))((Secondary((id \
         b9cd5340-cc75-40d8-8101-941b0c28f9b2)(content(Whitespace\" \
         \"))))(Tile((id \
         8774bd6d-6d26-4f5f-bf70-c4d06c782d75)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb52b68f-8998-4da2-ad90-f294e00c944f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e5842627-644a-43b4-b78b-a16e51f3e149)(content(Whitespace\" \
         \"))))(Tile((id 5c9e1d27-e475-4d86-a7d4-cc55aea4d713)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         978d15e5-0465-4301-9db5-aa7f0655fddd)(content(Whitespace\" \
         \"))))(Tile((id \
         9cd1a046-4f08-419b-b9f7-b5868c1ec195)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         defad2c0-1963-4d3a-b6fd-4dc42264a1b6)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39e7f4c5-840b-409a-87b3-62cd68a1342f)(content(Whitespace\" \
         \"))))(Tile((id \
         c709129e-f8a5-411c-957c-babb7a782ac1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4892518-8760-4c82-a361-a942604260e7)(content(Whitespace\" \
         \"))))(Tile((id \
         a4caf701-cbe3-4ec4-828b-5a0565c7b595)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         51a0b0b5-0e35-4585-a04d-c8d76908a4b1)(content(Whitespace\" \
         \"))))(Tile((id \
         1f4b594f-8875-460f-9749-ae7e7cb65b34)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4f094c1-1fd5-4617-be6d-010eb80c2a5e)(content(Whitespace\" \
         \"))))(Tile((id \
         32c9ece5-77b5-4e05-b5ca-800bd9637c31)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a58028b-1403-4edd-b20b-d301bde157d7)(content(Whitespace\" \
         \"))))(Tile((id \
         0d92d4da-2bd2-416e-bd4d-aeb97644c697)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccf83fc1-c870-4279-bca8-1d719320099a)(content(Whitespace\" \
         \"))))(Tile((id \
         b8d4bdcc-52f3-4c92-858d-1d7d26c02961)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f5bb8398-2006-4f10-8692-cbd70585bea6)(content(Whitespace\" \
         \"))))(Tile((id \
         95f2889c-86d4-47f5-a226-709a9cca80f0)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88805a07-fe22-4036-afe7-b8238043175a)(content(Whitespace\"\\n\"))))(Tile((id \
         d875c79d-0352-454a-8452-a139498c6983)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a6698671-1b7c-4909-afe1-a47ad5154f0b)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d331f6ca-5ece-406e-9e0a-ae8aeca6cdd4)(content(Whitespace\" \
         \"))))(Tile((id \
         316e951b-4e20-43d2-9e4e-158a28e82a96)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9104d9b9-2ea1-4bf5-97df-d85570a30e58)(content(Whitespace\" \
         \"))))(Tile((id \
         e9a5c190-b427-473d-b0a9-f184eff61e7e)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a879d35-5116-47a2-b709-5de272883a86)(content(Whitespace\" \
         \"))))(Tile((id \
         d2343d68-e6ca-4e11-b45a-f351874fc54f)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91e2bd57-9201-42bb-be6a-fc5925d9cc16)(content(Whitespace\" \
         \"))))(Tile((id \
         3c7da004-cf49-4f6b-a96d-f7da21d9ba44)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6984efd5-029e-432e-ac05-c9a0542e9bd9)(content(Whitespace\" \
         \"))))(Tile((id \
         91ced1cd-9c75-482c-944a-3bed87ba7981)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b793f89-5e92-408e-ad7c-12e92dde7998)(content(Whitespace\" \
         \"))))(Tile((id \
         49f3d1be-2731-4f37-b273-9591d0a341a8)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cd837e00-a516-4b10-9018-c4dab054c441)(content(Whitespace\" \
         \")))))((Secondary((id \
         c89cea7a-790d-4d18-a611-d91d527f6b8b)(content(Whitespace\" \
         \"))))(Tile((id \
         ba04b2e2-60e6-4bf4-880d-c5002936dc7d)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa92e014-6e1e-4737-9031-273f55f130c7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8d972776-100f-4b24-9d2b-d2c093bf4a02)(content(Whitespace\" \
         \"))))(Tile((id bd84d465-739d-46fb-81cc-8f2742b15122)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         14b14005-67d9-404d-8446-469f2eac1060)(content(Whitespace\" \
         \"))))(Tile((id \
         5d28578b-c6ab-44f7-9c93-5e16fffa9e41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         035ff3f3-4783-4f58-ae96-09c6631f513d)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1ce165d-6cc1-4fc2-be50-0d7846e2983c)(content(Whitespace\" \
         \"))))(Tile((id \
         f7035626-4001-4a1f-9a57-22b7bb842064)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03653636-95aa-4bc7-b260-88bf4854be70)(content(Whitespace\" \
         \"))))(Tile((id \
         8892017a-53b5-4308-a23a-b53b1536bc87)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8cf5557f-636c-42d1-80ba-532cedc4ab41)(content(Whitespace\" \
         \"))))(Tile((id \
         7afa6e3f-6c08-49af-bed5-e069ab0734bb)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1bb1878-3a99-4e2b-9047-2d89f9ff6003)(content(Whitespace\" \
         \"))))(Tile((id \
         27437ea8-045f-4395-9215-51b283d33008)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1b72e08-e570-45c7-a33a-7a48aa98ec5c)(content(Whitespace\" \
         \"))))(Tile((id \
         c3889d66-a93c-4e20-9085-7c66a3681355)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1200222e-64e3-4cf7-8195-d19512d74e7f)(content(Whitespace\" \
         \"))))(Tile((id \
         335b9d6e-612e-4fb2-8b82-5a58d970bdbb)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         790cde39-fcc0-4a42-9c41-8e58f7b90446)(content(Whitespace\" \
         \"))))(Tile((id \
         b18650f5-102b-40f3-a650-16c615c3976b)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6ca4a63-c40d-4c55-821f-f51cc56ee56c)(content(Whitespace\"\\n\"))))(Tile((id \
         20b84b3d-0e5d-42c0-938c-28b7935db93d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         50348e70-c1c3-49dd-958f-616d1ba83699)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ada360f-e481-4864-9f7a-5ff9a6f5126b)(content(Whitespace\" \
         \"))))(Tile((id \
         988d115f-8c03-48f7-b009-e215422a6fea)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         880cf2f6-5dcf-4864-bd72-f9d265c344b1)(content(Whitespace\" \
         \"))))(Tile((id \
         b4dff881-3892-4910-97f5-dd3c077a15ed)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         404499ce-b670-49fd-aa2f-54024777f4db)(content(Whitespace\" \
         \"))))(Tile((id \
         1a0b2685-7f96-45fe-bc43-62b6d7cc195c)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b6a0af9-24a3-40a7-96a3-3fd9d7db84c3)(content(Whitespace\" \
         \"))))(Tile((id \
         308eb9aa-f5c0-406a-b78f-6863d4e50de8)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c0dfe958-1bc4-4125-a885-734cae660288)(content(Whitespace\" \
         \"))))(Tile((id \
         7e895d1b-da72-4f26-a8d4-1918d46cb4fe)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fec4aaf1-37c3-4571-9dfb-33ee944d9c89)(content(Whitespace\" \
         \"))))(Tile((id \
         6e5aaa02-3eb1-4275-8c0b-66df92726b0a)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb3bce81-b450-410a-890e-46ceabee7e0b)(content(Whitespace\" \
         \")))))((Secondary((id \
         93d865f9-1dd5-4a2a-81dc-34ea8fe589a4)(content(Whitespace\" \
         \"))))(Tile((id \
         226624ee-7e81-4364-ac3a-9d4beed444b7)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9d8b405b-ed37-498d-8a91-c7941bd22442)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d9ee4854-d671-48fa-bf67-d05b6668f010)(content(Whitespace\" \
         \"))))(Tile((id 3611eb7f-0b15-4445-ad8b-a13b04d3fe1a)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         d387eec9-959e-4075-9588-4b70490e7daf)(content(Whitespace\" \
         \"))))(Tile((id \
         1f5cd203-8466-4510-9f5f-9c1ea5603bff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5f1391a5-b48b-45e3-a613-8946423127e9)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9485b1dd-14b1-4106-9f7f-c3b0cd244824)(content(Whitespace\" \
         \"))))(Tile((id \
         c2868112-6002-4fff-ad92-1f19682fd275)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         570352be-70ad-45dc-ad6c-4ba2a7b463b9)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed4cc5d-3a7c-49bf-9c66-728f82cfebaa)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         567ba6b9-8ab4-4750-bbce-2244e5decc8e)(content(Whitespace\" \
         \"))))(Tile((id \
         cac1e5e7-332d-4f35-bbee-0839fad6df48)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         654ddfd4-9661-402a-b7d8-1ca733f45f26)(content(Whitespace\" \
         \"))))(Tile((id \
         8b8074e6-c69c-42c1-b6a7-5c1f4b61235b)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         168f9d18-284a-4f1b-a03e-ab2638165f00)(content(Whitespace\" \
         \"))))(Tile((id \
         6a418308-e362-4acc-943a-d52f3cd4a1b2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0be9d6aa-e145-4d7d-b95a-d6dcf0e366e5)(content(Whitespace\" \
         \"))))(Tile((id \
         c67d476d-5c25-443d-be6a-ffa94ac22b5a)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfeb5bcb-d77a-4e0a-92c3-7c8fd9e33ac4)(content(Whitespace\" \
         \"))))(Tile((id \
         b8d23730-50f6-43ac-852f-8386b030b4b8)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b1f5863-89f6-4f51-897c-0b5a0df5b507)(content(Whitespace\"\\n\"))))(Tile((id \
         63b84bbd-ff4a-4c77-be24-ccc22ce0236c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2a6eb337-ed06-4a28-8708-60ed8f27ac79)(label(crop1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         096eaa8e-c3dc-4d5e-bea0-a11df6359cd2)(content(Whitespace\" \
         \"))))(Tile((id \
         cb24eac6-b767-43e7-9f9b-e8fe5a5bbee3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1f93560-d39b-4bbe-bed9-b4bf0cdd49a9)(content(Whitespace\" \
         \"))))(Tile((id \
         bed6f39f-b2f1-49fc-a27c-99d1dfccb1a3)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         01278459-4dc0-455e-a674-155d36fae0ab)(content(Whitespace\" \
         \"))))(Tile((id \
         2dcd052f-e49b-4689-91ca-d6e6afd1a491)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c84ee379-3403-4ab5-b5e1-4e190fd9c9c9)(content(Whitespace\" \
         \"))))(Tile((id \
         82028c8e-707c-40a3-ae8d-5c8a577849e1)(label(crop2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7a9962a3-b363-49f6-91a1-f27ac1c7e0ec)(content(Whitespace\" \
         \"))))(Tile((id \
         288ab1c6-aece-41f5-9154-8d571c6de673)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6001e7a-bae1-4f4a-a147-4291e600794f)(content(Whitespace\" \
         \"))))(Tile((id \
         795bbfe0-a01f-4e25-aa22-9d52111b1b9a)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fe178b03-a505-471d-ab21-e465d05a304f)(content(Whitespace\" \
         \")))))((Secondary((id \
         6e154e39-199e-40f6-a8e6-f433e398bf79)(content(Whitespace\" \
         \"))))(Tile((id \
         5e120ee3-ea15-4b02-85e6-188285166e54)(label(Harmful))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac4ed608-ee97-49a9-aacf-14ee45f975cf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         534ee531-5b3c-4f73-a1ee-f6ffc2b409cf)(content(Whitespace\" \
         \"))))(Tile((id \
         24b0b471-0cb8-423a-88e9-c1d0aaf8ead5)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         60c3b7d6-6092-4fe8-b685-39da40b9379e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d6b52684-39c4-428f-b7ee-5eb33d9c1e77)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e98f985-1737-47c8-9599-c521fb82bfd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e7a5679-5682-4886-a8a8-f8c00f882315)(content(Comment\"# Convert \
         effect to health modifier #\"))))(Secondary((id \
         c630f48c-7516-42d1-a7f1-e2e4ba145aef)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed6306e3-6ebd-4277-bd23-d6a3ea30931a)(content(Comment\"# Beneficial: \
         +10, Harmful: -10, Neutral: 0 #\"))))(Secondary((id \
         6551d02f-37be-48eb-9199-1b4abcd8e6d9)(content(Whitespace\"\\n\"))))(Tile((id \
         a9134438-e8e4-448e-a77a-be91d07978ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83fc8ff4-b3c2-4d9d-9299-2d4f72a3e3fb)(content(Whitespace\" \
         \"))))(Tile((id \
         c53ebdf2-1100-4caa-b5ac-9e494207f54a)(label(effectToModifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         53dfd144-7443-417f-8a7b-891cbb53abe5)(content(Whitespace\" \
         \"))))(Tile((id \
         d73de595-b0a5-4b1c-8716-a87fc793cadd)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ae3159b6-5df7-4311-9960-d08a289a7553)(content(Whitespace\" \
         \"))))(Tile((id \
         02b4c94c-e923-414f-87f7-24ac73215d73)(label(Effect))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4888622c-be49-4e6c-8bb2-e9b9c3a1e165)(content(Whitespace\" \
         \"))))(Tile((id \
         b4af1081-79d9-45cb-b139-b82c380def8c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9dff1142-c77f-4f5b-85fb-c3b93757316a)(content(Whitespace\" \
         \"))))(Tile((id \
         75126d6a-f3e5-48c1-896e-db013fc8bc15)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2fba8fb7-3a9b-4b59-8988-59369017b603)(content(Whitespace\" \
         \")))))((Secondary((id \
         3b691654-4fbe-429a-b54d-b59ae318ba16)(content(Whitespace\"\\n\"))))(Tile((id \
         7f53c71c-2aed-4baa-a257-18be541cc03b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         de236f59-cda4-494b-9ffb-1e05e2010598)(content(Whitespace\" \
         \"))))(Tile((id \
         903fae14-ba37-4b0a-aa6b-29d5e98b8a86)(label(effect))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7a2bfcee-b309-4478-9e24-4e4795c7b880)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7f52c5f-8209-4f6a-9265-74c3ee3b33e4)(content(Whitespace\"\\n\"))))(Tile((id \
         9d82ca58-3fde-4b33-bdc6-ff7fd73ece50)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fa7924f9-ebc8-49a0-93d9-efaefc4858e3)(content(Whitespace\" \
         \"))))(Tile((id \
         ef249bc6-c0e2-4754-9520-6ec1025d93b5)(label(effect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5dac751d-1542-466e-9a0b-1ced6ad50c59)(content(Whitespace\"\\n\"))))(Tile((id \
         f4aa9560-4330-46d8-901c-a76395790d07)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         79c9dc07-8c7d-4286-a013-62b35dc8125d)(content(Whitespace\" \
         \"))))(Tile((id \
         73afebe5-580f-4a7e-972e-bf49b2240df1)(label(Beneficial))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         79295f43-b41f-4f85-b3f4-7ef2a9de3888)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34ddc06b-25da-46cd-a5ec-f2c8079500c2)(content(Whitespace\" \
         \"))))(Tile((id \
         e1c65fb0-511a-40fc-a1c6-1b2994c66705)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2b5a208-9230-4835-9f36-e25cd69266bf)(content(Whitespace\"\\n\"))))(Tile((id \
         f1df185b-14c3-460c-a68e-6c16c8e71a53)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c334a6f3-71f6-45f8-82a5-e8e6d7feedca)(content(Whitespace\" \
         \"))))(Tile((id \
         7b8b4bd4-3253-4d22-a9bb-1560c8ff21eb)(label(Harmful))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6e0153b1-fed0-4130-a9af-4fb8999c4988)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         85b011a9-2082-48ac-aabc-dfb58be2a845)(content(Whitespace\" \
         \"))))(Tile((id \
         8a953db6-9671-46e7-85dc-fc017f2600bc)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f4cdab2-ecbf-4d25-85c0-fcc29731ad6e)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b59f2b5f-5311-4fa4-b890-c98fa5e7a31c)(content(Whitespace\"\\n\"))))(Tile((id \
         5c36e862-6b1b-4de6-aec7-a3c3ee07027f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8147d839-7138-41d1-b755-c44602422212)(content(Whitespace\" \
         \"))))(Tile((id \
         19827a9a-d8fc-4c90-b422-74c5e2da77a8)(label(Neutral))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cca6abf3-f256-472f-afc5-4834c20037be)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be8fd905-183a-45e1-bf11-1ffb0ee5a454)(content(Whitespace\" \
         \"))))(Tile((id \
         c8b413a3-65e3-4f6a-88de-8bea96354acf)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb14a790-253d-4bdf-80d9-18434702abe3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf5cd20c-f7d1-498c-83d5-5f37617abfe6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         465b7d3e-ac66-46e0-bd60-d9fe6eae4d8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8740284f-ae01-4e2a-b891-d4a4513f28c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd8a8c7a-5d79-4e83-a018-aa5685214128)(content(Comment\"# Get cell at \
         position, or empty if out of bounds #\"))))(Secondary((id \
         e3cfc344-3c23-451e-ad14-a155d0505298)(content(Whitespace\"\\n\"))))(Tile((id \
         3577c2e4-95b7-477a-a6a9-8bb872b28b09)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6907b108-bb8c-4bf4-8b5d-7ac035f40b47)(content(Whitespace\" \
         \"))))(Tile((id \
         55b41429-5ac5-4cf6-be97-f343a79e38df)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1ba77763-3fd5-4df0-8ebb-a253655e5051)(content(Whitespace\" \
         \"))))(Tile((id \
         e9f210fb-0672-4624-abd8-c137b38fbb40)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         03c06781-71b2-4efb-977d-ce7f30f69575)(content(Whitespace\" \
         \"))))(Tile((id \
         80a41687-b94f-474a-af12-d53804c11320)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f9bf10e0-0d22-4b67-ba44-21c27a9b8eda)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         718f6899-a021-4e38-8a14-28bdb602c0a6)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41c8ffb1-3019-40ed-8d92-c472de9e767e)(content(Whitespace\" \
         \"))))(Tile((id \
         0e3af7b8-e27e-4c8f-9f8c-bfa642401fb4)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         00f2ec8b-3c80-4105-9465-82f581c8dd4f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9f0fbc3a-21c7-4e43-afa7-12e397b0abc3)(content(Whitespace\" \
         \"))))(Tile((id \
         c875f188-956d-41d4-b05f-b47714aaa279)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         fc8af208-fb41-4e2d-8dca-89b72261357c)(content(Whitespace\" \
         \"))))(Tile((id \
         fe67ea3f-8e19-4381-b981-c4e626de6d47)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1d765903-a138-4fbe-b1e1-190b4c8adfef)(content(Whitespace\" \
         \"))))(Tile((id \
         b781fc07-80ed-428f-9d68-f1bd1b8025fd)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         467f19a4-d060-4109-9c75-c71f5ae2b70f)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d9aa923-a349-41d2-a14a-5f9ace68a520)(content(Whitespace\"\\n\"))))(Tile((id \
         c687a370-d0ac-4aa6-b2bc-c6762d01dadf)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f6744aa8-56de-40bb-a9ea-059e39186ac7)(content(Whitespace\" \
         \"))))(Tile((id \
         a1118383-3f6e-489b-ae81-2e013562b4fb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         97ea9bb4-672e-4462-bd7d-26fb1be6f8bf)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f4f39d57-1a67-4118-b69b-df978b85f74b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6e269dc5-75f7-4a12-a4aa-47b58453ecf0)(content(Whitespace\" \
         \"))))(Tile((id \
         3f3b5297-0638-488f-8c49-6775cfe17c84)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2722e00b-fcd9-4e93-bcaa-aa71d6e28571)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         137bfd31-4df7-4b42-a7b7-b652f47c6766)(content(Whitespace\" \
         \"))))(Tile((id \
         458debb0-6fd1-4bab-9b16-6a2674916717)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6d8e1408-a0df-4647-881b-a4d6b883c559)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         164c3e9e-dc59-4f0d-ba49-4e14968ffe9a)(content(Whitespace\"\\n\"))))(Tile((id \
         5ba7b7d2-1687-41e4-a481-8f87bce46338)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b72b67b1-da61-4f27-8f91-b96a2cfde712)(content(Whitespace\" \
         \"))))(Tile((id \
         bdf25b61-4cbc-40c7-bc0f-f838da850536)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e1afa73-fb9e-4717-a4f2-9a4ceff142bf)(content(Whitespace\" \
         \"))))(Tile((id \
         10756e6b-a99d-4800-980c-3b73c632af33)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e9cc611-4d7c-4d63-94bd-7205ed92d9fe)(content(Whitespace\" \
         \"))))(Tile((id \
         061d3865-552a-4eb2-9351-ff53103351eb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b859c88f-b154-4f2d-87e5-fd11968281dd)(content(Whitespace\" \
         \"))))(Tile((id \
         f87abbcd-35f7-435b-8e45-26fed0fff45e)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9cfb3e09-ff3b-45b5-8130-704fe73272e0)(content(Whitespace\" \
         \"))))(Tile((id \
         31390c04-bbee-4260-a385-243548d66c3c)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c7eb6485-5ca1-4821-b321-822df60d1e62)(content(Whitespace\" \
         \"))))(Tile((id \
         75a1402d-62bd-4578-8482-ec179ec54bbb)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5019d4cd-0e8c-4c91-be3e-40b710966b24)(content(Whitespace\" \
         \"))))(Tile((id \
         ed1e1cc8-60b1-4f0f-a168-bb0f52b283e1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         67db4088-9002-4c85-a302-6c3edb6b594e)(content(Whitespace\" \
         \")))))((Secondary((id \
         88983999-7462-4a7d-b68a-752219b74cc3)(content(Whitespace\" \
         \"))))(Tile((id \
         91adbeba-7e35-46b4-9cad-bde2fa9a6cc2)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         614580c9-f89b-4387-853a-d0aca5bbb433)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d3385564-656b-4ddf-8cec-16b18ce55519)(content(Whitespace\" \
         \"))))(Tile((id 3a1e616e-2f3a-452b-b650-38d9f8a7570e)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         748ae6ec-7805-4f86-bc65-45b2508bc396)(content(Whitespace\" \
         \"))))(Tile((id \
         a1fed35f-5d23-49ad-9390-0f04fe9a1f9e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         11cc078e-4764-4e1f-97eb-4e48f30ae03d)(content(Whitespace\" \
         \"))))(Tile((id \
         c657f554-0ca5-4f59-839b-9863bf513c26)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a42fb7eb-a9c7-48c5-a9df-0dfcae8a59b5)(content(Whitespace\" \
         \"))))(Tile((id \
         8c1432d2-0d89-4609-8dd4-aa528fdf45fe)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb6386c4-d8a0-47e0-9b3d-eb2c70cf306a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b59be7ec-e2ba-43fe-8d0b-c6fc480d5172)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b92df9f-f8c8-42c3-ae42-8841687fb215)(content(Whitespace\" \
         \")))))((Secondary((id \
         893ca83c-da5b-404e-9277-0f3a4541b800)(content(Whitespace\" \
         \"))))(Tile((id \
         e51b6938-34f2-463e-ab26-f5be9fef336e)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6240d6fc-6cbe-4f8b-84f7-21828276369e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4d209c3f-4eef-486f-840f-6bae384e5fab)(content(Whitespace\"\\n\"))))(Tile((id \
         2a2f723c-2f6d-4d66-a614-c118ff4ca364)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb56a82a-f149-4c6c-a195-ea210094215e)(content(Whitespace\" \
         \"))))(Tile((id \
         1d11e4c7-bdb8-4d05-8fd0-10aaab3bb855)(label(rowData))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a27bb59a-ff8b-40cb-83d2-32f18f44a830)(content(Whitespace\" \
         \")))))((Secondary((id \
         58c710d8-9aae-45a5-8931-9d6cb076f5e2)(content(Whitespace\" \
         \"))))(Tile((id \
         0f05ac73-708d-47ec-ad7e-f2496a6cb4a6)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b5d833b-5af4-44c8-8cb1-601e54940683)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a7b12d22-18d5-4132-8ff6-08de59bef7b4)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b766e9fe-9d8e-42fd-8b69-abbcac301284)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d3ec08c-77f8-403c-a963-576046d90c57)(content(Whitespace\" \
         \"))))(Tile((id \
         58e00bbc-8ed8-4531-9c8c-45cdc44c2ffc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         75f5d60c-3b62-41de-b5bf-cd8c22d63079)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a75c5b92-3969-4e00-92e5-f3b3d1e075b0)(content(Whitespace\"\\n\"))))(Tile((id \
         56854c4b-8f25-4ef4-b703-64130eb78aa9)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6b96ec7f-894b-4ef5-9da0-5b0fa161d6e5)(content(Whitespace\" \
         \"))))(Tile((id \
         dc5c55c4-b893-4b5d-b7ff-d3318abed863)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b65560f-11d0-4917-93cc-89d16cae89fa)(content(Whitespace\" \
         \"))))(Tile((id \
         ef4611b5-1cce-46ba-aa0d-65e23a489033)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c30f496-1f6d-4d40-a09c-ba936e8ca591)(content(Whitespace\" \
         \"))))(Tile((id \
         9728f633-4eab-4310-9fc8-a4480fdf4e8b)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0841e045-ae76-4718-bf36-ac4835d7ed37)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         645196b3-6482-4518-a66c-01b0e727af30)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ce90feff-9ca2-4a0b-8010-69c4167788df)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8a802e3-613a-4e13-b152-37db1077762e)(content(Whitespace\" \
         \"))))(Tile((id \
         92563a94-fecb-4baa-9cea-4a484ff2adbb)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdf50e2e-e52e-41d8-90cd-37ac036804ec)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         254d912e-933c-43ad-81c8-c9a1cda8e3eb)(content(Whitespace\" \
         \"))))(Tile((id \
         e38ae6d1-8dbc-4798-bd70-4c7bc96ce0fb)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8216def-49ab-4c2b-bf77-a869e496cd40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         50e23aa7-115d-42ff-9276-a91ea995af06)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a247d78a-554a-40fb-9c42-8b308fe57bc7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d356e0d-9ecd-401c-aa3a-54446ed20f86)(content(Whitespace\" \
         \"))))(Tile((id \
         4d170da6-1394-46f9-8ef7-6785c9c9e516)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         958dcc14-16b5-4b75-83ee-5938dc2e8dcc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c83eb448-0b2f-4402-b7a7-46a9f6781ef8)(content(Whitespace\"\\n\"))))(Secondary((id \
         55d9102b-a3f6-41bf-92ca-6e8e2e4b4a04)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1cc98be-e616-4b26-9ef8-9ab021ce2346)(content(Comment\"# Set cell at \
         position #\"))))(Secondary((id \
         1b7d14a6-f4bc-4710-bfec-f71f01de3216)(content(Whitespace\"\\n\"))))(Tile((id \
         f90b2b4e-976c-4efe-816d-7e3fdd99951f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         997fdb9a-4ad2-4962-831b-012eba9b9871)(content(Whitespace\" \
         \"))))(Tile((id \
         188a1707-e505-48e9-8cff-e5d3845ade32)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc1e03d1-8567-4696-828d-588a0f00be7f)(content(Whitespace\" \
         \"))))(Tile((id \
         06af91d2-0d87-439b-9c26-00978deefbd1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         83be23c8-8e7b-45fd-80e2-f6e634276d35)(content(Whitespace\" \
         \"))))(Tile((id \
         36b00777-f280-4d3a-b6e3-03529c8d0a46)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         125bdfe4-bb08-4c8f-a8d7-8b43976fb28b)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         176d73c9-92d5-40a3-833c-96b32545d2c8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8421c3ab-41bb-4330-9e94-450400276cdf)(content(Whitespace\" \
         \"))))(Tile((id \
         8a594a69-37e2-416e-bc20-edb13e5c0d55)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cb746d45-7e1d-4441-8160-6f918adda0d9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d38bbb60-e4d6-4f43-8220-a24b18835558)(content(Whitespace\" \
         \"))))(Tile((id \
         a3fd93f7-2c7f-4e23-919a-bb7cabd46b52)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9a023db4-f55f-4d3e-95ec-56fffbc41341)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7db985cb-9590-448c-8bfa-4ee2281661ee)(content(Whitespace\" \
         \"))))(Tile((id \
         cd9e341b-4ad2-416a-a94b-d21530223e29)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         746deb15-cd44-4f2a-821f-20b72944b643)(content(Whitespace\" \
         \"))))(Tile((id \
         86d02fc1-d381-42fb-b5bf-8a36dbbf2fe4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cc441069-fe6b-4996-a92f-a2d3405da46f)(content(Whitespace\" \
         \"))))(Tile((id \
         cdab8e57-aff2-4116-a90c-45d643bac363)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         45cc7949-18a3-4044-b002-5065a7f959b2)(content(Whitespace\" \
         \")))))((Secondary((id \
         f7a78388-f930-456c-9f83-ba509d8327f8)(content(Whitespace\"\\n\"))))(Tile((id \
         10bd8f91-cac5-493e-811f-66625e4e3a6c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a6ae8f68-ffd4-4766-b92b-73fd2f80f353)(content(Whitespace\" \
         \"))))(Tile((id \
         2ac0e2ad-24e3-441e-b062-0990f531bacc)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         88ca15ec-19fa-4c48-94c3-b54bbf280ddd)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         545b28e0-9a64-4b9e-896e-b905ebd1938e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         475ced24-6254-4868-b0d4-89987db671be)(content(Whitespace\" \
         \"))))(Tile((id \
         6499465d-c3a8-4c55-8385-a7e1c9076bd3)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e4448e63-8564-41f3-8e07-bffa5c3c299a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6794e2d8-6ca8-42a0-b36a-19a3b69ddca7)(content(Whitespace\" \
         \"))))(Tile((id \
         a717f322-d882-4a36-bed0-7b9f4b93c22a)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1bbc3cb0-7494-409c-94a3-a69a09bd1e4c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1d014c27-1270-420d-86fd-d98ef370e94b)(content(Whitespace\" \
         \"))))(Tile((id \
         3530245a-204a-493e-abc9-b53c283a6092)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a0b62be5-ffd0-4f09-a585-36346d203790)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b0534ec7-fb29-4eb1-9d07-f0a6d08a7db5)(content(Whitespace\"\\n\"))))(Tile((id \
         905fad9b-7547-447e-b1b3-55293d80c494)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b5e79ff3-4f5a-4fe6-a872-7f08f4ed9b88)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3a3f6c2-d91e-445c-9222-6c2d8a7e3aa9)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69f0885b-9758-4453-acb6-d4efb8c704a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36fec6e6-8e9e-4731-bf90-b53790d5e194)(content(Whitespace\" \
         \"))))(Tile((id 31b0ea1b-679a-4354-bd9a-f6569e52dbd1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5d2c10c1-5ee5-48ca-9728-2ffeb5b80fc6)(content(Whitespace\" \
         \"))))(Tile((id \
         36ad2069-23d6-44ac-b559-c44062ad057a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         519e08c6-b5a2-41a7-abb9-e8cddb02f0b5)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b54ff156-9a02-4c14-b12a-162ae90fb49d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6ab8f265-1849-49f2-945e-1bfde157e995)(content(Whitespace\" \
         \"))))(Tile((id \
         46625e8c-5ee9-4bea-98e4-aad7666786b9)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         940aec62-b2d3-442c-93b1-299cc0523894)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e9271e8-fb0c-4b42-a30e-ed91c42e5d38)(content(Whitespace\"\\n\"))))(Tile((id \
         55de1490-54cb-408f-927c-d08d9d036e25)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         485fcea2-be89-4086-8fb6-649540bfa10f)(content(Whitespace\" \
         \"))))(Tile((id \
         9ba082d7-6fa0-4f37-bbfe-c074190cb2ee)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33f2e8aa-7c5c-4a98-9c5a-f2b0a70d38e5)(content(Whitespace\" \
         \"))))(Tile((id \
         0c341088-811b-48aa-8944-7d85fbaca8d3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f49f0fc-e584-462d-b75b-6ccc31f8fb20)(content(Whitespace\" \
         \"))))(Tile((id \
         6e74e1fc-e457-42e4-9b41-255d9f08f435)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fbd7c192-fb6d-4317-b0d8-c99af7018f2a)(content(Whitespace\"\\n\")))))((Secondary((id \
         b72436cb-4528-47f4-868b-b577e12dc187)(content(Whitespace\" \
         \"))))(Tile((id \
         8bf7422e-2e39-4284-9c0c-33a2c7f3cf77)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebeea2df-602b-41f9-98c2-53874d43407b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bab4f982-86fc-47f4-b4c3-5898299179c1)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         996c04eb-c4a5-4df9-bb84-0cae0c9c3f13)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd3b6601-c8da-46f0-913a-02b3805d6693)(content(Whitespace\" \
         \"))))(Tile((id 6d92d6a0-341a-41d7-8f30-dfad2f2cb6a8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ca6374d5-643d-4535-8e54-4283b16fa726)(content(Whitespace\" \
         \"))))(Tile((id \
         dfebe1c7-95b1-422c-b986-de9d1f35ad5c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d726cfcb-6334-46e2-80d2-78ad0afdfb46)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a2923a55-17f4-4583-a570-faa541b0691d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a65c1c16-a06c-4366-85ac-a106b31647af)(content(Whitespace\" \
         \"))))(Tile((id \
         26ce86ff-7c34-412d-8c95-ed4624d58da7)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0d5feef4-37cf-4341-a563-2a4b11bf2552)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         87f4aee2-6e70-43e2-9695-68a3c6e24d44)(content(Whitespace\"\\n\"))))(Tile((id \
         08bb1034-17ab-4435-b88e-4d5047bd38f9)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         10ebc45b-5fb2-4aea-9422-a31f4179b0df)(content(Whitespace\" \
         \"))))(Tile((id \
         718862d5-6a01-4e46-90b9-ad8eb77c92b5)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6361b8a5-36fd-45e1-bc6b-c99a2007d91c)(content(Whitespace\" \
         \"))))(Tile((id \
         d604b30a-05d1-4a40-97ad-695349d2b772)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdcba80b-98ad-4c51-90ac-949c5e2c6a4a)(content(Whitespace\" \
         \"))))(Tile((id \
         99aeb8b9-0773-4b96-b8d3-aecc3c3da39b)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d1f08dfa-be6e-4272-b738-a9f2af3f86ba)(content(Whitespace\" \
         \")))))((Secondary((id \
         5eadf327-1532-4ae2-9189-fa31c06e7b82)(content(Whitespace\" \
         \"))))(Tile((id \
         1cc3a6e1-40ce-4cae-9f69-be4a4eeca222)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4635a555-18f4-49ad-8885-a70b27938234)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ba69556d-4551-4659-94bb-08202be83616)(content(Whitespace\" \
         \"))))(Tile((id \
         64a971cd-97a2-4048-9a63-62771e3a3e09)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cd2b03d9-1e91-4d43-a3cf-edc0101ff154)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7ca2f466-11c5-4ef7-a2f2-5ee5b5e9ddf0)(content(Whitespace\" \
         \"))))(Tile((id \
         96f3cd65-48ea-4d48-a7de-911905df625d)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c1341347-e2b9-44c8-b87d-45058ec59925)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7078bb61-5718-4ea7-9988-eb499c070473)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2e246c9-3f5d-4de6-b060-af48aa27c30c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e183dbb-51c1-4dd2-aa8c-07a127a1ae9e)(content(Comment\"# Get all \
         orthogonal neighbors of a position #\"))))(Secondary((id \
         e697d5ac-e836-46be-bfff-af683362889d)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd80a72c-4f6a-4fbd-b9e8-2a013f1c4bd0)(content(Comment\"# Returns \
         cells above, below, left, and right #\"))))(Secondary((id \
         e280f4c2-7d30-49ac-af03-9b3a2e6275c9)(content(Whitespace\"\\n\"))))(Tile((id \
         78f4e1b7-825b-41de-b106-cc76fac924b0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b5729c6a-6b03-4fd9-a172-74d1e5c5aaea)(content(Whitespace\" \
         \"))))(Tile((id \
         23493c4e-4bbb-4ce7-8d37-f4b6a32ad5f2)(label(getNeighborCells))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7ee7c90e-b52b-4d8f-bfe3-1e0e25c71bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         956292ba-1129-4a73-a081-2663fd7638bb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7def4a92-0c5a-4a1c-a232-f6e7c065c7c8)(content(Whitespace\" \
         \"))))(Tile((id \
         24f93213-29de-45b3-87c5-d284c8952a81)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0ba9e600-8976-494c-b89f-10ca9fbfd936)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a36e7642-a823-4dac-9954-0dc4370fd79e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1877ce1a-7ad9-4f9c-a304-73beac2b8647)(content(Whitespace\" \
         \"))))(Tile((id \
         ecc611a3-15e6-4ebb-8af7-96169509257c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9f0d6ed6-5c3e-4bcd-b53b-db2aa074d0d4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         11427c15-63a6-42b8-81eb-46188e188646)(content(Whitespace\" \
         \"))))(Tile((id \
         4f3d0c76-6177-4b4b-90d6-d0e56da8a22f)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ba761da3-2e1c-4c2f-ab04-ca6bef74857e)(content(Whitespace\" \
         \"))))(Tile((id \
         652fc574-8a15-4c82-91f6-492c8214a9bc)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8a6a9d1b-68a3-4596-b60b-67a1465a7165)(content(Whitespace\" \
         \"))))(Tile((id 6425d8fd-dd3f-4b2c-b3b8-5574efd2366b)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a893e470-a1b5-449e-b4c3-16415f05c83b)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         981ca2a4-a1dd-4a3a-81a3-388e456b53bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         7137a72c-6f62-4bfc-b006-726b5e1a8e72)(content(Whitespace\"\\n\"))))(Tile((id \
         d1fb098a-2817-4c58-871b-f77878020328)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8ff36faf-e06e-4e10-9858-f5c5b007040d)(content(Whitespace\" \
         \"))))(Tile((id \
         06a71fb4-6690-4ffe-9004-740d48039005)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         58974e86-298c-4e05-805f-7da0aa5e2d66)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2cd39f21-418f-4b06-bdbf-b614520254a5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c9b1bce5-d915-4ba9-a7db-1611bf3d1353)(content(Whitespace\" \
         \"))))(Tile((id \
         2fd26a6f-3c34-4c8a-a772-babaa89a4606)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bb5b3210-9944-4500-b111-ecc71433fd57)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1c19b8f9-0aaa-4611-bb20-a0ebd4f951c0)(content(Whitespace\" \
         \"))))(Tile((id \
         651b44eb-1026-4664-bc4d-cebd5e3697fb)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9c2b808b-3102-4028-b958-f581fa4c8509)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e8c6188-aac0-4f24-904d-2f65ca49142b)(content(Whitespace\"\\n\"))))(Tile((id \
         2fd24f6c-f9d6-423e-b758-30f4a93c6a5d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         69f402ef-e28a-4e9d-bcc6-e6ee1b35954e)(content(Whitespace\"\\n\"))))(Tile((id \
         4b4b1d17-5794-4555-a058-08bbfe07aec2)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcfd4bf4-7aae-4735-a44b-0431d05ce995)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af5f3775-6f16-4c16-b921-abdda004b9aa)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea708e50-5d4e-49bb-a3ca-914bcf62442a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         daa4356a-defa-4e16-bb38-2ea8bc751474)(content(Whitespace\" \
         \"))))(Tile((id \
         1fc30cc4-07ea-4162-a239-4a1809bb3c62)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         13cbf0b3-ab4f-4c3a-a1c2-190b66ebbfd9)(content(Whitespace\" \
         \"))))(Tile((id \
         983c03e9-3a7d-4f81-afe3-d56a168357e9)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f842db0-b980-4b42-ba90-0a2febb61eb7)(content(Whitespace\" \
         \"))))(Tile((id \
         873c6d38-676f-4e84-bec4-7bbe077badd9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85e03515-3781-4930-a5dc-20fb1c69be05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75539867-bc1b-4605-af05-17eb9991ba4d)(content(Whitespace\" \
         \"))))(Tile((id \
         097ca841-870c-4344-8113-416b5fe4e90f)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         50008471-000a-4b8a-8c6e-677ebfe564b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96dc70fb-e0d4-4c94-b1d4-06247f03e1af)(content(Whitespace\" \
         \"))))(Secondary((id \
         10e3024c-7160-4fa2-9b4e-8598d9d4deb5)(content(Whitespace\" \
         \"))))(Secondary((id \
         15b0bcf0-9424-41e3-9005-c49c856a06ed)(content(Comment\"# Above \
         #\"))))(Secondary((id \
         b17c19e5-060a-4af2-a90a-30d4c659c347)(content(Whitespace\"\\n\"))))(Tile((id \
         a5d8c8ca-716b-479c-9120-2704a866c344)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67cbf6a9-43f8-41b2-beac-b5dbf7a8c285)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3686776e-bd78-43c2-82b3-1c117289fbc7)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1cf5df8-4ff1-4327-83ba-a4d35ce9c224)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11ce7426-ffc6-451c-ae45-2a5eaba5a9cb)(content(Whitespace\" \
         \"))))(Tile((id \
         cf0f4c43-79db-4058-824c-67d82c13eb8e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b01de5c-3de4-4ba3-b132-cd00eefeab54)(content(Whitespace\" \
         \"))))(Tile((id \
         4e519cd4-c56b-4aa3-b794-e00688e1366d)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0196c65-f415-408c-9068-e8a0f667b857)(content(Whitespace\" \
         \"))))(Tile((id \
         8b049265-93b5-4ac3-9bc8-21eee5380d56)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a88a2f67-cfce-4bb5-aefe-5e6d5f9829f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9db807ca-f21e-4edf-82ff-087d7d762444)(content(Whitespace\" \
         \"))))(Tile((id \
         2f320049-78ba-495f-88d9-70d6c4078d01)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d41a3181-f688-4a29-977b-abc8d699b18a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff2aac57-05bf-4f87-bf00-223f4f97bb0b)(content(Whitespace\" \
         \"))))(Secondary((id \
         57fa6d63-75a8-44a4-9428-63c2b04dfad7)(content(Whitespace\" \
         \"))))(Secondary((id \
         b5f9ea1a-dc36-458b-a509-722ab4cef0e6)(content(Comment\"# Below \
         #\"))))(Secondary((id \
         f7bb58e4-7d91-4c39-8636-81ffd75597e5)(content(Whitespace\"\\n\"))))(Tile((id \
         7291846c-0261-4255-811b-c8b2bb0977eb)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb26aac4-4768-4370-a177-2b16f481eec5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d89ef575-9c99-4a61-b9bc-d9d5e2059117)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8babd5d-3ec1-40c5-87fc-b8d38ae48f0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e77b69dd-bc17-430e-b4fd-ed84f38ab22e)(content(Whitespace\" \
         \"))))(Tile((id \
         0c9b49f0-983b-4cd8-8b57-50ec0c9212f7)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e091f6d-553f-4bfd-8379-0326311d3c04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbfaf308-1df8-4e5f-b6e2-1796b0fafb73)(content(Whitespace\" \
         \"))))(Tile((id \
         7fbcb1a1-e4fc-4c62-b04d-4081b1313db0)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1697ee2-4b72-4413-b53d-c8f302deba80)(content(Whitespace\" \
         \"))))(Tile((id \
         96206065-af13-4657-9f7c-03e2a681768f)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0b41a3e-7baf-4ec1-a892-f9fc6eaeeb2c)(content(Whitespace\" \
         \"))))(Tile((id \
         bc004f0f-5805-41eb-be38-9ea7f1eb1f7d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a6ab3cc2-6eae-44c5-a345-de66a480f758)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bf42fd3-d757-4335-8f34-9eabfc1e7402)(content(Whitespace\" \
         \"))))(Secondary((id \
         528d9cbc-97bc-468c-a56a-6b2641ed8bed)(content(Whitespace\" \
         \"))))(Secondary((id \
         662a0430-ac88-404b-b157-2812763c0c8d)(content(Comment\"# Left \
         #\"))))(Secondary((id \
         5a7d247b-0a88-4b2a-b00f-d2488919e204)(content(Whitespace\"\\n\"))))(Tile((id \
         b789b957-31bd-40e2-9e9b-55632687531b)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2bad5cd9-7a29-4853-bb7b-2902a006f009)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34d41186-46f7-480e-8b19-63597375e0ab)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e3403c4-3d2a-4707-bb79-5f61ee1b99bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         387a470c-153c-47ac-9e78-7a7b6ead5e9a)(content(Whitespace\" \
         \"))))(Tile((id \
         f4b7761e-38c3-47ff-b2a5-6d68bb9269d4)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         322d10de-4f2e-4216-8849-1cc79a9ae845)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef533038-65b4-4333-8f52-5720281309c0)(content(Whitespace\" \
         \"))))(Tile((id \
         fa542e38-d0ad-44f0-87ce-623a00886fcc)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb8d9b55-da3e-498d-8c63-08c7e0d9c9ef)(content(Whitespace\" \
         \"))))(Tile((id \
         c6127009-8790-4ef0-8016-00eb51bffe8e)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b303288-b172-4f1f-9dcf-575345d8ec8d)(content(Whitespace\" \
         \"))))(Tile((id \
         fc370263-48c7-42cd-92d6-84e015f3823b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         218c12ce-b03b-4b66-af09-4223e674d3b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         d476fb18-ef8a-4aee-80eb-8e7d71f25452)(content(Whitespace\" \
         \"))))(Secondary((id \
         fb77f2af-99a6-475b-bfbf-4a7facc72555)(content(Whitespace\" \
         \"))))(Secondary((id \
         61d33760-783c-4ad9-b483-c2d6c5ff91c4)(content(Comment\"# Right \
         #\"))))(Secondary((id \
         4c42fd26-3ba2-43db-9b37-b0c6b651e2ee)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5c6d764b-9215-447a-bc90-f9a82bfec07c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         31439a23-b9ba-4eb3-b22c-56c215ace778)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d8428e5-feb9-4c29-9a41-df638b4f6304)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7e88a5d-040c-437c-b456-a243ec792405)(content(Comment\"# Calculate \
         total health modifier from all neighbors #\"))))(Secondary((id \
         17b618a3-0148-46ca-9958-ac5f6ce63a34)(content(Whitespace\"\\n\"))))(Secondary((id \
         e022dcfd-5dcc-4af8-aef5-56b29eed733a)(content(Comment\"# This \
         function computes the companion effect for each neighbor \
         #\"))))(Secondary((id \
         206c7b58-d8c8-40eb-b410-b94318ab013e)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ce9b845-01ee-472f-b4e4-72ffd70193ed)(content(Comment\"# and sums up \
         the modifiers #\"))))(Secondary((id \
         3c5a5576-f09c-4c60-a6be-6ac86d07bb1b)(content(Whitespace\"\\n\"))))(Tile((id \
         7d267697-50be-4a08-a3ec-5ff04f28386e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9b49732d-1d37-47d0-afac-14e365a4588a)(content(Whitespace\" \
         \"))))(Tile((id \
         d64d50d0-acd8-4e42-ac6c-f035a92c0747)(label(neighborModifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2fbcd7dc-6b58-4b28-90e3-9c4ac46a073b)(content(Whitespace\" \
         \"))))(Tile((id \
         9a2455d6-6b5d-4370-a177-7ad220fee806)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15bed45c-a82f-4919-bbd2-3e61dd5a9918)(content(Whitespace\" \
         \"))))(Tile((id \
         c3bede93-66ef-48be-ab40-db5282e53d75)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         849e08a2-1293-4878-b7e0-3c672b8595f8)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5348119b-49a1-40ee-bd95-56f9908d9d72)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3c8e11e9-f4f7-4439-901b-b864f51658fc)(content(Whitespace\" \
         \"))))(Tile((id \
         7a76166d-72e0-494f-8f8a-22ec2fd6e5d9)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2dd68f14-91ec-4eab-82c6-156c026b6baa)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         66d269c8-47c1-488a-b6a2-4ba3ad0ab874)(content(Whitespace\" \
         \"))))(Tile((id \
         17a46b21-1391-408b-a934-a0bcd52674c7)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0967fcde-277e-4a00-a012-89e577733576)(content(Whitespace\" \
         \"))))(Tile((id \
         5755992c-0c27-4423-9296-07cb35eeb83d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c5630324-c05a-4518-bcc0-30d1e2f9ce9e)(content(Whitespace\" \
         \"))))(Tile((id \
         16caa9c5-fd4b-4351-93bc-e7629eb6bd29)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0a7924d2-a6fb-4e25-9da1-162391410ebf)(content(Whitespace\" \
         \")))))((Secondary((id \
         d47d144d-c61b-4830-8668-75d4117e5305)(content(Whitespace\"\\n\"))))(Tile((id \
         d511dc2d-9782-41a3-a34e-0b687840f94b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3da1a8ff-8716-491b-9016-824617cf1b46)(content(Whitespace\" \
         \"))))(Tile((id \
         8bd222d5-a298-486a-9618-b55c53600037)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7f19feb9-8d8c-4636-9caa-1e4e6b902bcb)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e9374e71-fd62-42b7-9d46-f41d8ba54a39)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         450badaf-29ad-4c2a-b61a-e70960247daf)(content(Whitespace\" \
         \"))))(Tile((id \
         fa4eb477-9cb9-4232-9432-bed8c3fe11ea)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         084a2647-340e-4c62-8ba7-8f3d805b5905)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         45aaea8f-bf24-4450-b3d1-913142860789)(content(Whitespace\" \
         \"))))(Tile((id \
         cb61f70d-00b6-447d-9baa-6ef67295749d)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6a9c1cd2-8462-4323-9c4e-8379386fe340)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c482987-f863-42ec-91a6-bcf3675f8939)(content(Whitespace\"\\n\"))))(Tile((id \
         8d0066d3-c076-469e-bcd5-2b16a7814008)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1794020f-126f-4958-8025-448e5b3ac65e)(content(Whitespace\" \
         \"))))(Tile((id \
         d3ba5d09-e69a-401e-b607-4814ffc4048b)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         86d41e19-8161-455f-a31c-97e4113e125f)(content(Whitespace\" \
         \")))))((Secondary((id \
         3ed7ac8d-5454-4591-9dc8-48146fedb12d)(content(Whitespace\" \
         \"))))(Tile((id \
         c7b99bb2-ab6a-48fc-8cb7-60e21b816888)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec4eb642-73a3-4b92-a9e5-fef090a5f044)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9358d9e-43c4-4ecc-8693-a33cbcba52b3)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5198b123-2787-482c-9a63-43d6a8f28826)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fc6e758-27da-44c4-9dc2-d833148011b2)(content(Whitespace\" \
         \"))))(Tile((id \
         177128f4-096d-447e-81a0-173358953a97)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86722bf1-dab8-4669-949c-12ecc870ee86)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12cdfa50-9f7c-41d0-9c2f-65c1b79742cf)(content(Whitespace\" \
         \"))))(Tile((id \
         fd2ca169-7a7a-45c5-a1ab-6cdf1fe1a523)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0c2ac98c-859f-4cd8-92c5-23233b44e8d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c8cf52d4-d5ad-4c98-935d-5bc7730cee13)(content(Whitespace\"\\n\"))))(Tile((id \
         8ea867f4-9e7d-4303-84f9-f67937683f28)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f25e7c5-9807-4905-ba4f-7bd8836a5ae1)(content(Whitespace\" \
         \"))))(Tile((id \
         4d3925da-ab76-497b-8802-f3b83822a057)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fee0221-8fa7-4012-9f41-55ee86b23ca6)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         40ceeab7-0c87-4eb6-a3fd-777d99c40a25)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22b02580-b6f0-456c-bc25-452ded90467a)(content(Whitespace\" \
         \"))))(Tile((id \
         2c42106f-ad3c-45c7-9310-71f3783716a5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4328f71-1d65-479c-826e-07e604ea9623)(content(Whitespace\" \
         \"))))(Tile((id \
         74a35645-738d-4f21-afe7-9416c759d6d2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         60b496f7-8972-4406-949d-342234af1e7f)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4ab4cba-105f-41c2-ae56-b810febb5d46)(content(Whitespace\" \
         \"))))(Tile((id \
         f2f10767-57e4-49ec-beac-5307a663703b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         acd71dc3-e621-4fa0-926b-c44cdd8fad6c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         87fdcb3f-9fd9-408d-926b-81f1c1069f3d)(content(Whitespace\"\\n\"))))(Tile((id \
         1f3bbbc3-1a11-4389-8f59-12da634c43cb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         761b1bd7-8209-413b-bcd7-a232ba627fa2)(content(Whitespace\" \
         \"))))(Tile((id \
         2292ce37-65d3-4e61-88f8-943326d20097)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e5c07b9f-1e98-4f1c-9d45-9739209cec3f)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f838c4f-79b8-4532-94eb-53e6a62845de)(content(Whitespace\" \
         \"))))(Tile((id \
         46488526-4ba0-4bf4-a78a-303d9d9551e0)(label(getNeighborCells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67ac28f3-a9ae-4e2c-8e10-10a7f7b6bf4e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ec6e596-9691-4ada-b97c-15afe2637e01)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e7faadb-a9ce-4c76-8906-8d13b3b01ece)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd262345-0609-446c-8859-5ea544598fb7)(content(Whitespace\" \
         \"))))(Tile((id \
         df245166-7c7f-4473-898c-18e2c8d83a86)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e6d9dfd-6e2c-4316-bd21-e7a7d99be8f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd15b06a-9b22-48e7-b998-3da1668c8e57)(content(Whitespace\" \
         \"))))(Tile((id \
         d0ba6b89-d9d3-4cec-a9d9-f6cddd09d93d)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fec024d2-2b4c-4093-aba5-e5f565e08299)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2b018e18-44f6-42e3-9abe-e3bcb818476d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e6fc619-7af8-4c58-b9e4-b89fd3846a3b)(content(Comment\"# For each \
         neighbor, calculate the companion effect #\"))))(Secondary((id \
         4c5e490b-9f6b-47bb-ba44-5336cded0e72)(content(Whitespace\"\\n\"))))(Secondary((id \
         b86264fd-81bd-4819-bfe2-354876b932d9)(content(Comment\"# Effect \
         should be between the current cell's crop and the neighbor's crop \
         #\"))))(Secondary((id \
         c68b6d7a-2df2-4b51-a76f-13f832e9af1c)(content(Whitespace\"\\n\"))))(Tile((id \
         43d74167-85c3-42bb-a1e9-afae83161ea5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         268322f9-a204-459d-aa49-e12ca91676fb)(content(Whitespace\" \
         \"))))(Tile((id \
         450371cf-f770-4895-bc18-e64483887a8c)(label(effects))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         262635af-3f8a-4be2-aab5-da8311a8af04)(content(Whitespace\" \
         \")))))((Secondary((id \
         b85a96bf-417b-4956-a173-63d5c92f4392)(content(Whitespace\" \
         \"))))(Tile((id \
         2fc68621-c917-470a-9a7f-baf8e3b05b6c)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85836dfc-426d-4bc1-94df-dbb11c82b4e9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea7e94bb-a662-43c3-9572-c28b5d9d3af6)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba5b647a-19fc-4166-aab2-f69be085c0aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         368c353b-21d7-4960-93e7-e96e7b354984)(content(Whitespace\" \
         \"))))(Tile((id df75064f-c675-402a-b1e0-d9304d30a9d9)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         60f32637-88f0-4b40-91af-d84a520027ce)(content(Whitespace\" \
         \"))))(Tile((id \
         180f8f61-b902-48c4-b821-782936333922)(label(neighbor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         67a1b112-b511-48b6-8466-f683235f099a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc762a0d-c54e-419f-999e-019702828f51)(content(Whitespace\"\\n\"))))(Tile((id \
         9e8e39ac-b4f3-4a32-b947-8c66a1fe27b6)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b34efcc-3171-4bbf-835f-5cf81b85b521)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5fc6cb6d-8cfc-4cdf-b6c2-38fbc79cf377)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f261134a-aa80-425a-ae2d-7997ba35301b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         773efd69-da36-494c-86de-624a18b9ceb8)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         549c620c-6d2d-421d-a2e3-81760369f936)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8867474-5486-412c-8989-43add101547c)(content(Whitespace\" \
         \"))))(Tile((id \
         70bbf89c-d0fc-4a2f-ac30-42467ba0af7f)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         590d59b6-19bc-4d64-9d79-4d8abe47152f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e961cade-9123-4c93-a29b-e2e4d1655cec)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7ce45d8e-ef35-4e1b-8e30-df6cddbbfb7b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d2ee113f-50ba-47fc-932c-bc6ef43322f3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13a63915-fc5e-44f6-a708-db706b0e7ba2)(content(Whitespace\"\\n\"))))(Tile((id \
         1bf331ec-0f1c-481f-ae82-1651a09b6311)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55978c58-6f31-4e59-ab61-4d7dfde1172f)(content(Whitespace\" \
         \"))))(Tile((id \
         76d07530-2a06-43cf-85c7-5310cafeb2c4)(label(modifiers))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6d8d56b-d823-4377-a224-23f8420a112f)(content(Whitespace\" \
         \")))))((Secondary((id \
         c842d27a-82f2-4aea-9b6d-d6860b390e02)(content(Whitespace\" \
         \"))))(Tile((id \
         1954d28c-fda2-43f8-9149-65a721801f17)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e9d43d8-83af-4781-8989-38d40527fa63)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bbf5ea25-1f9f-44d8-9c8b-6ddd85e51901)(label(effects))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca7bed43-699d-4893-98b4-3150153680f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fa1402a-6f3b-4fb1-844b-31b3de86b1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         f282f2a4-8e5b-4046-b797-2ba16fafbda4)(label(effectToModifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         10f8425a-88cd-4fe7-a1a7-ad9545b1ab07)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a23dc162-3e2a-4ee3-a8be-137be12313ab)(content(Whitespace\"\\n\"))))(Tile((id \
         716b5937-2294-41c0-b949-c8d03e47639e)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39fb9c0d-a7b8-49b8-bcf7-32059c818a07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7d5efb89-faaa-4800-bbb9-3435f16f6873)(label(modifiers))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00d05e8b-a620-4684-8285-14e83a8ca2f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c0f1359-3caa-47b6-95ac-4b0f2767102e)(content(Whitespace\" \
         \"))))(Tile((id 99dee90d-1b09-43e5-9861-2c4c0cd674fd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         79ab8840-3ef5-437b-8f01-54e8edfde11f)(content(Whitespace\" \
         \"))))(Tile((id \
         42006f3d-989e-496d-9d5a-6bde1a083987)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5d4b9bcc-3237-4f2a-a140-d2285aa37f5c)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dc65cdfd-0996-42cf-91c7-e41ac9904864)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         50860200-6d82-4c45-aade-3a5981ff5797)(content(Whitespace\" \
         \"))))(Tile((id \
         ab290a25-6a9f-4cce-969b-20680bc4fb74)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7fc2d809-e535-49bb-a32e-be5eb314678c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         102ed5e9-ff52-4406-9d82-538f48e260d5)(content(Whitespace\" \
         \"))))(Tile((id \
         3e3c53e1-9737-4794-8bb8-eedfc222777c)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62982d9b-e255-4065-b38e-18253e9a4710)(content(Whitespace\" \
         \"))))(Tile((id \
         48eb4b33-41af-40d0-9fa5-03310e331406)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         004eef12-a80f-499e-a551-2fc44e0e2851)(content(Whitespace\" \
         \"))))(Tile((id \
         e8cd4630-7ef2-4334-9376-b74fbe05f6e9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdefaab3-2a61-411f-b227-5838976d6cac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28b03986-8956-4f6a-965b-8927c14e1971)(content(Whitespace\" \
         \"))))(Tile((id \
         87268a3b-ebcb-48dc-a66d-7fc0165747c8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7678ed3d-a2d7-4452-8662-c647e2b25e27)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aabe2077-681f-4a16-a8de-891eade7790f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8fb2efb-435d-4449-8504-6323e2086fb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d944cc9-aff0-481a-98d9-e6cd4d707333)(content(Comment\"# Recalculate \
         health for a single cell based on neighbors #\"))))(Secondary((id \
         46046964-8783-4ea1-aadb-be2df0794a23)(content(Whitespace\"\\n\"))))(Secondary((id \
         8757d8fd-a802-441f-b73b-fc12339cf6aa)(content(Comment\"# Base health \
         is 50, modified by neighbor effects #\"))))(Secondary((id \
         31e58440-cdb3-433c-b2ad-49ed8006b83f)(content(Whitespace\"\\n\"))))(Tile((id \
         01a9c7d7-289e-464a-a647-5911919af5af)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e9f712fe-a34f-44fc-b751-1ed568765ab5)(content(Whitespace\" \
         \"))))(Tile((id \
         21eadef1-0731-462d-8fb9-484a1b8bf3f9)(label(recalculateCellHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         58270a9a-e0ff-457d-9cfe-fbdd291e9b5b)(content(Whitespace\" \
         \"))))(Tile((id \
         18f03893-3653-4af6-9bb4-e5739f7855c2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         837d9b53-719d-42f7-be19-ffc3aded12ea)(content(Whitespace\" \
         \"))))(Tile((id \
         47436e7c-c322-42a7-b444-faa1b7f0dc96)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8f674d39-a3e3-4e81-9ab5-dd56cea56403)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8aec2ee2-e68c-4d74-afc1-0527b4ed791e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2ee38c51-314e-4c35-910b-1473d296e128)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf741f8-d963-421e-8c8e-f0f323ff0a87)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2a7523c3-e9e6-4e4d-9777-ed24805dc111)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         aa44045f-01c1-4be8-9687-10858d12e13c)(content(Whitespace\" \
         \"))))(Tile((id \
         b3ecfb13-5229-4860-bc67-eef237f58dcc)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c519563d-22d0-407d-84a1-5c248fa15bbd)(content(Whitespace\" \
         \"))))(Tile((id \
         761c8504-c879-487c-bf6f-23d1bfe52bbb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         82f5c4e5-3a10-437c-ae76-737f5012d75a)(content(Whitespace\" \
         \"))))(Tile((id \
         9db4ef31-58a1-4597-9440-2650ebf0aee1)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5a8569c8-801d-464d-817d-ec7d8caeb130)(content(Whitespace\" \
         \")))))((Secondary((id \
         6657df31-7b23-408b-934f-de9f29901a74)(content(Whitespace\"\\n\"))))(Tile((id \
         9e9e2d53-5540-40d1-94d8-55465af08d8b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         888b3225-2704-4643-ab69-02c7f32f1298)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf0ae18-6a8e-4ecc-8c74-59a5a53d638f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bcd17317-8cf7-4813-9e67-644bf6198941)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         225597f5-f48b-49bd-b9b7-77503b036f0a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a54a8b1b-b92a-4ecd-999f-bfede4efa269)(content(Whitespace\" \
         \"))))(Tile((id \
         a173e037-7906-4266-ae20-2bc937bdfac1)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dc6c9b66-1f2d-49e2-88b1-8ad49bae593d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         30958322-bc09-42ce-9a69-73be65ac550e)(content(Whitespace\" \
         \"))))(Tile((id \
         c90f1e57-339e-4053-b7f6-ca50446258ce)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         74b14535-8059-4e6a-9987-8e81e336f441)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d2fc5f3f-5ac4-4484-8dac-852b7235513b)(content(Whitespace\"\\n\"))))(Tile((id \
         ab154dbf-53ca-4c71-a989-f53d2d72e041)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5168bbbe-b087-488d-8c5e-4cdb82342942)(content(Whitespace\" \
         \"))))(Tile((id \
         9a26b9ce-c594-4069-9006-e18869fa9665)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         954cccfe-7d74-480a-bf09-0c15a46fb7d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         a82cd537-e653-4be3-b8ff-f852ef53fdaa)(content(Whitespace\" \
         \"))))(Tile((id \
         6f6b4eca-f0d8-473b-8752-ddecaf95b157)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ae91c6e-12bf-45cc-abaa-ddc1bfe8dbe1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1495748b-8edb-42f8-9685-00eeb96f892f)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50900251-4b1e-49cc-82eb-1bcd77ff451c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2beee301-0dcf-41db-97b5-6d4c6e1b6198)(content(Whitespace\" \
         \"))))(Tile((id \
         827f2647-ed21-4799-bbad-c774abd78f53)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed37b1e9-f4f4-4999-a9de-db837611a362)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13ae76f3-2890-4445-912d-c46535bbd90e)(content(Whitespace\" \
         \"))))(Tile((id \
         22cf2183-cca7-4854-8a81-e5feb4765240)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8dbbde92-1255-4067-b929-4217e1325660)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34d7604c-61c9-4619-a88b-27cbfc50a3a7)(content(Whitespace\"\\n\"))))(Tile((id \
         e6716656-535c-4658-a7b6-3330b8908b2c)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f603f50-2e5c-4e9a-9793-3c8f694a3dd9)(content(Whitespace\" \
         \"))))(Tile((id \
         dae25e4b-a705-494b-8372-b643b3c1ae6e)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55dfe4d4-524b-4495-804b-9889f03b9b9e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7a82720a-895f-4190-8152-2a39deae550a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ec7b374-2653-412d-8f57-0252fadcb6dd)(content(Whitespace\" \
         \"))))(Tile((id \
         42c7ca1e-bb94-4431-8e09-b91c3578cdb4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f617eac8-d6b3-472f-b6f3-aa9b7c1aae82)(content(Whitespace\" \
         \"))))(Tile((id \
         21086257-4687-4c70-8972-1cb5464122b9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         752161be-0e63-4fa6-b1de-27a49a2a318d)(content(Whitespace\" \
         \")))))((Secondary((id \
         439cafd4-6852-462b-b2cf-04b237ae8d2d)(content(Whitespace\" \
         \"))))(Tile((id \
         15a8d030-ee22-4501-a105-c3234fc32785)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1fb20fd0-eae5-489b-84e4-9c5b364ac2d0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b8548cd0-dfca-47ad-a66a-450440e81e7f)(content(Whitespace\"\\n\"))))(Tile((id \
         3e008671-05c7-4423-b3b6-dc3981432cdf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         255da7c2-8597-4f35-844f-1bd9004deb57)(content(Whitespace\" \
         \"))))(Tile((id \
         09226012-0f6e-429c-963f-e5b0696fd125)(label(modifier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c805c1bf-b0cb-4d4c-a3ee-937295794042)(content(Whitespace\" \
         \")))))((Secondary((id \
         8ea9dc6b-0f24-4998-98ff-0c1562ceab69)(content(Whitespace\" \
         \"))))(Tile((id \
         6c09244c-7005-431e-9d7d-de6732787dc6)(label(neighborModifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         152378e6-d0d9-49a9-a0bb-0fcd1c23b67f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c581d45e-f59d-47fb-be40-d593b6d50dca)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f66260dc-ca0c-49a2-9b57-cc245ffffe76)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c75ee7c3-bc44-4faa-b4c0-295fee5e4ef0)(content(Whitespace\" \
         \"))))(Tile((id \
         fea60086-7227-4301-bba9-580b072ca849)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea491d42-ccda-428b-b445-bf187195bc50)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3f727fc-9482-4fb1-b8b1-a9bde5d3fe17)(content(Whitespace\" \
         \"))))(Tile((id \
         f9665199-ae25-4c3d-b3cf-8814bb40af5a)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8467953c-d747-4471-adf6-75fa11259345)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0396e22a-f08b-4b77-8aa5-4ee73d04bc76)(content(Whitespace\"\\n\"))))(Tile((id \
         41e1614a-4d7a-4d10-9cb2-fb9249a60e48)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         886dc379-d0cc-407a-81bc-4c2becaf2625)(content(Whitespace\" \
         \"))))(Tile((id \
         1c9ce7e0-198d-4631-927e-547c78eaf703)(label(baseHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         afcb60cc-f85b-4aef-be69-6929450110fc)(content(Whitespace\" \
         \")))))((Secondary((id \
         ed30d65a-32b4-466c-bde9-cb9cef68d4a1)(content(Whitespace\" \
         \"))))(Tile((id \
         972c3171-ab91-4b1e-92d6-809e7114114e)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6eba2f9b-4af2-4ea3-bc7b-ffe61cdbf6a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         23ff0618-c160-4982-b115-0ec6165c70af)(content(Whitespace\"\\n\"))))(Tile((id \
         a89d89ae-a0a3-4873-b20b-74eedd2f9b7b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3e39d992-81ab-4fa3-a31f-3ed6ba20cef8)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb651330-7942-4934-a2c1-659ea4233925)(content(Whitespace\" \
         \"))))(Tile((id \
         4afbed10-bdbf-43e8-8fbb-6407b452a598)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4eb62652-f5c5-4027-99c9-fa75ba0f5ee7)(content(Whitespace\" \
         \"))))(Tile((id \
         a348e12e-bd54-4e02-8534-d0772fc046a7)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b67dc9a7-9164-46cf-8552-ca1227e886b6)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         618002e4-f237-4c69-b60d-416d412d7b09)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b243966a-4b3a-4068-a4b2-8fffca88131a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9dcee25e-ad35-4386-b84e-a84b46fbc5fd)(content(Whitespace\" \
         \"))))(Tile((id \
         ad6bb01c-6cd8-4597-b745-c08c1f6ea7dd)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bbf276f2-a4e5-41f8-8ac9-00edf87dcf75)(content(Whitespace\" \
         \"))))(Tile((id \
         cc9d20bd-dafa-4ec7-af76-4ee675fdc122)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a13a6259-94d8-4402-90b9-1474ef3c0d96)(content(Whitespace\" \
         \"))))(Tile((id \
         d1d37398-e960-4f21-ba59-3c66eec388c8)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efedc063-4dc8-4625-a2ce-2763a38d1648)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         77638903-a70a-4537-8555-a72c93bfed85)(label(baseHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         59a3f578-1aca-44d7-b797-a095a3c833f3)(content(Whitespace\" \
         \"))))(Tile((id \
         9929f155-83b6-4d38-afc2-9f9e01cdbad3)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f44db9ff-3d25-43f9-8e8f-5cae2add1243)(content(Whitespace\" \
         \"))))(Tile((id \
         10a441c1-f9c3-409f-8355-b0711588db72)(label(modifier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         255c8c3f-88d1-43ca-83f7-e6eb766f11de)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5595ff3f-e80b-4a63-813e-180359ef5811)(content(Whitespace\"\\n\"))))(Secondary((id \
         49f4ed27-2517-47cd-af6f-972dd6ac61e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         30092b8d-0152-4808-b189-9700f5479f36)(content(Comment\"# Recalculate \
         health for entire field #\"))))(Secondary((id \
         61dbed58-d0a4-485b-b63d-742d8bfe0f0e)(content(Whitespace\"\\n\"))))(Tile((id \
         be30e32b-63de-4b25-b174-b7d8e5e74ac7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83565911-edb8-43cd-be52-8a5227f98b57)(content(Whitespace\" \
         \"))))(Tile((id \
         109a45ce-5570-41b6-af42-ab53643bd89f)(label(recalculateAllHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adb911be-6daa-4bd9-b7aa-9f6830dcd863)(content(Whitespace\" \
         \"))))(Tile((id \
         c2e3b3d3-9783-4b71-b0dd-2e08db87a16a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         20532fdd-3006-4cfa-ad7d-6294473ba015)(content(Whitespace\" \
         \"))))(Tile((id \
         97aef98b-7a54-44f9-8532-15e985328dfd)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b796cd53-0341-4355-a5e4-6ffa2d5d9a67)(content(Whitespace\" \
         \"))))(Tile((id \
         3c27d7eb-8f13-46be-912f-82c19309188b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         89538e35-9b9c-441e-951d-72ca3693765c)(content(Whitespace\" \
         \"))))(Tile((id \
         78eefdd8-3512-432a-9987-f553f5690637)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1156f92e-c993-463f-ba67-37fa4a974f25)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c7959a8-1d66-434a-afd4-3c3d78d8d7df)(content(Whitespace\"\\n\"))))(Tile((id \
         e25f4517-7f3c-43a5-99c6-cb0bb7018710)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         78383df6-b672-403d-ae01-a17ebbb2490c)(content(Whitespace\" \
         \"))))(Tile((id \
         4eabf5fe-0fa8-4c33-87b9-563b51a92881)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6af1b66-68a7-4810-837b-ec0327860dc2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e4884715-ace9-4f55-972a-8433c4bf2127)(content(Whitespace\"\\n\"))))(Tile((id \
         a9eb1c8f-b85d-4dcf-9159-7cdd61b8b299)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d88b288-be30-4169-a3b2-08ea65a8ccea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0e0427ad-8d3b-4582-bdb3-ffcec8048a9d)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c22a6ddf-0a11-4669-a9ec-fc1a5ca4c287)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97a4ff2a-10f3-44c4-97b7-6e7f66446237)(content(Whitespace\" \
         \"))))(Tile((id 8620b5ff-6724-4938-9945-5284c59e37e0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         db5fe90d-4b67-42bc-bad4-b587c5c5e908)(content(Whitespace\" \
         \"))))(Tile((id \
         4541d14e-f4fd-4e8c-b38c-4d85acd01dc0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5f41fd4a-3f1b-4cb6-a5b2-2ad01d28055c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         83db2cc5-c511-44e8-9779-7b1836b3000c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eea9c57b-32cd-40aa-8165-998cb211a794)(content(Whitespace\" \
         \"))))(Tile((id \
         5976e0da-0789-4644-ba4f-bb6d1f705a2e)(label(rowData))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e23853db-fce3-45c6-885a-a4e7aba6d093)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46d285d6-cd19-4509-baf4-c566d8ca9bd1)(content(Whitespace\"\\n\"))))(Tile((id \
         34e3f2ae-0a5a-46e9-a452-c0fc527f0089)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb2e7641-0722-4b54-996a-3d87eaedeed7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9f4604ca-3182-4178-ad73-b46db7c1f8fd)(label(rowData))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b36e3138-b085-45ed-8772-10b37db17edb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e62bd3a-2073-4507-a27f-44677a554ddb)(content(Whitespace\" \
         \"))))(Tile((id 5324a22d-0352-494c-b1c8-5d65a7923bc8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4c3b4f62-e806-4a47-914a-e97a6c04d93f)(content(Whitespace\" \
         \"))))(Tile((id \
         ed8b2c39-9e2a-4257-ba84-71652abfb3ec)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         56896f12-c835-400f-b013-5ba75ccc1ac4)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8095660f-8c16-438e-902a-d883d078fc04)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         beff9524-f8c8-4123-a6ab-5cb146ad707f)(content(Whitespace\" \
         \"))))(Tile((id \
         6a9bb813-fbc1-4fc2-afc2-6c106bbe2363)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5dc9124c-d7c1-4b9b-99af-c887aea32558)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9f0bc617-3afc-49c3-8eb7-a549e9276d75)(content(Whitespace\"\\n\"))))(Tile((id \
         bdacb4d1-8f1d-439f-bdf4-59c9e1624bb6)(label(recalculateCellHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e86d42b9-5fcc-4d4d-989e-d7fbd87447a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         12b43f2a-4db6-4e47-9208-4403bd9a4417)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0be09a5-e184-4413-ba9f-de57e4b8d65a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1217ba7b-18eb-456d-a7df-1711e461ffcc)(content(Whitespace\" \
         \"))))(Tile((id \
         7d27fc17-9bfa-46d7-b065-b67dc60bc7aa)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         077ea322-d4cd-4277-8b5a-6f328c1b9f13)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acd9023d-4836-402d-80b4-9118eaf8d006)(content(Whitespace\" \
         \"))))(Tile((id \
         fd897331-a92b-422d-becf-bca1ba277e8c)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5822d9de-ab90-4924-ad77-6d89466a0df5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b4496d64-0d38-4335-9e0a-a42df84f6950)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e2b6ad72-578c-4929-b2db-6c2a5d5b7300)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c34d008f-f42f-4a1a-8855-9f3cbe53cd42)(content(Whitespace\"\\n\"))))(Secondary((id \
         199b5a10-6cc2-4bbd-b41b-b123a0d43280)(content(Whitespace\"\\n\"))))(Secondary((id \
         56abdd26-0508-4930-ae80-f5b8e1d4e3ee)(content(Comment\"# Create \
         initial empty 3x3 garden #\"))))(Secondary((id \
         c3a8d023-6170-489c-936b-b80fe0c81d8f)(content(Whitespace\"\\n\"))))(Tile((id \
         ba662428-daab-46fb-9157-6d91466d931f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ba2a0ee0-e389-44ee-927e-bdafbaaf13fe)(content(Whitespace\" \
         \"))))(Tile((id \
         11498964-f9e3-4b53-8e2b-73c0308ac78e)(label(emptyField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b44488f-0449-42ba-a0cd-b73b975ba18a)(content(Whitespace\" \
         \"))))(Tile((id \
         60ae4e35-886e-4d5f-a5e0-7665b636f43d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e5486c45-41d1-472d-970f-408edeede9db)(content(Whitespace\" \
         \"))))(Tile((id \
         894bef35-f1ec-4347-9a8d-9955dada7e39)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         47c54335-84ac-41a0-9e73-d129b6a71062)(content(Whitespace\" \
         \")))))((Secondary((id \
         c9d30b53-bc22-46a8-8494-c16743d6eb9a)(content(Whitespace\"\\n\"))))(Tile((id \
         477505ea-f3c7-4166-9bef-7a690a115dc6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1da0801a-dafc-4e2d-848f-b76c52aabb67)(content(Whitespace\"\\n\"))))(Tile((id \
         5bc784b0-07a4-482e-b1fa-031cc1291877)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         83029cf1-5fbe-462a-b3c8-8940c83191c9)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6caac24-2420-4afa-b62b-2756e10af08b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa2ede32-2c0a-4105-8adc-df71108dda6f)(content(Whitespace\" \
         \"))))(Tile((id \
         dcb5153d-fd25-4f81-8b6c-e810d18c3241)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bd0940e-c178-463e-9879-c2454a9e299a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d75dc851-5987-45c1-9284-beb983a10c49)(content(Whitespace\" \
         \"))))(Tile((id \
         ecb5c3da-8436-4368-8167-6d2056555ead)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2cc1f0ff-6a49-4718-8c32-0aff1e75b41d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b9cf4f0-2a97-4eb9-bb62-5b91342f059e)(content(Whitespace\"\\n\"))))(Tile((id \
         fb47078e-397e-4e46-815f-3f403fd90853)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0795154b-e1a0-47d1-bf75-8b06f6172d4b)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a3e7153-bb65-4630-a95c-b05564a0b4da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05d76b97-6140-4e63-97ae-8ff0ea740bb7)(content(Whitespace\" \
         \"))))(Tile((id \
         1308a304-dae0-41eb-9c31-5dcd6651934a)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbd4c234-43a1-4e13-93ec-4ef2b0d273e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfe611c3-78d0-4de6-8fb7-6505662a765c)(content(Whitespace\" \
         \"))))(Tile((id \
         657e747c-783f-4b94-9bc6-8ce13daaacfe)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e848ce11-0098-4873-a655-63171dbc592e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ab20e56-a2cc-4fec-b676-63a4dccd6a55)(content(Whitespace\"\\n\"))))(Tile((id \
         9e43e44e-44f7-4c5f-bf63-cd886b74f263)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c612ad55-2f70-4077-897a-d0a2cf7f96e1)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e941cbcc-b395-491e-92c4-c940732ffca3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33354f7f-a696-4e8f-b1e3-54ea16dd195a)(content(Whitespace\" \
         \"))))(Tile((id \
         512fa411-d251-4a69-9f38-b67b5b961a42)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7027d8b-c6af-4827-98b2-b14166ffdc97)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b41148cb-d83b-4170-b10b-d0907ae6e567)(content(Whitespace\" \
         \"))))(Tile((id \
         29d0f11a-beb4-4749-ab92-e064bf825206)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f4972424-2490-4191-91c4-7e7b53334662)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b8d3c4ce-5963-4f78-b9d9-e17059d10fc2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f73eaad8-f1cf-467c-857f-33fac34a016d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d62bdc6-2cfc-4506-add3-75260517fb64)(content(Whitespace\"\\n\"))))(Secondary((id \
         b15f316b-3df8-4b15-a349-2c639352f6c2)(content(Comment\"# Initial \
         model state #\"))))(Secondary((id \
         0881b7f7-891a-4f10-9c31-fd83824dc30b)(content(Whitespace\"\\n\"))))(Tile((id \
         d8dea617-bcef-40a9-865a-79cd9c157fa4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8787ae3e-50f7-48a4-8513-2d2189c76579)(content(Whitespace\" \
         \"))))(Tile((id \
         c0ed70f9-d9f0-4b03-bb64-8fa6b939e651)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         592a9fcc-08bd-4cb3-834d-46e4cc5eef58)(content(Whitespace\" \
         \"))))(Tile((id \
         f619d29c-cb2f-4e13-a5ea-b24ed5878177)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8fef292e-a4b6-4826-b339-f83e5210c2e8)(content(Whitespace\" \
         \"))))(Tile((id \
         ef4f53e7-7485-4c07-a01e-bf39a7ad81c4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cf7f7f35-0fea-4cb8-a191-a658c367baca)(content(Whitespace\" \
         \")))))((Secondary((id \
         8eeadfa8-2bc3-42a5-ad74-d05d325a6250)(content(Whitespace\" \
         \"))))(Tile((id \
         2809a6d9-2f9d-4615-8e25-c9363b00ca6d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1b1ad5e0-476b-46d9-bbc6-bea75e888466)(content(Whitespace\"\\n\"))))(Tile((id \
         0882afbe-6866-47ab-8a7f-3f7a59b2c5ec)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         043c3443-f0aa-4651-bd9e-60814bbc6da9)(content(Whitespace\" \
         \"))))(Tile((id \
         6dd3ad20-ea71-489f-90a2-cc49effd432e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87ca611b-9830-4380-a360-6cd94284b09b)(content(Whitespace\" \
         \"))))(Tile((id \
         636206f8-0df9-4e20-912f-8dea626853e2)(label(emptyField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a748a2e-e4ee-4078-98ab-e5feec13f184)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b34d8ac-19e1-462b-a902-adec048bd059)(content(Whitespace\"\\n\"))))(Tile((id \
         592409d9-282e-471e-b75d-cf05a52178a6)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         feb0f65c-c1fa-4304-80f0-cdd8a191facf)(content(Whitespace\" \
         \"))))(Tile((id \
         4e2a8d9e-175a-450f-8320-d73e666a266e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2229332d-be13-4087-b207-9d62bb5bd316)(content(Whitespace\" \
         \"))))(Tile((id \
         d7385848-2c42-4c93-806b-fa1ffd593182)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5de63f12-e923-4a6a-814c-2e76a9f1f738)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e12cef21-8fee-4779-96ae-9e0db7b3e11e)(content(Whitespace\"\\n\"))))(Tile((id \
         bd18b9c6-2ae8-4276-aee4-28d66b637861)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a76386bf-703b-4ac3-a5c4-96851591440c)(content(Whitespace\" \
         \"))))(Tile((id \
         4529d351-99cf-4666-bc6b-67d3a49822d2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c832a952-62ea-4dca-9697-10d0667b565f)(content(Whitespace\" \
         \"))))(Tile((id 5ece6b91-76b7-4283-b5fb-4d3b223f14b9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ebc51db1-3559-439e-9513-f5da6077822d)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3b2b453-2fbb-4912-91ae-4e3ff86ba310)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a56021e-ac8c-46d4-9121-25e0a8925d41)(content(Whitespace\" \
         \"))))(Tile((id \
         1d82068c-611d-42e1-9c84-3c8befe36fd7)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6dd51761-950d-4afc-af07-152a635fe2c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0814a68-1626-4b5f-9d68-edf9167dc9c6)(content(Whitespace\" \
         \"))))(Tile((id \
         08beea1d-debe-43e1-bb92-eebeda7e113b)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcefd868-8082-47d1-aac1-addfee9951ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e914d1b-eeeb-4f81-9fbf-bdaa02056fef)(content(Whitespace\" \
         \"))))(Tile((id \
         7b6d94e8-e830-443c-920d-f6f99ba17572)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32fee3fe-fc6b-4f2d-ae4a-4722cff4f2a1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2563954-96f5-4b8a-b849-03d09be92c53)(content(Whitespace\" \
         \"))))(Tile((id \
         691233bd-2d28-45ec-9616-aba1ed09eaea)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ebb3954c-63de-4395-b7b5-d09120c132fe)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8d48ad21-7baf-478a-b88e-4972d4cbb60f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d21a2e7f-ca9d-4f31-91e5-d8f86df7fbf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a629ccf8-881c-474d-bbaa-8f203a349f75)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1a44258-87e9-44cc-955c-b8dae32fa4c9)(content(Comment\"# Apply an \
         action to the model #\"))))(Secondary((id \
         8ce3b284-8b9e-40e1-86b7-ae4654071bb3)(content(Whitespace\"\\n\"))))(Tile((id \
         ce70cc7a-d9fb-4483-b247-287563cff089)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eaba4aaf-8f99-4a25-b100-c5a1ae5f828e)(content(Whitespace\" \
         \"))))(Tile((id \
         a097ad9e-e407-4337-9f64-7bda246a63b2)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2abcb2d1-efee-496f-80e1-75eaf3b0c92b)(content(Whitespace\" \
         \"))))(Tile((id \
         23524bd9-abc6-45f4-aee4-6749248dc503)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e3598c70-1fd6-45e8-a63a-219ff248d02d)(content(Whitespace\" \
         \"))))(Tile((id \
         72e0c71d-66f5-4e8b-9d51-52f4952524b2)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         2f88c66e-a609-487b-b1bb-2a213eeca753)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6017bdfd-0271-4482-9f1d-521c478bc384)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3fda9ec8-19cb-4227-9896-aaa1580c5151)(content(Whitespace\" \
         \"))))(Tile((id \
         0a12e470-1826-42d2-9aa7-ddaa92c8911f)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         abfdf57a-17bc-44c3-b5ce-da3b237dcd24)(content(Whitespace\" \
         \"))))(Tile((id \
         ccaef2df-bb5a-4c61-aa2e-59e0c4b44690)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a7a0cbd1-b39e-4fe7-8b7a-6ec282520716)(content(Whitespace\" \
         \"))))(Tile((id \
         b75dfc85-f24c-44e5-ae8d-3460d4e18758)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         861eff6a-3731-402a-bf1b-49ef0d2b3687)(content(Whitespace\" \
         \")))))((Secondary((id \
         a87c367e-5375-4561-b2aa-cf950e79ec41)(content(Whitespace\"\\n\"))))(Tile((id \
         006bd148-fafc-4ac1-b7ac-9296f6454d38)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fb5bcb15-3630-4bb6-b1c0-05f6dd393046)(content(Whitespace\" \
         \"))))(Tile((id \
         f519a09b-9007-4888-aa09-53e3cb720225)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         414423fa-d87c-46bb-8bb2-d2ae91fef646)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bef52fd2-ec1b-48f2-9f88-26693c434aff)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bcd0ed13-06f7-4518-80e0-90b800c1ae3c)(content(Whitespace\" \
         \"))))(Tile((id \
         74443498-8c37-43d0-a72c-1f44012cead1)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c0a15dab-f28e-4d77-b258-9d24459bd628)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59cf9f9f-afee-4d8f-953b-edc90d06f276)(content(Whitespace\"\\n\"))))(Tile((id \
         aa483bb2-a226-475c-8f40-a20e0e9afb81)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d85e9a0c-4bdf-402b-92e9-b1373d49b293)(content(Whitespace\" \
         \"))))(Tile((id \
         b06ceb69-38cb-44cb-a2aa-47de1d6392f0)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd82c528-430f-4dc1-9cb7-c0393bfe1025)(content(Whitespace\"\\n\"))))(Tile((id \
         2003c529-3760-46fc-a23b-f29dde8fbc58)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         120c3697-f683-40f4-ab51-245ef2ab3fdc)(content(Whitespace\" \
         \"))))(Tile((id \
         9b44efca-cb09-4ea1-8e9c-d63317ec2771)(label(PlantCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f7e64805-d2f6-425d-8a08-fad66b71d0aa)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         821cacb2-4faa-4d01-b27c-dec30f01777a)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1c8808e0-6b7d-4006-a548-511cad829512)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7b6b5943-37c5-495d-8dc5-f77b9dfec7b6)(content(Whitespace\" \
         \"))))(Tile((id \
         e854e2c8-a6d5-4b01-91c1-be1b7a9a1ab7)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3a013373-4d5e-4d38-938f-a8d1616a92b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4cf98408-0f55-40a6-96a0-e0de808e09d7)(content(Whitespace\"\\n\"))))(Tile((id \
         fc3d84b4-43b6-43d9-852c-2ff5f2080164)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7b0199f7-0b09-4bfe-9abb-0ddd0e934b23)(content(Whitespace\" \
         \"))))(Tile((id \
         d0c6257a-7097-4df0-8055-e6a65e3e3891)(label(currentCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af536355-877a-462f-a4a9-282c3bdd3712)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d329008-45ba-4b2c-a3be-047b3b84ba68)(content(Whitespace\" \
         \"))))(Tile((id \
         53e7333e-425b-4d32-b36f-eb8752c11e49)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c121947-a086-4ce0-a1f2-6c177504fe21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         76628917-34f0-469f-8175-390caff50027)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9afd3ef-0923-43b6-a032-4845e828e20f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9e040e78-eb26-4fb2-8b3f-825014aec421)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         655589a1-ef2a-4e8f-9094-01cdff984d12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b90c35a-2ab8-4ee5-9c36-42940f2914ad)(content(Whitespace\" \
         \"))))(Tile((id \
         cf8d8c65-c004-4e76-9dd4-7f2a0ac27396)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         278392b0-4547-414b-82d0-fac3489d7cdc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bccc133b-87ba-4280-ab10-e9d9e230f6d2)(content(Whitespace\" \
         \"))))(Tile((id \
         12669a79-c3a3-4d14-beae-8bbb898ba207)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4925361b-1e8c-45bc-8733-a2d0a1fd43b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0bc6a7b3-3f38-4fb1-a4bb-d457ed3e6e6c)(content(Whitespace\"\\n\"))))(Tile((id \
         4086cfe7-0f35-41d9-bd0f-6ea816d0be12)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f3a057bf-fb6c-41ec-8e89-50565113c567)(content(Whitespace\" \
         \"))))(Tile((id \
         35a88c7b-cdac-48d1-82eb-7e17498ed8d6)(label(currentCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e9bce17-6660-4ea2-a3c3-e281bb3ba794)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6f9fc2cd-c18c-4ece-ac9f-82e0e1d174b9)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dec2892f-6365-4531-bf46-435498dc4dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         8f571a94-20c4-432d-9b78-1baf63494547)(label(!=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a275a161-5601-44db-b960-290fe97f2b5c)(content(Whitespace\" \
         \"))))(Tile((id \
         0cf2c259-f6a6-4a53-bcf3-08cb3b5a8884)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e0d3f31-a1f3-41f8-98fe-c9549b27323b)(content(Whitespace\" \
         \")))))((Secondary((id \
         57bd08c8-7d6b-4013-9b7d-cb5d98149ca0)(content(Whitespace\" \
         \"))))(Tile((id \
         67a8f355-ba76-423a-8366-6d353de8c2a2)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e4b250e-0d07-478a-a182-12dab223e723)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         edcaf58b-d294-4584-9cc4-22bc924971c3)(content(Whitespace\"\\n\"))))(Tile((id \
         6ba8c0cf-444a-461c-b5f2-d42a1ec052a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         28866590-0210-4076-867e-a7af2ccadd91)(content(Whitespace\" \
         \"))))(Tile((id \
         142069d2-eaa3-4a68-89dc-460b4840cd51)(label(newCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f81b2176-6707-4d43-abd7-41d4f1fb9e4c)(content(Whitespace\" \
         \")))))((Secondary((id \
         dd1e0611-2fbc-4deb-8f46-ec96629d2fbb)(content(Whitespace\" \
         \"))))(Tile((id \
         f7a1740a-b9ae-48c6-bb1b-af7e905eb6cb)(label(makeCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ef3ce1e-060d-43a2-8b9f-f31332d57316)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a83c7d8b-f612-4908-8937-e8ba3c235fbc)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d940123-3b95-4cf1-9438-a9e725f7291b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         788103b2-b6ca-4875-a9ee-f9fede9ffede)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b6c298bd-db0b-4c6b-8520-d0fc8c4dd9d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adb7ac3c-af12-4134-b236-0b06004109a4)(content(Whitespace\"\\n\"))))(Tile((id \
         e37a1b58-5ac8-4cef-8b6a-3744cebdc790)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         78e38b0c-a4b2-4a09-b854-f19c62ff98aa)(content(Whitespace\" \
         \"))))(Tile((id \
         7de60d16-440d-4f89-8bbe-a096a85b6621)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         21c30bed-d131-4c4a-82cc-21d95ae12e56)(content(Whitespace\" \
         \")))))((Secondary((id \
         f4660cbe-6bcb-45ed-b44c-d838b126e887)(content(Whitespace\" \
         \"))))(Tile((id \
         baf58558-9fe5-4a38-94cf-f8b4d3d637cd)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6dc62883-ff77-43f1-a4f7-af67c39afbe1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91694289-b498-4b15-8492-5604e9899114)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2098a97-1036-44ea-a88c-b7f14dac36ec)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4bcb178a-7482-4ae9-8c32-f1ac472bb0f3)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46797bf8-e2dc-4278-a671-8e44e9c24fa8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7031aec-da7b-492c-8e00-efa240afaa90)(content(Whitespace\" \
         \"))))(Tile((id \
         a27f9364-e66c-4396-aa12-9c5bc84e601d)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbbe6e66-410d-405f-81bf-027e06bf2832)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         151bb988-6131-4aa5-8787-f72ab33fb342)(content(Whitespace\" \
         \"))))(Tile((id \
         86951df9-66b0-443a-8f22-c4fd0f3b5627)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89d358e0-5c26-4eb1-8be7-88483d678afe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b01da35-7f34-415b-95f2-0bfec0b3eef4)(content(Whitespace\" \
         \"))))(Tile((id \
         d93ff058-4a15-4919-ba27-9db816e772ea)(label(newCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c49d346c-462f-4a6f-a243-171e1450caea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         78aeb788-cdf4-4a03-9f66-d647285dadc7)(content(Whitespace\"\\n\"))))(Tile((id \
         b3342f77-8e55-477b-966d-e02f151d2f18)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         47218868-9471-45be-b497-03307fdd4fc3)(content(Whitespace\"\\n\"))))(Tile((id \
         b18af1b7-e7fa-4447-a78a-962d1f9d891c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0213a4f4-0fd5-4b3e-bd21-3fad9abe688a)(content(Whitespace\" \
         \"))))(Tile((id \
         81576458-9c72-4433-ab7e-fc80cd205f6c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d103f48-0663-4c07-bc02-3aee923e0127)(content(Whitespace\" \
         \"))))(Tile((id \
         7bdd58f4-dfb8-4c0f-99a3-04031e0bdbd6)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7b35526-2b65-44de-bf46-fcc910658ad5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa8274d1-11ee-42fe-bc3b-afbfe195d9c4)(content(Whitespace\"\\n\"))))(Tile((id \
         4afd55c8-7682-4b84-94ca-6a29d8bfdffc)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c07d8254-e26d-4079-ab4b-7882f307256e)(content(Whitespace\" \
         \"))))(Tile((id \
         afca86cb-2dad-4332-8c7c-de637bcd9522)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5fcfa96-da70-438f-9607-f18136cb404c)(content(Whitespace\" \
         \"))))(Tile((id \
         b6785d17-ad05-4f5c-b038-1a7ec0956172)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cd682c9-a7bc-4b65-9cde-ba6c48474024)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a5d609d1-aeac-40df-87ab-060c56fd0ba3)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da59a4de-8aab-4c4a-ac4f-de4a921d64ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ae31d68-65ad-425e-9f66-69f62c8bb10f)(content(Whitespace\"\\n\"))))(Tile((id \
         f70085b4-1c4c-4491-9d9a-2429a6b16847)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         68d39da1-9c73-439f-9cd7-c855bb69013f)(content(Whitespace\" \
         \"))))(Tile((id \
         64fe9638-1be0-4ff5-87f2-1150420a199c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48731971-be49-4aa0-90ec-ca6d79bd37ae)(content(Whitespace\" \
         \"))))(Tile((id \
         aa567664-9c86-4958-9296-3db40e714c2c)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a2f4098-9527-46bc-9ebc-24dc66b1b6cd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ab003ead-2e64-4076-a982-0191cdf9f408)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1de340c1-db28-4b76-bb92-97a1d0d86128)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1854699d-8cac-41d3-b65a-9e4025528078)(content(Whitespace\"\\n\"))))(Tile((id \
         8abd640b-bea8-405b-a4e2-02f4cc90c469)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         892715d8-2918-4af5-88fb-a1608739c9cf)(content(Whitespace\" \
         \"))))(Tile((id \
         d877a6ef-4ff0-4c44-af2a-2d27800ab362)(label(HarvestCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bc4e352a-9efa-4115-9f0d-db1c80a9159b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         6ee7f0bc-4c46-46a6-bbde-60ddc6c28619)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1d1c78a3-a3a7-4cfe-beb7-5063334ffc84)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6c451746-d743-461e-a6f5-5476fd3cd1e4)(content(Whitespace\" \
         \"))))(Tile((id \
         87bf44ae-f057-43ec-a79e-cab21ff0a9c4)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d488523d-22b1-4036-aed0-edfc5edd9633)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4efbe17e-b936-4d62-90ee-1394b788034d)(content(Whitespace\"\\n\"))))(Tile((id \
         6c20a482-56b5-4a50-95ae-68aa45eb6297)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1106c1ec-b98e-46a7-969a-dbeb9cb19a47)(content(Whitespace\" \
         \"))))(Tile((id \
         0ac95bb3-2d85-4bd4-8e58-7ab27a1119b4)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7f548df3-84a4-4664-a5bb-34d314ebff53)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4d181cb-346f-4145-8133-88f7eda6b4ee)(content(Whitespace\" \
         \"))))(Tile((id \
         b15e14a6-ef4d-4bf2-92d2-45faeb2e2201)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f710dc7f-841c-4271-b73a-583ce04ddaf5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ea235e1-3457-4bae-b26b-387de16b4a03)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f9545c8-02da-44ea-bf0f-96c91e737d35)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3622e249-b6b6-40dd-bbdb-8cefa14500ee)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07dd26f7-b35c-48dd-a372-3b4b513439f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2308947a-4795-487b-82ea-f1087124e7ab)(content(Whitespace\" \
         \"))))(Tile((id \
         2df16c5b-9628-4a88-9ad8-a8885b0a1f62)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f72a75a1-84a8-4ac1-abb4-a3b12b290466)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         202ac7b4-f07c-4239-a426-c1c4f22cfba6)(content(Whitespace\" \
         \"))))(Tile((id \
         cbee0ba9-e3c6-4a59-a420-03b5b0c924ed)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd203fd1-ab37-43b3-aa1f-94552d0d8174)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         706438ba-a30b-4566-8b7d-6faa7c13402c)(content(Whitespace\" \
         \"))))(Tile((id \
         50747952-579c-4dca-9ecd-8857aa5be4a0)(label(emptyCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9d88ae0a-1161-430c-86a3-0d00b558da80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c168b56e-6ce7-462f-a017-83f791e45001)(content(Whitespace\"\\n\"))))(Tile((id \
         a4ed15f3-382f-44db-ae41-aeaa28bf098e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         683952be-a26e-4142-baf7-c232d1d4ba75)(content(Whitespace\"\\n\"))))(Tile((id \
         e331150d-29f8-470d-ae64-79a86da22e9f)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f7d3a962-e610-4424-a310-860415b687d0)(content(Whitespace\" \
         \"))))(Tile((id \
         19edd009-fc5c-421e-bc4c-79cc8a6ea28d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         090fa67f-a548-432d-8922-220dd72bdfa1)(content(Whitespace\" \
         \"))))(Tile((id \
         786398b0-7d50-4190-8977-8edd6b9dc211)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e93e6e8c-847e-4728-81fe-fa5abb1d9e5a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95c96330-3cca-4cad-b0d6-69cd52c41711)(content(Whitespace\"\\n\"))))(Tile((id \
         e92f49ea-53b2-4639-a2f1-7d292021e7a2)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a7abcca-6bed-4ea4-a31d-49b5b1ee4020)(content(Whitespace\" \
         \"))))(Tile((id \
         85db0770-f010-4641-adf9-7b2451aee13e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8723eb78-6058-4225-be5a-1452d8fc6c95)(content(Whitespace\" \
         \"))))(Tile((id \
         b5d893d5-568e-4869-941e-592638ea9e87)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d2b2588-1365-4cb3-8e07-88c2a37a6dd5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6572013f-c71f-4bd4-a2ce-e517c0e3b0c1)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6adc6da8-c6f3-4456-8117-cea0f410acb6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7686b73-5b6d-4ea8-9e79-229b952c63ad)(content(Whitespace\"\\n\"))))(Tile((id \
         d63e48f2-c73d-417b-9c2d-d76791499f06)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44af0413-1dc5-4e54-ba76-bb594fc3a7f3)(content(Whitespace\" \
         \"))))(Tile((id \
         b4b71901-40ea-4c67-ac6d-ad08df3b3d97)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bc9f8f1-3267-43ba-b149-7dae64834fcf)(content(Whitespace\" \
         \"))))(Tile((id \
         d5cd7064-8bcb-4971-8585-81d3a5be3df7)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a62afc2-61f1-4d9a-9f2e-623ad73b8806)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7629d5b2-290c-49cb-8e28-1df2547b2933)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4888b58-cc47-4f44-8d59-d89b49a0904a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7b61988e-ecf2-49da-934a-8b9286c1286a)(content(Whitespace\"\\n\"))))(Tile((id \
         99b63e18-0125-49b7-abc1-ae5309ede77b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c8531e9d-cac6-4f2a-9184-8261113e3f15)(content(Whitespace\" \
         \"))))(Tile((id \
         efe86fee-d79f-4c97-8142-46b233707900)(label(CalculateHealth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f81caab3-40b4-4e25-8429-f14237e5bcfd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fdc65f0a-d737-467b-a850-31166ad9a2bc)(content(Whitespace\"\\n\"))))(Tile((id \
         89223408-247b-4f0a-9fdd-828fe075d3c4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b5ba2114-02e8-4060-9b9c-b4538ed869d5)(content(Whitespace\" \
         \"))))(Tile((id \
         ff70ed03-6532-4cb4-841c-7c2a6cfe0e17)(label(newField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8160843-1461-47f1-ad66-459652848f7e)(content(Whitespace\" \
         \")))))((Secondary((id \
         3af5fa96-d38a-48a5-b0af-9efc13c4da4b)(content(Whitespace\" \
         \"))))(Tile((id \
         f23df971-7c33-4888-a5cd-2af8250e37a2)(label(recalculateAllHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3504e6e-e237-47bf-96ce-0be9c6940984)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee293546-ff66-4fac-a4cf-e38b59af0b2d)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ff05cae-bd45-42d2-9a4b-3ca325404e14)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         027fc409-c9b9-41a5-85fe-d4ed2f45c8cc)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0768602d-c9e6-43b4-8d17-02b040db604c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5f1868f-b27f-4ff4-924e-dec44148a590)(content(Whitespace\"\\n\"))))(Tile((id \
         ed850a74-b0ec-4d1e-a0ad-9219640c4743)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         40922225-9629-4f39-a05e-72de386a36e5)(content(Whitespace\"\\n\"))))(Tile((id \
         bb5fbab0-10e5-41d5-9813-dd2293dc304c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         519e77fe-87ce-4ce6-ab1e-d387b06137df)(content(Whitespace\" \
         \"))))(Tile((id \
         1bd7f7c9-59f6-47f6-b412-d3a33e88d052)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09757fe8-8e5c-4873-99bd-f214fb01cbee)(content(Whitespace\" \
         \"))))(Tile((id \
         4d050a12-7a4f-4703-998c-032664026537)(label(newField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce42920a-41b6-44ab-bd5c-6b1d9d665182)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1fa931c9-9e35-438b-8010-b6efd951c007)(content(Whitespace\"\\n\"))))(Tile((id \
         0e1251f4-ce42-417e-b467-fcce42dc50ea)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8e7e1b8-b771-43ff-8459-3f1bf54f9e2d)(content(Whitespace\" \
         \"))))(Tile((id \
         196def76-9668-4dbe-8cd9-be84a4385c88)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1d912bc-d9a1-4a68-bbc6-cd19f1e17648)(content(Whitespace\" \
         \"))))(Tile((id \
         52d00bd0-9efb-4b40-9ff9-15a2509f1343)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         277d0d89-66d2-4b34-b190-97590892df44)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3c93de69-5126-4edd-a5ce-69f3f9b10089)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69a7610f-5576-48e6-af81-86348cf56c82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d792eed-760b-4a07-b0fe-c1661a8d02dc)(content(Whitespace\"\\n\"))))(Tile((id \
         857711f8-f062-4cea-9eba-5e80ab3eb412)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39a9e320-e7ee-4cb0-8183-c44e00342833)(content(Whitespace\" \
         \"))))(Tile((id \
         ec43cca1-b5e5-400f-9b1d-4aa47783a687)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98cb2411-8bb6-4736-a733-5f2498c8ec17)(content(Whitespace\" \
         \"))))(Tile((id \
         27940dce-1ddb-425c-88fa-1023d684a32d)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e6043d7-bd44-48f5-9bbd-3b44e140bb31)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         32e490f3-0e8b-44af-8eca-cf5ddea6bfb3)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14810b61-9404-4c8d-acc3-b174dd0050a8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1694ab62-810e-4866-b718-14187e574fdd)(content(Whitespace\"\\n\"))))(Tile((id \
         d599a43e-7361-4fe2-875f-a53473157dea)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         71ab7df6-e0df-4a03-b282-4bc002bcd0f1)(content(Whitespace\" \
         \"))))(Tile((id \
         257552ac-077c-48d1-9e52-47e75a06b47f)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c04d1e3e-57a5-4617-ad50-990179a25a36)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         8e4326bd-bf8b-4c01-b45e-ae6894bdd5d1)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1367fee5-cef8-41cc-afc8-e71765c5149f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15a87f07-0b45-49b5-b1fb-85eaf2544c77)(content(Whitespace\"\\n\"))))(Tile((id \
         f71d5675-ccb1-4f8a-92b3-71fa2e777f39)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b6f91057-a748-4023-b09c-c4ad2e80e595)(content(Whitespace\"\\n\"))))(Tile((id \
         f7089f57-5296-47fb-b9c0-a3efda7b9934)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         98f4df6d-c741-40f7-8168-921cab94c18e)(content(Whitespace\" \
         \"))))(Tile((id \
         22596e3c-e8f7-4712-b95a-7c2968fd4a19)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         584bc6b1-3a7c-4edb-ba5e-09775a6b0584)(content(Whitespace\" \
         \"))))(Tile((id \
         c8db6320-3911-4c4f-b364-d9038ed3bef4)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa0e4d30-82fb-4833-92ac-6357d6677b93)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2e552c5b-5aa1-4b16-9445-d29f34dec568)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66821c0c-4afe-411c-80ce-731ee8fb6063)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1854d4d-7b67-47bb-8495-9fd952e0cdfe)(content(Whitespace\"\\n\"))))(Tile((id \
         38e7ff77-a158-4790-a4e8-a293916f2ab8)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         50d035a7-9133-4942-a807-5c6bbbe9de75)(content(Whitespace\" \
         \"))))(Tile((id \
         285c9911-9b6b-4dbc-956a-43e9065f37ab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         953df3fa-c936-4d43-b6e6-f954b9a8ff3f)(content(Whitespace\" \
         \"))))(Tile((id \
         80da6b77-3f88-45f8-bb0e-8adb016cbdca)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0400c192-fca5-44b6-a3d4-41bacd3695c7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2560d12-c9b5-4421-9cd8-c4e95ce8133d)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c05fa6a3-c4e2-4a0e-a9fa-a5f5cc8f5e7f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1969293a-2f0b-4965-8eac-5c2f9edbe8f2)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f85f11a4-538b-47e8-842d-21c8832e6a44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76210c1f-fb26-4ca9-80ac-2a7add81d74a)(content(Whitespace\" \
         \"))))(Tile((id \
         31bcaaf4-d77c-44ce-ace7-df1ae2d8307b)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c6412869-cea4-460f-a636-282d195e79e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e7634de-5010-43d8-9ecf-f50dd204b722)(content(Whitespace\"\\n\"))))(Tile((id \
         4c3169e3-e8c2-406e-82fc-f4dff9e2b45d)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a602e9b1-32b2-4568-bfda-b29f49890e7f)(content(Whitespace\" \
         \"))))(Tile((id \
         8cbfb314-9292-44ee-8e60-7e0ebba51bbd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc883586-f581-42b6-b023-8db0f1ccc524)(content(Whitespace\" \
         \"))))(Tile((id \
         32f0fad0-c3e5-44f8-8187-a30e434095ff)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a38140e-90a1-460a-aafc-a060b767c1fa)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9af0755f-74b6-4f3b-b484-7c7b20074c2b)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b16dc38-769f-4913-b21a-6ec64eadb2f9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aee5df2c-2ce5-4c96-a42f-b89491cb14b0)(content(Whitespace\"\\n\"))))(Tile((id \
         8b7fc03a-7d77-4ef4-a110-5f98066e4471)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         faa4e9f5-bcd9-48c7-b136-0bc50e210848)(content(Whitespace\" \
         \"))))(Tile((id \
         fa301e3e-e77c-45ef-b4fb-56d321329cb2)(label(WaterAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e28e66ae-140a-4e14-8d49-2e9a57995867)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4e3799de-bba0-4e34-8360-c95c1cc8d242)(content(Whitespace\"\\n\"))))(Tile((id \
         6e20c9e5-13a7-4684-9c0d-b24dc53432ad)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0b982669-d6e1-4098-99e8-e88eca85227b)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8045a1-a3b6-47de-a31a-4d3e6c4c2b73)(label(wateredField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         86a50767-e31f-4b91-bae0-fe3827e23fbf)(content(Whitespace\" \
         \")))))((Secondary((id \
         01a805f7-2bd5-4cf4-98c6-78c5791ee2c7)(content(Whitespace\" \
         \"))))(Tile((id \
         35ee44ba-fb4c-49a2-a2e5-ba14e2daf72d)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8edafe91-094c-4b05-bf32-4e4b8f66b6c3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a0c5f483-45d7-46be-9ceb-11afce1ff779)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfcac669-7efa-40c9-be72-cf255d4fca84)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8fa301a6-eb83-48a3-aec3-5f6ad4718bf7)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7f971ec-8248-45cd-b8bd-4c3e0142fab8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6874c07-edaa-4c23-9cd3-18a708e36ef3)(content(Whitespace\" \
         \"))))(Tile((id ba417f83-b8d0-4fb0-b770-3c0bd9f44f14)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         36c30467-0807-48e3-a0a0-fb8599f92ae8)(content(Whitespace\" \
         \"))))(Tile((id \
         b94e9dd1-9fb0-4864-966b-aed79c02d304)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c0864a31-b5d0-4075-8e98-ab0f32140da5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75f83ae3-f582-45ea-86ad-4ec9f38a8efb)(content(Whitespace\"\\n\"))))(Tile((id \
         d623fcc2-cbfe-40e0-bca1-3493af3de70e)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54a845a4-e73c-4a86-9273-acde7484c6e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce2bbb66-5a9f-4439-81c1-408a24b688bc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab4289fc-57f2-4276-9ff1-7458ece20d94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03a13248-9c30-4eb4-b041-745ff96253fc)(content(Whitespace\" \
         \"))))(Tile((id bb7b23f1-e2a6-4050-a3f0-6b703c740df2)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         855537cd-7398-4e82-8662-c1200bff628a)(content(Whitespace\" \
         \"))))(Tile((id \
         469b5943-77a9-4a06-80b1-35b8fc5e2d05)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d05f5a32-60b4-4c1c-866d-b25abde67ca4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         95b367a9-e6b8-4d38-a3bd-c33aab65f619)(content(Whitespace\"\\n\"))))(Tile((id \
         231c048f-79e9-46d7-8891-da01b6cce76f)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5d4e609-2332-49a1-b5ca-ebf246a545b9)(content(Whitespace\" \
         \"))))(Tile((id \
         07c992e5-5dc7-4bae-85b8-93e94b13e95d)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cade594-d45c-4ba0-b3ff-1397af8f6abb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5cccf4e6-6909-4875-9d46-10687625bc78)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         026f62de-a6bd-460c-9237-2dd9323abf28)(content(Whitespace\" \
         \"))))(Tile((id \
         ab929d65-9df1-4f4a-bdd6-a8ea80f66ba3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0a35535-1432-461c-b137-f02c9c5cb109)(content(Whitespace\" \
         \"))))(Tile((id \
         23a042fa-961f-4fb1-a64b-89ce482db87f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7cc96aa1-241d-4d9e-b786-2d11663117b6)(content(Whitespace\" \
         \")))))((Secondary((id \
         d5f7760f-2587-418b-a32d-25c6a1a38ab0)(content(Whitespace\" \
         \"))))(Tile((id \
         d7d7e481-6558-44db-942b-ecae72677cd0)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8320f345-25da-4f21-b549-a8562ef938b7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3e4fa4d0-f6e9-405f-a560-b7f85fc2c745)(content(Whitespace\" \
         \"))))(Tile((id \
         82b5e426-8241-41dd-92c6-fe1e4e51974a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         193c2e62-43a5-4db6-ac07-6aae5baafe77)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02669467-7a11-41ba-9bc6-57b6b3136879)(content(Whitespace\" \
         \"))))(Tile((id \
         75d23402-579b-48c7-b18e-d50bf4847b66)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         586f0d70-613b-48e6-bb28-7cb676e74f2e)(content(Whitespace\" \
         \"))))(Tile((id \
         99f3b5c7-7cb1-46ed-a5a9-1b2a2538df76)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81cd7acd-af7a-40c6-ba98-5a1678cb0dce)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4cfd35a4-6152-4057-9c90-aa29b0e51f32)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14b271b8-eb9e-46e6-a031-eeb95eebbe97)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63765644-636a-4745-aadc-b7d2c2dcbe85)(content(Whitespace\" \
         \"))))(Tile((id \
         0f435637-2740-4d67-99a0-efb1ac7aa5c8)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         10b2f1ad-1510-48ff-a47f-586761001cf7)(content(Whitespace\" \
         \"))))(Tile((id \
         a2d41f17-4b7b-4297-b548-930740fe7775)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cac0e6ce-44d5-4e6d-9610-2a3960a6f0dc)(content(Whitespace\" \
         \"))))(Tile((id \
         cd996e18-43ee-48ad-ad19-fdac78d75010)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0948aeda-dbfd-43b4-a281-c119e08c7c1e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         45b04d0a-c9ba-4402-8d35-0cee82cc1711)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92455113-5b5b-4a87-baaf-31f40b43043f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8f8de334-5d08-4d5c-a654-32db6bd4aab6)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00edef95-9d03-44f7-8ed0-a742858e4184)(content(Whitespace\" \
         \"))))(Tile((id \
         bad4e0c0-c094-428c-b58f-33f2c07934d6)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         640119ef-1206-4d0a-b746-3a8128caabee)(content(Whitespace\" \
         \"))))(Tile((id \
         6d4abe8a-03d7-4dab-9049-f8fc8c4e5d32)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f7887172-c717-423a-8dcf-ea4497ca89cc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f5ebe147-fdd5-4744-ae86-82cf6d116345)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf41561f-91de-444d-9e8a-b5b90e80d77f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9c58001e-2453-4cf5-813f-bcae11315765)(content(Whitespace\"\\n\"))))(Tile((id \
         c54cf6c3-e3be-43f2-8e26-bfdc363c04db)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5107d80b-2cf1-431e-b5fa-e54839d60f67)(content(Whitespace\"\\n\"))))(Tile((id \
         983db0d5-8ee7-4af3-a534-efd33664c6d9)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b8b87131-faf4-491b-bf5d-ddda8af73a9a)(content(Whitespace\" \
         \"))))(Tile((id \
         d15aba90-9406-4bd7-96c2-af792c3b067f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72c7da3c-65d9-4391-b319-7ab6f84e9356)(content(Whitespace\" \
         \"))))(Tile((id \
         59f2030f-a1fc-42b0-b385-251000d3defa)(label(wateredField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b69b3769-9c2d-4b33-b431-c91c03c4e562)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bc0997e-91f2-4ea5-b25e-f8795c7bbc2f)(content(Whitespace\"\\n\"))))(Tile((id \
         3812e2db-d045-4640-9758-9b5b4d75e06d)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e5efda2-0249-4030-902e-e8762c43eedb)(content(Whitespace\" \
         \"))))(Tile((id \
         d47b20cf-6f53-4503-888a-9490328642cd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bacf7cc-d7bf-43af-aebc-ce03bdcf590c)(content(Whitespace\" \
         \"))))(Tile((id \
         2ad1ea47-3f2d-4de6-9c03-dcfde26df9f1)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67acf8d4-4e21-4a5d-b1a9-aa8c8dc0932f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e831ad76-71c6-4758-8943-fc88fa80f1e3)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b1706c2-caa9-4de1-a423-0472eb168f02)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a245222-49f4-42d0-b44f-ea9770f43382)(content(Whitespace\"\\n\"))))(Tile((id \
         8233e37f-8e9a-4820-8fdc-44a83ac601ac)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c04bbf1-db5a-493f-ad4c-d8d3456d1c1f)(content(Whitespace\" \
         \"))))(Tile((id \
         e0c12765-f4bb-4f88-8064-2d73704a9dd4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77320f3d-6752-4841-a6d1-83b0b5a3df37)(content(Whitespace\" \
         \"))))(Tile((id \
         1c162ac3-f1f1-4683-89de-45660515e59b)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce4450b1-555f-4e72-889f-7a93c6d70be5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         073558a5-40ec-4d40-bf7d-fab4692674ea)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d951371c-7414-473c-9018-2347d1d56861)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         41484934-5c43-4a9a-bab1-b0a3e49ab303)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7da6fa7c-a31d-4797-bd83-83b7ef34aff8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e7962ab6-29cf-4feb-a699-a44208327d51)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c5dc456-59ed-49e4-af59-c402a0d9633b)(content(Whitespace\"\\n\"))))(Secondary((id \
         74ca5c62-2f1b-48c8-9a90-0e954e6384de)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         ad93061c-b49e-4ad9-a2aa-441a3e694f89)(content(Whitespace\"\\n\"))))(Tile((id \
         ecd0e2a9-a575-4aca-b49b-cbd774c3a9bb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         776d1c87-ebf7-478c-9317-3cfebba6c31a)(content(Whitespace\" \
         \"))))(Tile((id \
         177aac0f-669e-47cd-ad3d-c94c9fbfe23a)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e834a1dc-d914-4cb1-8c6f-c4a186d8ed90)(content(Whitespace\" \
         \"))))(Tile((id \
         71345185-c5fb-490c-a6bd-67a274833f00)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ff0c3733-7ae7-4f79-b705-4e88eea27726)(content(Whitespace\" \
         \"))))(Tile((id \
         a5f1cbe1-455b-47fa-ab14-c2303c62aaf6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0c80595e-f852-4764-af7b-f6916ec61999)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         85a54b11-ed97-48a7-9589-7d1c1ede8e26)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         adf16adb-1d71-4e05-9e2d-1271a58b5518)(content(Whitespace\" \
         \"))))(Tile((id 69de5c4f-2206-48a5-b69f-83c254fb322e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         bfa5a3aa-b26b-49ba-901a-3a84843aa9db)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         1d7fd896-0da5-41e7-844d-4b4e858e783f)(content(Whitespace\" \
         \"))))(Tile((id \
         67bfa03e-80e0-449f-9525-e5db53afe34a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4f30e29b-2bab-4166-9491-237ad01f3605)(content(Whitespace\" \
         \"))))(Tile((id \
         5bb1a7a9-f7b6-4983-9929-d50b5fd98bdc)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         68b5294c-2b6b-4f94-b885-7ee45c5918eb)(content(Whitespace\" \
         \")))))((Secondary((id \
         55ef2341-5e8c-4e6a-b8dc-528815af7700)(content(Whitespace\"\\n\"))))(Tile((id \
         d6fd1f2c-ca33-4cfd-a872-c08b13d55e7c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a9c7ccac-60bc-4d19-9973-372141a6f05e)(content(Whitespace\" \
         \"))))(Tile((id \
         8b5144da-44e6-4e45-978a-b0ddc89bd7ae)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         12fb9049-4a2f-4b8a-a8d3-f01c8c068c48)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e28c5ed0-1c01-4503-bd13-681bf97e491e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         860590de-453b-4d0d-8294-3ae2a599cbbd)(content(Whitespace\" \
         \"))))(Tile((id \
         6c246a56-0f6b-44ba-ad1d-cf16995653f7)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9460240b-5d1c-4622-836a-a6af147b7e8f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f0317832-0e7a-41c7-a280-7a330263e3d9)(content(Whitespace\" \
         \"))))(Tile((id \
         4f36a8fa-6854-4380-984f-0b9da977b676)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c16930ff-51b0-4df0-8a76-e8e73e2c73d2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8170b25d-7444-4e69-a42a-70cddc5ba62d)(content(Whitespace\" \
         \"))))(Tile((id 0fa4c473-a62e-4865-8e4c-17725b46277c)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a0c10c2f-72a1-4229-8eab-6eca13ae0b6b)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         f214fd41-7093-4bf0-9e4a-577372f341e8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         39b46f8a-4433-4b7f-b69b-713c4468e950)(content(Whitespace\"\\n\"))))(Tile((id \
         cb34c7bc-1376-4119-857b-29c469dcf1b9)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e95ac7f-8061-4282-b0d7-9fb772ff2f23)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         722ba086-5ce9-49a2-8ccc-428474079c06)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eeafaa4d-83e4-4154-9e0c-538714a7250a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         279b7c83-d4eb-475d-bc30-2e937a98728b)(content(Whitespace\" \
         \"))))(Tile((id \
         df9db0a4-6485-4b07-a5f0-5677fd16f14b)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0c1ec9d-0289-4bad-bf95-4302b3e55fb6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4d40c95-b67f-49b3-9edc-1da267165128)(content(Whitespace\" \
         \"))))(Tile((id \
         57e1a6b1-836d-4cb0-ae3a-6237739697eb)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2c9425be-6908-4fcd-8d15-b0d86e9e0bfa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7a2ad808-1f64-46bb-9839-fb8349b89688)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f2d1b89-312d-4a15-9f7a-f5e3e6a9ed08)(content(Whitespace\"\\n\"))))(Secondary((id \
         41c7fa03-fab9-4d61-8ced-921c319b1ffc)(content(Comment\"# Helper to \
         get health at a position #\"))))(Secondary((id \
         01382705-babf-4d9f-a1e1-6712ecc1474e)(content(Whitespace\"\\n\"))))(Tile((id \
         70b9dd3a-d0ff-4ea6-b2a7-95ea1e50215a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6a1a55f7-ee61-467e-80e6-5c7445ca5fd9)(content(Whitespace\" \
         \"))))(Tile((id \
         308b610e-4b45-4e02-a193-92a72a2d7f46)(label(healthAt))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7cb36c3c-c33e-4fd9-9bbc-07523de0e0a4)(content(Whitespace\" \
         \"))))(Tile((id \
         0a2d5b04-addb-4a93-ac9b-d396c121582d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bba4f764-d30a-4391-9c9f-f4c1f8da8658)(content(Whitespace\" \
         \"))))(Tile((id \
         d24e3ba1-ff67-4d96-91ca-baf18b252955)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8beaee18-f74d-4510-897b-10bf54ff9d35)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         88004b45-c0b3-4925-995c-99882c67ca6c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         be48dc24-c701-4653-8d07-a474c19d73ca)(content(Whitespace\" \
         \"))))(Tile((id \
         1414cb70-d950-409c-8536-45c65af06f4e)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         abd250b6-1c0f-4bb8-9992-3669300db5a2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ef8e8055-d39a-422e-a63f-4d0d78d52f01)(content(Whitespace\" \
         \"))))(Tile((id \
         bb29aada-7e59-47c7-adbb-89eef01f7d8f)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b1e6c1c3-10f0-40f6-a67c-bdef33296fae)(content(Whitespace\" \
         \"))))(Tile((id \
         0a07eb19-8f12-45e4-b30c-fb81984bc335)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e8908973-ad1b-49cb-8ad8-c4daeb758bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         c3871e74-f27a-4f24-b829-d291d83857c3)(label(Health))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         90f39f83-63f1-49b5-8e44-3e03cec132d9)(content(Whitespace\" \
         \")))))((Secondary((id \
         15bac461-3104-46fc-84d8-dd793a180207)(content(Whitespace\"\\n\"))))(Tile((id \
         8ea584ca-6848-4219-bb2d-3270c74d514c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         88c67cf0-c26c-4d8c-bd95-b9a438832e3a)(content(Whitespace\" \
         \"))))(Tile((id \
         7bc058f5-43b2-417d-a9fb-9550e4d1fc97)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c0697dec-f073-4641-b74f-15e2df3c3991)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1ac6f52f-92c9-4e98-8cfb-abaf1442d97c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3ce4fce2-29ae-473a-91c9-8f9d3c4de0b2)(content(Whitespace\" \
         \"))))(Tile((id \
         b110e18d-5cbc-4c68-aed6-9cc3f7fff377)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b709484c-3959-481a-a34f-4aa65d0672a1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a93d1347-8c70-41e8-b100-89f88eefc862)(content(Whitespace\" \
         \"))))(Tile((id \
         bacb8f57-c0f5-4fea-ba11-e4b8d8fd87a5)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c8a56048-9f10-4c5a-81cc-642b804561fc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c428348-d431-4b2c-823a-2d44518cdbed)(content(Whitespace\"\\n\"))))(Tile((id \
         2858870a-d4e0-4de0-9198-3f7288bcb193)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         adf0cdd2-e32f-40a3-8798-3cbb0670598d)(content(Whitespace\" \
         \"))))(Tile((id \
         6525c6ba-a7f8-43cf-b087-1e4784783d5d)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a0fb2b5a-27ac-4843-8ab6-968f0e538ce2)(content(Whitespace\" \
         \")))))((Secondary((id \
         db8ccb63-c4b8-445c-b32c-1de98f770e75)(content(Whitespace\" \
         \"))))(Tile((id \
         eb03ba45-5788-453b-bf09-e8effba9714c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         51af3181-5b2e-4015-b111-dc259ab2368d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a8b81e3c-79a3-4f16-8b41-d12598a90001)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b58db08b-30c5-4f8d-8948-992332caa23f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d58114f3-c4c0-401f-96d8-4e95137bb78a)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35a3e380-e707-4705-a922-6cd3f7209639)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d09d863-49a2-45c8-a0cd-110f81b51396)(content(Whitespace\" \
         \"))))(Tile((id \
         0a3975d9-c064-4717-8cf0-e6fb98ffe564)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         267cd169-bc42-4b58-b7cc-7c47335c616a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e01b2d9b-7e0d-418d-bfc0-ba271621a27c)(content(Whitespace\" \
         \"))))(Tile((id \
         ffeba485-262a-4757-aac7-4b5ffe845c51)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5634442-c004-42ff-b103-fbcd1e544fa5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3209e8ac-0eec-4233-9459-48de9fb57046)(content(Whitespace\"\\n\"))))(Tile((id \
         eea1c3ca-7c14-4207-8bbb-6c548790560c)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78f4c947-f209-40b1-9cbe-2640a133b965)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a53d9c63-e8fc-4d22-93fc-f3d9ecd5e434)(label(health))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd556dea-5288-449d-a1dd-22d5dd2f3a2d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4fc21f8d-9032-4253-8dc4-bb9f821ea8a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         56451c84-47a3-4ee1-a2b8-c38e24e39b74)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab3ff7fa-a0e0-4ff0-942c-36763847061a)(content(Comment\"# Helper to \
         get crop at a position #\"))))(Secondary((id \
         940a5e30-30dc-44f0-bd13-6889c1891946)(content(Whitespace\"\\n\"))))(Tile((id \
         2f06cc62-ebdf-4f9a-b88a-223c42d2c6bd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d6807bc-de5a-427b-a31b-492e6bfdaab1)(content(Whitespace\" \
         \"))))(Tile((id \
         20ee6220-7f0d-4fd7-a74e-f20338ca85cb)(label(cropAt))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dbcdd282-0873-48cc-84ab-82b8c4978438)(content(Whitespace\" \
         \"))))(Tile((id \
         53949160-11f2-4677-b735-1d434b433b81)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2158455d-4228-47e7-b02c-a36e96ce5d47)(content(Whitespace\" \
         \"))))(Tile((id \
         2b474c2c-c2ae-4f9c-b17b-6d11e85e1148)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         075346f2-03ff-48e6-8683-52fa35f7733f)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6643c36c-1c87-48f4-a527-624bff20a739)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         371ab7df-bb9b-4b4f-83a3-4cdf6c44984c)(content(Whitespace\" \
         \"))))(Tile((id \
         cd94b1d2-4245-45d1-a34a-e676ef1fe09a)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7d146278-1314-4d95-88cd-1c89ba8c1be5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fdaeaf08-4d16-4bd1-bede-1184b45a63e9)(content(Whitespace\" \
         \"))))(Tile((id \
         41354018-c67a-427f-9630-bf0adeda52c0)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a7db8efe-f704-45e4-aea4-cb0bf7635481)(content(Whitespace\" \
         \"))))(Tile((id \
         dcc84abd-472c-48bd-8d02-6385203037aa)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8ce0168e-04de-4a26-9ef9-fd04e0a49ba7)(content(Whitespace\" \
         \"))))(Tile((id \
         1d4d9165-fdeb-41ad-8cc3-42a9309b64d0)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         263e4699-ad9b-4316-9dac-5ac6080114de)(content(Whitespace\" \
         \")))))((Secondary((id \
         0bb7cf87-b4d6-4710-8231-293b9dde49ce)(content(Whitespace\"\\n\"))))(Tile((id \
         32eaa7d4-bb59-49d4-8a15-84d346707e97)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9ea9b52c-af3d-469a-9fa1-00efbbab1af0)(content(Whitespace\" \
         \"))))(Tile((id \
         e8b09fe8-2f1c-403f-871c-304a50841dc2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         54b7c53b-4a67-4795-9540-931d77b27ad2)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         91cdb43a-2f3a-40b9-bbc9-58c889cdcdf3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7a421da8-3583-4b98-b4d0-6cfaab6fda4b)(content(Whitespace\" \
         \"))))(Tile((id \
         2b7eb75f-5292-49bd-a7c7-04e20e64e252)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0ddd8e2f-f27f-4920-9727-c504967f3181)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6be910ad-b0b0-44b0-aca3-6abc4137f2ff)(content(Whitespace\" \
         \"))))(Tile((id \
         945cd53f-b9c5-41f6-b9aa-f5ec990d3a15)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2a9a4d9a-47a9-4136-8b82-c3ae52179382)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c5ea1411-3bca-4fe7-b0b3-068c6826ca89)(content(Whitespace\"\\n\"))))(Tile((id \
         25ff0b03-6da8-4ff9-baab-685b5a9f9b8f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4815968e-5ed7-47dc-9542-6458d175a240)(content(Whitespace\" \
         \"))))(Tile((id \
         ec1a1cc5-14e4-4f17-875e-ae1b7b1c91b4)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         36dcef19-f133-43ad-9d75-90258592f783)(content(Whitespace\" \
         \")))))((Secondary((id \
         7a73cd42-cd2e-46bc-8d62-35f6eb75c89a)(content(Whitespace\" \
         \"))))(Tile((id \
         58bd3fa3-df04-4309-bf28-b1e355ba9c83)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         def3ec1a-2331-4ded-a520-8c1589248239)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c7974d8-554d-46ff-a0de-064ff60f9c0e)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7de8f577-0c43-48e9-80f5-6a4bec6a4a35)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         005be826-5762-4b80-a01d-769010040fe8)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97dbe65b-16b1-40e4-8678-db45e22c1ffc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d4fa75d-c7a6-4add-b847-ab67c8792c52)(content(Whitespace\" \
         \"))))(Tile((id \
         5907eeef-4f82-479f-89d2-6bb6bd22f8ed)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0882bcf-3f10-4de2-ad32-8780d8ec5d23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d62a607-7428-4b8c-bd0f-bc269ae7e5e6)(content(Whitespace\" \
         \"))))(Tile((id \
         62d525b5-f07c-4b70-a718-7f368745d18f)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         105d6a72-a52a-4584-94fa-f96f70412027)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5e06bff5-069a-460f-bc65-39c464c077d4)(content(Whitespace\"\\n\"))))(Tile((id \
         cd79e112-7927-4855-82a6-34ee93db3466)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eae69cc5-5b73-4771-963d-c7127f34b291)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cfa529bb-ded3-4368-a2a9-51bd909edc52)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9126857d-b71c-461b-8696-056c0db735a4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6399e454-786d-4e75-a904-46e07a8bb663)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9a628a7-94f4-4f66-9f69-8c1e1af280ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2f7d059-739b-44c2-8d47-3e57090edc80)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         f878e05c-8728-4122-8b13-0b05c2f692e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0af29f79-2854-4601-ab7e-a783b9a87d3f)(content(Whitespace\"\\n\"))))(Secondary((id \
         18e8dc47-a6ae-4d41-a6d3-e009694b7efc)(content(Comment\"# Basic \
         planting #\"))))(Secondary((id \
         3c80ba56-e2d9-47e7-9645-0816cfcf3825)(content(Whitespace\"\\n\"))))(Tile((id \
         d05c5d63-eeb8-4947-9558-35ebd0003481)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         25f6ecc6-a287-4cca-8d99-55d5fa233929)(content(Whitespace\" \
         \"))))(Tile((id 1222760e-bd58-40a8-ad40-5698df336d04)(label(\"\\\"can \
         plant a crop\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7df74475-becd-4250-9080-eabe9c79a1a4)(content(Whitespace\"\\n\")))))((Secondary((id \
         715d1a7c-f171-47f0-8b7b-8150eae72c13)(content(Whitespace\"\\n\"))))(Tile((id \
         98e57960-027d-4277-aacc-7bfaf0f2d442)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e696324e-13ca-4325-8a83-2da98cc8da56)(content(Whitespace\" \
         \"))))(Tile((id \
         ded9d663-3f63-4afd-a583-68f21bd27103)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25ecdf4b-80ef-4a06-b362-41da3e5ab903)(content(Whitespace\" \
         \")))))((Secondary((id \
         02a0d2ab-ec84-4918-aa25-85e842c79583)(content(Whitespace\" \
         \"))))(Tile((id \
         d883e8bc-edce-423a-8cab-9685a89e47aa)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aff79af9-2551-4377-a048-04d3ac07b73a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5150ad66-03e4-4ce1-8914-401e7b6dbea7)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76910c05-ecd5-4d3a-97d2-2d95bd1f6cf1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf220f82-5fea-47ee-b1ff-7bff5a4b5267)(content(Whitespace\" \
         \"))))(Tile((id \
         59bb5446-af8a-4ec0-80dc-936e86b6b717)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cff5ccb6-a8a9-4670-8b6d-865f711fd3f8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1fa3d165-c6e3-42a4-9530-146b17a11eed)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3dcd5bf-3bb0-46e8-ae1c-7479eabf72de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1bb4568-2177-499a-8ab8-0702546f9a04)(content(Whitespace\" \
         \"))))(Tile((id \
         76235c38-6383-4b2b-bc68-0e9356c0d9c7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         21a10f6c-c357-479c-9056-9c6dd9728f6c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d40df312-35e9-47c2-9ade-ad7ca0812603)(content(Whitespace\"\\n\"))))(Tile((id \
         b6468204-d1dc-4294-8533-f645f1ca1925)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a20d4bd0-098d-4395-ab9b-b18a0e0b9701)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eab92ab4-813f-49d7-a459-3e392d18261b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31856adf-762e-4a61-8966-92d4dd0e3cf2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         791bd177-d008-45f2-a688-a8f66afa48fa)(content(Whitespace\" \
         \"))))(Tile((id \
         512f90a2-7744-477e-8b09-bee23a343f23)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae4b87b9-2584-4dc5-b229-051c28f3ba05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         332359a6-fbe2-4139-8e95-f49f8575b802)(content(Whitespace\" \
         \"))))(Tile((id \
         d4c8c525-1ec2-43f8-9a19-47f2029c3c33)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5d15b7fb-52cf-4403-827d-91a34b19c533)(content(Whitespace\" \
         \"))))(Tile((id \
         b2b5da63-6533-4e41-8a19-b8cd8fc7a61f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec328423-b6c8-403a-bf38-47919a77524e)(content(Whitespace\" \
         \"))))(Tile((id \
         cd84d495-a37e-4922-bdd1-a4c9e8165809)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f69255e-25f5-4219-b8d9-3e443beaca0f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         24374a30-befc-44db-ba63-7973cd68a279)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         376193e6-156b-412b-ac0d-5053a513ebf0)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd5547a9-6599-499f-9a53-35acfef4c981)(content(Whitespace\"\\n\"))))(Tile((id \
         75b9b60d-a785-483c-bde3-8cd743c2e54b)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a1dd5e8f-ef1c-4aae-a4bc-935f17ae6071)(content(Whitespace\" \
         \"))))(Tile((id ed1cd64d-be84-42aa-9051-b14b0f3e9587)(label(\"\\\"new \
         crop starts at base health\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43aad0b1-01d7-41b3-b345-13be7569b3d6)(content(Whitespace\"\\n\")))))((Secondary((id \
         28f6367f-148e-4cf3-9a0f-26a4987eba62)(content(Whitespace\"\\n\"))))(Tile((id \
         57de9043-d174-4eea-86d3-8f4a0e4cf499)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         701d5005-76cc-4d4e-9545-ff12cf8a84d6)(content(Whitespace\" \
         \"))))(Tile((id \
         3d8bb5de-2645-4cac-be91-cde1c16d3e57)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         17128fe7-0793-4921-b299-46ca479dc43e)(content(Whitespace\" \
         \")))))((Secondary((id \
         0e23eb2a-ca25-4d4d-a331-8a51b2601017)(content(Whitespace\" \
         \"))))(Tile((id \
         318dacba-d3be-469d-902a-465ae05b2c23)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8358befa-b0e7-4fa2-ac06-3982e9588a41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         37666157-89f4-4670-ac7d-7f63f8f8c4c5)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b5553fd-6f8c-4e72-bab7-2b953dc965dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d6313b5-db05-43f7-ac94-cf7baae6d7dd)(content(Whitespace\" \
         \"))))(Tile((id \
         a0cb239a-0ace-4cd7-a688-a169b75a8fad)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1905b8ef-6e60-4763-8e79-a736db45636d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c471e6e1-d1d1-49d2-8821-a7dabbc5a612)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7bb92b6d-7d4e-43c2-99cc-6c30817b1957)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e98e636-86e2-4fea-8d2f-b2c313788c18)(content(Whitespace\" \
         \"))))(Tile((id \
         cfe431f9-cb85-4d24-b04d-9f6201fcd0ca)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         718ff5d2-3fba-45ac-af96-77d483d7f916)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         535cfd54-24d3-48c9-b49d-a31df76cf093)(content(Whitespace\"\\n\"))))(Tile((id \
         12d98aef-6b1f-4f3a-8cea-92d4d43655ca)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ddac031c-1dbf-4b2d-9739-23c784db2bdb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         96e958cb-07d0-4f13-8415-5364715766c0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad141db0-eb7e-44e3-94e2-39077466c288)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b347b1d-6220-440b-900a-0e9df04719f9)(content(Whitespace\" \
         \"))))(Tile((id \
         e60d9c0d-591e-4b14-858c-70482d22fa9a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b63aa615-5eed-4c4c-aca1-08858cff859d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6acb8ccb-258b-4bc1-98bf-89f1efc98667)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8ec439-859c-4c02-8c95-936cb07c7110)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb288fed-f8d8-47f8-bc92-f31d5480a0c0)(content(Whitespace\" \
         \"))))(Tile((id \
         fa707e98-4870-4467-a158-dd38ae32785c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22d6a5af-6f70-46b5-a2c8-3942ed6b6141)(content(Whitespace\" \
         \"))))(Tile((id \
         06799777-4076-435f-a91e-5e40dfc43b2e)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         604bf244-848b-40f1-8e70-fb2b10ed2e62)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bd6648c8-5e44-48bb-920d-8b036d4b0ca2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7f6c5ab-e3f3-4cf9-9901-82e341db3c25)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0d37805-7f75-4d9e-846c-20bdf3277f83)(content(Whitespace\"\\n\"))))(Tile((id \
         08291a14-c521-4a3a-94c1-1d7b7e12662f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8200532b-251a-4852-a163-192610496731)(content(Whitespace\" \
         \"))))(Tile((id \
         7d7c1a4f-a322-4e2e-98cf-60b06d48302a)(label(\"\\\"cannot plant on \
         occupied cell\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c068f35-fc16-4a03-a21e-f71db15c61ba)(content(Whitespace\"\\n\")))))((Secondary((id \
         3902d8e4-89d9-4bb1-be39-81e2216f0e9b)(content(Whitespace\"\\n\"))))(Tile((id \
         d7412c5d-2c40-4f6b-bc3d-3cae9c14ed0c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ab706f6e-aabb-49ba-a57a-fb50a5dc127a)(content(Whitespace\" \
         \"))))(Tile((id \
         c1613609-7582-4c1d-871c-8e126c6cbf04)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adfec553-c8ab-4f17-91a1-b779298b553e)(content(Whitespace\" \
         \")))))((Secondary((id \
         b6086092-8f9f-4a55-9a4b-22f1062fbcaa)(content(Whitespace\" \
         \"))))(Tile((id \
         dd4f555d-8a60-4914-a6c4-2623c9b23724)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29b924ef-72db-4e88-9eca-e3b5066d2fea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b16087f-6287-4e7e-a56b-50ff4ef16279)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df6717c0-a2bc-43c1-b9d0-1aaf8d8b1a2b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e91776d0-8d02-416a-a10f-fedd4028a32a)(content(Whitespace\" \
         \"))))(Tile((id e948891d-c922-4d1d-b97b-1b0466c1dda8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         38d619e9-9c6b-4d25-bea6-632b5022a74b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         570d4337-4a6f-42b8-9b9c-43c6e92a1da8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f710d205-13ce-4bd3-85ab-d68a6865216f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         001a367f-5f0a-4b57-a6de-2691e9e917ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3320fa44-ea75-4547-b3b2-b50eb49d6636)(content(Whitespace\" \
         \"))))(Tile((id \
         2bc39e21-1fa1-4bae-8b2c-b73878aaf555)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ae21b56e-38f0-40c7-b83b-e435253a60c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         328476a5-6cf1-420b-8fc9-68d6709b9e1d)(content(Whitespace\" \
         \"))))(Tile((id \
         f2396423-7669-490f-84bb-6b41b2295aed)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         372b14fe-ece6-4051-b271-9f9546e0cde8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3741245-789e-4176-b6e0-3e85c99b77b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4879d46e-fa86-4d0b-bf91-68a553077729)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c63d9c1-eb25-4eb9-8661-4cb1e3473e61)(content(Whitespace\" \
         \"))))(Tile((id \
         71f7ad17-f003-4c53-84d7-ff22bc486870)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f74c7fb6-bb3c-4a9a-99ef-3106c2aec1af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bb25347a-fe68-48a3-9ac4-d5b8ae9f837a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         096035ec-9c5a-4606-aea9-4d0f7650dee8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3118da70-e36e-44d0-acec-42711784d1a7)(content(Whitespace\" \
         \"))))(Tile((id \
         2d4fc731-414e-4212-b68c-60b8a168ff92)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         85013d73-01ca-4d8b-a570-74919a3fe9bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ffead5d0-326d-43e8-aeeb-8c48f755411d)(content(Whitespace\"\\n\"))))(Tile((id \
         9a35d69b-0f9e-4fec-a0c0-7e9f276252ea)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         507cfe4e-f2fd-428b-8507-3eca9a3b6c2b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         958d33bb-7507-495d-a07c-c8e3ef90cd76)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32987b5c-487b-4ab7-ab88-78bfa0a28f03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1f46b9d-57db-455b-81b3-9740f6b0f5a7)(content(Whitespace\" \
         \"))))(Tile((id \
         f2e87c24-ae7b-46ea-ae2b-5975c4d3d77b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0f10b15-df69-49d4-9f6c-75dea338cea0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d48f5b21-073b-4815-9a60-c2daf014c5de)(content(Whitespace\" \
         \"))))(Tile((id \
         fb6b7c2d-d459-4166-b9f4-a8e68e740226)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9dbe07f8-b312-4b35-a13d-a790eb51ff8c)(content(Whitespace\" \
         \"))))(Tile((id \
         f545db9b-5b56-4029-8b8c-826b989d8604)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         656d4d3d-5743-4c55-bf09-1d230c9b3a8f)(content(Whitespace\" \
         \"))))(Tile((id \
         8d58953f-e1e5-4fad-a704-aeb996527064)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18df421f-487a-4678-b795-6d4d4bbc21eb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         353b8214-c596-414b-b177-8c2941d0da6a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9645654-d868-4e55-a7f9-f3cdfe002652)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd7ac705-6475-47b0-b00f-2aa32ed9559c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5aedfb5b-1987-492c-a887-fbacc5322dc5)(content(Comment\"# Seed \
         selection #\"))))(Secondary((id \
         980ac7eb-5fd4-48b0-9b05-73937c8fe9a2)(content(Whitespace\"\\n\"))))(Tile((id \
         a441c100-8e63-4cc8-a788-2e4be42c2647)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3bab90b4-6085-46fd-a795-b19387ab1320)(content(Whitespace\" \
         \"))))(Tile((id \
         310ac8e6-4f19-4d71-821e-e595b5c1fd62)(label(\"\\\"select different \
         seed\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1875690-cc90-43fc-88bc-16f3365d3bd2)(content(Whitespace\"\\n\")))))((Secondary((id \
         6c33eab5-7eba-4d66-bc29-005920297f73)(content(Whitespace\"\\n\"))))(Tile((id \
         48eb89ff-ae1c-405e-99b6-3ef3ea775816)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c6297e54-318f-48c0-a908-30db1fa0ef8b)(content(Whitespace\" \
         \"))))(Tile((id \
         602de473-918c-43ba-b280-07c67b49fc59)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7ebb5c76-79fe-45c0-a4e7-3fdee5520047)(content(Whitespace\" \
         \")))))((Secondary((id \
         140ab108-8c8a-4b39-a3ac-70eba618d68d)(content(Whitespace\" \
         \"))))(Tile((id \
         df7b7db5-19a9-4576-a99d-50d5218432c4)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2ea8221-7ed5-4d1d-87c4-c90500ae3cd9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a7c952d4-c11f-432a-816d-7ff75348eee0)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7406a34-95ae-4547-a100-eae91d080b9c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e21bc108-e45f-4c43-8608-f4bb16e3cc4b)(content(Whitespace\" \
         \"))))(Tile((id 80f40167-9220-40be-a5cf-296e0d1bea73)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         536ee652-c9b5-4266-9577-8d600856ec49)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a455682a-f669-491f-84b4-a00147294608)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0bace1ab-741b-4638-9282-c5fcd2c8fc73)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4b9604bb-5380-42b9-9304-8c50723da212)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4db78ac7-2e99-48d2-9d18-de0f41953f62)(content(Whitespace\" \
         \"))))(Tile((id \
         036aefcb-0973-40f9-900e-2dfca0adab3a)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f41f643e-7ba3-4a4e-bcbf-d117400292e4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0439835c-644f-42a5-9cc1-0b8624442997)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c045eae-7714-4ab2-84f3-b8b3d91c9e26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d0e34fe-4543-404d-abc8-188c822413c0)(content(Whitespace\" \
         \"))))(Tile((id \
         fa581618-22be-4367-b215-d782e071ca6d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         7a32caa0-5de4-4c64-a472-34db90315909)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d85a5867-0a82-4d37-b098-5e02244b7ef7)(content(Whitespace\"\\n\"))))(Tile((id \
         fc833dc4-ef53-414d-84f2-803eeb81d6ec)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b06ae45a-3b7f-4270-b0e4-3c6de57c9e9b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d190c11a-0734-4f50-a857-5791d5ccccc9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         205ad2e1-4637-4ccc-9187-998fc2cafc96)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad860156-4b53-4104-9963-6e1854f05b19)(content(Whitespace\" \
         \"))))(Tile((id \
         895e745d-efca-429b-9c8b-8933e2469a17)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         571eca91-0e63-403d-9d9f-7225a0710e3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7696a02-2fa6-4cab-ad79-de75671206a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b951fce1-e1bd-4bd2-8ec1-d24fc48f5f3d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3ef0ada7-49be-4d16-a29c-32aa0fec071c)(content(Whitespace\" \
         \"))))(Tile((id \
         c5ae8ffc-9134-4d76-b154-d56bcd2155a4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6304a01c-57ec-4f10-8d49-78a6e8129c08)(content(Whitespace\" \
         \"))))(Tile((id \
         c3b83f15-9e3f-44af-8e18-5d25bd42aed7)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f37a90c0-27c4-47c7-8cd4-3641ae264775)(content(Whitespace\"\\n\")))))))))(Tile((id \
         12ad32b6-2a19-4782-a523-115c5a914170)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8ec5b07-7fd1-465d-a586-334cf4e989df)(content(Whitespace\"\\n\"))))(Secondary((id \
         0aec4b9f-d70e-431b-989d-978bbf3f9964)(content(Whitespace\"\\n\"))))(Secondary((id \
         292f708d-4390-48b2-92d8-5b9f9abbcea3)(content(Comment\"# Harvesting \
         #\"))))(Secondary((id \
         04b45184-84bd-40c1-93bc-293dba78ccb4)(content(Whitespace\"\\n\"))))(Tile((id \
         538b2aec-7676-4f68-8c25-cfca2b4830f2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         06cccff1-79fa-47dd-826d-6a6316198d41)(content(Whitespace\" \
         \"))))(Tile((id 3380ac2c-9a3b-4e7a-95e7-78d09e7c970d)(label(\"\\\"can \
         harvest a crop\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1af42c79-ccf2-4363-8a58-fe51e87ae9e8)(content(Whitespace\"\\n\")))))((Secondary((id \
         4a076d0c-6056-4198-912d-3249d1a41fad)(content(Whitespace\"\\n\"))))(Tile((id \
         5f57fc53-b181-4946-ad00-9c54eefb9c43)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         92b752f2-c266-4c93-805a-9c25290e0152)(content(Whitespace\" \
         \"))))(Tile((id \
         2a189b74-7e64-4813-9766-51c3e11713de)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c51dbca0-c231-4ed8-a5be-1fdf7c6dbb8e)(content(Whitespace\" \
         \")))))((Secondary((id \
         c8faddce-df6e-4dd4-8dc7-d9b328188a8e)(content(Whitespace\" \
         \"))))(Tile((id \
         0e541ca3-dcf0-4b5f-8703-80a387347335)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7531308f-108a-4e3e-a161-44c878f9736f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ecfe3c08-377a-456f-aa3d-59fe26e56762)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e7d4a24-296d-4956-bb8d-173b4bf3ab1d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4089e0d-2ca3-4ca7-9e82-42cffaabe3a8)(content(Whitespace\" \
         \"))))(Tile((id 26254258-f9d0-472e-a09e-a93e2abb2d8d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3c8d01b-66df-4848-8865-a31565c918fd)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15ee7ab8-0f46-4669-89bf-6166b1a14040)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c483c03-1baa-4faf-9bd6-2c969408a5ab)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb5e8813-453c-4c7e-8f27-f4e0580ac0fa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f02925a-e99e-48e1-acb8-259332b0bd38)(content(Whitespace\" \
         \"))))(Tile((id \
         02f49908-a7fc-46b1-8a17-ba0dc1a0e324)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cea47353-4a2a-4d9a-8346-3da7d65c6329)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f764dbc6-2545-4371-883d-d61e1bded436)(content(Whitespace\" \
         \"))))(Tile((id \
         ff6ed3dd-0e7d-4425-be3e-e601e79f763c)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         357f5b00-55df-4b12-a3af-bbff0395bce9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         191ec7b0-da0b-4b00-abc3-8df33e92d968)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba1111e8-45e6-453c-830a-4952854ca32b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83f09c21-192e-45ef-92dc-2676b5391135)(content(Whitespace\" \
         \"))))(Tile((id \
         8e978127-7a93-4078-90c8-0bf243cda4e8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         163a09a5-e9d7-4ce6-ad9d-13d6f2df15ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c72d5012-6686-441f-93b9-a5c78d08d2da)(content(Whitespace\"\\n\"))))(Tile((id \
         1779bb52-2a54-4269-be5e-8468db2eb711)(label(cropAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca3d8765-51ac-4726-a3c7-3b40b0c22e10)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95d29c3f-0668-42a6-8694-0156657b620e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e232693-37f4-438c-b0c4-75c8de7a089e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1784b75-df00-4a78-84a8-16fe98456ccb)(content(Whitespace\" \
         \"))))(Tile((id \
         f862d17e-b881-4d7e-9334-c91a909cc83a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb92b9bf-69ec-4c94-b9ca-a8d7f3b8bc7e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bed4ae13-95e3-44ef-ac11-b0d3f0044705)(content(Whitespace\" \
         \"))))(Tile((id \
         80a152d3-cfb2-4a15-97be-a47460ba73e4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         82aacdf0-9ad2-4f16-a300-a099474a97d0)(content(Whitespace\" \
         \"))))(Tile((id \
         ef109f7e-5a44-486b-80be-8b861f0fec03)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a81015c4-0fdd-44aa-b88f-b60adfa97563)(content(Whitespace\" \
         \"))))(Tile((id \
         99f6660e-35ee-4bf3-a1e5-aab2746d91b8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1df0cdff-069b-4a5a-88c0-4e037216b7ec)(content(Whitespace\"\\n\")))))))))(Tile((id \
         dc263ba2-160a-4f5c-a831-8ea333968e44)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff42a727-1cdc-4cf7-9f19-51adbc8c4d50)(content(Whitespace\"\\n\"))))(Secondary((id \
         59b6caa7-5eee-449d-bb60-fb92adf4e44e)(content(Whitespace\"\\n\"))))(Tile((id \
         270f9456-10c5-49e4-ae78-17f786df5f47)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d96d1b80-3fe8-405f-b3b5-e39938b60a57)(content(Whitespace\" \
         \"))))(Tile((id \
         65038249-5fb8-438c-a6dc-8f3ce881dd1d)(label(\"\\\"harvested cell has \
         0 health\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2287af22-c8e9-4a06-a1d4-2f5070ec6000)(content(Whitespace\"\\n\")))))((Secondary((id \
         2e5e1a8e-bf11-4021-8065-9746770ab520)(content(Whitespace\"\\n\"))))(Tile((id \
         0a99a954-f1a4-4a16-acb0-ac88456dd1d0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d618d6df-6a0a-4f07-987f-bc46fecd3240)(content(Whitespace\" \
         \"))))(Tile((id \
         a44d3062-95ce-428f-9739-8c7c05814b34)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1ed03cb3-5dbb-412f-bdb1-b3055a77961b)(content(Whitespace\" \
         \")))))((Secondary((id \
         b6b6d8fe-43ff-49bb-9a06-e13c6bf6aae3)(content(Whitespace\" \
         \"))))(Tile((id \
         70bc642b-8dba-43d1-93aa-6a566eb72a6b)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78a6fb15-e28e-4747-9303-b800dc637c17)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bbe9f889-99e1-4f5f-8e03-d5ed0e8a2355)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18bfe1b5-ea48-4fb8-82b4-f1fb0a4cdfc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c11ad01a-c16d-40cf-9b89-e006c75c575c)(content(Whitespace\" \
         \"))))(Tile((id 634aea8e-dbdb-47dd-84dc-588787cf6e47)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7d18204-cba6-42d9-aa6b-140f8ae6fb67)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e29d7798-0e5f-4f12-a408-41a583cfcd3e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6b59fed4-5105-48fb-bc76-018e1eb7f579)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6fa0da8-7719-4ae9-9dfb-c4202edf2df9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         851a0c17-9add-4168-8b1e-08cce0041443)(content(Whitespace\" \
         \"))))(Tile((id \
         152b12c3-331a-4eee-a59e-ea18ce26d6ec)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         de03161e-9dc8-44cd-9861-d1ec4a4e7eca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         339ab95b-9ce1-4bc7-8d6e-8203d80c16a9)(content(Whitespace\" \
         \"))))(Tile((id \
         21f7dd31-d9b6-42c9-b947-7c5198c40924)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82e9c3b1-f5f1-42bf-9877-661fe53dcde4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a69738ec-8bc3-4caa-b483-e5289f9a828c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a0fc7e4e-a604-444b-8a3d-44c5cc4d97cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec9ae442-0f7d-437c-9683-c678b2cc8e38)(content(Whitespace\" \
         \"))))(Tile((id \
         0b565da8-77f6-45c1-b4d2-39198ad4c847)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         22125f38-47f6-4053-bc3d-901095f0cadc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         11b7a966-57c7-42bf-9ea8-2cd042fe1822)(content(Whitespace\"\\n\"))))(Tile((id \
         dfd54894-5006-427f-acda-7d9e906de136)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3059fafc-3b0b-433c-b9c0-bdbab81c988d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         09a3bcd8-b552-4057-a42e-0c8bee52004e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f091b1a0-9351-4b08-b584-3245e9c2f983)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82f58805-67d3-4a6a-b81b-7e24bd67b5ff)(content(Whitespace\" \
         \"))))(Tile((id \
         f93363b7-7ce7-4d94-b8b5-a453e4f9c90e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1be3502b-1a52-4a10-87df-ec20fd3ec6e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c182817-9a73-4b52-81c2-3a359e7c5c24)(content(Whitespace\" \
         \"))))(Tile((id \
         9eb3ff10-b615-4184-a9f8-3547f613ce52)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         669e57a8-21e0-4088-86de-756a0b42e184)(content(Whitespace\" \
         \"))))(Tile((id \
         6dddda2d-c5c4-46ef-a9ef-8bd5b7e58299)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aab9c155-a98a-41e6-a803-ada1a5f93db4)(content(Whitespace\" \
         \"))))(Tile((id \
         f3dd51d3-8ce0-4126-82f3-78e12680e7ab)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be13a9c4-577a-4ba2-9b07-7eb3f7bea236)(content(Whitespace\"\\n\")))))))))(Tile((id \
         29013726-fbad-42ac-b2ce-7adf9491a0c3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a29e1a0c-cf4d-41cf-ae1e-3b272510c6d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea1e44be-adfd-4835-8bbb-95c34852c6d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         3068b351-501c-448c-82d5-b013c02d6872)(content(Comment\"# Companion \
         effects - Beneficial #\"))))(Secondary((id \
         a18031f5-f90f-4a01-adde-a219c4e2ac04)(content(Whitespace\"\\n\"))))(Tile((id \
         766cfd8c-469f-4e22-8fc7-ed440736db4a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e99ae249-a125-4acf-86b4-ea4288522c42)(content(Whitespace\" \
         \"))))(Tile((id \
         df7703d3-8e1e-4b26-9232-72fa312a01bc)(label(\"\\\"\\240\\159\\140\\177 \
         and \\240\\159\\140\\191 are companions\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e29c2af3-fdb6-4083-b229-19a788d9a527)(content(Whitespace\"\\n\")))))((Secondary((id \
         71e18fe8-5f8e-403b-833a-fd90c678afb4)(content(Whitespace\"\\n\"))))(Tile((id \
         b47cd4b8-cbde-4f93-bf7a-a6586c784aab)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fde75eda-a61b-4f84-9c12-4e5b12609419)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6706ca54-9469-49d6-a49d-dfcacfa714c5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e911a8e-6489-4b02-a2e5-0d59ab8a7fb8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c370e04-f684-43fe-8ba6-96a0f354bb2c)(content(Whitespace\" \
         \"))))(Tile((id \
         fadc5d35-e2b5-4a1b-8749-7b088e8ec6d7)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         98f6af46-fdb8-40e9-b84a-35d1ac884ef7)(content(Whitespace\" \
         \"))))(Tile((id \
         6705596c-0f83-4b66-a4e9-7a655cd4b8da)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c387892c-d7b7-4782-bc3f-51266a920f05)(content(Whitespace\" \
         \"))))(Tile((id \
         25744956-8cad-4db3-8616-48d6c4380716)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c0bdeb7-03fe-4704-b4ac-e9d4c0c42223)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4ccc979b-7918-458b-9fee-91aa79d6ee56)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         084b1588-eed6-4095-b069-16e1608e7444)(content(Whitespace\"\\n\"))))(Secondary((id \
         24246554-5c8b-4336-be44-9fa1b4b271b3)(content(Whitespace\"\\n\"))))(Tile((id \
         c0bbcc4b-dc09-478c-a215-b96b1dd38e1a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         25d944cd-723a-4ea3-a6d3-e0e6ba7d4568)(content(Whitespace\" \
         \"))))(Tile((id \
         e16a811e-7046-43ca-8605-271ec2ed4bb2)(label(\"\\\"\\240\\159\\141\\132 \
         and \\226\\152\\152\\239\\184\\143 are companions\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         79fc234a-0a49-43f0-b9b1-0170d1776415)(content(Whitespace\"\\n\")))))((Secondary((id \
         4b2bea74-a198-4e20-ad0e-2790b518ddf0)(content(Whitespace\"\\n\"))))(Tile((id \
         bd65b885-ad6c-4381-bbb0-0eb5e9a11a8b)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08fa0960-4755-45ce-b346-c62cb35daa95)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e004897-9f0c-4fd1-9679-34ab2c5a0a35)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7dba8b5-a12a-4ae9-b8e2-dbd54c547d48)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3d9c267-8966-48f3-836c-1ff9896adc77)(content(Whitespace\" \
         \"))))(Tile((id \
         b423b242-a364-46f0-8e99-f2cfb6413a2c)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c876dafc-f754-4ba6-ac57-a19dfd3da7d7)(content(Whitespace\" \
         \"))))(Tile((id \
         55fb170e-97f1-4da7-b9e2-328452ba498b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b943c218-9e27-450d-909d-cd894855339b)(content(Whitespace\" \
         \"))))(Tile((id \
         3818dd57-9fa2-4812-9d9b-629e73bbf993)(label(Beneficial))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8ee3845-254d-4add-9979-14a52ef0fe9c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         56955362-2b9d-446e-8105-47b195e82da9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f7634a4-0180-49ca-beb8-443f6cc948d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         4fcc1b24-7421-4f04-bede-fe73eeb208a5)(content(Whitespace\"\\n\"))))(Tile((id \
         431424b5-2e0e-4956-915f-45a0d81bbb34)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         737c8ad8-bdce-498b-938d-21096825d132)(content(Whitespace\" \
         \"))))(Tile((id \
         ba6749e0-65cd-48e8-8770-9c1094248240)(label(\"\\\"\\240\\159\\140\\177 \
         next to \\240\\159\\140\\191 gains health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0b79556-22d2-4908-8b40-180a61a42703)(content(Whitespace\"\\n\")))))((Secondary((id \
         efe56a18-04d3-46aa-ad5e-cf7f92a4b548)(content(Whitespace\"\\n\"))))(Tile((id \
         0e672523-e36a-4cdc-9617-599829013b27)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         deb4ad52-b2aa-4856-8603-6bd5ed4aec94)(content(Whitespace\" \
         \"))))(Tile((id \
         7db2740d-eada-4e8f-b70c-84efeb3cb274)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8774f469-6207-4c68-9fd3-0c37ea9b226e)(content(Whitespace\" \
         \")))))((Secondary((id \
         058c94a6-135c-4aaa-a9d8-60ae95036b4f)(content(Whitespace\" \
         \"))))(Tile((id \
         a5ff8809-b5c5-4e2b-bf9d-21d4ceeaad02)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33ba7065-9162-4dba-80ff-888afc90f0b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14a7eae7-490a-4123-a43e-69b83c64e7b1)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d2912bc-4336-4306-aec2-7c5ed27660d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         640e7c17-ad6f-4e89-bacf-191a56d3322d)(content(Whitespace\" \
         \"))))(Tile((id 2993cd36-0fc9-440e-9baf-255dd6a0fb73)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8ef02413-580c-4a06-baa3-68b810d88431)(content(Whitespace\"\\n\"))))(Tile((id \
         e1511664-f710-455a-b582-e87a1ccc65d4)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f20c2f8a-3a10-4a19-b0b9-688ee98b66b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34c80887-eeb0-4a7e-9465-2d1cc533b13e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aec7698a-5f25-46f2-9ae3-1ef751a50a42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d06f94e1-cdc5-4543-a2da-7c179c6e69b5)(content(Whitespace\" \
         \"))))(Tile((id \
         6068e1b7-2d26-40e6-873c-41a4d9bf9577)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b1c6120d-99e5-40ea-a4d4-46c002862213)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd4bdb41-503a-406a-94a5-a071b94dbab5)(content(Whitespace\" \
         \"))))(Secondary((id \
         8cf8371e-989f-4228-99ce-564cb8de7acf)(content(Whitespace\" \
         \"))))(Secondary((id \
         79c2fe81-db38-4ba8-9478-2c00e44d162e)(content(Whitespace\" \
         \"))))(Secondary((id \
         42deae9e-f6da-4336-b2a6-3e11857aa916)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe7956d8-e483-4015-8be7-85d7b2cd808f)(content(Whitespace\" \
         \"))))(Secondary((id \
         296fcb20-63c8-4e60-9566-c23f4298ccde)(content(Whitespace\" \
         \"))))(Secondary((id \
         31a86f6b-2e4d-48cd-8858-84037b9769ea)(content(Whitespace\" \
         \"))))(Secondary((id \
         26b333dd-1e06-45be-ab91-62977ca281ca)(content(Whitespace\" \
         \"))))(Secondary((id \
         d6e9dcba-3a7b-4906-925b-69b01788da99)(content(Whitespace\" \
         \"))))(Secondary((id \
         0f23fe1f-a150-4eb5-a8c6-66381373fa5e)(content(Whitespace\" \
         \"))))(Secondary((id \
         687688c6-b126-4be6-9bf5-d5673e44115f)(content(Whitespace\" \
         \"))))(Secondary((id \
         d7ce4b6b-2ea6-41ae-9eb0-aaf11888210c)(content(Whitespace\" \
         \"))))(Secondary((id \
         85da6ef6-577b-455d-a997-f7b598e83bf8)(content(Whitespace\" \
         \"))))(Secondary((id \
         123f5439-5c22-4778-8841-fbf3f299072c)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb8a7434-5e9b-4346-b158-89dc78796400)(content(Comment\"# \
         \\240\\159\\140\\177 at (0,0) #\"))))(Secondary((id \
         243cb857-f029-4e2e-aa13-cbb57af05c01)(content(Whitespace\"\\n\"))))(Tile((id \
         70814d61-679c-4c03-92b1-89aa14fd9efc)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20d994a7-7335-412c-a084-986cf39faccf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3601f8a7-d80e-424f-8743-09ab308e454d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         96caa7ff-7cbc-4db5-a666-c9016f147219)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd4ab0a0-3fae-438c-80de-197aaf412b47)(content(Whitespace\"\\n\"))))(Tile((id \
         391b251c-b92a-48aa-bd5e-ab0d0b3d206b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         03db197b-e60e-442f-b9ac-f91e0deea8c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2bdd6238-1f92-406c-a4f0-eabaf9d417ec)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0982eae2-a8fa-4b95-ac8a-53a4847dae31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2c3eb08-28d7-48a0-ab31-c58bdd611648)(content(Whitespace\" \
         \"))))(Tile((id \
         454c2133-5905-48b8-a968-1297e3490bf6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e7c6abe-4e2d-44fa-935b-2b9b4b69b441)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         122d1932-47fd-488e-8691-2b181ecfe7f0)(content(Whitespace\" \
         \"))))(Secondary((id \
         bd11eb17-22cc-408b-9996-432c9efbfc0e)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a2e0886-a36b-4917-a5eb-fb0ac6fe78ef)(content(Whitespace\" \
         \"))))(Secondary((id \
         a2dc8774-5c0d-47dc-bae8-74e86a0b98b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         cf5077c5-adc7-457d-b255-006acdab1954)(content(Whitespace\" \
         \"))))(Secondary((id \
         04145298-1fae-4a0c-8e66-c761f9aec765)(content(Whitespace\" \
         \"))))(Secondary((id \
         a5ba9681-aac9-4047-ba9f-dc3f8f7da898)(content(Whitespace\" \
         \"))))(Secondary((id \
         9995a620-95fa-45d8-a7c9-c5919cd55b25)(content(Whitespace\" \
         \"))))(Secondary((id \
         8305339e-49de-47ff-a315-5e64c3d2eedf)(content(Whitespace\" \
         \"))))(Secondary((id \
         a0a71bb3-9ddd-4e95-9bc0-77047609a623)(content(Whitespace\" \
         \"))))(Secondary((id \
         32b0b45c-644a-451f-86a9-04c839a31b56)(content(Whitespace\" \
         \"))))(Secondary((id \
         77045158-77e0-47b4-8a2c-f7a74ed0b750)(content(Whitespace\" \
         \"))))(Secondary((id \
         574fcc62-310c-44df-966a-bfd24628507f)(content(Whitespace\" \
         \"))))(Secondary((id \
         f00cdf41-aeaf-4bd1-bac7-9ecfa935537c)(content(Whitespace\" \
         \"))))(Secondary((id \
         75fdc985-777c-4bba-8485-becf075591a7)(content(Comment\"# \
         \\240\\159\\140\\191 at (0,1) #\"))))(Secondary((id \
         b909738b-779a-4773-9e1e-38951cbabfa3)(content(Whitespace\"\\n\"))))(Tile((id \
         8995716f-ac21-459c-812e-fe0a9d6399d4)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4c9777b-13bd-4bdb-93f7-ccd5892f3c8a)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         7a975852-8391-45d1-8cea-f8b5b8fc3fe3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3bd4368f-866c-4093-acd3-227f90fc2fe4)(content(Whitespace\"\\n\"))))(Tile((id \
         af6f6889-ea04-4679-ba0f-1f252834fa39)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         163f8dfa-363e-4b01-b605-253b3787fec5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         173da8f5-cb54-4231-af8f-f3fa4a34dae5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61f29dda-626d-4a22-ad4b-b0592e5df084)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02c998ed-1e94-4901-8bdf-632799ad43b4)(content(Whitespace\" \
         \"))))(Tile((id \
         d2db42a7-26f4-437b-a976-cb85adf70dbb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81836974-4de4-4cca-9001-6873b89fcabf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6de55a0-0ab4-41a7-8dc9-6c57c4f6865f)(content(Whitespace\" \
         \"))))(Tile((id \
         a1893cf8-9db0-485c-9d1f-11ef89b076a5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8b6e4f5f-7c4f-450b-96af-748d1ba215b4)(content(Whitespace\" \
         \"))))(Tile((id \
         a55db330-fd7f-4ad7-a3bf-66099ef6f0f8)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c02aff3-f4d3-4189-9c53-a260f2b60645)(content(Whitespace\" \
         \"))))(Tile((id \
         a4739c05-fce8-4d5e-999e-f4f7ef6eafe7)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a55d01e7-f46d-4ea5-bb29-33d88395b417)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cb579c29-8685-419a-b400-ba8a9a9da038)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80b04a2f-6c43-46bf-a07b-a7df21703b4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c2ce87c-4e5b-4cea-bd50-f0505aeda5f2)(content(Whitespace\"\\n\"))))(Tile((id \
         80bbbb95-6ca9-4eae-89fd-f412bade92dc)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9deb05a7-6445-44d3-8665-6c9a4171147d)(content(Whitespace\" \
         \"))))(Tile((id \
         ce2a942c-36da-4150-aac6-96e6e686cf46)(label(\"\\\"\\240\\159\\140\\191 \
         next to \\240\\159\\140\\177 gains health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33f40937-433d-47d6-919c-dcad59b22601)(content(Whitespace\"\\n\")))))((Secondary((id \
         d8fd8bbb-90e0-41a5-af0a-ae8165d647fc)(content(Whitespace\"\\n\"))))(Tile((id \
         a5e54fcb-868d-4c96-86c7-e71cbacafd5d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b598658a-772f-40e4-8371-411c15a5ee4e)(content(Whitespace\" \
         \"))))(Tile((id \
         aff67404-0b74-4a46-a73d-9b4984480eec)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4905362e-daa4-4724-b6f1-96360cbda228)(content(Whitespace\" \
         \")))))((Secondary((id \
         8d3ddcd3-dd9b-49a6-8753-ecccbc35bbfe)(content(Whitespace\" \
         \"))))(Tile((id \
         4a4e57ca-4f15-44f5-ae50-06b57a250d95)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef7571af-c4c7-4898-a1fc-b5ba86149aa8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e698e716-70cb-4a39-ae31-2b8b217bac12)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfc73234-a017-4503-bb79-ff01055346a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ad32f75-1ca8-4be2-ab8a-331902fe41ca)(content(Whitespace\" \
         \"))))(Tile((id 12c32d2b-ee6c-4b93-8f27-d070302a4243)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f0aa75f0-7857-455d-867c-30fe37c0fdfe)(content(Whitespace\"\\n\"))))(Tile((id \
         7616bc50-e5a4-4ddb-8a77-f2eaaad24de2)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a59650b7-4484-4f49-b8b2-9955c07298d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a9f58096-4fd0-4dda-8293-46b1f21dfe4e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d85ad5a3-05e6-4c0e-a9d8-a55fdd049e31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5220a86-dc92-49d4-b5d4-acadfceebf30)(content(Whitespace\" \
         \"))))(Tile((id \
         5c01ff2f-2ca6-43b5-bcbd-9f2a4c3e6975)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         96eacef3-081d-41a7-ae58-23164101afc1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2ad430a-13b5-4d2f-b029-e797260a4f0b)(content(Whitespace\" \
         \"))))(Secondary((id \
         3b94fbc2-a5c3-451e-866e-96751d2a3c62)(content(Whitespace\" \
         \"))))(Secondary((id \
         d0f1ea5c-3e42-4ded-92a8-56c6a1afb147)(content(Whitespace\" \
         \"))))(Secondary((id \
         d90c6699-04ae-49b1-a84b-62fd6fdae8d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         4c883985-6c8e-4ebe-bc57-74b859855bd0)(content(Whitespace\" \
         \"))))(Secondary((id \
         85cbf0bf-373e-4457-bfda-423eec31c8a9)(content(Whitespace\" \
         \"))))(Secondary((id \
         9e39a846-abc0-47ea-8093-20b2c9c3d335)(content(Whitespace\" \
         \"))))(Secondary((id \
         ccc6bbcc-96ad-410e-8b0e-28433d91b972)(content(Whitespace\" \
         \"))))(Secondary((id \
         137cc7d0-a6f8-4008-991f-1a1f784c015d)(content(Whitespace\" \
         \"))))(Secondary((id \
         c1f6b1c5-a842-4daf-b7dc-c017719e7c2b)(content(Whitespace\" \
         \"))))(Secondary((id \
         1162f741-9a8d-47e6-837d-f38f9bae40b3)(content(Whitespace\" \
         \"))))(Secondary((id \
         19ac3166-dfbf-4a87-be7f-c199c522335a)(content(Whitespace\" \
         \"))))(Secondary((id \
         56f43636-2557-4854-8c14-2018f3e85aaf)(content(Whitespace\" \
         \"))))(Secondary((id \
         c6a00467-38cc-4945-a5a4-f880755caf8f)(content(Whitespace\" \
         \"))))(Secondary((id \
         6ac731b5-51a9-41c6-96fd-2dde69a46fc3)(content(Comment\"# \
         \\240\\159\\140\\177 #\"))))(Secondary((id \
         c088edc4-731f-4f2c-b455-e0acb31d8acc)(content(Whitespace\"\\n\"))))(Tile((id \
         09141d44-b6b6-43eb-a4a1-4575876a5fb6)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a311133b-cce5-4472-a6a5-266b05ec34ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b2df4d30-e632-41d5-9de3-01affc957fbe)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         00dc4dbe-86ad-44ae-ae35-2676ae0bda0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         334f3cd3-5cb9-4c57-a470-89bd3097651b)(content(Whitespace\"\\n\"))))(Tile((id \
         3d100c25-ee78-49b7-8db1-1c6407441470)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d414698b-315d-49f2-a5f2-949e620bdf22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e7e4e2f3-a33d-4d24-88b9-e72062bbad13)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         199c7a49-7bb9-4624-a5ba-f268967aaf2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f8e4cb6-6510-4795-9a03-5d6c6d7e5ddd)(content(Whitespace\" \
         \"))))(Tile((id \
         4e6b9bcf-070c-418d-a656-1f68adcd62f2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6f6c90e7-290b-45b5-8d7a-950423032ede)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9466e5a4-f8ae-460f-9721-144e8eb2040b)(content(Whitespace\" \
         \"))))(Secondary((id \
         ecf2bc44-233d-4346-8670-ed19e8911f15)(content(Whitespace\" \
         \"))))(Secondary((id \
         02e20a65-3764-4b02-b8ea-8cd74fd9aa8e)(content(Whitespace\" \
         \"))))(Secondary((id \
         69d3e436-2920-4b53-92be-220653a4494b)(content(Whitespace\" \
         \"))))(Secondary((id \
         5b6f3dc0-d237-4410-91cc-ebbc33f30177)(content(Whitespace\" \
         \"))))(Secondary((id \
         3e955bba-dbb9-4270-90fe-1b03715c3805)(content(Whitespace\" \
         \"))))(Secondary((id \
         54f20ebc-cf82-4656-b3fd-497440db367a)(content(Whitespace\" \
         \"))))(Secondary((id \
         62ee8a8a-40b0-4778-bfeb-2367b75aec09)(content(Whitespace\" \
         \"))))(Secondary((id \
         92bd67bb-7d21-4fe1-919d-af2190421ce2)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe5d7c9b-81af-4698-91ca-052cfc842581)(content(Whitespace\" \
         \"))))(Secondary((id \
         723370d6-304c-44b3-97c1-8f79c205a53a)(content(Whitespace\" \
         \"))))(Secondary((id \
         5d2cba99-7d9d-4050-bf32-7d2da2f414ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         45b81767-f33a-4ab7-b824-fccdcc83826d)(content(Whitespace\" \
         \"))))(Secondary((id \
         9eb1d48d-d137-4830-9606-abcc72f37d01)(content(Whitespace\" \
         \"))))(Secondary((id \
         bc06ba00-7bc9-4735-83f0-1e07155a4f3a)(content(Comment\"# \
         \\240\\159\\140\\191 #\"))))(Secondary((id \
         b8609bcd-f05c-4128-81b8-3e28e0a7c53f)(content(Whitespace\"\\n\"))))(Tile((id \
         34c1b2d6-304f-4853-99e2-1f98137fc83b)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c0e4c712-aea8-44db-b8ef-0eb5f2f13cd9)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         bc26f681-04d2-49bf-b8a0-df4b1cdab85b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0798a0c8-a449-4d58-86f8-0530441163aa)(content(Whitespace\"\\n\"))))(Tile((id \
         14323d3f-abea-4a17-a5be-ba2af2f2bea9)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ddee4922-d1a8-4589-8570-38ca7a4b57c9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af2d77ef-0a02-47c9-80a6-e33d12f54bcf)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e1addca-6b2f-422d-bbbb-b109c2cf4b95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9086aeff-124a-4d98-acb8-c05fbb50f25d)(content(Whitespace\" \
         \"))))(Tile((id \
         2acd8f69-e2ab-4743-90c3-7e26a25c9ffe)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfa69d3f-4a0c-4f56-9914-9e69ff44edf6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         085443eb-860e-4a5e-b75c-0124c15e7793)(content(Whitespace\" \
         \"))))(Tile((id \
         5613c768-9ab1-442a-a15f-5946ad4ba0ca)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         54ce8c2c-c88c-40e5-abd9-53a8b8c4d0ac)(content(Whitespace\" \
         \"))))(Tile((id \
         c541743c-6e95-422b-a020-d69fb65d035b)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cb434ea-fba0-41cb-8997-87a54c9afdcc)(content(Whitespace\" \
         \"))))(Tile((id \
         8f4ac238-ded6-4d9d-9887-61fc8fbed138)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c877b2af-7f0b-4c6b-882b-1d98003a79b3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3ac2ff5e-88ec-49e8-85fc-09c797546d6b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ddae110-7854-44f8-949c-fa56835ff221)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ccd7612-0b0f-4114-a1f9-f73bc27f88a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf8da9ea-5934-405f-b536-2b47d4b13f5f)(content(Comment\"# Companion \
         effects - Harmful #\"))))(Secondary((id \
         19bcdc9c-fe2e-409a-8f7a-16ecab0718e5)(content(Whitespace\"\\n\"))))(Tile((id \
         edc4c246-c2d7-42f1-81d2-64ff7c709516)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c090a5b7-befc-47a2-998d-9e8ee25465fa)(content(Whitespace\" \
         \"))))(Tile((id \
         38b178c0-78fe-4d52-9142-8d3a667511ea)(label(\"\\\"\\240\\159\\140\\177 \
         and \\240\\159\\141\\132 are rivals\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         568db7fc-7356-4b14-aa89-ec330016b33e)(content(Whitespace\"\\n\")))))((Secondary((id \
         18260410-612c-416b-9670-e741150a47b5)(content(Whitespace\"\\n\"))))(Tile((id \
         9e1a6263-216a-4c32-aa66-03ad2bba8f5b)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7b74da2-047f-48cd-a391-b64557a02f93)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         018aab14-a51b-4d69-bc89-cdaa2c0ba343)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30f47f27-f74c-4100-8131-ac0347d02b4d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0817946e-9d18-4ca3-bc7c-d85d4abdd1ac)(content(Whitespace\" \
         \"))))(Tile((id \
         c14f012a-a9e4-4e5a-9730-4d9c74af98d4)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a7a55ca-c501-4471-b33e-467785824ba8)(content(Whitespace\" \
         \"))))(Tile((id \
         cf5610d7-fe3c-4f86-aa70-4c4697b7c187)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5baca258-9091-4450-8665-76c49642a887)(content(Whitespace\" \
         \"))))(Tile((id \
         d9dd5b6a-e1a0-4c00-b388-95a574a253a5)(label(Harmful))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eae0d29d-aa6e-4d9d-bba0-8b8fcb5f4a81)(content(Whitespace\"\\n\")))))))))(Tile((id \
         72fae21e-d838-479c-ae8e-8f889a091ed6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7bec03fd-eda2-44c5-a2b1-6114b3467763)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ac94671-72f6-47e8-a33a-1e921eab2d9d)(content(Whitespace\"\\n\"))))(Tile((id \
         779d3d44-6f7f-41a0-914a-074193e1702d)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08c35740-2ddc-4838-91da-7022fe0ede8d)(content(Whitespace\" \
         \"))))(Tile((id \
         6e913a3e-fad6-4f06-bcac-2343fc378973)(label(\"\\\"\\240\\159\\140\\177 \
         next to \\240\\159\\141\\132 loses health\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9be6a7fa-434e-4899-866f-86679e3faeca)(content(Whitespace\"\\n\")))))((Secondary((id \
         3fdeac0d-1dc0-41b1-9178-0e771879f64c)(content(Whitespace\"\\n\"))))(Tile((id \
         7a65ab1c-b2ca-40bd-985c-d91d1b674c56)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a0ef2bf-e7a3-467a-9f85-9dcd4a558865)(content(Whitespace\" \
         \"))))(Tile((id \
         89fee55e-0dd5-42d8-bb8b-99be9efbd27a)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c7f0c86a-2fde-49d6-a470-dfefa2698452)(content(Whitespace\" \
         \")))))((Secondary((id \
         6deb8bd2-a30d-4efa-b121-634cc963fe83)(content(Whitespace\" \
         \"))))(Tile((id \
         aa89ae34-8382-4810-99dd-9af19f3ab45d)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8fa661b-9b33-44a1-b9c8-d263b626bac5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         21e0a0e1-244c-4416-be84-87ea2e417b05)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d31622a6-b9cd-42c6-88db-3b7213cdcbbe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b4f2b78-5d32-4227-9f35-622497cece4b)(content(Whitespace\" \
         \"))))(Tile((id 0f8c2a6b-28ef-4619-b3ae-2ece9ec7ccad)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         01b7d508-d410-4720-b506-2562396f1c84)(content(Whitespace\"\\n\"))))(Tile((id \
         bd2aa698-1d56-445c-8934-e1a52aa02251)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a86f3cf-fff9-4ff1-ab98-eb02b8bcf345)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2754f2d4-af6e-44f9-b373-602c3382173e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c19ac9cb-5606-4293-a0f1-7462ac493e61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cc3c5dd-9b65-4aa3-8eb8-e582af336747)(content(Whitespace\" \
         \"))))(Tile((id \
         73f83ab9-6680-48fc-8adb-c0b5d4175682)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3af93a62-9e6e-46f9-b00e-5058bd55b558)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b31da52-de65-4dde-ad92-2c814a7fde27)(content(Whitespace\" \
         \"))))(Secondary((id \
         695445fb-73e9-43a5-a27e-b089ca5c0325)(content(Whitespace\" \
         \"))))(Secondary((id \
         ff5873fd-dd80-4dac-9b06-078571ba9d83)(content(Whitespace\" \
         \"))))(Secondary((id \
         142d4e4a-1e70-44d5-9359-ca1aa27c43ae)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe6ad863-0afe-4e90-a450-bc754a884f98)(content(Whitespace\" \
         \"))))(Secondary((id \
         c745f31e-bcac-4927-95f0-15181265c5e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         73abbd5e-ebbe-4616-b51e-aad7e376fc24)(content(Whitespace\" \
         \"))))(Secondary((id \
         9086275a-2962-42d6-a096-43e07091437c)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b229f20-cfdd-4731-8c54-0b164dac952d)(content(Whitespace\" \
         \"))))(Secondary((id \
         49e51bc2-faf3-4425-88d5-ccb2b60b94af)(content(Whitespace\" \
         \"))))(Secondary((id \
         69a4ca3a-4212-4a18-8952-7c3c3991339b)(content(Whitespace\" \
         \"))))(Secondary((id \
         4fbb1e03-edf1-4563-bd8d-8ae01c322d3f)(content(Whitespace\" \
         \"))))(Secondary((id \
         4ab5cd33-662b-4579-b55f-712e38c6c16c)(content(Whitespace\" \
         \"))))(Secondary((id \
         f1b2519b-926e-410a-b949-3803eac1525a)(content(Whitespace\" \
         \"))))(Secondary((id \
         42718a90-8e4a-4744-b6d2-13a544f75fa2)(content(Comment\"# \
         \\240\\159\\140\\177 at center #\"))))(Secondary((id \
         7b458744-41a0-4d4e-a176-24e03a5f0e33)(content(Whitespace\"\\n\"))))(Tile((id \
         f74df194-ae67-4b36-bda2-865d3a6ad5d3)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         27e9cbd8-f7df-4cf5-94ee-45a40584bc9c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a6e5f47-3514-4aef-b628-d424cf82b3ba)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b7e4f4ff-ea1c-4207-977c-b112753df507)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04d68619-8abf-40e9-87c2-8328bd63a5b2)(content(Whitespace\"\\n\"))))(Tile((id \
         dbf4f63d-cf19-4471-b834-645fdc661cca)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         352748cf-7d14-4c92-a4f8-afccf353a788)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1ee167ad-4ad7-4d13-b96d-fecc8a4a525d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62a2ebcf-1cd0-4a6b-a5f7-51cc364d4ea0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4dca1b98-6426-4ad9-8e88-ba74bc348058)(content(Whitespace\" \
         \"))))(Tile((id \
         92038539-a217-4e29-bc26-269923417156)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c8bd9ae9-221e-4cda-a407-f29c1d8cd60a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20fb550e-fe15-45c4-9961-328ec9d8cba4)(content(Whitespace\" \
         \"))))(Secondary((id \
         fa863ece-37de-4e48-8890-e2eb9688f1a6)(content(Whitespace\" \
         \"))))(Secondary((id \
         feb01883-d91f-4b21-93ab-169b5b53aa21)(content(Whitespace\" \
         \"))))(Secondary((id \
         0e32518e-52cf-44bb-833b-276ce1b7e69f)(content(Whitespace\" \
         \"))))(Secondary((id \
         7e9d0447-8831-4605-a988-a1d2f01eaf03)(content(Whitespace\" \
         \"))))(Secondary((id \
         087714b7-908c-496d-8a74-50370897bbd8)(content(Whitespace\" \
         \"))))(Secondary((id \
         d3380b58-4097-46e0-9f02-1008e3acfed8)(content(Whitespace\" \
         \"))))(Secondary((id \
         a7a26c2b-8559-429f-ad6e-e972ee713f1b)(content(Whitespace\" \
         \"))))(Secondary((id \
         1d145ef5-2e34-4d3e-8f81-e6dbb096d924)(content(Whitespace\" \
         \"))))(Secondary((id \
         d329dd6f-f9aa-4f79-8b12-a032ac6d8d13)(content(Whitespace\" \
         \"))))(Secondary((id \
         91baa099-14ce-47e0-bae1-715f4e535c03)(content(Whitespace\" \
         \"))))(Secondary((id \
         fb160cca-cf4d-449c-bdd4-d327ae565a19)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a20698e-455e-43bf-8ecb-2e06063c5b26)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e15f7ed-6464-4564-a4ca-552fefc4c59c)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1d597ae-dd40-4885-848a-1c726d832851)(content(Comment\"# \
         \\240\\159\\141\\132 to the left #\"))))(Secondary((id \
         a15612b0-25d5-4a29-9f2f-1e33bc6ec879)(content(Whitespace\"\\n\"))))(Tile((id \
         65ba5d1d-7922-4877-9231-3d58acf44b38)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d9689330-e15d-4232-8f30-d8c940fd813b)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         9135ae65-6f43-40f6-ad6d-602bc5b40f21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2071c7b-df71-4279-a38e-d9378eafbfb4)(content(Whitespace\"\\n\"))))(Tile((id \
         937b4c83-1fa3-4f44-ae6c-41f2138a821f)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0de28e7a-4ccf-4ec2-8f81-b012042754aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b2b5dfa-ef05-4bb1-9fb7-418f3b7c6abc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e012b89a-8be3-481b-ae8c-e748d77708c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff73c456-3879-4ec8-95f1-439a5c2ca0f7)(content(Whitespace\" \
         \"))))(Tile((id \
         97507ed5-da56-4bef-8dc1-6865a08aed46)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ac2f03d-599d-4672-af40-9bf308e8c237)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         100afe35-2cea-43a6-9d26-f27ea93d1c32)(content(Whitespace\" \
         \"))))(Tile((id \
         a7670aac-5e23-4801-9dce-46e08c784d54)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7133ca49-d0a3-4f37-bacc-da4a39dcadf7)(content(Whitespace\" \
         \"))))(Tile((id \
         42ddf26c-1e44-4de6-b5b2-4d5fc4e19184)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fdf9ab4c-e54b-4eef-afcd-444f4838291f)(content(Whitespace\" \
         \"))))(Tile((id \
         b7e25553-10a0-426b-ac51-775d8b50ac74)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f12b99fb-a2fe-4181-9d5f-a176d43199b5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d839a587-ade6-4566-8e66-719bb863923b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d94b656b-8def-49b0-afec-1171c5bc8a8b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0903610e-5694-4120-a081-859898ab5119)(content(Whitespace\"\\n\"))))(Secondary((id \
         0160dc06-cb2d-4fc3-9e5b-1dd74e4a659c)(content(Comment\"# Neutral \
         effects #\"))))(Secondary((id \
         f04cad8b-6668-46b9-b5b5-2cc4b03c0ca4)(content(Whitespace\"\\n\"))))(Tile((id \
         1c10c816-cfdc-4bf9-bd9b-6b5227ba6a61)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6bb9de4-36e7-4ea4-8b7c-44c221eabc3e)(content(Whitespace\" \
         \"))))(Tile((id \
         c324cf50-98bb-400a-b9d6-72b14b24d9e9)(label(\"\\\"same crop has no \
         effect\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a707bcc1-94c8-4578-b838-55d59f180222)(content(Whitespace\"\\n\")))))((Secondary((id \
         e12aeea1-8eb9-4d99-a0ef-15868d3c9603)(content(Whitespace\"\\n\"))))(Tile((id \
         e2ea92d8-dcf1-41f2-b068-173d2c31bc0f)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af3432c2-6a60-46c3-a62e-78d2df05eafa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         335ef628-41e9-42db-a5cb-424d74103dd0)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b42959cb-ebea-4024-9bab-d91a681b1501)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         462ca9c3-44e4-4d14-ae4d-b96633c14dc2)(content(Whitespace\" \
         \"))))(Tile((id \
         77b68a17-2029-4750-833f-a93bfbb0bd70)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9a418b38-ee4f-4041-83db-9143a47d0576)(content(Whitespace\" \
         \"))))(Tile((id \
         0858f942-f4e0-4ff7-a073-47ab4ae27501)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb688a89-fa59-44b1-9920-589ac7684450)(content(Whitespace\" \
         \"))))(Tile((id \
         77ec87f5-6c24-44ee-979f-b2d7f087b52e)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         753c8f99-4ca8-4b55-a8fd-8599099d876d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d719c3da-2229-4e96-81ce-d3a26c5100cc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d707bdc3-0e7b-479c-bb6c-3382ee1cd9e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         da7d1745-52b7-4c18-b3d5-a3e2c9369321)(content(Whitespace\"\\n\"))))(Tile((id \
         fb3cd0b6-1ecf-4a81-b9f1-06f903f31d8b)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         575ec86f-cf69-40c6-86a6-dd6d5081a68b)(content(Whitespace\" \
         \"))))(Tile((id \
         96ff7ff1-06a4-4e13-b4cf-405769171d98)(label(\"\\\"unrelated crops are \
         neutral\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6a594b6-1584-423b-85b3-f582354bcb9a)(content(Whitespace\"\\n\")))))((Secondary((id \
         320a2ed1-de6e-408a-ae7c-7748fd33a169)(content(Whitespace\"\\n\"))))(Tile((id \
         2b4b6684-b5c9-4549-80b6-19b8726192b9)(label(companionEffect))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52d957e0-1ed0-453e-8e33-9f0ac5f1972f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         702950c5-7abc-4b4b-9dfa-7259bb037449)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5163f763-5a7e-40c2-87bd-7638ba07f5c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37d91091-4a2b-43a1-8099-af2872aabcd1)(content(Whitespace\" \
         \"))))(Tile((id \
         a3a9a29e-3644-4811-b0b9-29e4d96d4243)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19d7d1e0-a5de-425a-8dde-62c5922a82eb)(content(Whitespace\" \
         \"))))(Tile((id \
         03496bdf-4545-4263-8f40-0646608c177c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0254ab7c-2cef-4614-83c7-74a501c9083a)(content(Whitespace\" \
         \"))))(Tile((id \
         5cf35680-de5e-4819-958b-024d64f25d0b)(label(Neutral))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c49dac1c-4471-4213-b28b-51ef8f32f465)(content(Whitespace\"\\n\")))))))))(Tile((id \
         591e6b64-71c6-4107-855a-d650139cd415)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95fe7e81-6e74-4fe9-8e8f-5110e0cca1f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         04628fe6-bc2e-48a1-809d-01d4a20890f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         00e83a81-5425-4bb1-93f7-158f7924faad)(content(Comment\"# Multiple \
         neighbors #\"))))(Secondary((id \
         01a46fe1-7872-40fe-ad8d-7ba37b251e46)(content(Whitespace\"\\n\"))))(Tile((id \
         7fe7cc97-022c-453c-84e3-387ba8ce1b96)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         00cdb1eb-260a-4425-b462-596058aca460)(content(Whitespace\" \
         \"))))(Tile((id \
         68b4b73f-404e-4f2c-9f3c-4b1d5f725e1f)(label(\"\\\"multiple companions \
         stack benefits\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9dbdbb11-e16b-481f-a56a-b55433ad7b67)(content(Whitespace\"\\n\")))))((Secondary((id \
         9b58d04d-07ef-4100-9e4d-1c1ac91f7eba)(content(Whitespace\"\\n\"))))(Tile((id \
         a6d5d9b1-df07-410a-bd47-e4e862cd46be)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa7c58a2-7464-4744-8676-b9044dd15d7d)(content(Whitespace\" \
         \"))))(Tile((id \
         f5c50a52-d2be-4328-9e96-65725ad432d3)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3f4f6fee-3318-4701-81ab-a91087d965e7)(content(Whitespace\" \
         \")))))((Secondary((id \
         e6304de8-0b18-4d18-b7ca-f5330c78b1cb)(content(Whitespace\" \
         \"))))(Tile((id \
         1fcb2cd5-59c3-4819-9cd2-f606348aeee3)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f81ab70-7bf1-4748-9fec-7ea144865490)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         242698f4-006a-40fb-9f4b-fba9bb27e4e4)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8063838a-000e-44d0-a662-dab7d86ebb54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07c91087-53c4-4621-aa1f-05b97b993b25)(content(Whitespace\" \
         \"))))(Tile((id 7a0b3087-f250-4512-8db5-defd5f53198f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ccd69f44-8e7c-4981-b02b-1ff5927fb9d4)(content(Whitespace\"\\n\"))))(Tile((id \
         eb3efb37-b9f5-4c2a-a9ae-6dda7d60d08b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54a1e5fb-6e58-4da4-84ed-ec6f7234c796)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aed4d3d5-06a5-43b7-8d7e-7bafc76e21ee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c8df3b3-ea06-43e1-892f-a3e50a2fd432)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46ca21a4-1abc-4aa4-9671-f092a06a3388)(content(Whitespace\" \
         \"))))(Tile((id \
         dc730165-d56d-4773-b7ea-b1a5769c83ee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e72cef49-d69a-42d1-8de9-811a4fe328b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf6b8573-cfcc-464d-ae7d-edeca29e9df4)(content(Whitespace\" \
         \"))))(Secondary((id \
         21ff6f0c-6ebd-4f54-9b16-7a74d5bea34c)(content(Whitespace\" \
         \"))))(Secondary((id \
         1cfa160b-cc62-454f-b53e-bf448956eae8)(content(Whitespace\" \
         \"))))(Secondary((id \
         c2d0b089-64a7-4f64-a354-cf7a7e84351b)(content(Whitespace\" \
         \"))))(Secondary((id \
         14091e6e-d403-4613-bd37-b474e55d0ec9)(content(Whitespace\" \
         \"))))(Secondary((id \
         e753516b-5d38-4957-9c0f-e541929b3b6b)(content(Whitespace\" \
         \"))))(Secondary((id \
         49d93107-b099-4154-96c0-346a00f09e4f)(content(Whitespace\" \
         \"))))(Secondary((id \
         ab82ce52-e03b-461c-8685-7189f0d110b5)(content(Whitespace\" \
         \"))))(Secondary((id \
         0f6e32ea-9076-434c-8840-ea933a8615a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         a29ceec6-70d5-4249-b4e9-b11f1bd5b65f)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e77d92b-5ef2-4ac3-8b80-4716ad26b21e)(content(Whitespace\" \
         \"))))(Secondary((id \
         f69b0170-8e66-4c2b-9bd8-b8b77f47c98c)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1b074f4-ef39-44ca-91db-27338dacc429)(content(Whitespace\" \
         \"))))(Secondary((id \
         0accf62b-22ee-4e9f-9c31-046b08a47ecc)(content(Whitespace\" \
         \"))))(Secondary((id \
         c0716fab-ed40-45e3-a937-ae20553cb1fb)(content(Comment\"# \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         d89132f7-a6ee-4377-b6e0-553861681463)(content(Whitespace\"\\n\"))))(Tile((id \
         71668f3c-653d-49a1-bc11-3b3cbb4f0ec2)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce24ca11-2880-4b7a-958b-b17f07432993)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d62860cf-0667-4155-ba66-19b3213aff0f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f0a82036-dc49-4fb7-953d-7652dfefe5c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cf5aafc-f424-464a-bc03-18f65a7bb02a)(content(Whitespace\"\\n\"))))(Tile((id \
         f09c81a7-b845-4e27-92b4-f6e9101a96fc)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a8038a1-3cda-4534-8a10-36376b6f750b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df1a37cb-830c-4715-996d-e8df98f3e319)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6967602-00d8-4690-b855-019815679421)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9048ccbd-5ca9-4382-9de3-353df6cd45ad)(content(Whitespace\" \
         \"))))(Tile((id \
         572922d5-ee14-4968-a33c-79868b07a2ce)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         26119f61-2da7-409c-a06a-320b609fafc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42b3d575-5db4-4d09-ab9c-4ce55f22232a)(content(Whitespace\" \
         \"))))(Secondary((id \
         bc7849b1-7182-4383-af0d-d86369292c4c)(content(Whitespace\" \
         \"))))(Secondary((id \
         0bca47ac-7a65-452e-b7e7-33b52c9386ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbd4a6ed-2d9f-41e1-bfcf-6b0b775a88bd)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb4f8139-09a2-42da-9e18-be4b14a51254)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a032e01-42a9-4bfc-b374-15e5978d408a)(content(Whitespace\" \
         \"))))(Secondary((id \
         430448df-a8e7-45a3-a405-0003c8ae3c41)(content(Whitespace\" \
         \"))))(Secondary((id \
         80540f49-1479-4921-8808-e8f91cec8ffd)(content(Whitespace\" \
         \"))))(Secondary((id \
         e717105e-4619-45df-92af-1315df0e95d5)(content(Whitespace\" \
         \"))))(Secondary((id \
         fdd84893-975e-4df0-b6ef-24047a506a3a)(content(Whitespace\" \
         \"))))(Secondary((id \
         b46d9fa4-1cc0-41da-9f28-f2d14476ed24)(content(Whitespace\" \
         \"))))(Secondary((id \
         d661c739-cd97-41c3-aacb-600594f039e2)(content(Whitespace\" \
         \"))))(Secondary((id \
         220ea91d-1bf3-471f-8af3-fee5a2865c38)(content(Whitespace\" \
         \"))))(Secondary((id \
         3c044792-44a2-4d31-8221-63bf82b719a2)(content(Whitespace\" \
         \"))))(Secondary((id \
         1717f12a-6e30-4080-93d3-aa6e31a00c55)(content(Comment\"# \
         \\240\\159\\140\\191 above #\"))))(Secondary((id \
         d326bd0a-4999-49a4-831a-fc3214fa6e84)(content(Whitespace\"\\n\"))))(Tile((id \
         c3bd3cbf-6ee7-4f8e-bea3-62687af2d370)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c973aba5-3541-4b98-9577-15104dfc83f4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d35beda2-c94c-4511-b286-baf48d63ce88)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d6930aa-2380-4737-8038-57f98ea99244)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b17c2342-0551-4832-818d-78b239dbc1f3)(content(Whitespace\" \
         \"))))(Tile((id \
         b4fbc5d6-0b08-4df9-ad6f-fac9a83fc1e7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9210087d-2065-41cd-bef5-b6d13009f2b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9b1ddfa-f2e4-43ec-b374-e2407f98f6be)(content(Whitespace\" \
         \"))))(Secondary((id \
         88851aa2-d271-4c4e-bed7-c331f9cb5e4e)(content(Whitespace\" \
         \"))))(Secondary((id \
         79074ac9-81ea-407c-b408-4c3253409e78)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce5cb67f-d8ab-47e5-a7b0-d986ec678362)(content(Whitespace\" \
         \"))))(Secondary((id \
         4968b47a-87ca-4845-9e34-583fcfe10515)(content(Whitespace\" \
         \"))))(Secondary((id \
         572e7705-f964-4629-8ab9-e8a845f804d8)(content(Whitespace\" \
         \"))))(Secondary((id \
         ef04274d-7082-4239-b7cc-cd487e5ee5e7)(content(Whitespace\" \
         \"))))(Secondary((id \
         08cd7e1c-a8ae-4823-86b8-f593e3a541cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         69a8a30f-f19b-4ac7-ac9f-838144f688e2)(content(Whitespace\" \
         \"))))(Secondary((id \
         e7c5f3a3-9ba4-42bf-91df-6fce54359b89)(content(Whitespace\" \
         \"))))(Secondary((id \
         40418f14-a150-4990-ac43-42f87cfa7eac)(content(Whitespace\" \
         \"))))(Secondary((id \
         2f0c7e53-a0a7-4333-bffe-4962f7b3677a)(content(Whitespace\" \
         \"))))(Secondary((id \
         9dbd6a43-ce93-4e85-86cc-a6fbaaefbc2e)(content(Whitespace\" \
         \"))))(Secondary((id \
         f1d587e0-dee0-4fd1-bb9f-8e56eff36e5d)(content(Whitespace\" \
         \"))))(Secondary((id \
         5909d399-e901-494e-b29f-4a55693d9c77)(content(Comment\"# \
         \\240\\159\\140\\191 left #\"))))(Secondary((id \
         9639792b-9d07-45ec-8443-2c3c895d4dad)(content(Whitespace\"\\n\"))))(Tile((id \
         e837a197-4299-4e1e-a081-1f64ca55a620)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2958d815-3df6-4086-bfc4-6cab78e767f9)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         fd2ce5b4-4a00-4957-9675-18303fefeb3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92b45223-8cad-4113-9bc6-650de45c79c2)(content(Whitespace\"\\n\"))))(Tile((id \
         db67f938-72d7-427d-81de-98150860ebce)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1643d19-d982-412b-a990-de68b8484c6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6520902-634b-47bf-ab1d-226349c7a184)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea1e8867-1266-42ab-952d-223c2bdbe189)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         525f0846-0f2b-4a91-a916-e1319fbc637e)(content(Whitespace\" \
         \"))))(Tile((id \
         d85c75c4-cc41-4a2c-841d-6d2f162ffa19)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         541ad08e-f02e-4193-acc7-fc5f42d32464)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c74d9e32-91d7-4671-80f9-46a4889bda9e)(content(Whitespace\" \
         \"))))(Tile((id \
         5d52a0ae-5cda-4f0c-8f37-d68a03fc483b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cc6a987b-abf5-4f58-92d0-911388c3c123)(content(Whitespace\" \
         \"))))(Tile((id \
         41c7ae50-80af-4626-9478-380b31530c5c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99e3d8b8-93e4-49a6-b1cf-16f25f10109c)(content(Whitespace\" \
         \"))))(Tile((id \
         9208d128-24d4-483f-906b-29428d258cc4)(label(70))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e7a76a0-ddf0-40bf-a1cc-90400a1dea85)(content(Whitespace\" \
         \"))))(Secondary((id \
         1ac7cf3c-e05f-44d1-a0f8-1756375d42ac)(content(Whitespace\" \
         \"))))(Secondary((id \
         72a8d016-9ce4-4471-b137-645cdc0102cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         08abf373-1160-4927-b764-45c71cadabc9)(content(Whitespace\" \
         \"))))(Secondary((id \
         cd261415-0453-4b9b-9f1e-72a9e703bfc1)(content(Whitespace\" \
         \"))))(Secondary((id \
         b9184b9c-2dbb-406e-81c9-ea1e06f02881)(content(Whitespace\" \
         \"))))(Secondary((id \
         98ed803c-c848-4689-bfec-18a9e872a57a)(content(Whitespace\" \
         \"))))(Secondary((id \
         333a0ffa-973e-4dfd-99ec-af33504976d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         8f8caa0b-4bc7-480e-a8ab-3480b1d94e8f)(content(Whitespace\" \
         \"))))(Secondary((id \
         a8d1e1ef-3074-4d5a-b1db-1c886ef62971)(content(Comment\"# 50 + 10 + 10 \
         #\"))))(Secondary((id \
         0bff52f3-355f-481d-85bb-cccbd9a65aba)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9ba3179b-2e0c-4e2e-89fe-2f94d5be974a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a32234dd-5e27-4239-901e-017482a7117a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1aa458c-ab2c-4b88-9312-5eb6985a9399)(content(Whitespace\"\\n\"))))(Tile((id \
         09607b8c-c7de-4026-8d71-0803dad52b86)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ab9b07ad-3b39-4958-aafd-af4ea450e729)(content(Whitespace\" \
         \"))))(Tile((id \
         58f7a437-4629-439b-b780-e16013bfd52c)(label(\"\\\"mixed neighbors \
         balance out\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ea0c5b9-05f0-4a8b-9c4a-ad3e9af32ce9)(content(Whitespace\"\\n\")))))((Secondary((id \
         66a01b3c-cd16-44f9-a1f2-6389055952de)(content(Whitespace\"\\n\"))))(Tile((id \
         b76f8d7d-49eb-4c45-9573-8c8cea08b864)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bd97eb4f-7d18-4a7a-9910-cb4da6ef3ad6)(content(Whitespace\" \
         \"))))(Tile((id \
         beee9404-db2c-4f7b-b28d-d779895e6d41)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         705beee6-c3ce-4d4a-89f4-b4fcfb4ef1b9)(content(Whitespace\" \
         \")))))((Secondary((id \
         adc517e9-5af0-4412-8987-0f45f178febe)(content(Whitespace\" \
         \"))))(Tile((id \
         7a0dd926-cbcc-48e8-9485-401f6f47c0c5)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cd06344-5a43-4d5d-8ef5-0c931d617dfb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         383e4146-bdbf-4cff-9408-ec19fcec97ee)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20d31397-8783-4dce-b64d-c2892b37cf44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f47e17c2-9751-480c-ba3b-802282ab115b)(content(Whitespace\" \
         \"))))(Tile((id 542b3f72-e51e-4128-8231-1e8cfe8e7ab7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a8856263-d455-4cb3-bbcb-9aec7b3694e5)(content(Whitespace\"\\n\"))))(Tile((id \
         9acb29ac-1425-43e0-93c1-949f41ca8491)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17c6c8c9-f490-4605-9e26-9f0cd0264eed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4ef3b87-f760-4441-9d55-de2cbb76e297)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ae398d0-bce4-41ef-8f60-37683474b0ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8dfc771-86fa-4d12-a0cb-89e615b99485)(content(Whitespace\" \
         \"))))(Tile((id \
         a1e6b551-5567-4949-9206-68f18e1d4577)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         817beebc-86e7-4ecb-9942-f701115d4535)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a3d7a49-8ee5-4738-86c5-fccbcf713c5d)(content(Whitespace\" \
         \"))))(Secondary((id \
         96a7731f-3d1a-4507-986c-d283ea630a48)(content(Whitespace\" \
         \"))))(Secondary((id \
         90750e40-547d-4c49-b6d4-ba01cb6f6c25)(content(Whitespace\" \
         \"))))(Secondary((id \
         a7b677e6-e75f-48ff-888a-e30a3e720936)(content(Whitespace\" \
         \"))))(Secondary((id \
         7586d626-ce82-4d80-8e83-3538ed777d26)(content(Whitespace\" \
         \"))))(Secondary((id \
         cce2876e-560d-454a-bdcd-74c4b064bea7)(content(Whitespace\" \
         \"))))(Secondary((id \
         d2309cca-9ada-4624-a345-0bd9c28e9fe5)(content(Whitespace\" \
         \"))))(Secondary((id \
         8ef4ea73-4a72-4315-aeb3-d00f7c4610cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         34819eb4-7a22-4fa2-af16-b88b549a54bd)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc7d7606-f0c4-453b-8c32-b89bfe31c40c)(content(Whitespace\" \
         \"))))(Secondary((id \
         900001a4-2b14-4712-abe0-484ece1d7c57)(content(Whitespace\" \
         \"))))(Secondary((id \
         87bf300e-9dd9-4ddd-89aa-a5e6d88c47db)(content(Whitespace\" \
         \"))))(Secondary((id \
         85a7febc-7488-4c77-9af9-f04ef3dfab9e)(content(Whitespace\" \
         \"))))(Secondary((id \
         86e54ee9-9d83-4fbd-b420-7d2da366f922)(content(Whitespace\" \
         \"))))(Secondary((id \
         2dd849de-2db8-4f0d-a9ac-de8823ef4288)(content(Comment\"# \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         edc36dbc-31bd-4119-b713-3283628091cd)(content(Whitespace\"\\n\"))))(Tile((id \
         c6141e38-f49d-48cd-957d-d1fc098fdf49)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66c9188c-9b26-4800-8afa-19cc494309a2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b7cb2aa5-71aa-4122-a1eb-d360ffbe5a92)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c1e24b80-d967-45df-ad8d-d82195d082f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b90cf033-5276-43f5-9598-390fc06d09e1)(content(Whitespace\"\\n\"))))(Tile((id \
         efd17266-59cb-4b92-8f49-6771cfe3cfe3)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         488c042b-8f6c-477a-b1e9-48376518aa07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87787329-24f2-44a7-b309-a06263d6c1c9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fccddec5-f67c-4d0b-93e3-d4df8c936666)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65b40b7b-1578-4965-b90a-6f26facbad75)(content(Whitespace\" \
         \"))))(Tile((id \
         8055cec8-f8f3-4294-b056-ca6c426312c5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2c6de0b7-35b1-4bbf-9cd8-3934b3f033ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd9340f8-dbbf-46ab-9fbf-5f42503df171)(content(Whitespace\" \
         \"))))(Secondary((id \
         39690d49-c502-41f8-a0ca-0acf79b028b4)(content(Whitespace\" \
         \"))))(Secondary((id \
         c13e2062-d0f3-49d5-8ae0-dd4015e639d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         616a1e11-f641-453a-a8db-103ef8776916)(content(Whitespace\" \
         \"))))(Secondary((id \
         c4053de1-9009-4da7-aad5-cdd4b19d35c0)(content(Whitespace\" \
         \"))))(Secondary((id \
         063356d7-9e6d-43f5-a79c-75209f68048e)(content(Whitespace\" \
         \"))))(Secondary((id \
         43c37842-a9ab-4ef3-a193-80c7b212be86)(content(Whitespace\" \
         \"))))(Secondary((id \
         fd22bc3f-ee0d-4375-804e-f648941c571c)(content(Whitespace\" \
         \"))))(Secondary((id \
         d1be379a-2377-4e78-9005-1f842955736d)(content(Whitespace\" \
         \"))))(Secondary((id \
         64dd5b5a-8a3d-4f02-a198-678233e2d6e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b3084ee-34d1-44e4-b7fe-4d9ad303ab9e)(content(Whitespace\" \
         \"))))(Secondary((id \
         f2676edc-8cbe-4834-9196-fc68449e88dc)(content(Whitespace\" \
         \"))))(Secondary((id \
         f1a40bf6-3b26-4bf6-a13e-059f4d98240a)(content(Whitespace\" \
         \"))))(Secondary((id \
         8f5ade90-79a5-4832-a7fc-555fd9308c6f)(content(Whitespace\" \
         \"))))(Secondary((id \
         cebc23f9-006b-4662-b22f-40b8fb2ba6b9)(content(Comment\"# \
         \\240\\159\\140\\191 above: +10 #\"))))(Secondary((id \
         8b815779-fc4a-4f0b-9d7e-41c3bb9af66e)(content(Whitespace\"\\n\"))))(Tile((id \
         6df903c8-845c-4690-b4f2-148f3d702dd6)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0655e919-8485-4cbb-a689-23e17806e35a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b584bf7d-29a9-4eb1-96f9-f45d33aee744)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1f8a3332-db9f-4e89-9062-527f7044ccb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd35a460-7f36-49fe-8836-5a2e7c33cec0)(content(Whitespace\"\\n\"))))(Tile((id \
         4f6c51a8-659c-452f-9678-fc5202ec6075)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01dde6d4-c374-458e-9372-fc25986a1586)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3333a32-08f7-4e6d-90aa-674806b4a1a8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         324eb0c7-840b-4094-aa92-cbb243da15b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         085dc384-5dfa-40b1-b494-67995eaeaa63)(content(Whitespace\" \
         \"))))(Tile((id \
         b75508b3-4a8a-47ef-83b0-78cb9ed618d7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c9a35f77-7a3b-435a-b7b3-5b4ab91edc7c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82a242ac-29d1-4201-ace9-b8a3dec62e66)(content(Whitespace\" \
         \"))))(Secondary((id \
         3dd81632-fe8f-4210-9fe1-32d846c86d75)(content(Whitespace\" \
         \"))))(Secondary((id \
         87c40f2b-00a9-44d0-96f1-742d741ba634)(content(Whitespace\" \
         \"))))(Secondary((id \
         98689aad-1b04-4bd4-8690-cabb2e4f020c)(content(Whitespace\" \
         \"))))(Secondary((id \
         6ea2757e-726b-403c-8b8b-cf61acaad337)(content(Whitespace\" \
         \"))))(Secondary((id \
         8d233d41-1b45-45b1-aa8c-eaaf0be61aeb)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc43c227-1cee-410b-a786-bd90ff9e680e)(content(Whitespace\" \
         \"))))(Secondary((id \
         7f679aeb-85c4-4e43-b1de-8630840c4429)(content(Whitespace\" \
         \"))))(Secondary((id \
         84f56278-21a6-4481-9073-68629f61a9cd)(content(Whitespace\" \
         \"))))(Secondary((id \
         2245d879-6f8d-4d07-b3f0-b3fb3c509743)(content(Whitespace\" \
         \"))))(Secondary((id \
         5416b1bc-5fe5-435d-98de-ffb457a84ffd)(content(Whitespace\" \
         \"))))(Secondary((id \
         1cb62e57-1b14-4a5b-9f7c-8d57d2fa984c)(content(Whitespace\" \
         \"))))(Secondary((id \
         2d82fb4c-5b0c-4726-aa32-ea0dff4fe5a2)(content(Whitespace\" \
         \"))))(Secondary((id \
         ca5ea730-dcc0-4b7f-9e03-4372dc782c23)(content(Whitespace\" \
         \"))))(Secondary((id \
         32244fa7-f9d3-4937-847e-99ebb3ea7554)(content(Comment\"# \
         \\240\\159\\141\\132 left: -10 #\"))))(Secondary((id \
         719ff184-db6f-40a5-bf76-c323fa6f71a4)(content(Whitespace\"\\n\"))))(Tile((id \
         f955fc64-6037-4cfb-b308-fa98590f301b)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         135c2c4c-9793-43f6-837a-1809bb969221)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         52c8ce2d-9964-4dbd-a888-fc8d98756398)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5d64998-d704-4e5a-ba6d-27150a19d290)(content(Whitespace\"\\n\"))))(Tile((id \
         32e21de9-5e09-4c3c-aedd-a8ca5abaf975)(label(healthAt))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f83d8375-f4fc-463b-89ef-32898fe88d5e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4df67634-5bc6-4076-8632-ef54ff5a5d16)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc39a25d-1387-4e2f-9a73-8b4202489dd3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5271d94-79bb-4bdb-b2f3-dd99fb472004)(content(Whitespace\" \
         \"))))(Tile((id \
         4c96452d-2899-4d1d-8139-55b23d8dd9d2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6158cdbc-ee5e-43ba-a003-ecb8dc44da22)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5364a3a-a69e-4885-9f27-e4f669471ff8)(content(Whitespace\" \
         \"))))(Tile((id \
         4ec3a72f-4b48-458d-85e3-de6b415883e9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c74323ac-811a-445b-a9a3-cd0e5b50bd29)(content(Whitespace\" \
         \"))))(Tile((id \
         a226957d-941b-4178-acaa-9cef1309b7dd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10ec5638-c749-4d97-b779-17fec41b865e)(content(Whitespace\" \
         \"))))(Tile((id \
         15849c37-e2e6-433e-8ee4-5ace027e943e)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd475fb3-e84d-4400-b262-5ab19a7a2e01)(content(Whitespace\" \
         \"))))(Secondary((id \
         dfb5af32-b0ed-494d-a24c-1257c1cc446c)(content(Whitespace\" \
         \"))))(Secondary((id \
         580eea4a-e9bd-4804-aa88-eacd66787461)(content(Whitespace\" \
         \"))))(Secondary((id \
         40273549-35e1-4af7-ab28-1e8ee014e011)(content(Whitespace\" \
         \"))))(Secondary((id \
         2464deaf-f5b7-48bb-95f0-25fac568b063)(content(Whitespace\" \
         \"))))(Secondary((id \
         17d72097-b22e-4c68-af41-e5cb0260a8d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         72d9c95f-c78b-4c3a-b307-a0117dfc609f)(content(Whitespace\" \
         \"))))(Secondary((id \
         26536b05-2e1e-451e-8e48-7eec45c40cb8)(content(Whitespace\" \
         \"))))(Secondary((id \
         bb6988b8-6400-494c-9442-15a3da81d630)(content(Whitespace\" \
         \"))))(Secondary((id \
         ab533ba5-bdd7-4ab2-9fbd-9b3eeaee1549)(content(Comment\"# 50 + 10 - 10 \
         = 50 #\"))))(Secondary((id \
         be1daabf-962a-458f-8d30-9ddfba750228)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5e20bdc8-ce7c-447d-a673-26449aeef7b3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7b1af26-5129-4e42-a31f-8a82152fb69c)(content(Whitespace\"\\n\"))))(Secondary((id \
         092ca4af-fb98-4eb8-b087-53f6027f0e65)(content(Whitespace\"\\n\"))))(Secondary((id \
         32b69b82-382d-4fb1-935c-1c218e9f8e1d)(content(Comment\"# Demo: A \
         companion garden under moonlight #\"))))(Secondary((id \
         88c8b703-4858-4270-a4bf-1e1f5206a75a)(content(Whitespace\"\\n\"))))(Tile((id \
         ce4e18f2-2d86-4d99-b927-2e2f3ba2c00d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e293c609-b246-4e45-866e-d9149409d436)(content(Whitespace\" \
         \"))))(Tile((id \
         11e76eab-b841-4508-8147-c2cc49837f7e)(label(moonGarden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7aeef624-2700-4782-842d-02058df56e85)(content(Whitespace\" \
         \")))))((Secondary((id \
         8fa32acc-6b1a-42dc-8230-283852b57fa6)(content(Whitespace\" \
         \"))))(Tile((id \
         931546fc-0bad-4f14-87dc-1193da0b51e7)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2f9f54b-d0dd-43f0-b013-ae6dc6aff9bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7c9d269f-47bc-4d45-9fda-a3b0ab47a74a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a731bfa7-55f6-409e-b6ec-0980df654bfb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         848b9ef3-8e9d-4ccb-9b12-c5da833d9704)(content(Whitespace\" \
         \"))))(Tile((id 4291bcfd-78bb-4294-8604-a819f15cacd9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5e94a216-9682-4690-9f88-73c80c23d114)(content(Whitespace\"\\n\"))))(Secondary((id \
         6eaa3a7a-1749-42f0-8192-fcae0ee84987)(content(Comment\"# Plant \
         \\240\\159\\140\\177 in center #\"))))(Secondary((id \
         b21b9edb-d43e-435a-bfbe-af763f5af83f)(content(Whitespace\"\\n\"))))(Tile((id \
         2d121848-dc29-4d13-8615-61b6f25d82f6)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57a41af4-c57f-410d-8e86-4f09ba707f67)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bdf58a34-af7e-4ca3-a4a2-b0b05cd6f851)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         756589ac-001d-493e-98e4-356946fc70d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9aeb2e5-2853-4d25-a7f0-31ce4b5c3e5a)(content(Whitespace\" \
         \"))))(Tile((id \
         79fc44b0-cbf8-4b70-87c4-55558363b6e6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1f8d988b-5c3b-44c1-b830-33638e238225)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         761ab569-d972-4065-b721-4eab1401ca9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6add15cc-aa90-4864-ae94-7e124b036fb3)(content(Comment\"# Surround \
         with \\240\\159\\140\\191 companions #\"))))(Secondary((id \
         ae5da365-1fa8-4f02-bb35-54a3e127f01f)(content(Whitespace\"\\n\"))))(Tile((id \
         4bf02056-eb93-4038-af40-e88952511ed7)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34ecf175-daa7-4d99-9620-3dd74d9b0351)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c24410c9-9d76-4380-b6dc-d99c80f8baf2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         96efa27c-7f29-4c3e-b2a5-3785127c1eb8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         365931b2-f349-4d16-9c94-ab35b1e1a959)(content(Whitespace\"\\n\"))))(Tile((id \
         58114aca-f8f9-464a-8c1a-2821840e511f)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06396519-bfd2-419b-be4d-744bb420f57b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0615e00b-530a-481f-86c7-0a61209ee05a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         360dd598-0c06-42ff-b7dd-554e355f1d1e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd3b6180-29de-41a6-b4f3-207c59a32ac9)(content(Whitespace\" \
         \"))))(Tile((id \
         477888e3-90dd-4b39-a8ea-e558e173bec7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0a264175-2f79-4df6-b919-b94a2b29cbd7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7380b8bf-fd9b-4caf-8fa1-d9c5fa4d0bc4)(content(Whitespace\"\\n\"))))(Tile((id \
         36ef1ebb-57cd-47f4-bc56-fcee5214c199)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdfe4763-6830-4c26-be72-0b5a8eddca80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8434b295-36ff-4067-b7e5-e955afe549f8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d42feb9-8831-4ffe-a695-a2f262994a48)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e087004-b4ce-4048-89ca-9c7fd2378038)(content(Whitespace\" \
         \"))))(Tile((id \
         bdb632d2-59c6-47b9-a382-242c9d29d052)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3796b6f1-0432-4595-bce9-d9b1be2efe72)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6e3a0c5-a501-4555-a391-38a7507e0c4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         66d6927d-1e26-422e-a6a9-846c392b4443)(content(Comment\"# Add some \
         \\226\\152\\152\\239\\184\\143 #\"))))(Secondary((id \
         1bf9264e-c778-406f-bf83-4a33b98541f8)(content(Whitespace\"\\n\"))))(Tile((id \
         3e3c12ba-8240-4bfd-aba8-6dcb3b1ddeaf)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70939369-10df-4c69-86fc-e36c423cb686)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2711f78f-8982-4ec4-b35e-3fa5a9d9f15b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8181014a-df1b-445e-8964-efa4d588eb05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f07463f-d188-4bb4-837f-f8def2aa0c81)(content(Whitespace\"\\n\"))))(Tile((id \
         dc7e0fb5-f3c3-4ae1-bd72-43f8be87d113)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96c242a7-e6f4-458a-b59b-aa2ba26d2318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a09d90a2-94a1-46b7-b224-004d877afaca)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1751009-7104-485b-b27b-04e37a7e744b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a114c084-0318-4315-966c-a1b8025fe5d0)(content(Whitespace\" \
         \"))))(Tile((id \
         b0bd159b-096b-422d-a495-f7d6ee7c6e56)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         75e3a796-a8ab-414d-94ac-16ff0ce11070)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         759cd568-8f3a-4b64-abed-17ccf7c1469f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fcb067d-e0ea-4a2a-8aae-9d57b34f9a10)(content(Comment\"# \
         \\240\\159\\141\\132 next to \\226\\152\\152\\239\\184\\143 for \
         companionship #\"))))(Secondary((id \
         ba59412e-ffc1-4622-a7ae-98da80edf2b7)(content(Whitespace\"\\n\"))))(Tile((id \
         316b153f-0cf4-4d05-ba2a-80e1ede3087a)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9979e583-5551-46f3-a765-46d5c4dadaf9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e5b9743-455d-4591-a5b6-db51f5a9fc0f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7c996d8c-a245-4012-96ac-6e986bf10455)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9977946a-3e14-4d9e-8da6-28fc4f734110)(content(Whitespace\"\\n\"))))(Tile((id \
         3ff472c8-c4be-421a-afae-7931c2a6dd2d)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e84a308-5fee-42a2-9bd8-478d9763d336)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f531472d-af04-4d47-ba2f-e1dfb669b98d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         394d0223-0169-438d-866a-1f9e7a3ce778)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bddb397-6cb0-441d-a0b6-1bf4e6df2337)(content(Whitespace\" \
         \"))))(Tile((id \
         823e920b-c87a-43be-8407-446fb00f3d8d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         06134ef2-82f5-49c3-a124-f60c347a1b29)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f5f2013-7a9b-4b5e-81c9-bcdd2b19dc5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         480e250e-f576-4b4d-8c7c-6278b70928a6)(content(Comment\"# Calculate \
         how they affect each other #\"))))(Secondary((id \
         968e2fe0-9300-4200-91cb-8cd49d517d62)(content(Whitespace\"\\n\"))))(Tile((id \
         c0b3474e-8e85-4fec-b88a-a08edd7ad89b)(label(CalculateHealth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ffb3f42d-97fd-461d-9672-7ccec1b8bdc9)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         9a1393e9-67ac-4214-8182-70af2f3d9380)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f9ba0218-98d8-4ffd-8f44-402442f35b4c)(content(Whitespace\"\\n\"))))(Tile((id \
         a5e527ae-6edc-41a9-ad3b-6646b2bc0262)(label(moonGarden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47709b4b-7c41-4277-bb79-60e38b180a92)(content(Whitespace\"\\n\")))))";
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
