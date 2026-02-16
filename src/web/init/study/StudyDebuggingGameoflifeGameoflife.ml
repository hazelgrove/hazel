let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / debugging / gameoflife / gameoflife",
    {
      segment =
        "((Secondary((id \
         deac7bf9-c1fc-4f84-bca0-9f9f7c8b060d)(content(Comment\"# CONWAY'S \
         GAME OF LIFE #\"))))(Secondary((id \
         4b332a54-22ad-4a5a-9565-9b011779a8de)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad6891db-f90d-4301-8727-1e7531c85320)(content(Comment\"# Cellular \
         automaton with birth/death rules #\"))))(Secondary((id \
         337c22c3-5259-4020-8ed0-509109b1471f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0c24815-a1b7-482e-bb7b-b9e8eefe7235)(content(Whitespace\"\\n\"))))(Tile((id \
         bd5e3be9-d33d-4ce8-90a0-b637a4445091)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         331ddf8f-82bd-45a2-b2a2-2a315a05ee9a)(content(Whitespace\" \
         \"))))(Tile((id \
         83a64700-5c7d-48de-a2d8-90bf5c76b891)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ae608eb8-ef51-4a0d-a96c-74c018601b66)(content(Whitespace\" \
         \")))))((Secondary((id \
         5c5d0da3-8e71-4187-97df-8537b62d67e8)(content(Whitespace\" \
         \"))))(Tile((id \
         9ac15e1c-7d3e-4a48-8334-530e6e62ab7c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5d85954a-4e89-4dab-9c95-43092bcff32e)(content(Whitespace\" \
         \"))))(Tile((id \
         001907da-ebd0-4907-ab28-dd7d027487d1)(label(Dead))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         69e9b1b0-8dd7-4e4c-8ec8-30f5f055c0c6)(content(Whitespace\" \
         \"))))(Tile((id \
         ed8d4c07-787e-4a02-8144-780cb35e37d9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d3522973-0298-42f8-babb-8492b474a47d)(content(Whitespace\" \
         \"))))(Tile((id \
         0b1c7d9d-d5a1-4f53-a2df-0ca06d6f8372)(label(Alive))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         918c1494-7d42-4a82-9d6d-09568e66c028)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56d43203-cc9a-43fd-8023-5f550c3b0eb8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8079e9ae-7fb3-4c7a-95ad-31174160d88e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f02d6a7-ba4c-4bfe-8fb0-65c778d51ce9)(content(Comment\"# Grid is a \
         flat list with width/height metadata #\"))))(Secondary((id \
         3f9f8694-a91f-40fa-aeaa-f0d2e985ea4f)(content(Whitespace\"\\n\"))))(Tile((id \
         8aae2439-fe64-41f0-9eec-c7b34f86a503)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3b406d0-36a9-4653-b682-8d5c802de12e)(content(Whitespace\" \
         \"))))(Tile((id \
         caef31e8-7058-4b6c-914e-c2ef50621287)(label(Grid))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6c960505-0fbf-4d09-b0a7-eb7232fb6a64)(content(Whitespace\" \
         \")))))((Secondary((id \
         7608ef0a-fb58-4f09-af04-bea6b46824be)(content(Whitespace\" \
         \"))))(Tile((id \
         e30fca0c-fd48-48ff-976f-fff85d1d6e5c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         73c8982d-dd0f-465c-adc0-0a1728f33014)(content(Whitespace\"\\n\"))))(Tile((id \
         7f91a44d-3e99-4469-bf0c-912c5704580e)(label(cells))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6fa63ead-112e-4313-a67c-611f6eb3c5ea)(content(Whitespace\" \
         \"))))(Tile((id \
         80af4ec3-55e7-42df-be7a-ff1c784ffa23)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ad6ddca4-9852-46fb-a264-19b8855c170d)(content(Whitespace\" \
         \"))))(Tile((id 766ca1cd-41ed-4bb7-a66b-d30f3ec5414c)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         989cfa4f-6076-4898-9bda-4471af3ae8ae)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         15ac2191-9334-4a6b-b25a-f0ca82f0ed78)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8027c7fb-d7ff-4af5-a3c2-fd32ab256f94)(content(Whitespace\"\\n\"))))(Tile((id \
         ea583b01-5f8d-4bfd-865f-5695b6a14576)(label(width))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a79ac816-3256-4efe-a295-f6b9d77f2ffb)(content(Whitespace\" \
         \"))))(Tile((id \
         41a3e66c-e8d6-49ec-91c5-e0d18271e733)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5a9fee2e-3510-40a2-8104-356214c85cf4)(content(Whitespace\" \
         \"))))(Tile((id \
         1281f69d-38b6-467f-b2a1-1b29dfb8bafb)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cae665e0-ae33-4b5f-a43c-f561153a8c2e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         734d57b7-7022-402d-8ada-8d3855dd8400)(content(Whitespace\"\\n\"))))(Tile((id \
         474d4ce1-de69-49ec-a7b0-d69c2c56f3c2)(label(height))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6dc5328a-3bfc-43b1-b2df-ae1243847a1b)(content(Whitespace\" \
         \"))))(Tile((id \
         c76188a2-9685-4baa-9ffe-5d6261f50abb)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab8a37cd-08fd-49cf-94c8-e3da1c4a6e33)(content(Whitespace\" \
         \"))))(Tile((id \
         0d2d85d3-6804-4cef-9d7d-bb8751e1369b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e7e304d8-f87f-4046-b6c1-9bbd673a359b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aea4f198-7c44-4ad9-9b69-a23a1658ebb1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6f904864-ea65-48ec-a2cd-a3e0b7d00b67)(content(Whitespace\"\\n\"))))(Secondary((id \
         11f521a2-9dca-4bc4-a634-172efe96cd81)(content(Whitespace\"\\n\"))))(Secondary((id \
         99fe20f7-140c-4526-bc43-66483b1d5c61)(content(Comment\"# Create empty \
         grid #\"))))(Secondary((id \
         5f27975e-7680-4c6f-bab8-aa37c66bfa25)(content(Whitespace\"\\n\"))))(Tile((id \
         16752875-a2a3-48c9-ac37-d049e0fd6716)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         084efd8f-b4fe-45a0-abc7-f9c2565b792a)(content(Whitespace\" \
         \"))))(Tile((id \
         67bec6ed-7598-4f84-adb0-a4add9b8fd89)(label(makeGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aec2cb66-d230-4493-ae33-1ac5a7586b52)(content(Whitespace\" \
         \"))))(Tile((id \
         dd9467a4-9261-4db2-ab14-c2ff3fa190ef)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         39eb0514-5e93-4eab-aef6-8a1727d21c36)(content(Whitespace\" \
         \"))))(Tile((id \
         57d8d4cb-9687-4d77-b50c-cb4d30a5ed52)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f9233210-adf4-4fd8-8d8b-9490533cece3)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ff53326f-1786-453a-9c60-a6ca6336ca7e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d12506d0-7c3d-4280-bfa7-1de4fdedfacb)(content(Whitespace\" \
         \"))))(Tile((id \
         57ee4aa5-7ec4-48e1-8a14-d0b1a2429167)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1d376973-b31a-4ed2-9fbe-92e717657d0e)(content(Whitespace\" \
         \"))))(Tile((id \
         83c76eab-a2a9-4780-8071-cd0a331bccbd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         38e74f68-c505-464e-bc8b-5263ffaee31d)(content(Whitespace\" \
         \"))))(Tile((id \
         c100eaf2-4920-46bd-98ed-66ed2726416d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b71c8ea4-e732-4d90-9e4f-d279d05a422f)(content(Whitespace\" \
         \")))))((Secondary((id \
         a2536ce8-9da0-4f34-90d7-6818568c033f)(content(Whitespace\"\\n\"))))(Tile((id \
         4254418d-4a76-462b-b0ef-6bdbe19e4659)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c2552438-5a6b-4439-b08a-a19c2c547a77)(content(Whitespace\" \
         \"))))(Tile((id \
         0ffa6138-11fa-4237-b2cf-636b9b1ba779)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6c470e9b-90e3-42a6-a750-22f6fda77ffb)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         94d280c6-201a-4712-a2af-b35d40ee0ae6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8df44a36-b7ee-4c14-9fc6-6c59b480255d)(content(Whitespace\" \
         \"))))(Tile((id \
         a6f7db74-7b5b-413f-9fe2-540a5d91b910)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         119954d7-937c-4ccd-bed7-ab8f91d46f5c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a9eacad3-8252-4e98-9687-e16448483cb8)(content(Whitespace\"\\n\"))))(Tile((id \
         9d7b63f2-b027-4705-a176-0a5b61fbc7e8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c29e42e8-411e-48be-bc05-27a7c75c5976)(content(Whitespace\"\\n\"))))(Tile((id \
         b3f6f463-2578-4358-a1d7-224a9ce9da35)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7963044a-8165-4a43-8213-bffcb6528a64)(content(Whitespace\" \
         \"))))(Tile((id \
         a9e550ad-5ae7-4be8-82a4-e601030aae59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81fca5f5-f5ad-49ac-88e8-6a53fc35ec22)(content(Whitespace\" \
         \"))))(Tile((id \
         b46e0b37-7366-4501-892e-5253edbf08f0)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04c045d4-a299-4dbb-b336-e2adf6e7faf3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91435d4e-7cee-4cc1-9f64-de301b44bb38)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         268f3bc5-3610-4596-a7e6-10dc8141f586)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a5cc935-1c91-4632-8021-95b7d1407b0a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea2b8a8f-939b-4bdd-9023-55906cd70e98)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7eacf351-8972-4398-8d2e-6caed0b3ab57)(content(Whitespace\" \
         \"))))(Tile((id \
         f99d4e16-e9d2-4a63-be00-6c96cf485fc3)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44c848f9-9242-48a6-babe-526bf262909a)(content(Whitespace\" \
         \"))))(Tile((id \
         eef8722b-4107-4d43-913e-1e561d239139)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfd5de2c-1833-45ba-865a-232d243c9153)(content(Whitespace\" \
         \"))))(Tile((id \
         585d93e3-aa20-4046-9333-c3427fe895d0)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3a62db2-2d3c-4b51-b45e-84909dbcd9bf)(content(Whitespace\" \
         \"))))(Tile((id \
         4cb5991e-f4b5-4afc-bd4e-08cc8657ab5a)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19606e12-6c03-4c5e-9e5c-209be0d150ab)(content(Whitespace\" \
         \"))))(Tile((id \
         1213d19c-d7c7-454f-bd68-20fcb8ddf8d7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a1a9e080-95e9-4c3d-8785-4e4320c5afd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d86ef965-fe44-4275-9e48-fa91d58a1945)(content(Whitespace\" \
         \"))))(Tile((id 92c83edc-eba8-428f-b5f9-077fff30b406)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cfb32fa8-3874-4b14-9edd-16021b003554)(content(Whitespace\" \
         \"))))(Tile((id \
         68cfdaf8-0940-4204-b41d-dcd3c918cfaf)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3918d579-3186-405f-935b-4f08b3631c47)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f03b5a87-e409-4118-9daa-16cd696d8477)(content(Whitespace\" \
         \"))))(Tile((id \
         58b4c962-8f31-447d-8e42-9e40fcc566ae)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         765819ad-d72d-4393-9b38-b205fae682b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a747e16-a53a-4746-87da-f9734856457d)(content(Whitespace\"\\n\"))))(Tile((id \
         f96ec53a-bbdf-481b-ad4c-af39726547bb)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1cc42a81-e2eb-4ed6-b2ed-2a06df6f8170)(content(Whitespace\" \
         \"))))(Tile((id \
         41f62fad-adb5-49fc-8719-c41cea6ead2c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dbeb89ef-6c52-49b1-8638-f690a2985546)(content(Whitespace\" \
         \"))))(Tile((id \
         e4040d17-5445-4753-a163-1433c6b2df82)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         157be206-40ea-47c0-bcca-323cf3de6a74)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ade43ab6-1847-47c4-8397-f226035cc5cb)(content(Whitespace\"\\n\"))))(Tile((id \
         6c1edb02-9657-4491-a653-d2e5262d49f3)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7bc320ee-67a0-4d1a-a0cf-9ab6b6ceda7b)(content(Whitespace\" \
         \"))))(Tile((id \
         40e4f329-6a7f-4275-be3e-6517cc194801)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d6c7281-0ca9-4c34-b162-8a55bffa4ed0)(content(Whitespace\" \
         \"))))(Tile((id \
         853b27fa-4a0f-45d1-940b-6c963dcf5ca9)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8892e482-2684-44eb-ad27-0ffce85afd94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b6b4cd7f-5444-4174-81ff-826d5dee0378)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7ebebe74-07fc-4c59-83b2-dd5f42952293)(content(Whitespace\"\\n\"))))(Secondary((id \
         eddc170f-5e7b-4580-ae43-acff82408901)(content(Whitespace\"\\n\"))))(Secondary((id \
         0379e03d-3da3-4368-9e79-94a973ef7365)(content(Comment\"# Convert (x, \
         y) to index #\"))))(Secondary((id \
         f48d87f7-b1b8-4457-ac46-908a1c331e1b)(content(Whitespace\"\\n\"))))(Tile((id \
         233c7fa2-9c64-4579-bfbe-c727061cbc41)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f342d79e-67d2-45bd-bc6d-db2d8f88083b)(content(Whitespace\" \
         \"))))(Tile((id \
         6cc81eef-1010-4353-8c94-65e9f5489839)(label(toIndex))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ec96853c-38c6-41e8-bf66-31c551188cdd)(content(Whitespace\" \
         \"))))(Tile((id \
         653bfe36-1e05-4882-962f-abf9472977b1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         99ab00fd-2582-467d-8ae8-946de050b8c7)(content(Whitespace\" \
         \"))))(Tile((id \
         abfd1cea-752c-4d56-9a5a-f81ce77dba44)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         afff02f8-a7ef-4858-b0cf-0ba4ecf4549f)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         06865286-beaf-48cf-9a3d-7b1e49570efe)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f98befce-0637-4deb-93a8-a0b36c943a2b)(content(Whitespace\" \
         \"))))(Tile((id \
         86150645-cb53-40c2-bd79-61d160c987af)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         969a06f7-6d62-4181-9ffe-b94ebd2a0cf4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7a3053c3-eadc-4806-89bc-c9b9dae60608)(content(Whitespace\" \
         \"))))(Tile((id \
         5f79eeb7-b4d0-4be5-a229-d3a79f939208)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d3273b30-9626-4c0c-b24e-76562a69099e)(content(Whitespace\" \
         \"))))(Tile((id \
         f3e4c689-0830-401d-9a00-9634ba3afa7e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fc799031-ad5b-47b5-ad0f-7e3369b7f9c7)(content(Whitespace\" \
         \"))))(Tile((id \
         b8e4b11c-45da-4af0-a0f9-5806f2baee31)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         87e2ee95-7b01-4274-9296-073121343e8a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6a8bc7f8-6098-4833-95a1-0a8064e3b4c2)(content(Whitespace\"\\n\"))))(Tile((id \
         000905ad-c0b6-4d02-a32c-f917fc576568)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e9bd4e0f-76dc-4d01-af40-cbde0d7587c8)(content(Whitespace\" \
         \"))))(Tile((id \
         a1adc21c-c068-4ed1-88dc-7e0105afe8f5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         44b4bd52-c08d-4fad-91a3-feb9902d80c2)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fd325f42-65e2-47d9-8f18-fc58ad1bc55f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eef0226a-a494-4c05-9090-f0650c54a383)(content(Whitespace\" \
         \"))))(Tile((id \
         48765020-b4c6-4cab-8536-bbedcc658c66)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         229df71e-9827-41e1-b8bc-73aac95d5b9d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3fe54a92-b0c0-42d2-8e02-aa685b754b49)(content(Whitespace\" \
         \"))))(Tile((id \
         378646d3-344a-4f2a-b854-19a69a361b40)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0abdac4c-628d-4246-94c3-9be42c1f8892)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7874e1e5-a79f-4c78-a4b1-e5fd6ed2e2a7)(content(Whitespace\"\\n\"))))(Tile((id \
         5134cdd2-00aa-4620-9eb6-8a1e5bcf1851)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0de1c90d-dcef-4519-aea8-c0ebf9bf33b3)(content(Whitespace\" \
         \"))))(Tile((id \
         31b7e4d7-8c2c-4f80-9d79-e8c035095f8b)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b651a80-629b-4f16-8c09-cafeff605dc5)(content(Whitespace\" \
         \"))))(Tile((id \
         0fcedd92-ee36-4080-adec-7447b7ac46ef)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4feb19af-d98d-49ff-bef3-15f15c3808d8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b1f95403-7dba-4875-9339-6c962b2e58ee)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5cbb6a0c-4010-4c8e-b0ca-d24c499a3c3c)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb13ef1-5f31-4fb4-af01-4e62ba5472e0)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         faeac23b-86a4-412d-9e67-1ba4577000b0)(content(Whitespace\" \
         \"))))(Tile((id \
         c8d542f8-b222-44bc-8d86-3c690a8535a4)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a3ef87b-0902-4843-a900-34a0201fbb8e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1e72a675-7e83-418e-ad72-20bdfeed927a)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a0fdf99-2e39-4c62-8951-4924a5206f0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         08f4e966-5e83-4587-879a-fd4acc0138c1)(content(Comment\"# Check if \
         coords are in bounds #\"))))(Secondary((id \
         c38bfc03-c09f-42e1-8738-1f8e66068353)(content(Whitespace\"\\n\"))))(Tile((id \
         a90e3db7-fbf1-4a9e-aa7c-4159cd7169b0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2736e18b-6c6c-4c27-bb60-6b7f6761768d)(content(Whitespace\" \
         \"))))(Tile((id \
         96379db7-8cf5-4fb2-a25d-62f4dfdb972c)(label(inBounds))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9021ec54-ab42-4867-ae72-957dc3eb21a1)(content(Whitespace\" \
         \"))))(Tile((id \
         a8d6c799-f0fe-4b73-8152-0690598161da)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6c1bef0d-7f69-4907-8945-8408f840bbd9)(content(Whitespace\" \
         \"))))(Tile((id \
         f0ecaf39-d35b-446a-97e3-0690310a9a93)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         62278dd0-ddbf-4574-bc60-af18570ecd35)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a73a8186-1c78-40bc-b4db-5d35ca55d05f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         321a098e-0915-43e4-9942-9df0091ee492)(content(Whitespace\" \
         \"))))(Tile((id \
         15611b28-b35e-4172-b2a5-d1ccc0e7a712)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a60c87d3-3052-47ae-bbeb-77f6f3076918)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a81977f0-4e5a-4625-b7ef-7f8aa375dc4f)(content(Whitespace\" \
         \"))))(Tile((id \
         3fe5868d-4bbb-40a1-8148-3981dfc348f2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0fbf9486-f702-4255-8daf-53babefff2eb)(content(Whitespace\" \
         \"))))(Tile((id \
         d3f86164-75f2-4bfd-b364-76e684176c5c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9b7a733c-c86c-402b-890d-462aa159e505)(content(Whitespace\" \
         \"))))(Tile((id \
         4749c11b-dd07-4fee-bfcf-39c425c03e15)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         af743d1f-4482-41ac-b4fd-27f7c7bf2a7b)(content(Whitespace\" \
         \")))))((Secondary((id \
         762f95c2-3f01-4e75-8a5e-ba562b1a3c83)(content(Whitespace\"\\n\"))))(Tile((id \
         bb4ff2bb-a7ee-4fb1-a38a-7d1701f576f6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9db9f594-c9fd-4de7-8837-93c21a9d5de4)(content(Whitespace\" \
         \"))))(Tile((id \
         2c7df93e-7742-4a72-82b3-ec58ed1d0314)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c38acdb9-f31e-4a2f-8aa1-9de9aabb83a9)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         863275c9-522b-4668-910a-7852c42e5846)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3368547f-95c8-4e68-915c-9a1ad1a7c253)(content(Whitespace\" \
         \"))))(Tile((id \
         ab9cf37f-eb93-488b-955e-6d1401248f85)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         04819d75-1ced-47fc-abe7-a32868c0cec2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         201dd3b2-8811-4913-8dd3-cb9639add420)(content(Whitespace\" \
         \"))))(Tile((id \
         046c65de-f0ee-47f8-a15e-de3019d7d51e)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ad666dd8-89d2-435c-9d28-37b324e0a7f9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         596b2e6f-e040-4599-bf15-e00ab21e1642)(content(Whitespace\"\\n\"))))(Tile((id \
         123d2ee3-1db7-4aa3-b591-b90de3105696)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c726cb6f-b882-4e80-8067-7ef9b1051c2f)(content(Whitespace\" \
         \"))))(Tile((id \
         f67a9ab4-8e19-4b42-b2ff-db4b4a37277b)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cc32883-e6f2-4f11-8775-b68e19946126)(content(Whitespace\" \
         \"))))(Tile((id \
         ff44e591-d73d-4b87-9cb3-40feb39651b5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75f60073-8817-4035-9d34-6db7a46c1517)(content(Whitespace\" \
         \"))))(Tile((id \
         4c90861e-ce78-4c7d-b215-de5f08af0745)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3c7be5b-b610-47d4-80ad-fd1d1e4c611d)(content(Whitespace\" \
         \"))))(Tile((id \
         a601eb87-328a-422e-93f5-0b36f1c1ac5a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8eedf16a-d4a6-45a1-8d56-4833639a055c)(content(Whitespace\" \
         \"))))(Tile((id \
         e74c3138-66ad-4c44-b18f-8a2fe340323c)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b21084c8-39b8-41a9-a557-857717795a38)(content(Whitespace\" \
         \"))))(Tile((id \
         798af9dd-14a7-4858-80f0-b57f1f056084)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65b5d006-2e06-4fb4-920e-9431e7592add)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2dc01b90-0d03-419f-ab37-44188609e50a)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         59bb6555-6f88-45c1-abed-a5e4d8aeb0e7)(content(Whitespace\" \
         \"))))(Tile((id \
         694fd5f8-5653-43cd-b64b-5caed7fbfc1c)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         699894aa-dd5a-4a35-a934-74717e82012f)(content(Whitespace\" \
         \"))))(Tile((id \
         7c8c87ee-bb8e-4eea-be6c-cb5b2ac44d06)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4be3ce9-63f6-42f8-a38c-b0eaac33aa79)(content(Whitespace\" \
         \"))))(Tile((id \
         d825525f-5001-487e-bbbd-7c6404706b4b)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         729203e7-6618-42ef-9495-186541268ce3)(content(Whitespace\" \
         \"))))(Tile((id \
         e5c621c8-026d-427b-8aff-61409f24d745)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c49cc4e7-b615-4a45-9523-30c467b2e35c)(content(Whitespace\" \
         \"))))(Tile((id \
         276a4585-c632-45fa-85dc-aa567f02538b)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ed0d28f-1452-4090-9568-b380c493c500)(content(Whitespace\" \
         \"))))(Tile((id \
         bb053ed3-cf8f-4f62-a660-97cd88ed2658)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6f706d02-c7db-4d42-93c5-b03806fc3cbe)(content(Whitespace\" \
         \"))))(Tile((id \
         c6d62e21-7123-4c59-b0ee-bc1480429c86)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e3a6eb2-ddab-4b3e-a5f6-53dae403d526)(content(Whitespace\" \
         \"))))(Tile((id \
         84ecfa00-3641-49a9-ba72-37b42fb891d6)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb8d1466-def6-4c52-be58-bbe414212968)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4a77ceb0-56d2-4f07-b8ba-4c5f3552516b)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         563c39a9-6947-4077-a259-e1b130777fd4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0605e6d0-89bc-460f-b7e2-e0805fa575f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce18eb09-217f-41a6-b973-bbdb03fc3f1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4d65436-096c-499c-9209-47c8d03637dd)(content(Comment\"# Get cell at \
         (x, y), returns Dead if out of bounds #\"))))(Secondary((id \
         e30e5c8e-b892-41f2-96c0-66c093648ab4)(content(Whitespace\"\\n\"))))(Tile((id \
         8fa43906-b826-4772-9653-ccd39e8308ac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         98c6f22f-d7b5-4f82-ba78-f86372d4a76f)(content(Whitespace\" \
         \"))))(Tile((id \
         1c686cdc-0a83-433e-a20f-13862072d270)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24bef601-c817-43d3-bdff-ca4d26fb25f9)(content(Whitespace\" \
         \"))))(Tile((id \
         a0e8bda5-54dd-4ac4-81ef-6874884dd9f4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b6dd049f-fed4-4426-a292-16698fff88f0)(content(Whitespace\" \
         \"))))(Tile((id \
         2caadc5e-d7ea-4db8-8b57-d8ecf9293c6c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ba25c406-4eca-4414-932f-97f2ed0c1450)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cb319632-05c7-4350-808e-256294e38b7c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b51d401f-31cd-441c-8bd7-3f3d033db56f)(content(Whitespace\" \
         \"))))(Tile((id \
         59e445cc-90a3-4b3c-8523-5080b2ee40cc)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c88e719c-e036-457f-80f1-85f326922a47)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e5ef3122-96be-440d-a692-ca4e4a13dee8)(content(Whitespace\" \
         \"))))(Tile((id \
         2775118f-069c-43e3-882c-3d33e7483922)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1028f0a8-f590-4843-9939-a925c9af3809)(content(Whitespace\" \
         \"))))(Tile((id \
         4995cc0e-87ef-43b2-aea4-b0e9a093de32)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a502fcf8-afef-423f-9f0c-7add673a6db4)(content(Whitespace\" \
         \"))))(Tile((id \
         103aab09-5350-4d0f-8160-e64ef5f60677)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f18368c8-0cb2-4d07-a217-3e730734ddce)(content(Whitespace\" \
         \")))))((Secondary((id \
         3717da90-7217-44eb-8f37-cb6a1c71b804)(content(Whitespace\"\\n\"))))(Tile((id \
         407ea1ed-206e-4637-9692-9873d3992d74)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c228d56b-a3df-494a-9e4f-6c739eb8bbe4)(content(Whitespace\" \
         \"))))(Tile((id \
         2e40ddc5-3482-48b5-9c2f-4ced19459164)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4066879c-3a89-4a14-b1b2-1bae289f3596)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         60c057df-aa58-489a-a580-d9ac25eb29c5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         68386cd6-a914-4b13-9111-b7c34ce5c42d)(content(Whitespace\" \
         \"))))(Tile((id \
         e385e9e4-524d-4a75-8a78-211b91ceb3ba)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f1faab24-feb9-4ce2-8df5-4718cb4cf6d0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         63c214c2-6daa-4fd2-aa5d-a0dbb481fdbf)(content(Whitespace\" \
         \"))))(Tile((id \
         17fb40d9-0cdd-420a-a38c-b6b99361ff3b)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         142815b3-47a2-4516-9f35-7c33b31e9dfd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ea670ef9-ec2a-4c40-86d9-cb6fe44dc53b)(content(Whitespace\"\\n\"))))(Tile((id \
         b34865c8-2ee2-46c1-9e5c-16d1f2e8e35a)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5593dc07-0604-43a9-bf75-461b9a06a582)(content(Whitespace\" \
         \"))))(Tile((id \
         36fb4b88-d925-4cac-81a9-0b71823e1ec0)(label(inBounds))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         719b6b94-fd3f-4a45-becd-781bde5336e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c583a379-669d-44d7-9093-df2f53ed36b1)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cd78b4b-2f78-4d52-85fd-83d15dce12a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf944734-cf2e-43f6-a173-5d34035b9507)(content(Whitespace\" \
         \"))))(Tile((id \
         40378d62-412b-4a8b-b366-82dd9ace0837)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f29a7c6-3660-4755-b20a-f3a5279cb86b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89b2c0b6-0667-42a9-bf3a-9c05e46fc5f5)(content(Whitespace\" \
         \"))))(Tile((id \
         16e1dd4f-404a-4a19-9083-371737097ef0)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5880b01-2a82-41d9-a078-14b421bf2b92)(content(Whitespace\"\\n\")))))((Secondary((id \
         2e38e1fa-f1d7-4f09-bf0b-ee4ce346cdb0)(content(Whitespace\" \
         \"))))(Tile((id \
         905f84d7-c076-46b7-a9e8-9f2514dec5d2)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c04ea3cb-4441-4bb5-83dd-6df573d65a85)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fb93c7d7-8b17-45db-a3c5-61cfb2566b05)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         833e0273-2b52-48b7-92aa-9949cee4d4b1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4782473f-c305-4568-9be2-db3018a7b87d)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         869edd49-aca0-44ac-8b1d-418062b4fd42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f605f0bd-6350-4607-bfc5-2acaddd8ab52)(content(Whitespace\" \
         \"))))(Tile((id \
         a1dd3100-2fbb-422b-8882-7a3858111f7b)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ec4038b-bee3-46df-bbaa-c25acdc8182e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f87a81e-95fd-42c1-8ea0-244fef874380)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c3a9d06-8bd7-4714-ac8b-a04173f597de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e98706e-976a-4d0c-bd1e-e11631716be9)(content(Whitespace\" \
         \"))))(Tile((id \
         6603eb97-58fe-4762-83ca-547c004df4e7)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b95c7717-26ec-4d4b-88e2-fcc4a5d5bd55)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92a24d54-0864-47d1-86dd-dd962e8ff704)(content(Whitespace\" \
         \"))))(Tile((id \
         1fae3900-45fa-4cbb-aaf8-4fe54206383f)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fc63e6dc-40bb-4b4e-87e9-72d9d0751876)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2072c80b-cf11-4d55-a618-b973c56582cb)(content(Whitespace\" \
         \"))))(Tile((id \
         d482cadf-b49d-43b8-be7e-1ff4eb208fd0)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef46cd7d-3868-437e-9393-88f0b59d3f6f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         38b0a9a0-a5b6-4b5f-9ca4-889df401c42e)(content(Whitespace\"\\n\"))))(Secondary((id \
         244d025c-96d7-4727-b53a-6db396656258)(content(Whitespace\"\\n\"))))(Secondary((id \
         109f1d0a-f596-43f4-8e53-9860c5654ef7)(content(Comment\"# Set cell at \
         (x, y) #\"))))(Secondary((id \
         033eb72b-1f1e-4109-b2ba-9d43e330850a)(content(Whitespace\"\\n\"))))(Tile((id \
         7a4b46f5-1b77-41de-986a-bf848bdb9cf7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         15a1253d-3ba7-4837-8047-260e07767742)(content(Whitespace\" \
         \"))))(Tile((id \
         4de65597-6fcb-4af6-a819-d8f22ad84830)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8b0c34ec-2838-4417-a490-f84f8301a9a8)(content(Whitespace\" \
         \"))))(Tile((id \
         5f4d8fa5-47b0-4879-8585-848b54ed9b39)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8cf82dbd-5410-4d43-a3c7-8fd01db8d83d)(content(Whitespace\" \
         \"))))(Tile((id \
         e5f47fdf-be18-498f-96c5-256e676a0636)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         981b840e-a6a1-4a15-9154-993243f538b9)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9e6df5e4-d002-4de0-8564-6f9601cdd34d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d1d53ca0-8edc-4831-b75b-b9c6c6459f7b)(content(Whitespace\" \
         \"))))(Tile((id \
         96c25c16-a7c3-4435-b9ad-8bf83f0e85e8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ff137aa8-f2fc-43f4-969b-e2fa50384312)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         38df55ae-7f70-4ab2-acf5-cef025a1f6a9)(content(Whitespace\" \
         \"))))(Tile((id \
         9c0ce12a-f9ea-4eee-8f3a-8c6e589bfe2d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dd1ae1db-4957-469b-b7a0-e759dfc50a7a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         81c76a23-e9c8-4292-867e-9230824735e0)(content(Whitespace\" \
         \"))))(Tile((id \
         b9a45773-c48e-48e5-9c4b-b62db31331bd)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e971880b-923f-4fc3-9a9c-906c10ab7e1b)(content(Whitespace\" \
         \"))))(Tile((id \
         1e6a463e-0abd-45ad-ae0b-f232b01f99b9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0c70ab87-b4b8-4aab-8768-b85720dead64)(content(Whitespace\" \
         \"))))(Tile((id \
         49e1d82c-8e85-4af9-8c16-f03cec35ef7d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ce5dc4e1-1d64-4efa-86df-64b738144bb9)(content(Whitespace\" \
         \")))))((Secondary((id \
         19fdab97-ee80-4604-88f5-051e6ad00df3)(content(Whitespace\"\\n\"))))(Tile((id \
         bde2beab-798b-4d84-9f31-a4a7dd09fffe)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b8bdeae0-243f-47cd-b904-6f787447a156)(content(Whitespace\" \
         \"))))(Tile((id \
         b0b29036-12bc-4b92-bc86-e1866cbe237c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8f410cdc-f139-4fc6-897f-c559c0387937)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f0b2f1e8-917b-4476-983d-5a5c0b873ccc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d447cc28-0e84-4563-93b4-7702fe2e6cb5)(content(Whitespace\" \
         \"))))(Tile((id \
         6d4eb33a-cdeb-4147-babc-2d1ba8f45f10)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         689a3040-80b7-4ca4-a67c-741e6e8fb244)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ce075f92-ac2c-49ba-bbb7-11ba468d67e1)(content(Whitespace\" \
         \"))))(Tile((id \
         7df543e9-b5c3-4f09-8648-26822aa8a284)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1ae8f899-13f5-479a-a22b-ed860bf5ebe9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0efba6cf-65c4-471c-9669-fbd8da37caf4)(content(Whitespace\" \
         \"))))(Tile((id \
         ec73b0d1-c307-42f1-a1de-2fa97202893d)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         981e57a7-8bae-4852-811b-0892eb3275ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bc7f7d7a-aa9b-4605-8f84-0c907aef53b4)(content(Whitespace\"\\n\"))))(Tile((id \
         514e8b88-0c2a-434e-a737-ae1ddd7a2cdb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         26d11c7f-4f62-4d30-a77a-e774cc61555b)(content(Whitespace\" \
         \"))))(Tile((id \
         d35a80a0-d4e3-4ff0-a17a-74718ddde695)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b6c25bea-95c1-499f-92cf-d47d88ee2f9d)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb0d66de-8cd5-4e78-8729-90dc6319ae1a)(content(Whitespace\" \
         \"))))(Tile((id \
         2cbfc36d-8913-4233-a850-739e68663353)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64e2d616-7532-454d-bb1a-44487e87de6f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         668a2b52-3168-4c00-a141-564f9c9edaad)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d59f9223-c4d5-4c30-a24c-6111f95048fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89f8a3d0-0475-453d-84fa-dbf53989d21e)(content(Whitespace\" \
         \"))))(Tile((id \
         fe02b8d6-8a09-43c9-b00f-1ebe73ea9395)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9827c97-67e1-43f0-ba30-20752989d5fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f5f2c53-495c-4c83-badf-beb665b3e664)(content(Whitespace\" \
         \"))))(Tile((id \
         081726d2-2783-4300-9e94-0010ec0fde29)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         af0e9eb8-d4a7-4a7d-add1-f850d13a6309)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e884c1a3-5058-449d-8821-5e9984d989a5)(content(Whitespace\"\\n\"))))(Tile((id \
         e6615f84-9781-4e75-a501-f68a75dc6cf5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         08d92f2f-3582-436c-a164-67c1c6af9a51)(content(Whitespace\"\\n\"))))(Tile((id \
         9b59eb7c-d042-4562-96d7-8c47ef05a4b9)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21f4289b-46fb-4b6c-9f0e-7dde7b2b2f44)(content(Whitespace\" \
         \"))))(Tile((id \
         552ffa45-2c38-4db4-879c-d36aa45ac56b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         315368e6-3244-4fb2-9216-33235fa6a6a8)(content(Whitespace\" \
         \"))))(Tile((id \
         9a5d1a69-56b5-4cc9-b5be-e02c35f43f90)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ade3902-b93e-44e8-b475-1cf31046a548)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a964737-06eb-4f7e-bef2-cdc71746800b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad96e56f-645c-4556-8615-8d0bd8f42bfc)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c5c2a67b-c29c-4ee5-95b1-a41ad9e6c766)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c14e2b2-aedb-47f0-be7b-8d1d7ff1f7a1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92e7b291-5e7f-487e-8f7d-dd227bc2ce4e)(content(Whitespace\" \
         \"))))(Tile((id 50e2ce6f-de58-4286-b2cf-253399757739)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e087d9dd-8229-4b2b-a248-b006e2065017)(content(Whitespace\" \
         \"))))(Tile((id \
         8c8692dd-42b5-427f-a297-51848ac78573)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         54330306-0844-4e94-91e9-728efcc1400a)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         df07cda3-32a7-4036-9034-2b6c1b51a96c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e02e3eb0-ca25-47ff-aa01-32df9d719f47)(content(Whitespace\" \
         \"))))(Tile((id \
         0eed1415-fe29-41b9-9b6b-b7866f6a6644)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b7b00b5e-c847-4d48-b9a7-bd72cfc987b9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8dc0c236-26a2-4b7b-a8b3-1eb723d5c5c7)(content(Whitespace\" \
         \"))))(Tile((id 003a7997-4453-40c7-aecf-5998987e3419)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         2b0c920a-b025-4539-9221-50366c84760c)(content(Whitespace\" \
         \"))))(Tile((id \
         7e5c5db5-da75-48e0-be3a-1b03af2e83d4)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9c49743d-bf0a-438f-b4b6-bf0b32e97e18)(content(Whitespace\" \
         \"))))(Tile((id \
         79c2eb3c-47b4-4c75-b3e3-8a598a87504c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af1228e4-e780-4533-8b70-543106a8460a)(content(Whitespace\" \
         \"))))(Tile((id \
         2be80d79-2a44-4524-89ec-7227afbdd1c2)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0a02e352-9589-4cd8-83ec-228d45be7ccc)(content(Whitespace\" \
         \")))))((Secondary((id \
         7fa49aa1-da00-4358-bbf6-37d82cc016fc)(content(Whitespace\" \
         \"))))(Tile((id \
         eed4eade-bfeb-458f-98ab-34b5eaa474e8)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         13b0ac86-9598-41b0-9c9d-0ec08e4374c4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7286b997-be48-4e4c-a917-048dca819e7d)(content(Whitespace\" \
         \"))))(Tile((id \
         8aac98c9-7a9a-4def-b6f3-6f029b652d6a)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bd6064e7-ce86-4921-affb-b23100a4563a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3bc114d-fec6-458d-afd6-c8126d5180b9)(content(Whitespace\"\\n\"))))(Tile((id \
         eee9c82b-31b9-4e72-a2b7-20603e3f6e44)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16a46aa2-9da7-484a-9483-2f03c1b3428f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a05006e-8486-4c94-bcea-619110e5dffb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8f1d41b-6dbf-4ace-9ab6-0c7a807d318e)(content(Whitespace\" \
         \"))))(Tile((id \
         8fdb8979-996f-4685-a4f3-f4c575e395e7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41fb7b7c-8813-413c-94cd-7b7a64d83d2b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         84530a63-2e22-414c-a222-9fcaec3bf5a2)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33e85c1d-b6e7-49cb-8ddf-f3e7659b9489)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d370e5bf-2989-4ba6-ba1d-b4934a3bf924)(content(Whitespace\"\\n\"))))(Tile((id \
         0300323f-547f-4daf-8555-55ab6d59d644)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77ec290a-97e3-46d2-8d7e-81625ca0a6fd)(content(Whitespace\" \
         \"))))(Tile((id \
         fa5e5e46-635d-454d-888e-68771728503a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e30c10c5-6188-48cf-9b0c-fb4b3abc33e2)(content(Whitespace\" \
         \"))))(Tile((id \
         b64898c4-bd34-465c-9b29-bfb775806416)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ea7018b-097f-4252-b019-918d59cb8f6b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fbf57479-55ce-413e-9472-6204da55b21a)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48fa9d3e-ae63-4105-b727-d1256588e872)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         915a721c-501a-49b5-8eea-9ab8e0f4f2db)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         172acc97-6f81-4c04-9846-98d649575a94)(content(Whitespace\"\\n\"))))(Secondary((id \
         f16026b2-2ff1-4a93-976f-246ba38feab9)(content(Whitespace\"\\n\"))))(Secondary((id \
         417e89a4-f617-4833-96fa-4e1f2fb6db88)(content(Comment\"# Count alive \
         neighbors for cell at (x, y) #\"))))(Secondary((id \
         40cf4311-69ab-4fe5-9360-6e86daeb47c6)(content(Whitespace\"\\n\"))))(Tile((id \
         4dda188a-31f9-483f-aae9-287c7f872bcc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bbf1441f-9b8a-4913-8b9c-d626cdd0ab69)(content(Whitespace\" \
         \"))))(Tile((id \
         48beb080-f3de-4582-90c2-b38a5fc728a3)(label(countNeighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         963e0454-1364-466a-b456-3566a3714111)(content(Whitespace\" \
         \"))))(Tile((id \
         05f018b0-9e96-4d71-83ae-3da77c890d29)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         391d04c2-571a-4535-a3b5-0b571146ab5b)(content(Whitespace\" \
         \"))))(Tile((id \
         73d9b10f-0b08-46be-a237-f612fd47f6f4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8cc900d4-27f4-4142-82a2-d96406a640d1)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         62b925c5-9ee3-4dea-8883-62c7a0d4f604)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c0a3743-9e0a-4702-a2df-701c93ef32a0)(content(Whitespace\" \
         \"))))(Tile((id \
         fa3a41c2-6b08-487b-b463-3f8b0e186cfb)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         03a5199e-5de4-4e3f-93ab-b2e81b589b52)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a2a6157a-246c-4d8e-b0a0-12d5a517e85a)(content(Whitespace\" \
         \"))))(Tile((id \
         a1982286-5767-49eb-9f54-def66287da15)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         98367246-a2b0-4c80-b3f0-927952ce4d72)(content(Whitespace\" \
         \"))))(Tile((id \
         e0adb881-ba95-421c-a858-2460f995c27e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28e56307-68e7-4de7-99d9-58689e0750a2)(content(Whitespace\" \
         \"))))(Tile((id \
         3350289c-983a-4302-8af8-7263ae688f24)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         30b29274-da35-40fa-ac77-e12781bb4fd4)(content(Whitespace\" \
         \")))))((Secondary((id \
         aa85c276-d5d1-42d0-a2fb-6ce70d1c284f)(content(Whitespace\"\\n\"))))(Tile((id \
         6eabdda6-42b5-4a17-ab3c-3954131db3aa)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         12de4b5f-4c58-448b-8433-2866df36d917)(content(Whitespace\" \
         \"))))(Tile((id \
         7d58fa80-31d4-46cb-aaf2-c833bb47d448)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         621a2136-b2b8-4250-a418-1a8a77ea5773)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1d06fa9e-a2fe-4793-b538-768b1056f181)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         93c12a93-ee63-47b8-af7c-70afdd4e63c6)(content(Whitespace\" \
         \"))))(Tile((id \
         cf58f9db-35c5-4bc8-a75f-f78ae7e59646)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4f664116-28aa-47eb-8537-a38970936279)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e3440034-bf27-4358-8ab1-34843f018a06)(content(Whitespace\" \
         \"))))(Tile((id \
         32d317e9-7be6-4947-a271-2ccdc229f01a)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b6b66274-f23b-4d92-a065-ff39590f4c1f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7390ef1c-4f9c-48e6-8c41-5ec0dee87de4)(content(Whitespace\"\\n\"))))(Tile((id \
         b527fc3f-d895-4411-8b34-a32cd562693c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         04126dd7-b2a8-4ca3-9851-ac93be709815)(content(Whitespace\" \
         \"))))(Tile((id \
         7139ca7c-5ec9-4fe9-9dc8-1ff9475f231e)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6228eef7-7830-49b6-ae41-2a4049107033)(content(Whitespace\" \
         \")))))((Secondary((id \
         852023e0-2080-476f-a009-8a9a06df3b01)(content(Whitespace\" \
         \"))))(Tile((id e2cd2efc-99fa-445a-a487-c93caacf73a7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d4697aca-1971-4597-b060-261903459f16)(content(Whitespace\"\\n\"))))(Tile((id \
         bfda77f0-0f10-432e-adbc-f5948e45bd9f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43c96b82-726f-4ffe-9128-e8ddc5861861)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d09eaff0-b132-4bb0-9e0e-03cab6bd9400)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8fcfbb1-f982-4cda-ac10-220ff2deeab5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed3348d1-4ef2-4e7f-844a-d0ba48f76f89)(content(Whitespace\" \
         \"))))(Tile((id \
         6c5746db-41f4-4572-85e5-be692ccf82a5)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd7151d4-a1b9-4085-87cd-9734ca604aee)(content(Whitespace\" \
         \"))))(Tile((id \
         ffb029b1-a464-4b86-bb3b-c7357b796b68)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ce1d5d0-376a-44f6-9fc2-2b995c5ee4ea)(content(Whitespace\" \
         \"))))(Tile((id \
         035a13ca-0c3b-4b8d-8b65-09857dc069cb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be82dc3d-c8b6-49ef-a4bd-058fe1b0cffb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         544c7a50-ced5-4cf2-b71e-b6cbf9e36b4c)(content(Whitespace\" \
         \"))))(Tile((id \
         0f1db2ea-47e4-4e91-b22f-b3eed1f5bd12)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e8a93a8-8b86-4ebd-9e30-88dcf8664414)(content(Whitespace\" \
         \"))))(Tile((id \
         cf8792c4-7a75-4c94-ac4c-b0acbf8ec1b1)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90b88a06-e3de-403d-a723-1af38bcef9ec)(content(Whitespace\" \
         \"))))(Tile((id \
         bfa8cefe-4143-4dca-a237-18d537beb624)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         987a0687-835a-42e2-aa38-e98fa6ae55e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a78cdfa0-eb5c-49d8-bcac-a16cf2005576)(content(Whitespace\"\\n\"))))(Tile((id \
         c6274cdd-d392-43e4-b006-4c4e465ff8e9)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2309758-8c4c-489d-bdb4-0994edea05e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         19cb04b4-bdf0-4942-a419-62816e68216c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6fa3737-f335-455b-88ac-6ac5409fbe61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76d62c8e-c18b-4e2b-9f8d-3fb27db6c9f5)(content(Whitespace\" \
         \"))))(Tile((id \
         60dc6b91-4d2a-48ab-b551-bc03a6333358)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9dadd269-d5fc-4fb4-8153-8632ab57fb9f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47f9ab67-faf8-483b-b010-f16c546dafa4)(content(Whitespace\" \
         \"))))(Secondary((id \
         c5f3e083-8b95-4b33-8365-01a255186b99)(content(Whitespace\" \
         \"))))(Secondary((id \
         49564360-4e20-43a7-a211-04a6f4782063)(content(Whitespace\" \
         \"))))(Secondary((id \
         a974e572-e974-458a-a123-87511a3ac5b5)(content(Whitespace\" \
         \"))))(Secondary((id \
         cf93aac5-2ef7-4b32-8a0f-103adeea19e2)(content(Whitespace\" \
         \"))))(Tile((id \
         1fd3d40d-89df-4817-9cf2-e3354b33b9f6)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         523f240c-c4b8-4abb-b2a1-49e037d87744)(content(Whitespace\" \
         \"))))(Tile((id \
         f1392e65-d874-4b3e-b7f6-7f0855ad2a3b)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7fb0a46-fd2d-473b-afea-f9cada18dedf)(content(Whitespace\" \
         \"))))(Tile((id \
         f0ad2e85-ed6d-48a2-9ce0-57dd7ee8724b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         08a57417-3a5e-48e8-b888-631587b4e904)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43dce297-f7d4-4190-b6a3-506d10136a21)(content(Whitespace\"\\n\"))))(Tile((id \
         70ae138b-cefa-4b8c-b497-3d17af8c6e31)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c612ab5-7545-4dae-95d0-60656c2e9e21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1fca357-d1a4-4a42-9459-d7712546a547)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2ab6ed1-c9a4-4408-b29a-0cbe7e97cfa4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67f93c8b-a18d-489d-a892-1f067b4078d8)(content(Whitespace\" \
         \"))))(Tile((id \
         46faf963-e2c2-4fd3-8c86-539963ff0d38)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f1afbc1-a443-432b-b021-9040fdd85dfa)(content(Whitespace\" \
         \"))))(Tile((id \
         2fab67bc-f9a1-4edb-ba6c-ac973eef9756)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         adf2bbe9-63f5-48c6-9be7-1ae009408dbf)(content(Whitespace\" \
         \"))))(Tile((id \
         9e4d1fd4-a643-4e57-b51e-4f9f7303e46b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17e92bea-e9aa-47b1-b590-797d0f9337d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c2db759-aa10-45f9-9fb5-e1284f30bdb2)(content(Whitespace\" \
         \"))))(Tile((id \
         7d82876c-a7d4-4bd4-a98c-9ef2ab7b9ecd)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a8a1719e-3822-4e90-b676-7f80e9fb875e)(content(Whitespace\" \
         \"))))(Tile((id \
         3dd46b13-9ab9-4417-b58e-1f8fb17f9b9a)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23548652-527e-4c81-aeec-a063cc02d2a1)(content(Whitespace\" \
         \"))))(Tile((id \
         ecf7d227-3a02-4268-bf16-1f37f0a07d04)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         67efb8ba-0358-4e98-8564-e61167f5c93c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e67cb9e2-7b77-41cd-8300-e213a12b1247)(content(Whitespace\"\\n\"))))(Tile((id \
         d7018dc6-cdd7-449f-b934-dade8547748f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f37fa52-eae8-40f7-bdc1-58aa2b8ba399)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0c3a1c16-10de-4d8f-bb74-81b290a6a519)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00c706fd-85dc-47fd-8931-872811ad97e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0df647b0-b633-420d-a2e4-13078b43d30e)(content(Whitespace\" \
         \"))))(Tile((id \
         39be6867-5741-4677-950f-34348f4f1769)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2915f52f-a220-470a-9b1a-7806b1d141ea)(content(Whitespace\" \
         \"))))(Tile((id \
         1e62546f-3f97-48db-9168-e7e6d50dc598)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01d987d4-8e77-4cec-a764-0d358d8fb560)(content(Whitespace\" \
         \"))))(Tile((id \
         c267c7e9-f8fe-42a3-9b6a-73edd1b0c50f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c00338c3-e08a-4fb3-8399-ab72d20b2d90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9307355-549c-459d-8a27-d9bebd2840e5)(content(Whitespace\" \
         \"))))(Tile((id \
         9a5f7a7f-49a2-40b2-925a-b87f68147b56)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e6c1aa45-e9e3-4296-ad6d-3b711b32b39c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b24b38ab-a75b-4f48-b90d-1e75a19cd623)(content(Whitespace\"\\n\"))))(Tile((id \
         b0402788-9a06-45af-b05f-c00bd895b5b9)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61e93767-3c1a-49e7-9989-1c71d1b560c1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20cbdae9-a82f-423f-9298-df8e1debcd92)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c27cbee5-8337-49e8-ae3a-61ad5380a13e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1720f6ab-2020-4433-90a3-6ec2cd8caba0)(content(Whitespace\" \
         \"))))(Tile((id \
         06d8d7bd-0ad4-49e4-bb80-adcf6cf711c1)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0c4eaaa-2d2b-415e-b562-80b96fc7d5fb)(content(Whitespace\" \
         \"))))(Tile((id \
         32cde4ec-30e3-42b5-85d9-6d2681147671)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40b17a51-471c-441b-a6f5-0cfe4d60fb76)(content(Whitespace\" \
         \"))))(Tile((id \
         999e2949-ab13-4027-ac37-0e2907f6cdb0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43fefc8c-0230-41e7-b165-4c0d934ed162)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b00c99e3-bdcf-4d2d-b5da-9138d2ee07ec)(content(Whitespace\" \
         \"))))(Tile((id \
         b030ab62-92e8-40d3-882d-e35b3a386ce0)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         46c0505d-3156-4713-9ef7-be0cc5b64ac4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4c0f30e-997e-4ca6-b21d-94515ca9ac82)(content(Whitespace\"\\n\"))))(Tile((id \
         72d268a4-19a4-405a-a556-5ffdfd68ac57)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28a5552b-0a27-4495-aff4-be7cc4a55434)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55dbcd43-4515-4b5c-b778-0622f9876e83)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89a08fcf-99be-4f67-a143-ea1b51b27c9d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a789569-4fc0-4589-8253-1ed0450cfea5)(content(Whitespace\" \
         \"))))(Tile((id \
         bcf3cb66-0697-47b0-813e-4afba5ec2387)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1d6048c-f964-4d8d-8cc2-d8e16b5a79d5)(content(Whitespace\" \
         \"))))(Tile((id \
         0226ac9a-768e-4e7b-b8d9-d5fcbbe64c44)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f39cbf08-6a29-48e5-8553-30e4c3176664)(content(Whitespace\" \
         \"))))(Tile((id \
         cec0e2f0-048c-48ce-b7bc-322f05870a69)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a801a735-d07c-496f-96a1-6ba3a9430bb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ce8bc9c-23c9-41a7-be54-1809d6702235)(content(Whitespace\" \
         \"))))(Tile((id \
         2cde03fd-b42a-47fc-bb3e-6b5bc3cd7b7c)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c38c7cfb-2920-497c-88c0-af96b01cdcfd)(content(Whitespace\" \
         \"))))(Tile((id \
         5952b2bb-e4af-4c27-a086-50da1c430384)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f03226d-1c09-4b14-bff5-0605d8be5fb1)(content(Whitespace\" \
         \"))))(Tile((id \
         0a97138d-ecda-45cd-8ddd-df389cfcc9b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c838e83c-b0a3-45b8-8420-aac537e81534)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9af3ac5c-7b57-401b-9725-466506c1e094)(content(Whitespace\"\\n\"))))(Tile((id \
         b6f1d6fc-ed75-41b3-99b4-34f5d661ee93)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c67474b1-83ad-4c16-83e1-52da64cbbf5b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a9af92a-5e3c-4360-a7d0-5ac77280f9af)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54288a79-1daf-4204-a2ee-ea803fbe9be9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6753be8c-8a85-4786-befd-f2a0d2d8ead5)(content(Whitespace\" \
         \"))))(Tile((id \
         00c6d72b-83c0-4c82-89d9-0f9770917455)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f274aeee-7f49-48c9-99e6-1377e34b6b09)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1859bdda-927f-450f-873c-bfb65965d5db)(content(Whitespace\" \
         \"))))(Secondary((id \
         efbee9d8-111e-433e-ac62-fd37af9803ed)(content(Whitespace\" \
         \"))))(Secondary((id \
         4144af3b-f2d9-47f9-af6b-3654fa288863)(content(Whitespace\" \
         \"))))(Secondary((id \
         e4a4c3d6-9d78-4f00-b50c-59d730c71de3)(content(Whitespace\" \
         \"))))(Secondary((id \
         d5ba1172-38ae-408c-a808-290f7966cbfa)(content(Whitespace\" \
         \"))))(Tile((id \
         4c58398d-222c-4c80-8824-63800aee1092)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0030088c-34ac-47c2-91c2-017d140651ad)(content(Whitespace\" \
         \"))))(Tile((id \
         4beeab0a-05da-4dca-afd1-a31d19fd7bb7)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65ce8e4d-48b8-41ec-b7e4-7b71c11de946)(content(Whitespace\" \
         \"))))(Tile((id \
         dc3f2329-d6f4-45a1-be2d-faf7def64992)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5c790d24-8fbc-43b6-88f3-a3b3c1a5de23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97625e22-d823-47ea-b3e9-402623336003)(content(Whitespace\"\\n\"))))(Tile((id \
         dcca2581-8058-491e-a176-889e8a80c0a8)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7f380b4-9f63-436d-acd4-436100cfa886)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         76da4d84-03b9-4578-b17a-a8b58dea5828)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b62fcd81-13c7-416e-91c2-9e571af023c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c4a9053-9cf4-4f8f-ab70-b03257a23af1)(content(Whitespace\" \
         \"))))(Tile((id \
         c8240ae7-78d6-48a3-a71e-32aad616494d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f7642266-b0b4-4a3e-84da-1f79f6040008)(content(Whitespace\" \
         \"))))(Tile((id \
         4e5fc95c-e38b-4c25-afc8-a5c9baacb1ab)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4ffc591-4bdc-4451-aa59-e6bf82238e5d)(content(Whitespace\" \
         \"))))(Tile((id \
         b07e025c-2c0a-4ee1-9221-dd8607deb3f5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a37489b9-26b9-4f79-90f7-e270b41cb685)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7473eff4-d109-4271-b190-608ef35f46a6)(content(Whitespace\" \
         \"))))(Tile((id \
         4b335e6a-eea3-4b01-924d-e2820c890d50)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         49c494bd-d193-4c8e-9626-1a8006d8681c)(content(Whitespace\" \
         \"))))(Tile((id \
         de730549-b9fe-4ea5-b35a-255bca19b61a)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92f1d54b-31c0-4c1b-a974-c1fd99b6c321)(content(Whitespace\" \
         \"))))(Tile((id \
         65dda4c5-7c5e-4741-b7b9-05b62bd7508b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6f680286-7040-454b-b7ee-35d008726fba)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         053cc1a7-af03-4d0b-8862-f4916b7e0996)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1ad6dc33-8678-40ce-8198-bb65ff3afba4)(content(Whitespace\"\\n\"))))(Tile((id \
         90ce3e75-64c8-4c2b-9e85-888f2bbb1cdb)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         809ee265-b1ac-472e-b958-13d2efda5bb8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         75d0656c-baa7-462c-aee5-fbdc6dcda036)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76120584-0cfe-44b7-bfe7-6c50cb1a6c37)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b38bd4e1-17e0-4478-b1a9-785b827cd706)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5cd47505-a394-4b8b-923b-de3054ed223b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3a619c0-b304-429a-a9c6-19cf719dc541)(content(Whitespace\" \
         \"))))(Tile((id d268c061-6016-4272-ab3b-87df32a984b6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         45cb921b-10c3-4ed6-acb0-f8fcc0d31a9a)(content(Whitespace\" \
         \"))))(Tile((id \
         0763394e-a26d-49c1-afc8-994745266233)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         54a6f156-d2aa-4dae-b79e-211625f218b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e8d99633-8d71-47c9-ae05-d490fd4ebb6c)(content(Whitespace\" \
         \"))))(Tile((id \
         1d8c148e-c6f7-489c-ae9f-6f8c0e15f902)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e071775b-bbff-4651-a576-26a4cdd1cc46)(content(Whitespace\" \
         \"))))(Tile((id \
         03ce74f4-dfd5-4bb6-b55a-41ccdf32293f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         200749f6-100b-4187-804a-d5dfc112bdba)(content(Whitespace\" \
         \"))))(Tile((id \
         edc3157e-1afc-4e16-8683-74c8b111d000)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c3b6ff95-e2b8-4a4c-adbf-0d5df9926038)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         da9ab23e-2ffe-4fdb-b42a-860e9c5834f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         a35e8638-7963-4fae-8891-0a097293dd28)(content(Whitespace\"\\n\"))))(Secondary((id \
         44c7dfea-1cbc-47ee-a516-a745382887c1)(content(Comment\"# Apply Game \
         of Life rules to a single cell #\"))))(Secondary((id \
         505ecbc6-a5d4-490f-a682-d0d7e04fe9c7)(content(Whitespace\"\\n\"))))(Tile((id \
         f18b8f2e-e7e8-49ea-baf2-68d18b9fd4db)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1626d131-4db8-48de-80dc-2a4b13b42812)(content(Whitespace\" \
         \"))))(Tile((id \
         93507784-59bc-47fa-a8bb-18a87c32a296)(label(nextCellState))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         930323cf-ee67-4b70-aab0-c2fcd1b005f9)(content(Whitespace\" \
         \"))))(Tile((id \
         68ce98b4-ae9e-4432-afc4-aa6711bebe1a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f13fe9e8-fe08-48b2-a49c-6cf115090cd0)(content(Whitespace\" \
         \"))))(Tile((id \
         87149f1b-98fc-4265-9bd0-bdd5a005e22e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         42609bbe-337b-4a73-8c06-2677a729183c)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         22ce12d0-d293-4b34-bc91-31fc7295a176)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ffa244ef-f43b-4658-951f-7882aa804864)(content(Whitespace\" \
         \"))))(Tile((id \
         2063579e-1e6f-481c-b23e-634ca8db1622)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         633b40fd-b502-4d94-952e-abd51745127a)(content(Whitespace\" \
         \"))))(Tile((id \
         16b67344-e1e7-48db-a1f6-d6c7a9e57b98)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b75d0ee6-14bd-4c67-bae1-31233d794dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         82a9d1c7-b97b-4a2d-bd42-cd4356c4832b)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         af83a56d-6300-47eb-b2a7-da52b1077e6a)(content(Whitespace\" \
         \")))))((Secondary((id \
         361a2547-0a97-4df4-af8a-27fff3a6826a)(content(Whitespace\"\\n\"))))(Tile((id \
         8980029d-66ce-4474-991d-232343fb8cf5)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5dda7fe3-8c10-4783-b4c8-80f406d49933)(content(Whitespace\" \
         \"))))(Tile((id \
         e642ed95-f56f-4b3a-afea-baec69ebea00)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5a11de1a-b021-40c6-a10f-4675b595573d)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6e892b55-f38d-43bd-af2d-da3fb87f93fb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cd08cc34-d755-4b7c-bfa5-b4296c9c32d9)(content(Whitespace\" \
         \"))))(Tile((id \
         e0ff2300-4e7b-48ab-a891-c31666559a9c)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9d413e79-2d75-4481-a3b8-fb98b82a4e0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1600760a-bc4c-4ab8-8560-e99e6f337f7d)(content(Whitespace\"\\n\"))))(Tile((id \
         9e72979d-522f-4877-95b0-781eaa8fb3c2)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1ae4ce56-15ac-4f56-af84-09638029f7f3)(content(Whitespace\" \
         \"))))(Tile((id \
         91339b3d-6214-4924-8335-95129ec8e62d)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85bcf519-d983-4010-94fa-3d5e2f31c2e7)(content(Whitespace\"\\n\"))))(Tile((id \
         c73c8177-f8b9-4509-bf71-54d2d7e472c0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         758f4ad8-4be9-4218-b7c3-fb51906c9479)(content(Whitespace\" \
         \"))))(Tile((id \
         5e86b227-66de-4441-a56d-6012eaa28524)(label(Alive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         31574437-c3d6-400e-857e-c2887ded9291)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9c245e99-9be1-4691-8c62-2a0ef8e60a81)(content(Whitespace\"\\n\"))))(Tile((id \
         037354a0-79a9-4a8a-8d66-0429f19d7c18)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4342e9f1-b7a5-4f0f-b382-804aee1411d6)(content(Whitespace\" \
         \"))))(Tile((id \
         3b460ae3-f73f-4a63-9320-6d59a962bdbf)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f57e080-099f-4203-a17e-a4606fd0af71)(content(Whitespace\" \
         \"))))(Tile((id \
         369637f3-e5c0-46a7-9cee-ec5071c088a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5577710-8167-4f7f-b912-f5e73d1605e4)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf1e732-8a21-471d-8d6e-c80cdeb5bd39)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52ad5fec-9432-4bc8-b190-84b6e7768c3d)(content(Whitespace\" \
         \"))))(Tile((id \
         6171905f-365c-4a0a-bfad-666557fb1aa5)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4db31251-200a-4eb3-841c-d485df11000f)(content(Whitespace\" \
         \"))))(Tile((id \
         65bf8382-6671-412e-9f5b-c7f918e65e75)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7761dd8f-3e32-4f23-93d0-15a8b8c7d43b)(content(Whitespace\" \
         \"))))(Tile((id \
         da69129c-48ae-4910-88dc-1b9f20f735b6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         164925d0-5e5b-4205-9267-1948ff5d1c53)(content(Whitespace\" \
         \"))))(Tile((id \
         6de1d244-765d-4079-ad0f-8807633dad88)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97a9ff5d-5cc3-4462-af38-7e8a60c379d0)(content(Whitespace\"\\n\")))))((Secondary((id \
         c4eb3ed8-1962-4e59-b972-cfbcfd203fbd)(content(Whitespace\" \
         \"))))(Tile((id \
         ed1fc715-6734-42ca-b7cf-f01e2fe504ae)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0149bcee-6662-49a6-a229-6d7cc5ba2e22)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8d19a33d-a5b3-46a9-b817-fca8c071b9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         7da80837-2a54-4a4b-8cdf-533ee07ef1c4)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d12aeef5-d644-4345-a676-e246a384761b)(content(Whitespace\"\\n\"))))(Tile((id \
         16ce542e-ccf2-4c2b-af02-5b6bb5248cef)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ec47220f-1993-4b4d-97a5-da302a0c3d8b)(content(Whitespace\" \
         \"))))(Tile((id \
         e27eaa7d-b180-4696-a985-05c467380388)(label(Dead))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4195fea1-3d41-4e4b-810c-2f246bf2e02f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9753ead-bbc1-4985-b416-899a31195910)(content(Whitespace\"\\n\"))))(Tile((id \
         2cea7cf2-e931-447f-8332-5c3829d37178)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b67a2918-2c18-4d9a-942c-19103e1afd19)(content(Whitespace\" \
         \"))))(Tile((id \
         bec5e665-aa88-4c44-a4ab-f5fdb4e00f2a)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         382b2280-8951-4d0b-bac3-d5082d6e6238)(content(Whitespace\" \
         \"))))(Tile((id \
         9c7377c5-64ae-4d62-ab29-4b0239d4c9ec)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcd33380-71fd-4f35-86c4-9063ce32c617)(content(Whitespace\" \
         \"))))(Tile((id \
         f7c82a20-baee-4423-83bb-af2dd13dd7f5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a6c08bb-61c0-4069-b464-8438ef89290a)(content(Whitespace\"\\n\")))))((Secondary((id \
         4fb94f67-8c34-4ec6-a257-9e46cc9853d6)(content(Whitespace\" \
         \"))))(Tile((id \
         7e4c68ef-509f-4f49-83e4-7e85e29f6a02)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2cab78bb-b52d-40f0-ab25-7972aaf4b6d6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         acb1cadd-4379-4bcd-b38b-c776fd51621c)(content(Whitespace\" \
         \"))))(Tile((id \
         679f8bdc-71b0-4905-88a5-569447beaae8)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         128a5aa7-cf6c-48a5-a951-9d0fc726376c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         662ed311-6e43-43a5-ab86-5c9ff7f0176d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         50e3fd99-f53e-4765-8985-62d035216afe)(content(Whitespace\"\\n\"))))(Secondary((id \
         b55691c5-ddeb-43c9-893d-0be6deffe53e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ed448dc-0985-4fca-87b4-6b9f07d333f4)(content(Comment\"# Step the \
         entire grid (simultaneous update) #\"))))(Secondary((id \
         bfae2c44-7471-4d1f-9ea8-7bf2929bb007)(content(Whitespace\"\\n\"))))(Tile((id \
         15d9141f-2cd3-4c3a-a93d-8bfee6c8d1a2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         72ccf662-b5bd-4ad5-8d60-474503825dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         15c00fb4-adcd-423b-9cce-962f81a5cf71)(label(step))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c1760351-9daf-40ff-beb3-8a7dd6812fe8)(content(Whitespace\" \
         \"))))(Tile((id \
         b382ac6e-1d65-4692-984d-7015a544918c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e74989d5-72e3-45a6-9f4c-c961191844b9)(content(Whitespace\" \
         \"))))(Tile((id \
         333764d0-efe0-4903-b9a1-e4adad4265e2)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         77248174-5f07-4087-abb0-b9992a7b16b9)(content(Whitespace\" \
         \"))))(Tile((id \
         0f507382-a90f-4870-979f-fdedc29d7cd7)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41b9de4f-6174-49fe-9b87-787bee985d4a)(content(Whitespace\" \
         \"))))(Tile((id \
         c13adbd5-f205-430a-b542-858cfe937cc9)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         08d56ff4-33fc-4b84-983a-81d4495d828d)(content(Whitespace\" \
         \")))))((Secondary((id \
         3fc4c2a5-d193-4e65-b578-0d6544f597f9)(content(Whitespace\"\\n\"))))(Tile((id \
         2e527469-21dd-44e7-bde0-32034b6959f2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         60ef32c1-1483-44d3-bea9-6ce01938deb1)(content(Whitespace\" \
         \"))))(Tile((id \
         ae6615b2-9d33-4b27-92fe-1fca9a81036f)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         96f434fb-9d49-4888-b8c2-cb2245b28903)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b6b12074-f270-4cf7-ab03-3a77bf088046)(content(Whitespace\"\\n\"))))(Tile((id \
         014e8a52-eb8d-42d3-8f0c-be7c468e831b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8dc0755d-b91a-4f3d-b399-45778356eba5)(content(Whitespace\" \
         \"))))(Tile((id \
         f4ffbc31-c296-4514-9ae5-9451e2681b63)(label(newCells))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         66f16afb-0e6a-422c-8469-001d41c990e4)(content(Whitespace\" \
         \")))))((Secondary((id \
         7fa2ee2f-1277-47d9-abf7-5e82efca7ccf)(content(Whitespace\" \
         \"))))(Tile((id \
         13c90d68-0a85-41c7-9e45-815b552cc890)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d298184-d92f-4cfb-9c27-c7654c4e00b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8613c37d-13a3-44ba-8c4a-ee5463b4561b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25de39bd-9145-4506-8da1-13efcf1c141d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3dc32217-ebcc-41e0-bce0-4dfa7d092f12)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b0356aa-cbc1-4a08-96b0-8c3baf108c71)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a697a619-5ad5-418b-b06c-05566e2d14f6)(content(Whitespace\" \
         \"))))(Tile((id a6f5c542-78e8-42b9-9e6d-241726e5d028)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         09aff5c5-a577-4623-9b44-609156042eef)(content(Whitespace\" \
         \"))))(Tile((id \
         3d191da3-8d84-428d-a533-be10bab5f583)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         0cf76657-0839-4079-8ddb-cc92c80b23ea)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         820665b7-8625-42de-b8bf-122f25adfffd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a0f592e9-7a3f-44a3-963c-5b4569081f83)(content(Whitespace\" \
         \"))))(Tile((id \
         3e101b41-5ffa-4ae9-9575-a7ed62fd5fbf)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         42b26a1f-d85f-4211-bfcb-d416d6b595f7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         40275dc5-f68b-431d-bfd7-54b73262e5f2)(content(Whitespace\"\\n\"))))(Tile((id \
         2e8686da-403c-45d9-a8c1-f2ce9e682567)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ab391e2f-abb3-479c-8bed-c8dcee063ab0)(content(Whitespace\" \
         \"))))(Tile((id \
         466e1251-6f54-4aaa-8e65-1eb14dc55e9b)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7030b1e3-3577-46bf-be16-fb47e4021b18)(content(Whitespace\" \
         \")))))((Secondary((id \
         b51b4a16-7c0d-42b9-9a14-e684c6740276)(content(Whitespace\" \
         \"))))(Tile((id \
         e6222d7e-72a3-4cb3-8936-a10c5701413c)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f589542-3d8f-4715-a854-6d9aa9d0fe89)(content(Whitespace\" \
         \"))))(Tile((id \
         8848bfb5-080b-41a6-b33e-3195b923db41)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fd18ec2-e132-4805-8532-9d97ebaa5d0a)(content(Whitespace\" \
         \"))))(Tile((id \
         dd258f32-5293-40ad-94cc-db50609c9ada)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         72ed4417-cf91-498a-a3c2-a8281aac7c62)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         67a6901d-e9b2-4b65-ad90-345e056ec8fb)(content(Whitespace\" \
         \"))))(Tile((id \
         362e9adc-b9c3-4053-a5ba-114fb912485f)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63caab55-9c7b-4bf5-ba3d-916e715f61e7)(content(Whitespace\" \
         \"))))(Tile((id \
         555c55f1-9c22-4504-8657-47a4805d731d)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13c5816e-33be-4c02-9e65-d599a85cdb0f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         29618c14-a801-473b-8710-0a2b9f3eb047)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         386fc108-da56-44d2-a2af-64822477c5c9)(content(Whitespace\" \
         \"))))(Tile((id \
         5e6614e2-6031-4f07-8f2a-b9045d4b42d2)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb5ae755-97cd-443e-ad1d-72e4ec57305a)(content(Whitespace\" \
         \"))))(Tile((id \
         c6500303-21bf-48ac-bcef-1e2bf2756c76)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7277885c-d89d-45f9-9e96-560061c3e8ba)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         29b34e40-65ba-4f3c-a216-1d5e816c9a42)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         803cabde-90f2-4b6e-b792-8ac1175cb0ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b0cb325-f832-41aa-b2c6-81d00bfc3fa3)(content(Whitespace\"\\n\"))))(Tile((id \
         b5e4fe38-8684-42cf-b94c-0a52f9724e2b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         75ebf746-09ab-42c4-b6e7-df84e6593804)(content(Whitespace\" \
         \"))))(Tile((id \
         44eb6d4b-9e93-4e3b-902c-38cca30ed68b)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2903ef79-d135-42fa-ba0e-b2c7eecebe2e)(content(Whitespace\" \
         \")))))((Secondary((id \
         b7dcb7c6-5f6e-4c33-b408-ef78ab41f2c2)(content(Whitespace\" \
         \"))))(Tile((id \
         2d2eac81-728c-40a6-ab83-4e7058cfef72)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21a60c3a-a2e8-41b9-8772-f2a6c6e3fc10)(content(Whitespace\" \
         \"))))(Tile((id \
         c78d1f13-706b-4059-890d-065ca436216a)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a45ff7d8-6821-4c22-8ae9-630f9ad7d3eb)(content(Whitespace\" \
         \"))))(Tile((id \
         6a0e6ffb-817c-4675-a0b6-abc8ffd2ffcb)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a355428-f4ce-4108-8d50-b9d7579a27c2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f4110b3c-b39c-43be-a6e8-0ae0e8beb596)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e600613a-a693-4289-9eca-35dc3c136561)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         99757b65-42f0-4ffd-b31d-967c707d70e2)(content(Whitespace\"\\n\"))))(Tile((id \
         1f16ac30-604b-4363-9e58-7a5c69202894)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6a58bf31-e3e5-42a9-b254-fdbcf0917102)(content(Whitespace\" \
         \"))))(Tile((id \
         e220ec79-3147-4610-a04a-5dab11923080)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         01ad804d-341c-4b88-b3c8-67b99864589f)(content(Whitespace\" \
         \")))))((Secondary((id \
         bc2d84b6-0a0f-4a78-a882-864e2300ec35)(content(Whitespace\" \
         \"))))(Tile((id \
         00853162-1183-4487-8fbf-1e8a1f14e96d)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4893bdba-f3d9-430f-a2af-f06036a2b4dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         76616701-e03b-4773-bc05-fae98974c74b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebf8eab9-b1ad-471e-99ba-2aca6799dbc0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c78ac252-910b-4593-b84f-011fd29af2e0)(content(Whitespace\" \
         \"))))(Tile((id \
         cfabf1a0-49cd-4c7f-bb2f-c5f35330af4a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         abd56c8d-8218-49f6-be9c-100b5fac4370)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9927918c-628e-4e7d-adc1-2207792458d9)(content(Whitespace\" \
         \"))))(Tile((id \
         2713d85e-f64f-4249-8220-0abf67c11253)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         743f1b60-a725-4b77-b932-942d9dec5508)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e4757aa4-524b-4d33-ab04-d297ffb60954)(content(Whitespace\"\\n\"))))(Tile((id \
         865c6ca8-ca05-4ff5-9747-5a33a2447ad4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1397e0fe-3e2a-42d5-8d43-779d6b32bfa3)(content(Whitespace\" \
         \"))))(Tile((id \
         e7644c82-82e9-4802-b1aa-c66aef4d2169)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         40a6a091-b96d-4aac-95d9-b6a4418362ad)(content(Whitespace\" \
         \")))))((Secondary((id \
         8fe77ab3-6adb-4f54-add9-54899f3ddbde)(content(Whitespace\" \
         \"))))(Tile((id \
         c383c866-8f97-4606-af16-bea8592fe60a)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de961f1b-6007-490d-bf52-aed5ab07d8e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6147d888-0d4c-4fe1-9a57-35af13e400ef)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2942096-ebc8-4522-bb71-303737d0c8b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb14c15c-3e7b-4bd2-97ec-38c6c6426454)(content(Whitespace\" \
         \"))))(Tile((id \
         a126ae8e-b2b6-4dbb-9b4f-741fdd2288f5)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc927a1c-c5ff-4713-bf44-6e25986cec3d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         154eabbb-1ff3-403a-8ad0-9a35274d3d00)(content(Whitespace\" \
         \"))))(Tile((id \
         843def2c-d967-4317-9237-dc8667348df3)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f00603ca-6d31-4bd2-9c30-4131d7191afd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9fdda094-c226-4148-9634-e53bd9040a32)(content(Whitespace\"\\n\"))))(Tile((id \
         ef8e955b-6de0-406d-91ba-11d9d5c82158)(label(nextCellState))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26c9c543-88af-4d44-b1f0-bba7e9aa52c8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7053b4d6-ed6f-4ae1-8255-fe422be7d459)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2cf8813-198f-4e95-a032-c7fcff640552)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b0f6146-ec86-4c3e-9f1b-b43199c18603)(content(Whitespace\" \
         \"))))(Tile((id \
         467c49df-6474-4efc-8c91-5406fdb4b21d)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         85728c4b-325b-421c-99c5-a9cd2bc8de94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8839d7ac-1d26-47dd-9c82-b23c80f3c65e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0f6db115-0e60-4fdf-9da9-117741985213)(content(Whitespace\"\\n\"))))(Tile((id \
         79ebb4dd-a13a-4965-bd0d-c5422cbc6518)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         39644661-871c-4453-84ad-beab781f6731)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b214f00-765f-4842-b1d1-0a5826921190)(content(Whitespace\" \
         \"))))(Tile((id \
         4c40e281-a085-4120-9676-47a25621f718)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bfa1db5-527d-462d-a1a2-1c3da87b9fcb)(content(Whitespace\" \
         \"))))(Tile((id \
         ec3f9bd2-cc2b-4efe-a7ff-19586817b4b1)(label(newCells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0aa1d4d7-a9a5-4283-9af7-61d2bd0c33f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbcb446a-f5f0-40ae-918a-16cff2fd3057)(content(Whitespace\" \
         \"))))(Tile((id \
         4120e996-2145-4944-9b41-ae89a14728e3)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9c5c20b1-750c-4d37-8f30-35b24338de59)(content(Whitespace\" \
         \"))))(Tile((id \
         705c5f94-62af-4d6c-b260-1f771b0989b9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fdb035b-b8c7-4d22-9278-aa213539e7df)(content(Whitespace\" \
         \"))))(Tile((id \
         80338796-34d4-4fd8-b170-9344a50e65ad)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f711c696-f881-46cb-a589-0113efe321c5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2b168c13-1aed-4490-a98c-456d60073b3f)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a669f5eb-ed43-43d3-b340-c4be16becd99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cacffb2e-6430-4f47-b215-cc1995e1e06b)(content(Whitespace\" \
         \"))))(Tile((id \
         7f39bab9-7b5e-4b07-891b-310ba8142eed)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         53f74943-de4d-4378-b460-6c6c585356b9)(content(Whitespace\" \
         \"))))(Tile((id \
         8ca75b67-efa7-4dbe-8546-bb55083d0b30)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9cafe96-3afe-400a-8ffd-e925b8a5323d)(content(Whitespace\" \
         \"))))(Tile((id \
         bb5e4876-c965-496c-b15b-19ed99f6f7ab)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dfb868ed-43a1-4229-aa22-6593119b1e8a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c1dfc205-1031-44b7-b785-75fad6f20297)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4abadf32-1da7-41a3-a31d-5107df8dd01f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d35ec6a0-7df6-4560-b2e9-6bda7fc4face)(content(Whitespace\"\\n\"))))(Secondary((id \
         405b59cd-387f-4d43-b731-471895492855)(content(Whitespace\"\\n\"))))(Secondary((id \
         8796c85d-4ae4-47dd-8577-59b143f17765)(content(Comment\"# Run n steps \
         #\"))))(Secondary((id \
         3c329e47-d90a-4b16-a029-95ba774cd186)(content(Whitespace\"\\n\"))))(Tile((id \
         278339f5-3b95-4d2c-8c5c-bf4409fd163e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4a7e8fdd-58b2-491d-8dce-b2c198e99b29)(content(Whitespace\" \
         \"))))(Tile((id \
         1eeb95af-3e50-4d69-831b-18be1085f209)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         384c27c7-0950-4dbc-9290-9e7d4aa13038)(content(Whitespace\" \
         \"))))(Tile((id \
         c61cb895-57e4-4dfa-8c54-feb6394987cd)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5df00fe2-bef3-4af0-b550-076fdc03a04a)(content(Whitespace\" \
         \"))))(Tile((id \
         08752757-becb-40d0-9049-39da7a784d83)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b66ea2c6-2242-4a9b-8d93-6cdbbc6f1cd7)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fa553eba-5fe2-4a31-848a-4803dba82e99)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41cd102a-dc66-4ffe-9a4d-1780d6edc330)(content(Whitespace\" \
         \"))))(Tile((id \
         c038778a-d5db-46d4-b5ca-b811e9a2bb2b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a00728c8-0936-4e80-9b40-0f5b26734c6e)(content(Whitespace\" \
         \"))))(Tile((id \
         bfa5ebfe-0e48-4537-b937-0f088eaf0079)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         037ca552-b5d8-4921-b074-12811bd19a8c)(content(Whitespace\" \
         \"))))(Tile((id \
         9dcf22fe-c60f-4335-80ad-ac5646cd8a00)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7a87b051-22d8-430f-ba5e-2764cb62d6d8)(content(Whitespace\" \
         \")))))((Secondary((id \
         8396e612-6389-42f0-bd4b-b65c62f3d949)(content(Whitespace\"\\n\"))))(Tile((id \
         be4d973d-d7c7-40ac-8e06-9f3df64df8b7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         25ce9461-0cc5-4be8-b8a6-f3e34264a27e)(content(Whitespace\" \
         \"))))(Tile((id \
         af7e0a9c-199a-4e37-939a-30989274a20e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         28dc6ada-68e0-4fb2-a9b5-6a466d86e299)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9b469f43-a99c-4548-9083-5d2ce9a48410)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bfae6ca4-6cb9-4a5c-89ac-4dfd5ee8a014)(content(Whitespace\" \
         \"))))(Tile((id \
         6402df99-e144-4e6c-8be2-f0acbd520f0c)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e1a6b728-880c-4a49-90e0-79d47968d739)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70921d3b-5689-4491-9d2a-fc0e9849548f)(content(Whitespace\"\\n\"))))(Tile((id \
         dcfae29c-1ab1-4898-aa0f-18b50e9a390a)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         210bedfc-fc45-4ef5-8640-ea53af11c9a9)(content(Whitespace\" \
         \"))))(Tile((id \
         63133b15-02f3-4f62-92b1-3a92940e55d4)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5be01a11-3c86-43b3-99ed-62453c5c39f9)(content(Whitespace\" \
         \"))))(Tile((id \
         82d5d887-27ae-4f12-93a2-b8e9bff9aa95)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         882cab86-8f75-46fc-ae39-1f9b359c2eb3)(content(Whitespace\" \
         \"))))(Tile((id \
         2d433d0f-0314-4a7b-b697-662eb3e01cac)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5a0c812-e1ee-432f-938f-3b5b707d9a2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         ab3deb7e-48d9-4bc9-92bc-0ee863c94f5e)(content(Whitespace\" \
         \"))))(Tile((id \
         bb638746-cce8-4138-8138-118476496246)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a31cd985-9158-4e16-b84e-00e4466e32e7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         93cba49d-3f83-49c4-9764-d46504771258)(content(Whitespace\" \
         \"))))(Tile((id \
         e83dec6c-8cc7-4175-9dbd-d4604ebed0d5)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b27a44b-416f-4e98-aa5d-32b56370bbb6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         941b3578-2e29-4e97-9229-21e061a1fecb)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccc874c1-9e55-43bf-a5d6-5b9d9fe3cbd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c87cd01-3292-429f-a837-1beb2ec9ee97)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8eb5d2ff-455a-4ac4-8ed4-a1980907f260)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1fb47a21-499d-4c6f-b3b3-5ec8b08184be)(content(Whitespace\" \
         \"))))(Tile((id \
         4287becf-8e43-4899-b77f-e2b46f4fe703)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         84342f4c-7a23-45ab-9077-9757d3b4bc17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4abbf926-a72e-46cf-9495-3d0386093afc)(content(Whitespace\" \
         \"))))(Tile((id 5000164e-3ba4-4e66-a484-053dbdf387fd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         196d7a8b-f99c-4505-889b-db76f9ec98f2)(content(Whitespace\" \
         \"))))(Tile((id \
         9bd49161-644c-42ba-a260-1a60cd43c90f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3f4552a8-9331-4c6a-b23d-65875e4081d4)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fdadb157-6484-49e7-8b5e-cabb4102e74c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bc66f839-6cab-4d94-b202-38b89126d64f)(content(Whitespace\" \
         \"))))(Tile((id \
         82a724b1-e012-40e9-adc3-2cc9ec1c2aea)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f5d6653e-57a1-42e7-b9e7-1459c8d8d438)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         078a48c9-8b66-4873-a7a4-31b24db14e25)(content(Whitespace\" \
         \"))))(Tile((id \
         ccfc3b20-ffb7-4a3f-acbb-9399507cfffa)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa7c7252-e8ad-4874-acb3-5262b8af91a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a2e581d0-4ec0-4874-9200-8634bdce7eb8)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         63b98bcb-1e91-4ab2-af3a-64a1c6b3e2d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d667c089-e4fd-4e9e-bd0c-fd1ad2294568)(content(Whitespace\" \
         \"))))(Tile((id \
         cacce8a2-3a62-4cf9-82da-1399b813928e)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2741497e-ef31-4250-bf27-5d9defae0ded)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9b198c71-4e11-41e6-b907-ebf1a08d4974)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a12b239-a75e-445f-8999-304c80755e9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         06459dd4-94c6-4a4e-b6f6-31f1265eeaba)(content(Comment\"# Helper: set \
         multiple cells alive #\"))))(Secondary((id \
         6596e4f3-519c-48dc-8cf8-000453c906c6)(content(Whitespace\"\\n\"))))(Tile((id \
         344e808c-0382-4e4e-b131-ee47e1bcbd92)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         617a7dd7-b31f-4dc7-9370-986ee0edaa48)(content(Whitespace\" \
         \"))))(Tile((id \
         7e4a9def-7dda-4c88-b038-85de747e13db)(label(setAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9781ad70-606f-46b9-a859-0195323a1e9d)(content(Whitespace\" \
         \"))))(Tile((id \
         58ce49c8-e77d-4045-b1d9-45c060f7331f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         590c81e5-3365-466c-92b7-a99046cc7091)(content(Whitespace\" \
         \"))))(Tile((id \
         ef7b6fc9-76ac-42f6-826d-3265fd57732b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c26e6a88-2f02-4c0f-8966-3526142a124d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b67b8766-18d9-4b77-a37d-299de4181e02)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c3247407-72c3-4312-b44f-48a9d5f6e2c1)(content(Whitespace\" \
         \"))))(Tile((id 430d1129-b6a7-48a5-9c56-3549c60de315)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f55b1466-7ac2-40de-92aa-db5dadbcb1b1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c01d278b-6f7a-4173-8307-492dc2c55c86)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         84fa41f5-f7c0-45fd-aeae-e2f9af6485e7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3079672e-1b58-4027-aaa0-4131413346c9)(content(Whitespace\" \
         \"))))(Tile((id \
         e925fe3d-a406-405c-8be4-e628f55828e1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         549494b8-a0c4-4735-893e-ed8cf4c131b5)(content(Whitespace\" \
         \"))))(Tile((id \
         fe672bae-a7ec-4efe-8c51-5e2a4e52de46)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a3fea48e-38d0-46a4-b16f-4f200923a798)(content(Whitespace\" \
         \"))))(Tile((id \
         0ef1b13b-47ad-41f3-b6ae-cf27149a9222)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1c156ed0-db39-4b8d-b9c7-900920921fb7)(content(Whitespace\" \
         \")))))((Secondary((id \
         c1ff6512-cf14-4e9b-91ae-1a815a7ba504)(content(Whitespace\"\\n\"))))(Tile((id \
         779f3ee4-153f-4660-8b00-0a96b0df9793)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4a21cd0a-6576-4c44-bf22-02709ba34440)(content(Whitespace\" \
         \"))))(Tile((id \
         6bcbd12c-ebd2-4c16-bd1c-b87fd2ca7077)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a4814096-944c-44ef-9139-7be116b9b739)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5cacdd28-7992-4ed8-bd1b-73c87c2cd741)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         25e273b2-a25d-4f77-8b90-9d146b278880)(content(Whitespace\" \
         \"))))(Tile((id \
         945ab779-d416-40b3-97ab-5c8de83d5fef)(label(coords))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1207a4bf-3351-4b8a-aad7-4d3ec3232a1a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f1743eaf-baa6-4c5c-b61c-c59e2e5f1e95)(content(Whitespace\"\\n\"))))(Tile((id \
         f0a1b789-f734-4cc0-8376-a4434ba0c7bd)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96251689-8b32-471b-9e00-db15cc7614a3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6712bbe7-5e34-46a9-b40b-50d33694b0e0)(label(coords))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cf01ee5-3452-44b1-bcfd-3f089b4f1542)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91a6fb87-6c9d-4e87-b93e-bb8dbf28d1b1)(content(Whitespace\" \
         \"))))(Tile((id 8268b586-d944-43db-a555-f60b39889e08)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bef40b17-7dff-404a-b119-7e0a90142c4e)(content(Whitespace\" \
         \"))))(Tile((id \
         e668d9c0-fde6-4283-970f-11e26506664d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c70098ca-fc86-4edd-9367-02f07da24456)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cd33c072-d52b-413b-b4f2-a793b080bb1e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         49975ef7-7e9f-4f58-94c4-8aeeed937739)(content(Whitespace\" \
         \"))))(Tile((id \
         07afb635-d1d1-4c5e-8aa7-cedcac9d9f20)(label(xy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         14aa5e34-362f-40b0-aec7-54f685105788)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9881fc0c-74b9-4caf-baa6-7d631d37bd42)(content(Whitespace\"\\n\"))))(Tile((id \
         8e7838d5-f6f5-4ba8-a9f9-188fb4bc0e4f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         78756878-b877-4973-ae5a-88388b8f70ca)(content(Whitespace\" \
         \"))))(Tile((id \
         4d4b1756-4ff5-4dad-bd5e-48c4edc63f02)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         cbd24f0e-88c4-4077-8cae-3f70cd080a68)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bca30c93-893c-4237-a5da-8ef5d87eb6af)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7234ddc7-6e12-4888-a254-6668f95d7cde)(content(Whitespace\" \
         \"))))(Tile((id \
         c6d031cf-ff8d-4e0f-984d-68e04bc5071a)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c82bbba9-b18d-4474-ae4e-d01a122f58a0)(content(Whitespace\" \
         \")))))((Secondary((id \
         b35e0f6c-d646-47ea-bf99-f474af22dce4)(content(Whitespace\" \
         \"))))(Tile((id \
         9da9199f-5888-4a7d-b30e-4298e5a4285f)(label(xy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6dba0f25-cc66-46ca-a534-0005f0cf3d4a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6d45da2d-2efc-4f85-b081-71f48757f670)(content(Whitespace\"\\n\"))))(Tile((id \
         2691c5bb-384f-4558-a071-f6e381b55cd3)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41160410-2203-44fd-978c-b1436ba8e999)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d92f3299-59d1-4feb-a7fb-d018c43967ee)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5bbea845-b48f-4dae-bde8-dd812dea25ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         762d574d-ef49-45e6-848f-6330c0142d52)(content(Whitespace\" \
         \"))))(Tile((id \
         1fd02504-d5c0-44a0-9069-a43e853be497)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d8a5881-9b64-46f8-bfcb-cb1d9bf40870)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94df46a4-bcdc-43b2-af89-e5a598c649e2)(content(Whitespace\" \
         \"))))(Tile((id \
         3ee5e647-1a4f-4894-b0f7-f7a094919e9c)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc4936b0-ce9f-4150-b6a6-9e542700a6ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5f935f1-8d4a-40fc-8ce6-edc9ae55f58b)(content(Whitespace\" \
         \"))))(Tile((id \
         38f6852f-d2df-4e1c-8b4a-170fa94b2859)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4f71b155-cf18-4484-9f42-3311671595f1)(content(Whitespace\"\\n\"))))(Tile((id \
         47a06115-be25-4130-af81-256b7f29f363)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9babf80d-5272-496f-930d-5160f13f7159)(content(Whitespace\" \
         \"))))(Tile((id \
         39005608-c9f0-4913-8d84-7897a37f2475)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4af4a19c-b6d1-4442-b0b1-fa96d7ad83e0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         95b8525e-db98-4ced-b1a4-d3c65acbd36c)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e23dbfa-4966-444d-ab32-0bca3640283e)(content(Whitespace\"\\n\"))))(Secondary((id \
         99ea0adc-ae11-4921-9798-3b63ae847304)(content(Comment\"# Count total \
         alive cells #\"))))(Secondary((id \
         b461333f-dc82-47a8-96ab-dd1bb6c14921)(content(Whitespace\"\\n\"))))(Tile((id \
         b5fc3e45-98a7-4eb0-8867-c21c96853cac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b4fad474-4f74-446b-b76f-3b680a00425d)(content(Whitespace\" \
         \"))))(Tile((id \
         afad9b01-1cfe-4f09-940d-c43afd8c4798)(label(countAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c478a044-aeb7-485f-9b6c-e74b4310bd4c)(content(Whitespace\" \
         \"))))(Tile((id \
         65c7882e-0bf3-4512-80b2-88cd2693ab02)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         aa3e3a93-3532-4ca6-a4a4-dbea7305fdbf)(content(Whitespace\" \
         \"))))(Tile((id \
         68e4354f-12ad-488d-be42-91d262373a92)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f76435a7-37b1-455a-b51b-a0c16be0cb54)(content(Whitespace\" \
         \"))))(Tile((id \
         20625a9f-613f-451a-ae97-c4d00b56f4c2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e4972516-c0f9-4b0f-a09e-27d62923b71d)(content(Whitespace\" \
         \"))))(Tile((id \
         8928b699-f991-476b-be37-2adcd834e4e8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1bfcac71-5a57-4886-ad50-e4a87c23500c)(content(Whitespace\" \
         \")))))((Secondary((id \
         23f198eb-e008-4c0d-bb59-9e4cf773f7cf)(content(Whitespace\"\\n\"))))(Tile((id \
         50f68608-9209-4cf7-a276-d23eb84c14e2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         eac8328c-5e2f-47be-a112-230cb44bdca5)(content(Whitespace\" \
         \"))))(Tile((id \
         c8fe9c78-c88e-419e-b120-38d394a56102)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2ad2d946-22f2-4a1a-bca4-31dfa7bb2570)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a772c207-5404-4987-bd74-a941aa90a89f)(content(Whitespace\"\\n\"))))(Tile((id \
         6d2fa58e-2517-4196-aa9f-9fcac82af626)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d966693a-919f-4f06-b281-b3c7ee1bbaca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         37b279eb-0685-4dc6-b00e-065f07805408)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13972f44-db0f-4ee4-95c3-1775266fe27b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c41bc05c-1109-479c-a763-864de283554c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc0b0e06-f87f-4eb7-9ef5-476fdd3899b1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         66f495e0-c8f6-4110-8ef8-89e7eb31c0b7)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9948c97a-0257-441f-9643-17db491a078e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a046296-a7a9-4600-ab1b-e18f08117654)(content(Whitespace\" \
         \"))))(Tile((id 98b80932-96de-4038-addf-05479c0523ed)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f169b245-8b3f-4645-9da9-bc3fdd285fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         c8c112f6-ad44-4178-bd6b-e71d492bc79f)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         91f79c98-48ae-411d-9df1-22b3dcf79c38)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9f14dee7-778a-472b-ad65-dda77afaa6b0)(content(Whitespace\" \
         \"))))(Tile((id \
         65f94d5b-3374-4f84-b1c1-db289e65f6ff)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7a51d353-91c4-49c2-99a5-605cc9bae771)(content(Whitespace\" \
         \"))))(Tile((id \
         aae967bd-9378-48f7-bf63-47051f3aba6b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0ee39e6-cb07-4429-ab7a-807dc0c2e390)(content(Whitespace\" \
         \"))))(Tile((id \
         e1497bf6-ecf1-4c3a-b319-f27f37bd3453)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         587feaf8-3900-402d-ab63-35aa088f326f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0bef5fd9-8187-40d9-b6e0-1333fbb3abf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         028c5176-886d-4a56-a5e5-0debdc4e9f75)(content(Whitespace\"\\n\"))))(Secondary((id \
         5eba9dcd-14e2-415b-8350-1d0f15879aae)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         01c665ee-22cf-42f4-ae77-1bbeac36303c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a27d1b3-5be3-4255-8ad9-a82ff654bf14)(content(Whitespace\"\\n\"))))(Secondary((id \
         9237c03d-dc99-4230-87b0-0f315ab11a17)(content(Comment\"# Basic grid \
         operations #\"))))(Secondary((id \
         9b66854d-f0ae-4657-9aa0-0ff1e7666d60)(content(Whitespace\"\\n\"))))(Tile((id \
         a77d1a72-6fda-44bd-a484-ccbecfdf62b1)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df5d9078-1b28-4213-9b93-02207809847b)(content(Whitespace\" \
         \"))))(Tile((id \
         d63634c9-e5e7-41d0-9eb9-b14896ebe77b)(label(\"\\\"empty grid has all \
         dead cells\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         72786c93-4099-48a5-9a51-319665ababa6)(content(Whitespace\"\\n\")))))((Secondary((id \
         f544bebd-f99e-45a3-bf36-37f203580868)(content(Whitespace\"\\n\"))))(Tile((id \
         37bcae81-e6e8-424e-9d99-8727472918e0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34c3968b-467d-4c1c-a10d-f43ed0ba6334)(content(Whitespace\" \
         \"))))(Tile((id \
         892e1dd9-faf1-412e-9bc7-cde081580459)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         36c6e456-4587-489d-b50c-888f91a3a2a0)(content(Whitespace\" \
         \")))))((Secondary((id \
         686fce2f-ebd1-43fa-ba8f-7855a713646a)(content(Whitespace\" \
         \"))))(Tile((id \
         636688ee-1d80-4c2a-824d-1ca0845e414c)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6fabf12-d5e1-453f-bf68-5c3a04a15d9b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d10b7dcf-9f19-4ca4-9985-4751e0984a53)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40c13020-2c51-42f4-be2d-6a217bb6a63c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2366fc3-bcbb-4066-8508-711e6208eff8)(content(Whitespace\" \
         \"))))(Tile((id \
         4739b8e9-629c-4d03-83cc-d9d02881eced)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         56ec20c0-1229-4ded-93c4-7e770fb0506b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b1bb538-97c7-4a8e-a3ea-afa5ca7d5462)(content(Whitespace\"\\n\"))))(Tile((id \
         c33d3608-5f74-4f67-9046-3a70ad6187c9)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52589a3c-948a-4116-9d8b-07d3f65aba01)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         65d41fb2-f40c-4cf9-9514-7b75a76e1be1)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         20111dda-6239-4dfc-9fed-3cb62fc031d1)(content(Whitespace\" \
         \"))))(Tile((id \
         d1a9c5cd-b5a2-4e2a-a699-913df10bc998)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ecf578f2-026c-4b7f-8498-b5a1e7737b4e)(content(Whitespace\" \
         \"))))(Tile((id \
         9cb7acbc-fba1-4fc7-99b6-c447ffe60c09)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73a745c6-f970-4518-af10-6227a9efd693)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b6f10b95-1bfc-4a00-9222-dd21e7a6d8bc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dc81c31-5e9f-451c-8706-641a9d3d3396)(content(Whitespace\"\\n\"))))(Secondary((id \
         59a8129f-5926-402c-bade-5e47d50942ed)(content(Whitespace\"\\n\"))))(Tile((id \
         ceb8d355-2a60-4393-aa9c-85de4bd7292f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         377b8c4d-9528-49a8-bad6-a81dc136fa18)(content(Whitespace\" \
         \"))))(Tile((id f5e25bbb-3d7b-486f-aeeb-ecc3d47b7535)(label(\"\\\"can \
         set and get cell\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf969f80-83e0-4729-98c6-8cf974a19fa5)(content(Whitespace\"\\n\")))))((Secondary((id \
         71c96a5b-7597-4098-a2d1-df1ad0b1a509)(content(Whitespace\"\\n\"))))(Tile((id \
         dfb30afe-3375-4ca2-9210-09484769956e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a221fec7-555b-47a0-9f6e-f557b2700cac)(content(Whitespace\" \
         \"))))(Tile((id \
         bf1c0bc2-2fce-4e9d-9ecf-3bdd27ee5a25)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bd824541-25e1-49ea-8cc7-3fb5b641a1b4)(content(Whitespace\" \
         \")))))((Secondary((id \
         094757a6-0ed3-49ff-b76a-f4dd7f9f09a1)(content(Whitespace\" \
         \"))))(Tile((id \
         419c987e-7676-4e71-81ed-0f405ad34984)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e208996-38c5-4456-8363-a3e06329b184)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6346bd12-f14b-4026-b00e-8bbe0cec5f81)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         997883ba-5e01-41d9-b948-8a28397161e9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c814f4e1-1f0a-4708-a34d-ab816d22d4e5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19673220-fde8-441c-bcd7-4eab695b912a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09580555-b808-4bb8-8534-6e3f9a9ae9f1)(content(Whitespace\" \
         \"))))(Tile((id \
         184a8c23-0d09-48b0-8c0c-5232c167a0a1)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f1b0cb87-bfdd-4c31-a7f0-e980e3fb6e17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c58c32fb-88d1-40a7-a9af-48af57723621)(content(Whitespace\" \
         \"))))(Tile((id \
         b3834914-fe60-4a2a-8a17-13d27646453b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e6b1586-5ecb-4b88-997e-26cb9c35b012)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         356c947e-87a1-4a8b-99b9-e3cc0d880511)(content(Whitespace\" \
         \"))))(Tile((id \
         8bf9af40-ae15-4080-96f9-92ca4039a256)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55751c02-9ad9-4950-86f4-0a9f2ac007f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22487129-0fea-4423-8c9b-b5094709b99b)(content(Whitespace\" \
         \"))))(Tile((id \
         10d9ee2a-a3f4-40ec-b6a6-97e59c0e9fe0)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f44e19f-74ad-4150-bdcc-c8bb30151eb1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         538bf6e8-eb66-4a29-9f50-7887b50d46b8)(content(Whitespace\"\\n\"))))(Tile((id \
         e6a82a6a-9588-405c-a313-d7a7061d984c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83bb6569-acd7-4685-8f99-53fa7055ec2b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4a5bcf63-30b8-4429-ac66-a1cba2d41b0e)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d8c45fb-d82e-44a8-9bfc-171014892d95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71181de5-9371-404f-b681-931d61bc6a95)(content(Whitespace\" \
         \"))))(Tile((id \
         2c8fe742-ea6e-40c3-a5c7-caed25fd433a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4cff8a71-40dd-46be-a381-9c665a7e0896)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         486517d9-0b16-4831-b244-461972a2272d)(content(Whitespace\" \
         \"))))(Tile((id \
         ad6300bc-face-4a0f-b9c1-7810be376127)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f31b3d64-447b-4443-bbf9-e1990beb17fa)(content(Whitespace\" \
         \"))))(Tile((id \
         8ee9e3b4-5f86-470c-b2dd-7aff8ad38497)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89a3a418-ef1c-43dc-9044-ca2c754a952c)(content(Whitespace\" \
         \"))))(Tile((id \
         e974a6c8-7add-4d3a-9736-478265d865e1)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ff9d2b6-77ae-44de-b826-7a481d80e9d6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         618d054a-9a57-4d89-9cbd-4814b1d93162)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79402f89-5882-457c-bb7b-01e0ba60ceb4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a3a6e85-e1af-48af-b5ae-8194cca22eed)(content(Whitespace\"\\n\"))))(Tile((id \
         85b1eab4-3f1f-4a1c-b32d-2d013c63f4e2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         78e4ec20-73c9-4af9-acfe-d9c879ccaadf)(content(Whitespace\" \
         \"))))(Tile((id 2cc3aeff-f17c-4681-8fd8-2e54adf42538)(label(\"\\\"out \
         of bounds returns Dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bcfc854e-2728-457a-a990-71e3fe3a7158)(content(Whitespace\"\\n\")))))((Secondary((id \
         53145d58-310d-4f31-b3b4-a7e6ba65ce23)(content(Whitespace\"\\n\"))))(Tile((id \
         ddc4bc13-71a1-4ead-a98f-6af9af6281f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0ac8c993-ff5b-4097-a336-a5e2e01389d7)(content(Whitespace\" \
         \"))))(Tile((id \
         0a3f2092-5644-4839-8df5-bfa0cc16275d)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         37b79139-3c87-46b0-b263-1500592b2d14)(content(Whitespace\" \
         \")))))((Secondary((id \
         d65473f9-de21-431b-94a1-301d9ade45a9)(content(Whitespace\" \
         \"))))(Tile((id \
         f4f70899-b3ce-471d-919c-96a48d6048d5)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82e08feb-4dfe-4d8e-a913-da44a42e1a80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b8930712-030f-459f-8f70-b3e801e1e2cd)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         040bbdd8-a6de-4504-a0bb-02519aba0ad9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20d257c4-57a1-4424-a8b5-e3fa48fed626)(content(Whitespace\" \
         \"))))(Tile((id \
         28d7095e-c3f6-42a6-b780-e79797627561)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d577f73f-ba33-4c5c-adb2-fb960b0ef1ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         52007285-2c00-4909-8f77-7e1b35c4407e)(content(Whitespace\"\\n\"))))(Tile((id \
         262dbaca-bebf-4791-8773-c229d0ba8c50)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5dd3e31-7514-4b8f-9f51-bb2e8664d999)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e7641b2f-27a0-481f-8e8b-8ccea3e509a2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2179884-ca64-454c-9365-6b63aad51475)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a99a883-3753-4bf2-a926-7cb6e1091340)(content(Whitespace\" \
         \"))))(Tile((id \
         acbb37e5-5c3e-43d2-bcac-a0c1af14d5d4)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         225a6323-400a-48e3-9a12-416d9f93e889)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86743e12-1f41-4377-9b7e-72974451770a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35790fd4-27f2-4bcd-85bc-5b4a3a16c9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         14428002-b955-4700-ae87-c1792b0dbdc8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8c1ab2b7-34fe-4af5-8afc-656fc0834132)(content(Whitespace\" \
         \"))))(Tile((id \
         8ef127ca-88bd-4283-837b-38f4ff534527)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf3fd398-9734-4c76-b9a6-644160015fd2)(content(Whitespace\" \
         \"))))(Tile((id \
         ad2c65f7-3a03-4947-8635-b230d6548044)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         12eade68-d710-4c68-8899-291ad94f1523)(content(Whitespace\" \
         \"))))(Tile((id \
         4dcc50e7-65d3-406f-8c89-cf3e1e2ad802)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9d24a13-8d48-4b74-bce6-0b03733cf27d)(content(Whitespace\" \
         \"))))(Tile((id \
         966a6d49-ef2d-4fde-9701-4945dbb00c9c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef1c1727-4b9c-49ab-aa51-ad5908671793)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f79a38d2-3fe3-4cea-bca9-0b3a9b6497d8)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f2f3b16-edba-4ba7-989b-de1f158c4767)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d2af216-3ec1-401b-96d5-118cbf015495)(content(Whitespace\" \
         \"))))(Tile((id \
         5c0b063b-0b3e-448c-bb6e-95145558d92d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         851f5e62-ebce-4ecf-99a0-845cb6c15b60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cfca6bd-62e3-4b4d-9718-f915f6cd513f)(content(Whitespace\" \
         \"))))(Tile((id \
         8053fdda-266a-45ff-a95a-5fed9626fec5)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         71362daa-1754-4299-914a-46d88810b8dc)(content(Whitespace\" \
         \"))))(Tile((id \
         6a6939b1-b929-494e-8c03-9c7a9d70d22e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d6979af-c4f9-439f-ab4e-81b46c63033b)(content(Whitespace\" \
         \"))))(Tile((id \
         ed1bcd50-d4be-4a09-93eb-c9702287af43)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         922b7d6d-942d-40a0-9fd0-31f05c0a6625)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1d23be45-f94a-4e25-a157-8b616dd6b7db)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         463577ee-2158-40f2-b65f-d7700810db8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         acd651f8-c492-450d-a540-846b4554af88)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ba3401e-cd0a-4706-81b4-4bf582a16f29)(content(Comment\"# Neighbor \
         counting #\"))))(Secondary((id \
         067978a0-0742-4b63-ac42-57086b67cf87)(content(Whitespace\"\\n\"))))(Tile((id \
         f5df3ccc-9105-4ea1-9595-b8aeda968555)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         215ad407-5b60-4128-a78e-8c9f8ba26d87)(content(Whitespace\" \
         \"))))(Tile((id \
         60f03a4f-6b8e-459e-98b1-ce54b7bfa5eb)(label(\"\\\"isolated cell has 0 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         195169e7-3c9c-409f-96f3-89b1fee86631)(content(Whitespace\"\\n\")))))((Secondary((id \
         e24ba6e2-813a-44cc-ac0f-463bb38e7726)(content(Whitespace\"\\n\"))))(Tile((id \
         d7fcb7b5-6ff4-4fa6-82ab-2f379de3ba24)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         902f483c-bd75-4fce-99dc-f912830b4d6d)(content(Whitespace\" \
         \"))))(Tile((id \
         a87bf3a6-5e1b-4528-a663-de98e3de2508)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d78074b-9e61-4470-8066-4e917ddcba85)(content(Whitespace\" \
         \")))))((Secondary((id \
         71e499bc-3fce-4d71-b33b-11749a166d41)(content(Whitespace\" \
         \"))))(Tile((id \
         fd416e34-1036-4148-9787-c259bbe955f0)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4aaa4d96-9dc2-483c-9b8e-abdea3dfe281)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c438b4ed-c936-43e6-93c3-8ba0ab83e6b3)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         014f16e0-99f9-4a1d-b68d-d1386bb7cdd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         190e1b5c-7eb7-4c16-83dd-ffcd0cea2b4c)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ec00a1c-0ee2-4408-80a9-3abd05b767c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffa57ed4-1fba-437e-abd8-05e8bfbb8e06)(content(Whitespace\" \
         \"))))(Tile((id \
         9c755734-09a4-42eb-8d8f-3887251a25c0)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         afb70cb7-e329-4e73-9299-b2cb750a388f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         467ab053-ba1f-4b90-8f97-bad344c07196)(content(Whitespace\" \
         \"))))(Tile((id 0582c356-7858-43a1-89de-1b3b74a85349)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a8235c2-362b-4272-b1d3-51624f6c3150)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4fa3fd8c-c488-4076-9881-25c195d144d3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1b01f48-0772-4d71-8bdf-6391f6b4b9b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd2e094a-5f3a-41ee-ae63-e521bde29ea9)(content(Whitespace\" \
         \"))))(Tile((id \
         cae2125f-5920-4b16-9655-b9c37273f300)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         73938091-8ade-40f8-a861-e07d793f7f2e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         282397f1-f9d7-4edb-b47a-d0b21b43f4d5)(content(Whitespace\"\\n\"))))(Tile((id \
         b5877dd1-6751-4e5c-8a9b-97a04d310bb7)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         990621ce-9839-4736-95ca-c04655c5b16c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03239b9d-3958-458a-a516-d30a4908f4e5)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e919474a-f15e-4a35-af8d-f10264d35065)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         251db885-fb1e-4825-a3f1-ef88a8876968)(content(Whitespace\" \
         \"))))(Tile((id \
         49608acd-b328-4fab-a3e0-cccc6c98f242)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ea652fd-eef0-4f02-8dc2-e1bf6975f795)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9355aafe-46f0-4c16-84da-e077ed71bda8)(content(Whitespace\" \
         \"))))(Tile((id \
         26757dfc-bd2e-41eb-a6bd-94345223d2ba)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b3465ecc-cbb8-4545-ac4e-0df42696a657)(content(Whitespace\" \
         \"))))(Tile((id \
         f5d10657-f717-4594-b1dc-ed0b090780cb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e9e9110-ba47-4c50-8f47-050bdb9384c7)(content(Whitespace\" \
         \"))))(Tile((id \
         9a384335-9e67-4ed7-a90a-b9e1c1c0ebb1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a8163c48-5da8-4ebd-8fbd-ceec3f10bf8e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         76a29707-69f6-43db-808e-af759eae2ef3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d595b1d1-b502-4c36-9962-e75289494626)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b32d412-ed3a-4c8a-8750-36376b88ea6a)(content(Whitespace\"\\n\"))))(Tile((id \
         765c6217-144f-41b4-8ee8-f8b156ac34e3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05da53a1-0c90-4201-bf27-971f84adf1cb)(content(Whitespace\" \
         \"))))(Tile((id \
         65b1bb52-dc57-45df-8e84-5c9cfc7c842c)(label(\"\\\"cell with one \
         neighbor counts correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24087a8a-5a85-4855-98a3-617527db4c1e)(content(Whitespace\"\\n\")))))((Secondary((id \
         ad10a27e-fb0b-4e60-b22f-8d004a7458f9)(content(Whitespace\"\\n\"))))(Tile((id \
         e36aef31-0cf8-4b86-adfa-0ffca1fa1c68)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4b6f3505-179f-46e8-a58a-40fcbd40b237)(content(Whitespace\" \
         \"))))(Tile((id \
         c069f868-1315-47b4-a073-b883700920ae)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         025e0902-131e-44c2-bac5-da2c7f1df629)(content(Whitespace\" \
         \")))))((Secondary((id \
         d74fac4c-f0fb-41c8-9393-48c156d090ca)(content(Whitespace\" \
         \"))))(Tile((id \
         c169cf58-3b4f-4cca-9dc5-507a25d46fa2)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eda9424d-c594-493c-9d35-b33d23ddc862)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8f0a62fd-82f9-4df0-9a3b-abbda73bcecb)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9e39858-faf1-4b76-9be9-e838d0473034)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bd6f5492-46c5-45a2-911c-79e20863c970)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa77a099-3377-4303-aa93-daa8e069a64e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8d7c5ac-a81d-4586-b40f-289ef19908de)(content(Whitespace\" \
         \"))))(Tile((id \
         c08db0dd-8c6c-4f42-8b3b-ca0378cd0444)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         139faa3a-7b5e-45a8-944e-977552269b82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         588ac572-2a9b-474a-8523-87dc194d56ab)(content(Whitespace\" \
         \"))))(Tile((id 63fef16e-c870-49ce-ab61-c71e5697ac6e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6c0182f-86fa-4c30-bb6a-181ec8ec6dc1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f1bd815f-47bf-477a-ab9f-db68c5cfed21)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74c335d7-949d-451a-9fbd-bda4bfbb4db7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dad76e99-964a-4224-bcf1-623797513652)(content(Whitespace\" \
         \"))))(Tile((id \
         45d55f14-72ba-42cc-a9d7-63eaf92c95e3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3966b20f-45e6-4b10-aba4-b7bea6707e1f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2252b85b-6afb-4ba4-970b-8ec7d60f7e83)(content(Whitespace\" \
         \"))))(Tile((id \
         0d84742d-a00f-4238-af35-35f01131ba74)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         68ba79c6-b0dc-4ea8-8399-71d0b7dd9d37)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4213501c-460c-4205-9168-946f35472665)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         180d3517-7208-4ea8-8d60-558009482441)(content(Whitespace\" \
         \"))))(Tile((id \
         2d8a6a26-426e-4ea0-b0cd-6757cc0a8e08)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         a054c687-e37b-4e59-b22b-9d4264cf6229)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         db4badba-a324-46e9-afd6-cd77dac4c529)(content(Whitespace\"\\n\"))))(Tile((id \
         7d473f90-f04e-47dc-aee0-3fed8a40ddd8)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78adc2ee-b568-40e9-a87f-38a7ede1593f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c8ca7d6b-7867-417b-acd5-44e6d1806680)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05bb0a3e-8148-4001-864e-3304cea89db5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eceab245-4ffb-4f37-bf9e-33a8f0dad5ed)(content(Whitespace\" \
         \"))))(Tile((id \
         458dd517-e291-4db7-89ce-483f9e4336b5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb8ea19c-ebc8-45a2-ae02-d6b16615c060)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90d03036-05c6-4c01-99a6-d8b5c10b4c22)(content(Whitespace\" \
         \"))))(Tile((id \
         2ef70e79-0eae-47bd-93ea-641f61d3bd32)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ebc9eff9-90c5-4353-bd18-bc7179845e75)(content(Whitespace\" \
         \"))))(Tile((id \
         5e644475-6d1d-4981-b4eb-be1188c8a18a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69b19057-65ed-4f4c-ac49-dfac51acc3a1)(content(Whitespace\" \
         \"))))(Tile((id \
         fbed0bef-8ce2-4f08-924d-82d941109e55)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1770e4c9-1311-40f3-9ef3-248a2e3adcef)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0cab7634-2ebb-4463-b0ea-35d84b9719f9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d9dcd44-e0c8-413a-af0c-50f167f86667)(content(Whitespace\"\\n\"))))(Secondary((id \
         175cd3f8-72ef-4e36-83fb-26509876977f)(content(Whitespace\"\\n\"))))(Tile((id \
         1a619e52-cbf6-46ed-b6c0-c93be20ff3e9)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e5ae7004-5b37-442d-90f6-6b3ae205b240)(content(Whitespace\" \
         \"))))(Tile((id \
         9a7db3b9-c9f1-47ac-baf8-d42a03ba4fad)(label(\"\\\"corner cell counts \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d22a9045-b0a9-4734-b38a-7890fe3e8019)(content(Whitespace\"\\n\")))))((Secondary((id \
         0a49edfa-fe4b-43c2-9dbe-def0b546b5ff)(content(Whitespace\"\\n\"))))(Tile((id \
         a477d97a-5b0e-4f49-90e0-fddf8bbe5764)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         600eafd2-4160-4814-941c-09a60d226ff1)(content(Whitespace\" \
         \"))))(Tile((id \
         56fc7730-7219-4517-8d2f-23bf42e3964c)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         418508c4-84d1-41cc-9bdb-e1837d36b7a4)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff551af5-ffc4-4d6b-8582-c572beaf1cbc)(content(Whitespace\" \
         \"))))(Tile((id \
         4b7ba531-2427-474a-be9f-21493c29ad57)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42373379-2933-49d6-bf43-9e26da4fbb02)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f43f79f-1747-48d0-b503-ba19bf8f0aec)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56f132fa-d59d-4adb-abfd-c27ff0d6e67d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e0af64a-e108-45b6-9d88-65219605cd5a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         861f427f-9df8-4a71-8367-fafab46e57f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         958f84d5-f4ca-45be-ab4d-fb924b445402)(content(Whitespace\" \
         \"))))(Tile((id \
         36ae03a9-e838-4bc4-9a41-d0a1f186a482)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7a29f0e3-3327-40c2-839d-26a186ad8251)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a7f11ba-2f4a-49fa-af37-f72457b5fb76)(content(Whitespace\" \
         \"))))(Tile((id ad3ad277-61e9-46f2-a072-ab893c477b86)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         30763cb9-3ec5-4009-9c3f-ec36a2a893cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e3d56ad2-b910-493b-801d-56cfc03860ce)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2c16994-39c8-4ba8-9850-d1b9ee2fab8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a44bd948-8f8d-4e12-be9a-78dfed463bfb)(content(Whitespace\" \
         \"))))(Tile((id \
         f097b0af-b59f-420a-88b8-4b88b77b3064)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cd13c989-990e-4f0b-b3c9-163b42a401d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b5f2cb9-7c61-42c9-b99d-b2128b90ec4a)(content(Whitespace\" \
         \"))))(Tile((id \
         0c567b67-d49a-4719-8c9c-730aa3fe310a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5fe72f11-8576-4321-b434-390aa501a1dd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1481a0ca-8532-4331-a70b-0ca2530252e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa1791a5-5df6-4bd2-8567-9f9ce812596d)(content(Whitespace\" \
         \"))))(Tile((id \
         9692fc27-e765-41e1-b74f-0241a06abe9d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8d02b198-4ec3-4e9d-9b4f-688949c4edf8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32514223-1926-49a1-b3fb-ea70d50db946)(content(Whitespace\" \
         \"))))(Tile((id \
         0cf7977d-b83a-4a7e-b977-7eb14390870a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4ea4ecfb-5edf-4a82-8450-cee616e2bb1f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6aed269-5eb0-4f74-be78-9087a77522a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd308f24-37a0-40d0-bb8c-d194ea16e392)(content(Whitespace\" \
         \"))))(Tile((id \
         3e62da4e-3a45-4d17-b2df-9b50afcdf702)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         863b00bb-8793-4262-a627-1e4f77c0341f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6997beb3-443e-4e1f-8795-92d0fb8d0daa)(content(Whitespace\"\\n\"))))(Tile((id \
         2d3e9aaa-f0b2-467d-b894-74a1cb40cbe5)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e36fdfab-f322-46c7-aaea-509ba5bd28d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55b46d78-fcbe-494d-9c57-d0a9c86b4545)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         788a22c6-5665-43cb-b3b1-2d616bc4d6b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a92a7bb-a222-4449-a012-9ec46ab0618b)(content(Whitespace\" \
         \"))))(Tile((id \
         d57d8995-fa23-4e1f-abe8-8d744d7f6ba9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f27dfca-8a23-440e-970d-371d5240496b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b14d603-9586-4585-b88b-7496ad1a6265)(content(Whitespace\" \
         \"))))(Tile((id \
         b9903865-cbf3-4af9-9d2a-220c4f115be4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d37d2619-8149-4a01-a649-be6566c7b9f9)(content(Whitespace\" \
         \"))))(Tile((id \
         28cd91c7-24c3-404e-9ade-5ebc81fc4769)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0e5cbfb-ac1d-4926-96ff-649c4e073c64)(content(Whitespace\" \
         \"))))(Tile((id \
         1171a050-2e7e-4fd9-8563-d1a1a68cfb2c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5f991ad6-7533-4a6d-8af5-bbb77434fc3a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1a4f5803-ca16-48c0-ac35-bffa9e07bd40)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67c32545-64aa-4b30-bb83-6a939eaa7a9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         faeab4a2-fea1-4fb5-ac32-c602d5e8bba8)(content(Whitespace\"\\n\"))))(Tile((id \
         af2ab58e-1c5b-4631-a43d-9be384aa31b3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         17bd828c-3c6b-486b-a770-4afd091f9788)(content(Whitespace\" \
         \"))))(Tile((id \
         47490409-927b-402e-9e11-9e7d65c50396)(label(\"\\\"cell with 8 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e3b5dd6-50e6-42c3-83b7-0533f6e90443)(content(Whitespace\"\\n\")))))((Secondary((id \
         3b299517-7795-4e57-b845-458b1eeaba03)(content(Whitespace\"\\n\"))))(Tile((id \
         3854e314-4c59-49d4-bffa-35f8b5d5593d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         11199ec9-c593-4098-bd19-86be89b3cd41)(content(Whitespace\" \
         \"))))(Tile((id \
         c2a7174d-69da-4849-917d-5d06d377e828)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         74f90e64-d6da-45f2-89ac-3b5eebd59f1d)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f553e71-dd51-4b7e-80d6-7b72c3ba77c8)(content(Whitespace\" \
         \"))))(Tile((id \
         4ca66f4f-7ad1-4c52-ae54-a9a29120dd6e)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         967d09ee-c511-4ff7-8da3-49cc29e6242d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f75f20c-270d-4610-b63c-a4b48ab239c3)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3eefa21d-fdba-45d3-a023-58a778019fad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d797d9c0-140e-4756-8bb5-b0b71ac99bf4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1b02e3c-b460-4aa8-adb3-3950b0956d90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cf27715-7cef-49f6-b3cd-03d35094a7a5)(content(Whitespace\" \
         \"))))(Tile((id \
         2f6275d1-ef8f-4213-bef9-88f1f650c081)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dae8989b-41c4-4a74-b218-2147745529fa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad0b6b25-b968-4a64-abe3-ca532a5edef8)(content(Whitespace\" \
         \"))))(Tile((id 3f262a73-d766-4cc6-8a69-4561b03ebd6a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b8606277-1d20-45c1-b602-765377c4ddaf)(content(Whitespace\"\\n\"))))(Tile((id \
         2d81d077-82ab-4cb1-ba26-3a604a7ff85a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2e51b263-cd2e-402f-b2b9-09838c17d516)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09fc1843-9519-499b-97b7-7b027ef5999a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e9fe5f0-4111-4f51-9a1b-21b72987e1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         eaeaf974-c5ba-4a98-988d-dc2a9f9e435d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c9d78199-7a92-48cd-ba4b-a746759675b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45ecc2d9-d5a3-4989-ba89-677c436f317d)(content(Whitespace\" \
         \"))))(Tile((id \
         10beb9e7-cda9-4eda-86e1-273aab45f59a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc7f2181-2aab-49c5-a8ee-f95fbcecdb12)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         082292c8-3e46-495e-a8d2-7c65b2445b2b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1863a8a-d7fb-4d9f-8e5f-8f7d8f4f9607)(content(Whitespace\" \
         \"))))(Tile((id \
         26bbdc46-11e8-446f-818e-b8e28aa5ca52)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ba8e70bf-cb4a-44c2-a7d4-5180e134ecad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0a29058-6d6e-4b9c-9321-6b84632949ea)(content(Whitespace\" \
         \"))))(Tile((id \
         60c94821-1004-4fb6-aa16-878f95f7b6a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e2b126e2-16ff-4822-92c4-a4782f9c686b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59b0fc13-3e51-413e-b1e7-18d20bf0b07d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e136f0a5-8975-450e-af3b-18429c9fb5e1)(content(Whitespace\" \
         \"))))(Tile((id \
         7ca126c9-ab57-4cc6-beab-a01894c55159)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9e28bf27-34a9-40fd-8e23-b13d48b2b691)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33603318-2cec-4eae-9a4b-4fc729675fb1)(content(Whitespace\"\\n\"))))(Tile((id \
         057f22f6-bfd0-42bd-a092-a1260299de01)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1a1220df-f6ae-469f-998e-b460ddb876c7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1bdfeab5-27c1-4792-a985-7357a1b33e68)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93f2bb75-a6b8-4cbc-a328-c2c3c1cfad5d)(content(Whitespace\" \
         \"))))(Tile((id \
         c0b411d3-d64d-4e8e-8b52-8b140d8a086f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8af8cff2-04eb-4717-9c25-483a23c8f013)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4bd8e81-76b6-4480-b714-9748dfeed953)(content(Whitespace\" \
         \"))))(Secondary((id \
         1dd2a727-a02a-4593-a879-5be3ecf9f19d)(content(Whitespace\" \
         \"))))(Secondary((id \
         0bd4a056-aff8-4302-a59f-2ffb6a25191c)(content(Whitespace\" \
         \"))))(Secondary((id \
         cd69cabf-99d4-4006-91e2-8199168c29ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b1a3120-f69c-4f3b-84f8-e65c190c6ba1)(content(Whitespace\" \
         \"))))(Secondary((id \
         225906fa-0ac7-4b90-9f1c-f65546905d99)(content(Whitespace\" \
         \"))))(Secondary((id \
         46ea18e5-20aa-4bf2-a0ad-91d60aa833d4)(content(Whitespace\" \
         \"))))(Secondary((id \
         23da576b-d401-426a-99d8-b605d281ca15)(content(Whitespace\" \
         \"))))(Secondary((id \
         88e8f09d-213e-49c1-a520-634a5c32630f)(content(Whitespace\" \
         \"))))(Tile((id \
         a217be29-5c88-47ed-9d5b-091a87e83f74)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fe28af02-72f1-4fc7-8dba-b35680b11a70)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef2681ef-2cdd-4219-b7e6-8996f55b0dfe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         caee3ddd-92f9-48af-80bb-eeef6497e3ae)(content(Whitespace\" \
         \"))))(Tile((id \
         0ba55ea7-f793-4f26-88fc-38b260077ab9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         59632a7b-bf28-4630-93d4-c0b9ce21684a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b544c824-3786-4c2b-84d6-65002120a2c9)(content(Whitespace\"\\n\"))))(Tile((id \
         721962f3-5b21-4723-97dc-4af79b7b4449)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6dcce7b3-aef5-425a-adf5-7d8de78f8ba1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c9f040d-d95a-4eaf-862e-269d49a1f89c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6866a41e-f71d-4623-a9ed-95d69946d885)(content(Whitespace\" \
         \"))))(Tile((id \
         c2ffc511-0661-4736-86ce-acb40b959dcd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         aeb8abd9-4ef4-43a7-a32d-349a2c5128e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9562046-607b-476d-8671-26acb8e75d57)(content(Whitespace\" \
         \"))))(Tile((id \
         5b760ea5-b08c-4608-ae09-6cfb08d51918)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         897d2c44-7e57-4e41-98fd-4c2b9f2a553b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         101482b3-3741-4028-ba85-e83061158443)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f12a602-1f02-40f9-ba07-d9bdd7cdaf9a)(content(Whitespace\" \
         \"))))(Tile((id \
         d2c46005-84ee-42b7-8713-4b440b820db7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         41c008af-61e8-4771-b4d7-431216e63a80)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         941c9964-2856-417a-a292-5265c7abe77f)(content(Whitespace\" \
         \"))))(Tile((id \
         bfbae737-097b-4317-af01-341c16ee750f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         64d02afe-1eea-4a35-9134-b84bff2151f1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbdafeab-82e6-4d8c-b73a-13b45e5a7175)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         032e5887-c4b1-45fc-9ac1-28cae36497d3)(content(Whitespace\" \
         \"))))(Tile((id \
         e390c772-4592-48bb-ac43-5b10f4f4d839)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         47734f22-cf8a-4d4c-a0c8-144d783394e8)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         c989843d-6d00-44cd-bbae-9df4ea15deb6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c770bfc-4bff-4e74-b247-d31c93b4c964)(content(Whitespace\"\\n\"))))(Tile((id \
         b5f04344-783f-40d5-ae1e-717a5918eec8)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c406721e-a99a-4680-84cc-1e9773a6b932)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42cc25a0-b78f-4419-a681-6f61133f50e2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2b4c855-1b92-436d-b3e1-51dea2d5217e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42aeabf0-c30e-4631-a909-e10481acf848)(content(Whitespace\" \
         \"))))(Tile((id \
         ab05de83-03ab-48ef-9dfb-7e7367492bf8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4ef6ce4-25dc-4592-a5b3-f5c716191984)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f94bc3d7-5afc-4c71-a038-097ea33d2ef8)(content(Whitespace\" \
         \"))))(Tile((id \
         3af803e8-f1ae-4602-8450-d6f006056ce9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ffa0e0d-b135-4a51-bb56-bbb8f7bd0b35)(content(Whitespace\" \
         \"))))(Tile((id \
         b3596444-e24c-4eb7-8a63-4cc0593e926e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         790af126-867a-494f-8521-1a853e6434cb)(content(Whitespace\" \
         \"))))(Tile((id \
         9e2e6eca-4b38-4d03-98a7-d07068d26238)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6560a3fd-15b6-47cf-a2a9-e206782658b8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         709186af-c66c-4ca5-87cd-abb7b8b71901)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bc0212b-78e7-43da-9c7b-d3619b25d7dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3808cf2-fd85-423a-bd86-d22d8a5eed14)(content(Whitespace\"\\n\"))))(Secondary((id \
         3731d87a-1e0f-445f-b182-2644ef902cc0)(content(Comment\"# Cell state \
         rules #\"))))(Secondary((id \
         c1487c69-0f5c-4276-8765-1f7fd6a11131)(content(Whitespace\"\\n\"))))(Tile((id \
         bea19459-6f4d-4e20-8b5b-8a4d4cc03b94)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         74960179-6629-4a11-ab6d-bc17d445a16f)(content(Whitespace\" \
         \"))))(Tile((id \
         3ad59a04-db96-42d2-bb34-eb51d830fd70)(label(\"\\\"alive cell with 2 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5fe832c5-7c9b-4cbf-a9b6-fea2789252ec)(content(Whitespace\"\\n\")))))((Secondary((id \
         b434bcd9-57ce-45db-89a0-522f3c799ab0)(content(Whitespace\"\\n\"))))(Tile((id \
         b014b460-196d-4319-9309-f181f1e17073)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dfe28212-50e7-41d0-a84e-4e691738f26e)(content(Whitespace\" \
         \"))))(Tile((id \
         61a30a31-96fa-44b1-a637-9cfd33b9fb59)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         41ed3744-1460-415f-a67e-84b86c00246a)(content(Whitespace\" \
         \")))))((Secondary((id \
         d6acf7a6-8ef7-49b9-9f93-1dbc2e5ecab3)(content(Whitespace\" \
         \"))))(Tile((id \
         459202b3-3eb6-4902-95e9-96246c765015)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         051bcfa8-27a0-432e-9e91-e7eea6bc3fa1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03acf96e-78f9-444e-bc3c-2b2fcddad875)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8590056b-ad35-4020-a97c-6de1233213e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c45833ca-0927-487f-b0af-b14c9cbff267)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cffb712d-1e30-4a8b-9917-80788ba79a8e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddee473a-d73e-4a66-9005-04df8b03e970)(content(Whitespace\" \
         \"))))(Tile((id \
         3dad2860-aaed-4ecb-9884-cb9466193265)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9cb8d35c-56b6-4a51-8104-35a7346dbe17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4eced9e1-f737-44e4-847a-1a39c3723e6e)(content(Whitespace\" \
         \"))))(Tile((id 5cd336c0-3b87-437d-9862-c02bc34902b0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dd3eaf01-cc71-413e-8ee4-e2c94986ae0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3715e5f4-41b0-4bac-b761-06638a5dc16d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ca97a9d-2769-487b-9a90-ae0c6d01af6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d6732a7-6980-41ab-9109-1dc6320b665b)(content(Whitespace\" \
         \"))))(Tile((id \
         a22eee89-172d-4806-9f7b-0326a49af86b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a4fb0f68-0389-424c-919f-d8eb665ae29b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         086b538e-3dca-4f01-bb1b-91de842e7b21)(content(Whitespace\" \
         \"))))(Tile((id \
         e140b9f0-8cca-48e9-9cea-438279faeaad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         245939e1-bfbe-4815-ab3c-96fd5f2dd212)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a93d9fd2-5061-47f2-9cbf-c45493088044)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e8ef198-10af-4a68-8be3-5484be0e9dcf)(content(Whitespace\" \
         \"))))(Tile((id \
         0f5c3422-3d73-430b-83dd-98e3c575f996)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         912edb6b-d4a9-4590-8f1d-4be08a3701e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6cb8dc9-9149-4446-a84a-46868afc21ce)(content(Whitespace\" \
         \"))))(Tile((id \
         9a10244d-aa05-471c-99d0-8e2b24c35a77)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8f795129-6831-40c2-b0c9-7d0539934791)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fdc13db0-97c0-489e-a838-d32a53ad22d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26edbc22-8dc0-46da-b299-2fc11f5e394b)(content(Whitespace\" \
         \"))))(Tile((id \
         e7a425b0-8669-4605-8a3b-3d4ed8dfa6d4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         753cf8e8-c02a-4fec-a800-3c8f6758d382)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d8812359-a545-4355-8e51-c8f4dbee9329)(content(Whitespace\"\\n\"))))(Tile((id \
         efaa63a5-eca6-4a30-b0ad-d98c15ca1bac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         017e79e3-d7b7-4af3-acfe-cc6a744ef0d2)(content(Whitespace\" \
         \"))))(Tile((id \
         29b48992-08eb-4b72-bbcb-7d24a9e80add)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc7cd0dd-06b3-44d2-aad7-60a35ab23ce5)(content(Whitespace\" \
         \")))))((Secondary((id \
         83213a58-d2cd-4bcf-b18e-82b7efc5dd23)(content(Whitespace\" \
         \"))))(Tile((id \
         7aa61b57-d9d4-45c8-aafe-1a1d10c7ffe2)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         328454e6-8563-43f5-9d93-17ce86e4f91d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         593e5102-562b-4a47-a903-699e832fe62c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c15db1e3-0695-40f3-94cd-53629f6137e3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         196aff8c-a5c7-4ead-ae11-874fc5382960)(content(Whitespace\"\\n\"))))(Tile((id \
         b3798f96-6fb2-4458-b650-510c94b96e7c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa7ac395-4644-4070-b854-4e34a2c0d26a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         678da07b-72c5-4ad3-ba03-9fe57fedb542)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1718a55f-4aa6-482a-9bd4-9a6964cde9e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a88848e-6214-4412-870b-6a57ef135a79)(content(Whitespace\" \
         \"))))(Tile((id \
         34fb7f63-6bb3-42ad-847e-efe05b497c10)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae40fe9b-c7cc-4cc5-8c3d-2b3a76028ada)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50ef0312-2b11-45a0-aeca-7be673b0965f)(content(Whitespace\" \
         \"))))(Tile((id \
         2333dc71-fb39-48e7-b8dd-221a5c1eb28d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         90d5b8b2-5f21-4ba3-8956-42167e12c319)(content(Whitespace\" \
         \"))))(Tile((id \
         01b21cb2-b976-440b-be90-0a5f4899fa29)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f67c1b83-18b8-4799-ae86-8a4570a0141b)(content(Whitespace\" \
         \"))))(Tile((id \
         37ecfa80-1c6b-4a2d-943a-bd136d977375)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2aa6aae7-cac0-4882-8e86-0f3331d95c93)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fff55ab3-f3b0-45dd-81f1-5b059ec265cd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5db8e64-9415-433d-9081-04bad1851a90)(content(Whitespace\"\\n\"))))(Secondary((id \
         22ad63db-ae34-400c-b811-8edc854c1e9e)(content(Whitespace\"\\n\"))))(Tile((id \
         1a5e69b3-c2ca-4ba5-8009-d9e410631092)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3e39efe0-e9a5-4ce0-bdc6-e12650420bbf)(content(Whitespace\" \
         \"))))(Tile((id \
         dac6dfe7-572e-4526-8766-692ace4c1963)(label(\"\\\"alive cell with 3 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0da9c5c9-791b-4c81-8ad9-63373f050319)(content(Whitespace\"\\n\")))))((Secondary((id \
         03503653-bf98-4672-9227-c1cf24dc1762)(content(Whitespace\"\\n\"))))(Tile((id \
         7dbbdeac-1a67-441a-97a2-f2b3c8a0c206)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         678738b8-930f-4e6c-8b46-d7cdce9ca7da)(content(Whitespace\" \
         \"))))(Tile((id \
         ffb1732a-e6f7-4db8-8b25-547ae7d49623)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4628b7f3-1a04-4483-a026-31d572a7c279)(content(Whitespace\" \
         \")))))((Secondary((id \
         c94e2842-8a52-4f7d-8885-44156852eab3)(content(Whitespace\" \
         \"))))(Tile((id \
         ace38f2a-96b8-4753-98c4-1edb80917b24)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d83e3f4-b41c-4381-99d9-f3552a728630)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c079b2e-01d7-4468-ab09-d92617a13dcf)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         265cef6e-9ed7-49ec-82f5-9afaa4e3befc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         83e83a35-e8d9-4e19-beca-2265a88b8057)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         847658fa-9081-42f8-b1c3-fd9502f0acdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5427ee32-7fe5-4413-9674-7eff250fe9d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d19a96f4-633a-4d2b-84c6-c6970cf0c208)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b34d2bb8-cc81-4113-9ac2-80910eb34f7b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e4a8a438-7706-4596-9ce7-d5401f3f36ae)(content(Whitespace\" \
         \"))))(Tile((id 4dd94c5b-8a6e-401d-b01c-473ebb3d0c9c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         391f7cbd-9ef3-432e-bf94-a3602d6549f7)(content(Whitespace\"\\n\"))))(Tile((id \
         c8babc13-9579-411b-9749-b47aa7d19f0f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e8a97850-f223-4475-9422-a53b33e6d383)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19b8e01d-0815-4444-bee2-633a9a7af808)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5962637-a789-4ecf-978c-e43cf79239e2)(content(Whitespace\" \
         \"))))(Tile((id \
         6d361c1f-01dc-4edf-b80a-b591e65cede5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5268d61e-bfa0-4a24-8d7f-027b7d7a18d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de48b7c4-da2e-48fe-b9f7-84077b392005)(content(Whitespace\"\\n\"))))(Tile((id \
         c414e44d-a774-4067-be19-d67dd7a9050f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         65bddcac-2205-4b65-95f2-943a324eb83f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a1be5d7-2052-4dd1-92de-be84f8d068fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbbf306c-803f-43cc-a9b5-a160b5e040ab)(content(Whitespace\" \
         \"))))(Tile((id \
         355bf34d-f8c7-447f-8803-36e40577deb7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bd37a5fb-afce-4443-8ba9-93289433fda0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04ccf7f0-d8c9-42f0-9a51-47b6bf3f20aa)(content(Whitespace\" \
         \"))))(Tile((id \
         5f93e342-b325-42e2-8ee1-b33d3f92eed6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         81d9c9ad-498d-46aa-919d-1ddbee5f9cc3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae6a68b0-e66a-4b36-9d76-43883e1047c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ff2ad11-045d-4dbf-b053-59664e56e030)(content(Whitespace\" \
         \"))))(Tile((id \
         8ea699c4-9d91-42f1-8d96-2f2f3511fe2e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f20642d8-ff4e-434e-a065-5998b0ef1322)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36bedb56-b2f3-46fb-bb39-b2c5c4edc7aa)(content(Whitespace\" \
         \"))))(Tile((id \
         55bb638a-e8ad-465c-8371-5ba197693f81)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         645cf0e8-dfcd-4b9b-b633-c0125dde479f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd7b64a2-fa58-4474-86e6-af47545d3c39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2acca879-df81-42e5-8403-7aafcf0a1a8d)(content(Whitespace\" \
         \"))))(Tile((id \
         71b083e2-ca46-485f-8473-f0bc4d6e45f7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         db43f3d0-015f-4f21-986b-4513053f2c92)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         ea6daf9b-e969-4d75-bafc-c92681db6ab9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         07d11f08-3d41-492f-ad73-186c4750ea43)(content(Whitespace\"\\n\"))))(Tile((id \
         ff74f3b7-9bdf-4979-a492-42d660d83bd9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3de5c5e4-6f31-4da5-9663-d4ec0bbacd7c)(content(Whitespace\" \
         \"))))(Tile((id \
         e62cbb1e-6eb1-4f88-b82b-a8ef255ba97c)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4219608a-a7e5-4566-8652-298bb3279c1f)(content(Whitespace\" \
         \")))))((Secondary((id \
         8ed48da3-b2f4-46ce-8ed6-83b842df781e)(content(Whitespace\" \
         \"))))(Tile((id \
         3f8f0116-34cf-49bb-935a-95c6c5da0047)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c234a49f-11b1-4a20-8fe5-9284a5dc2dc8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e63c143-c733-4b1f-8afe-c67e0dc33917)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bfa224eb-164e-4e69-af96-ff3a6fdd80f5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b90c4750-04d1-4980-86c2-a6d9536d62bd)(content(Whitespace\"\\n\"))))(Tile((id \
         eb5a771a-9315-460b-a74e-d87adada77ed)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b72ae93-94ef-41f6-9a24-99be8d76f174)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ecc6801c-614c-466e-9589-c373264fc042)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59b2a1d5-7a6e-4cb1-9358-cf161c13f1c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d164865b-7ca1-454f-beb7-25634344ed6e)(content(Whitespace\" \
         \"))))(Tile((id \
         5c930527-0994-4889-a49b-2bc3be7b27af)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98d6eccd-551c-4d09-a2f3-f854af4445d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58b36da8-89f5-44d7-99a2-abe97006600f)(content(Whitespace\" \
         \"))))(Tile((id \
         3db965ef-8db4-41b1-ab62-47591a7d4d63)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         08b76080-000a-4d84-bbcf-ba6f92d5ae69)(content(Whitespace\" \
         \"))))(Tile((id \
         8ad91b0d-05d0-4b04-89c8-f8f787421d55)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c263ce7-5c24-4c07-9051-e9b4f7ed0705)(content(Whitespace\" \
         \"))))(Tile((id \
         62e3d337-6837-4e53-bef7-4ec7feb2e5ab)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c21c0f4d-22d2-4672-ab88-be95c08e6ecb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a3a64f95-1f63-4e5b-8690-301772aeec79)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf016203-5cc6-4ec0-9e3e-c0b92eaad095)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d18090b-c135-41fc-8ec1-2736edc53355)(content(Whitespace\"\\n\"))))(Tile((id \
         9fb9d641-e01d-4352-9e4e-df4e1ed24908)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         39447181-14b7-464f-9c04-1835244a0581)(content(Whitespace\" \
         \"))))(Tile((id \
         6230f9c6-ba01-44a8-a5c8-db6bfda37d2e)(label(\"\\\"alive cell with 1 \
         neighbor dies (underpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df8703ea-9a72-43b8-a23c-80e5004a324e)(content(Whitespace\"\\n\")))))((Secondary((id \
         e0ba66cb-3643-4488-9289-6f52fd41b602)(content(Whitespace\"\\n\"))))(Tile((id \
         c9073774-22cc-4ece-90a2-ac86a48f34b0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         27f946f9-03ef-4cad-92e8-7b1992d4bb34)(content(Whitespace\" \
         \"))))(Tile((id \
         26aa5b42-68a6-4b97-8fbf-e712ce632fd8)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         28e555c2-21c1-4a47-9c40-806e5daeece8)(content(Whitespace\" \
         \")))))((Secondary((id \
         4a06bebf-e32e-4f61-9477-a1ad26994a32)(content(Whitespace\" \
         \"))))(Tile((id \
         1727f943-16e0-4985-8482-5479fd1d0a96)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d2f5d1c-e2d1-4096-bf43-d6ca44c59623)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         53207d6f-0c7a-4770-8c2d-e823d1268dc4)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         157e4be4-7243-4896-beda-4683b6a24802)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         47493ae8-48b5-4e1c-9b40-c2d4b43e8d22)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7dcf6abd-7d75-42f5-8868-dd99491f47d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d361155f-e429-4833-879d-2809c7da9e7f)(content(Whitespace\" \
         \"))))(Tile((id \
         777fdf47-7a45-4268-97de-df6004ef82ee)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e7051b4b-5d2c-4282-8370-2aa03ef10518)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         099a17a4-22db-41af-92b5-dd8a06ae4631)(content(Whitespace\" \
         \"))))(Tile((id 45b4cc07-8aaf-4cb3-924c-82b43e3ab269)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f4da331c-3397-4f1c-955e-61e5ad4d4070)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ffc706d3-fd88-4f0c-a43c-f90ad87f9723)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd316a56-4bb2-4211-b218-20e343596c52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c349f84-dbb3-4f0e-829e-fcff74837043)(content(Whitespace\" \
         \"))))(Tile((id \
         38b6ccd0-3e3a-4ee7-9ce9-4a299407a475)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ef760921-67df-489f-b8bb-e2438e17892a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b33690be-0de1-456b-806c-d3e99f1a9043)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d70f34-dd8d-45d8-b271-14765134ad57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d8a77110-9249-4b90-bd26-b73e87b7a2e4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3e249c0-5f9c-48ae-a6d8-d34172c15488)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72c7bc85-9fd5-484c-99ac-0e86a212b175)(content(Whitespace\" \
         \"))))(Tile((id \
         6c4f7b51-7be5-47ed-bd3d-0e974c716630)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         98c4aaab-4c30-4399-875c-e5601f0e2a70)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4e0fdd8d-73d0-4cf8-8c15-490205b9f231)(content(Whitespace\"\\n\"))))(Tile((id \
         da5ac404-ebd7-49ac-b09f-1da489c0e16a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1c38e08b-405e-4666-a0e0-a0dae0f0f60b)(content(Whitespace\" \
         \"))))(Tile((id \
         13418ecb-3824-4939-95e4-45883755a34d)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fc8880ac-74a7-4d72-9c88-b2f71a64b282)(content(Whitespace\" \
         \")))))((Secondary((id \
         ce324ffa-679d-4354-98c1-7affa855db5e)(content(Whitespace\" \
         \"))))(Tile((id \
         cbee425a-0dbb-4f63-b053-d285d49e22fb)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         455e9658-fa2b-47a3-b6b3-a50cba21ceeb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         180f377e-ae90-4437-b3ec-4dccb699cfac)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         42c1c579-a088-4c5f-b123-ab55f2ff24e6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fd8eb5b7-2321-469e-9ff3-2efa812e5b3e)(content(Whitespace\"\\n\"))))(Tile((id \
         b12fe75c-0bb1-467e-b47c-bcd15e033c5d)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         699ec8da-4c6e-4eb5-b2fd-11b89f6092de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e9bde192-2d1c-459f-8cb3-936e33037a9a)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b5fb793e-73ef-46d0-ab5b-a39a092bb9c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7160795-e065-462d-b512-d77191d8b85d)(content(Whitespace\" \
         \"))))(Tile((id \
         1ddf9e84-2cf0-4a19-9be1-bb2a7a41727a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a071318-b533-45c8-bfc2-2fa3fa524926)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86947708-46c6-4bfc-b309-837abf0f6e8d)(content(Whitespace\" \
         \"))))(Tile((id \
         aec22e48-72a9-45be-beb9-ed37a7b6727f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f730455f-1f69-4a85-a1ae-c25c4de9ebbd)(content(Whitespace\" \
         \"))))(Tile((id \
         698bfa9a-0b96-4116-97a1-969a10a56d10)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         331a152a-2ed7-45a7-a659-366dfeed4cb4)(content(Whitespace\" \
         \"))))(Tile((id \
         f7189c04-86dd-4e99-9a71-e832a6e69ebc)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8418b406-1df5-487a-a8c9-270dc899e3dd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c67267e4-c13d-43c8-b03e-b3dfd1688cf9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c98060e-ecff-4ec2-8789-15deacae908f)(content(Whitespace\"\\n\"))))(Secondary((id \
         958939bf-23f1-413b-9c6d-f94ce790a384)(content(Whitespace\"\\n\"))))(Tile((id \
         83adf1fa-4edd-4bf0-8cbe-dbb0db4e50d9)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e19e08a8-18be-4776-a64e-c974e14f451a)(content(Whitespace\" \
         \"))))(Tile((id \
         eefa7333-31da-4b23-b8af-b7cdba4c65fc)(label(\"\\\"alive cell with 4 \
         neighbors dies (overpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78080bb5-94d6-4e61-a1f6-703de54702aa)(content(Whitespace\"\\n\")))))((Secondary((id \
         9dfb52df-d9b2-4b3f-b0df-d9c2f6661019)(content(Whitespace\"\\n\"))))(Tile((id \
         3ab20e94-97b4-423e-9ada-a009b630efa2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         90eef16b-b491-4266-9de8-cb2d301e7a3f)(content(Whitespace\" \
         \"))))(Tile((id \
         e4f17f92-c1ff-4a68-98f8-0fbdca8b2df2)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2b2d0407-e004-4fd2-9053-9bd0e5d22263)(content(Whitespace\" \
         \")))))((Secondary((id \
         69f2397b-5ad0-460e-b0f5-6aef60556705)(content(Whitespace\" \
         \"))))(Tile((id \
         79304029-8b0c-4220-aef9-f42cd654c374)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0de34947-9cd3-4a9b-82a2-3188c2f1075c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         abb5057d-e25e-4bcf-99da-7dbdf45036d4)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5c57545-005d-4be4-a6dd-5de52ffb2fe9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4f57c34b-664f-4dba-9f5a-9583eb49ab4b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e01608b3-abb1-4ebc-acc6-2f7cbef74dc8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7c9583b-8cab-4dc9-ad5c-d93463da9cb0)(content(Whitespace\" \
         \"))))(Tile((id \
         8f92d9b9-d4d2-4168-b6f3-f4102fd708eb)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cc612791-60ed-4266-8b49-f380e85f337c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d16071e8-9e0e-417b-bf76-83003b98a2b8)(content(Whitespace\" \
         \"))))(Tile((id dee293d2-f222-486b-b7df-280a34166a88)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4d3c03d5-07b0-425c-b9ab-2f01acfae2d3)(content(Whitespace\"\\n\"))))(Tile((id \
         7f0190db-568e-4acc-b67c-fe7201c80eb8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9c2e9b2a-6dde-4ac0-b16c-230beda580fb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24d36a10-f91c-4b36-89bc-d258114b4451)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04ea6549-8760-4378-9114-94a314435322)(content(Whitespace\" \
         \"))))(Tile((id \
         940b794c-bf98-4e4f-a172-e4e7e93612f8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ad8e28c8-4461-41e5-bbac-5bde7a3ca1d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fc8d8c9-beaa-41f1-8abb-3e35dac18389)(content(Whitespace\"\\n\"))))(Tile((id \
         ba4c579d-9d49-4477-8789-909d8dde07c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3352c408-d274-4534-9ba9-3936ff145fbf)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbae9d2c-e6d7-4744-89e7-036ae8705192)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb816a18-5ec2-4adf-885d-c3b3a09d2eb5)(content(Whitespace\" \
         \"))))(Tile((id \
         86e5f14d-ab34-4f32-abc9-9f635329ec0f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e467e975-a5e5-403a-a4cd-7a923466ec07)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b63a510-239d-491b-a17e-9bc06fd0d381)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5ad1a1-afb4-4b00-9359-926e75a9fe8c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3f643604-b885-4216-98c0-3326bbbf6f7f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2356911f-fd81-48d8-a1f0-5891c67adf83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2963ecf-993b-4dd0-8557-edd5037fa2f3)(content(Whitespace\" \
         \"))))(Tile((id \
         25423552-5d69-4f90-b072-6fd40acf1003)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         85d9f681-30b4-4567-aa60-27e80dee9b83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c4abe11-9f14-4495-b318-7f46af921bba)(content(Whitespace\" \
         \"))))(Tile((id \
         5f3f4b0e-0e57-4274-82d9-8855a3eafd12)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9e73c699-8e96-4b9c-8e4e-434635613a3f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80ee1e65-d2bd-44a5-85a2-fe3f3dbf9c23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09c47186-4921-45bf-aaa4-385f5da11ab4)(content(Whitespace\" \
         \"))))(Tile((id \
         845862ab-42b2-4b92-a3d6-f1b1cd8fd8bc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f2731bc6-1488-429f-81a6-858bd5194f1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3d618b3-0b5a-4d2a-bd68-ce7a4c3d2a27)(content(Whitespace\"\\n\"))))(Tile((id \
         25b08557-f958-4808-ad53-01c6be113624)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff99fd48-bd77-461c-ae2c-f59ffea393e2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f734ff5a-416f-433a-ac32-b4fb86dae222)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13fb2ac3-bd58-4c88-9ca8-e9a948dcd472)(content(Whitespace\" \
         \"))))(Tile((id \
         d3e8abc4-6f10-4b1f-89a0-5b37f005caf5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ba2ea027-f3a2-4d61-bfcd-b0cce4158777)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         5731aa4c-e9a0-4b10-b029-6eb052eb7677)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1913c730-3efb-4db1-a2c8-2cf233148e59)(content(Whitespace\"\\n\"))))(Tile((id \
         992cf553-d742-4ee8-b762-0daf667bf3d1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6ec1cca8-af16-405b-8ca2-1ef089761898)(content(Whitespace\" \
         \"))))(Tile((id \
         770ba7cd-da45-409a-bc60-092d6ebafc10)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3604e187-9010-44ea-b6e9-6353138315b4)(content(Whitespace\" \
         \")))))((Secondary((id \
         8631e00e-43a4-4853-9f30-448571d8955e)(content(Whitespace\" \
         \"))))(Tile((id \
         c879ad0c-79df-485f-b44f-ff62c2f4b2d5)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df81e63b-7a39-45dd-a080-599a4f377d5a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b772143c-39cc-4102-8bf7-18ca844c4046)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fc456db8-3c01-4f1f-9224-05f2bc0fb771)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fb48e96e-d33c-4314-8ed7-fd4609f041d8)(content(Whitespace\"\\n\"))))(Tile((id \
         8b11a9b8-6a60-4f0c-9550-f07164441d60)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc9cc00e-a4f5-445b-bc81-e668485352ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a9b2e4bb-ecb5-4eba-a3b2-ba3285cd792d)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4a678b4-b928-4729-bea9-8b01a6fd00f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         737d150b-8503-4dab-87b7-8f12876fc627)(content(Whitespace\" \
         \"))))(Tile((id \
         71fb6752-745b-4e58-a591-9496ef35637f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c430fae3-8a81-4a78-97fe-9aae4f9f92f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e31eb676-44ac-4ac4-af19-04621108b1c2)(content(Whitespace\" \
         \"))))(Tile((id \
         dfd2bdbb-6058-49d5-81d1-8cc552ef1866)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c7cf7190-11c5-4a4d-b91b-0b454633f760)(content(Whitespace\" \
         \"))))(Tile((id \
         319200cf-ddee-42ff-8869-5079779a361e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bb95e48-72d3-4892-8d83-fa05f087a73b)(content(Whitespace\" \
         \"))))(Tile((id \
         57ab7d30-4fb4-4d9e-81d4-7e80b2f61b00)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42a29ab2-95c3-4641-bcb3-b88cdf45a865)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7d4b4ccf-802f-41e7-a440-885982a32d97)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80c31315-9dab-4f9a-aa50-457fcb11fffd)(content(Whitespace\"\\n\"))))(Secondary((id \
         85569c83-7994-43f3-9e97-a134525e2966)(content(Whitespace\"\\n\"))))(Tile((id \
         9ca8559a-01b6-4596-9de9-41f25e42a2e9)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e731362-725f-40fa-9628-b8b1f3841abf)(content(Whitespace\" \
         \"))))(Tile((id \
         5841a844-c3c5-4d17-a6b6-ce6e9a9bcbfe)(label(\"\\\"dead cell with 3 \
         neighbors becomes alive (birth)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad4b4c2e-cb4f-4a6e-92f2-51fb708d0264)(content(Whitespace\"\\n\")))))((Secondary((id \
         242cd4d8-45c2-4b15-80d1-34512b76c7b0)(content(Whitespace\"\\n\"))))(Tile((id \
         27035351-db9d-4ea6-ab86-4be9099093d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8821c731-e03d-410a-a414-88cfffdff834)(content(Whitespace\" \
         \"))))(Tile((id \
         47f1f528-ce95-4914-bbcc-198e150c3f77)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0bca981f-b277-4e44-9955-b05f31098ef4)(content(Whitespace\" \
         \")))))((Secondary((id \
         50495c7f-2ec0-4bec-a426-f61dd6f1bf0f)(content(Whitespace\" \
         \"))))(Tile((id \
         45c18794-4ab5-44d4-81e2-ed2672f5a032)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         769c1ca6-0baa-4121-9d4c-28e7bd0d7e78)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         676e62f3-f379-456a-a3ce-535550167b71)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe1001ed-b3fb-4967-a7a1-8bb44f9500dc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9c4856e0-405d-4c37-8513-1c57426f82e8)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         991f859c-9e19-4a84-aa17-bc2d16e7cd04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d868345-2802-4659-bbde-a2366abca9cc)(content(Whitespace\" \
         \"))))(Tile((id \
         cc09d20f-f6a7-4ca2-9476-d2e3f06ccd07)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2bdf8a4c-27d2-4200-b101-d9782968c82f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         524133bf-4b4c-48e8-8f64-a987c94ee2c0)(content(Whitespace\" \
         \"))))(Tile((id c7681d09-d18d-4776-8460-a23802ade988)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         703475ed-9a82-4c7d-9516-f59176275417)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5df49cf1-2080-4338-a0bd-fcc663ac380d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b45c086f-d5f4-4cb9-8622-a4dd405d3b43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d76806d-c4d5-425b-8eeb-12bf6789d39b)(content(Whitespace\" \
         \"))))(Tile((id \
         d1391c48-68bb-4156-bc4b-39269366d71a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8dd2b711-40cd-41ac-a2d2-73ce2d655ed0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b077e20-4c31-4da0-91dd-d677df8944a0)(content(Whitespace\" \
         \"))))(Tile((id \
         2a474154-30c9-4ab5-ad1f-ce290f7084d8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         68084503-1935-40e6-868e-6c4e15482ec7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9980007d-c9e7-4f2f-882d-26709811f595)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8be0c114-f485-4b8f-bdb6-49cc011e40a9)(content(Whitespace\" \
         \"))))(Tile((id \
         0a13ab89-5a92-4ae4-afcb-a7e28acfe754)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         17e1c35a-156e-4cd7-bf7d-a7edab7ddc93)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21ba59be-ea83-4f57-83fc-dcf67f4ea6ab)(content(Whitespace\" \
         \"))))(Tile((id \
         96e5c486-029c-4597-9dc6-a8ab8a2cb34d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9f2d7baf-ec2c-4fa1-90cd-1aedc249260f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ffa400eb-4e81-4ca2-ac87-7325d3713b7f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20f9121a-e0df-49c2-a56f-b42be2512d77)(content(Whitespace\" \
         \"))))(Tile((id \
         b19285d2-bc15-4c41-b6ed-2d320f5b7336)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         fc124991-c9b2-4a3c-863a-8665ec59d615)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7a3abe80-2e8d-4d8b-ae9a-0f73481dcd0f)(content(Whitespace\"\\n\"))))(Tile((id \
         97544ebb-e04c-4df3-82b5-1b0a8e9134c3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a7903183-393d-4cc2-9cf7-f9732b12fae3)(content(Whitespace\" \
         \"))))(Tile((id \
         481043bf-50af-421f-9c51-c14415da5dcb)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2e464418-d677-4ba9-8f62-e07f9ac4d044)(content(Whitespace\" \
         \")))))((Secondary((id \
         9627e3cc-a60e-4322-8fd2-8a282405831a)(content(Whitespace\" \
         \"))))(Tile((id \
         b7b5eae6-b695-4175-8e42-48ca5d3f83d6)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c663a64d-613d-4e1a-87a7-9b34cd69970e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d4619ea-8bc4-4085-870d-874a414972d0)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b0013a5-4949-4719-a64b-c607dbfb13c7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70ac1561-4ff3-48b8-be21-a57f26f38c35)(content(Whitespace\"\\n\"))))(Tile((id \
         c4b14321-1180-4556-87e4-33f2a5e1934c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df651b73-cdc3-4259-85a4-0f76111b0bb5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5fb0f7c2-0283-40bf-8615-5ad04612737b)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7286825-81d3-4e70-a2a8-7697c5d44b8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbed9e4e-9ef5-4b64-ac0c-9c8b7b784e54)(content(Whitespace\" \
         \"))))(Tile((id \
         2231aca9-c3b7-45cf-92d6-90cc41f6d090)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59d435a2-772d-46b6-8855-88c1682d5523)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b5c5eaa-1800-4a83-9fb0-3887536bd0fd)(content(Whitespace\" \
         \"))))(Tile((id \
         7c044cf5-2ffc-4d15-823b-cda9d9d65b43)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f0d26f74-9cc4-47c2-ae94-ab23fb0de109)(content(Whitespace\" \
         \"))))(Tile((id \
         ca1e50bb-2a0c-47f6-9ac5-1e14e9127a15)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04ef0dde-1c8e-4e95-8b54-348996b86837)(content(Whitespace\" \
         \"))))(Tile((id \
         64933f85-5a0c-4227-a8b4-1cbe98bc5ed9)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e7576e9-d1e2-43af-9f4e-49d0f4af7a28)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5b44f864-f620-48e5-a7ac-210ae64a7bf2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         071379c4-5c49-485d-8c9f-175e024e2d55)(content(Whitespace\"\\n\"))))(Secondary((id \
         12eaa4eb-88e0-4620-959a-87e70fee253d)(content(Whitespace\"\\n\"))))(Tile((id \
         0862cf7a-2557-4317-bf59-ed7957fb189d)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ecdacd34-736f-48fa-bad6-d1527f7f8cf4)(content(Whitespace\" \
         \"))))(Tile((id \
         61f5ba3e-98b9-408b-ad0c-2902e7402210)(label(\"\\\"dead cell with 2 \
         neighbors stays dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5842ebaa-3337-4600-843a-2969055da2a3)(content(Whitespace\"\\n\")))))((Secondary((id \
         4a8302d8-d178-4af2-822a-97039c88b06f)(content(Whitespace\"\\n\"))))(Tile((id \
         37ce6462-3794-4547-8ad3-97d8ac9a36fa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         40384647-e15b-470d-a53f-4b2ec05ed993)(content(Whitespace\" \
         \"))))(Tile((id \
         74c5d322-2e6d-48cc-ab60-b8b816fae779)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d9f6a683-6285-4c6d-96a0-997dbea88dbd)(content(Whitespace\" \
         \")))))((Secondary((id \
         a78f8e39-a0ed-4737-b908-0f1f5c205719)(content(Whitespace\" \
         \"))))(Tile((id \
         baef50fb-3f3a-494a-b59d-300d2b38770d)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f352325-226e-40d7-b566-70e880fbcaf6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3dda6f11-6fdb-475a-8c2d-59f9ba7e9682)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2aaddd7-b66b-4937-8d01-6c6287a8df4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         588ab44c-1344-497d-879e-973ea60d7978)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd13fb5b-0573-419f-9c22-f8431f6301ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50b0cd44-89ae-43a7-9379-31dcd6b074ac)(content(Whitespace\" \
         \"))))(Tile((id \
         47c95590-3365-4c2b-9ef7-f03fe81b4bec)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a0758f89-a468-4286-bfa8-24673b19b4e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31771270-3ccc-4d7f-b5a1-813814a50021)(content(Whitespace\" \
         \"))))(Tile((id 81732ba1-efb9-4dd7-b2c5-551ad24db591)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f42285c-91d1-4327-9260-6280556dee2a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0b056cb0-a1a6-49ac-be50-bc18156af928)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfde842d-1b2c-4641-acdf-36479b80175a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c1990a0-26b6-4ba2-9c11-cfc25798f324)(content(Whitespace\" \
         \"))))(Tile((id \
         0f8809da-9e39-48d4-be2c-d7f21f4fb606)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         622df2c1-dbf5-409e-aaf8-35d9bae71f8f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28d0f0af-910a-4f2b-a72a-15078738b8f6)(content(Whitespace\" \
         \"))))(Tile((id \
         ef31810e-6c7b-4d15-bcf5-a517784f7f9d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c55eae77-bf70-421f-99b4-d75ea7484d04)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab02015e-400b-4e29-a687-e096f1ef6651)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31f78310-fa54-4f4e-acd6-d01d57763b3b)(content(Whitespace\" \
         \"))))(Tile((id \
         dd8e111f-ba72-4bba-a971-cb91eb8935b6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         f758192a-0f6d-46ef-b07b-3436a4cb7132)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2df0a00e-012d-43a1-a538-e98c6d3a1d3f)(content(Whitespace\"\\n\"))))(Tile((id \
         83ce0fdf-57f9-4f0e-a540-92265e7e2fad)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48ae2691-948b-4eb9-8c28-0b987026f454)(content(Whitespace\" \
         \"))))(Tile((id \
         af2f9911-b795-456b-a43f-520107e62602)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         255ce167-9506-45f7-81a4-506b4704f72d)(content(Whitespace\" \
         \")))))((Secondary((id \
         658018c3-3c61-4212-bbb1-ce1a4e12a733)(content(Whitespace\" \
         \"))))(Tile((id \
         5ae998a7-f557-48c1-aec0-437633675bad)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1b490c8-4bc3-45d1-9e87-20857d39597a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d8fcdf3-6e0e-44ac-9ea8-4e02477f1eb1)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0a28369a-b5f9-4cfc-a722-b60aad7255e6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e065ad6c-0a2e-4cdd-af1a-0351bc3c6a4a)(content(Whitespace\"\\n\"))))(Tile((id \
         0cf98546-3718-45d7-af56-1fd321ff016f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a05137cd-c088-44cd-bcad-2a17aa69cdc1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4bfa58e0-2387-448f-9643-7dbec9350ec8)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c27a62c-a6d5-4294-b255-9bfaf543ca57)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e817b27c-7663-4c95-8170-9c0d19469186)(content(Whitespace\" \
         \"))))(Tile((id \
         cfd2c7f5-5c66-41a2-8514-c83ea996283d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34f887cb-10e4-4eab-946d-3bd75a59f9c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b29863cb-21ad-4e2f-868d-0ffa58bda26b)(content(Whitespace\" \
         \"))))(Tile((id \
         146b2652-7e69-4adb-9da5-9b6bb636ec38)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ddc4d1eb-fddb-4a69-8df2-c7770e7f154f)(content(Whitespace\" \
         \"))))(Tile((id \
         8733ecde-856b-4284-8b80-90a7476f17c2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c14bf366-9652-4779-9b0e-2c9ade2a22da)(content(Whitespace\" \
         \"))))(Tile((id \
         e363b685-e943-4a48-a382-7353683ac253)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1821c2a-a12b-4d95-a982-dcfb2c5ed951)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cfef0e0e-c202-44f2-a749-2cab092cb9ca)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79a11f2f-c923-4704-a70d-331014152c42)(content(Whitespace\"\\n\"))))(Secondary((id \
         7670528c-c9df-49d2-821e-b6e0f8008f89)(content(Whitespace\"\\n\"))))(Secondary((id \
         5adc61e8-01f9-40bd-91b9-1abbe5b6f59d)(content(Comment\"# Classic \
         patterns #\"))))(Secondary((id \
         037df6f1-47b8-4861-87df-cf2525109796)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3286ab7-b06c-4cfa-ae2b-92c87146201d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a10055e4-e87a-4710-b752-7065272efd0b)(content(Comment\"# Blinker: \
         oscillates between horizontal and vertical #\"))))(Secondary((id \
         92087b45-5612-4c83-913b-aaf89aa59dac)(content(Whitespace\"\\n\"))))(Tile((id \
         7dae59d1-54a5-45a9-8361-0924b614d1b8)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7485d711-abc9-474e-a3d0-459ef2bb6c2e)(content(Whitespace\" \
         \"))))(Tile((id \
         5f9eb304-3818-41ed-ba3c-712d9190a739)(label(\"\\\"blinker oscillates \
         (horizontal to vertical)\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e5015dc-2cfa-4630-9587-9bcce3b15030)(content(Whitespace\"\\n\")))))((Secondary((id \
         0a45a0c6-5fde-44fb-9543-9cda91e0c917)(content(Whitespace\"\\n\"))))(Tile((id \
         4501d360-394b-4bd9-af83-19f417ab7bf5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13ee9281-07f4-4a94-bffa-cec48ea9be20)(content(Whitespace\" \
         \"))))(Tile((id \
         4c5d4358-5629-4c9f-9060-35a721af3a82)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d5bd78ba-ffe0-462a-815d-fbc9bb7fc22e)(content(Whitespace\" \
         \")))))((Secondary((id \
         6fe96341-96c4-4da2-9b34-ef177bdf85a2)(content(Whitespace\" \
         \"))))(Tile((id \
         4731d40e-69d4-4500-8364-52ea54c1f1c9)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01611f77-462d-4a85-a024-063a9e2377be)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         93243832-1a48-4f16-9eeb-92403141d886)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83e33dd4-aa4a-4a2f-a95a-ab6f446a4f32)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d032ed25-0c7f-47d8-a944-8a446e90ce14)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0afea637-7367-4007-a3b8-61553a0606e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         681c1123-669d-4445-a1dd-1cb8dd8c4d10)(content(Whitespace\" \
         \"))))(Tile((id \
         85c8ee5d-ec22-42bd-b7fe-c4803ac63642)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d38d5bc7-645f-4cc3-b00e-d0f67b9f323e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e528d57a-d082-4ac9-b8bc-1568f8a4c91e)(content(Whitespace\" \
         \"))))(Tile((id d3878397-1540-44a6-8b3d-95efafc8254f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ce407e2-a2b0-47df-b496-dd50413c402d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         66f2f49a-a381-4e35-8e31-790b1c8444f3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e92b489-e4d9-461b-b22b-3a2f3bdbc936)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a37ca111-50d2-4a2c-bdce-61a77ff9dfc5)(content(Whitespace\" \
         \"))))(Tile((id \
         65a76641-3c07-4b67-99eb-c59524120885)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fc5f44fc-e71e-4793-b0c9-04c323fd78b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51251020-b0b9-4feb-b711-878c575cb5a5)(content(Whitespace\" \
         \"))))(Tile((id \
         3d143164-c699-4525-b761-457410cdf848)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         17d20389-ee1a-4fb5-a2d4-6497399e7d57)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5596fff9-3a96-48b9-bad9-37d85c1963c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2452139f-20a2-4129-a380-57fa1fe402dd)(content(Whitespace\" \
         \"))))(Tile((id \
         c80e27c6-bf9b-4250-8aa1-395a342d206a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7482e251-5c53-4472-90a4-670b2b7b5e66)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f02a9277-94b9-42ba-a970-9d74a714c758)(content(Whitespace\" \
         \"))))(Tile((id \
         9056bc77-03ea-411e-8ab8-e27838fbfe99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8f0918b9-c0a3-47df-959d-a500bedc92d0)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e7ce996-1fea-4c69-a545-c6fe7699ca85)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed9c5982-8c2e-4ad3-a1f9-9519072e14fb)(content(Whitespace\" \
         \"))))(Tile((id \
         252a02e3-e3c0-4639-9277-b781026e5280)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         316c4f04-3eaa-4c76-9c00-e626a8dd3aff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f1fa18a8-3a24-4061-9d53-717acef3e788)(content(Whitespace\"\\n\"))))(Tile((id \
         f2eb1983-d622-4cd3-963e-63be8cbd4744)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5144636c-639e-4adc-b9f2-381721d0c0ef)(content(Whitespace\" \
         \"))))(Tile((id \
         501c4c90-e0ad-4ce8-98e3-3b856b6a341b)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c227c6ff-9b70-4acb-8e19-640538ae9d84)(content(Whitespace\" \
         \")))))((Secondary((id \
         b30e2066-25ae-4f0f-beec-fed6084acf6b)(content(Whitespace\" \
         \"))))(Tile((id \
         63bd958d-a27f-4b18-a72e-a7ea3c813c58)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7e15c53-a418-435d-9d4f-aed004246880)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         101dfa06-062f-4b39-9f76-d2a6aef06d96)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         03dbaff2-ea58-42df-96f1-057e474c8961)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ddaf59f2-8002-4f91-9c22-88e27efb1df8)(content(Whitespace\"\\n\"))))(Tile((id \
         aa599268-e02e-4162-9f43-1d28dc16b6e2)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         070a6218-9a9f-49a2-a207-1c6694d94426)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b35d35ba-3df3-47b7-bf6c-b21df3c8298e)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8dfec3a1-a9ef-41c4-893c-e6f5036b2800)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26ee4c46-e50e-44c6-b0af-81604618c5c1)(content(Whitespace\" \
         \"))))(Tile((id \
         4bea8375-610b-46e7-bf55-e3d3e692f97b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fed964d-8ca0-4bd6-9017-a0190b3fcdd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc61750a-e4c6-42b0-bdb9-73984c134e0b)(content(Whitespace\" \
         \"))))(Tile((id \
         af03ac44-32b1-4c5d-b734-a6476efb2e42)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac6346aa-6896-43e4-8a09-aac633d6306a)(content(Whitespace\" \
         \"))))(Tile((id \
         eae93fbd-8ab4-4569-8409-e7cbb8705e78)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         56f7c83a-609c-4dd8-9af8-a65932c0a446)(content(Whitespace\" \
         \"))))(Tile((id \
         066f5e42-3645-4d36-be0c-6e41782958d8)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         407a4e4e-fec0-42d1-b471-a3990e2a9348)(content(Whitespace\" \
         \"))))(Tile((id \
         f03f920e-a369-4ed8-93b1-c603f2880a63)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae8b358e-bd18-408e-855d-fc927ace4e02)(content(Whitespace\"\\n\"))))(Tile((id \
         4f958672-1c83-4582-bf41-847a83ee104f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         120c5c62-59f1-4329-9ce6-a923e9431249)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         905d3219-4c4e-4a6a-866b-0b04d6c29d40)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07da154d-792d-402b-ad38-3087f92ffac6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4767f54-510d-49af-a59d-ad80437ace76)(content(Whitespace\" \
         \"))))(Tile((id \
         353d0838-d4db-4a35-9986-0b7578369972)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cde8368-d7b5-410a-8697-35ed47f70621)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed887090-14c6-4012-8e8d-70d7757a6cd8)(content(Whitespace\" \
         \"))))(Tile((id \
         41e2aa27-10c9-47a9-9820-7db00e62f2c5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5520260e-5fc2-4fe0-9d5b-005bc121036a)(content(Whitespace\" \
         \"))))(Tile((id \
         66682132-7c27-44a7-a97b-8e3ad6e76747)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a1afdbf-33db-472b-8e95-7c1465e504bd)(content(Whitespace\" \
         \"))))(Tile((id \
         ac8b3e91-a770-404d-ac2d-54bc66833fa1)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         37cdade9-3e30-4ccf-90a5-8154cb887c36)(content(Whitespace\" \
         \"))))(Tile((id \
         4db3f7eb-116e-40c2-a486-4b8bda154bb3)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65e40de0-a7a2-47dd-a726-00d2df184473)(content(Whitespace\"\\n\"))))(Tile((id \
         260eb738-c37a-4e66-b2c0-0faf94cc6e39)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8e636e0-2392-4736-8273-6a745844a327)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3b794ff-2fb6-4ce6-b48f-97dbbec17ca7)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06c05cee-94eb-4d9b-95e8-fe3a3888dfb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0550b886-5bae-4028-b1a5-6923d411836a)(content(Whitespace\" \
         \"))))(Tile((id \
         33a564e4-8d64-456a-ac3b-5a669d1de30b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0a58412-9c23-41cc-9366-8f87613755f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c718161f-861a-4f63-ae14-4be1c3380b86)(content(Whitespace\" \
         \"))))(Tile((id \
         64557c49-b0e0-4227-b1d8-c6ecfe2a84d9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         22160f0a-6186-4e5a-8210-248937710018)(content(Whitespace\" \
         \"))))(Tile((id \
         9bd7f529-0b8f-445c-855d-1d0715698f47)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dacc69b2-e5f7-42f5-a996-f72fed72197c)(content(Whitespace\" \
         \"))))(Tile((id \
         f7a1bff5-32bb-4bf1-bc78-1c6f8f1aa54b)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eaf697c5-a741-4c15-8994-d4356fc54cad)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2b3b4f06-be91-4f16-b18a-a264df1a5eb5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc53c0da-a771-42a6-8c24-910aabaa4286)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb301d49-cb9b-41bb-8d05-b5bc1f756f55)(content(Whitespace\"\\n\"))))(Tile((id \
         693a21e5-c33c-43ee-80bb-2b57df0e596e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fc5bb8cf-aee5-41e9-a053-a17c94366375)(content(Whitespace\" \
         \"))))(Tile((id \
         f5ab27e1-e2a1-4f6d-a643-9210a4d69224)(label(\"\\\"blinker returns to \
         original after 2 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8584ebe-845c-4f6c-a57c-40145b558472)(content(Whitespace\"\\n\")))))((Secondary((id \
         3e28824e-b2e6-4b7d-807a-9ed8306a4787)(content(Whitespace\"\\n\"))))(Tile((id \
         36968c6e-4883-4a81-8448-9ad31e31e4fb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f8fe438-3ade-43f4-85ee-4e93a1f120b8)(content(Whitespace\" \
         \"))))(Tile((id \
         b97d7638-dc24-4acf-9c76-db5549a029f1)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         49adfd0d-f5d4-417e-96f4-a86471fa9e67)(content(Whitespace\" \
         \")))))((Secondary((id \
         57eaeedd-11c3-47b4-90b5-b96d3fbf11d3)(content(Whitespace\" \
         \"))))(Tile((id \
         5fd475f8-77db-4be7-9c01-ba516f41b181)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7418469-28e9-481a-8a03-2c280336cdd8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a410b1ee-72a4-4e56-a177-23e9c906b279)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34ccdb6d-b53b-433c-8958-88b43645bfab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1af2bd6b-22c0-47f6-aa43-4d71b83b87b5)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b6d3b53-7cd5-4db4-b4a8-395d6e07a997)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d24161d-c60a-4fb3-8220-4108963ea27b)(content(Whitespace\" \
         \"))))(Tile((id \
         11d4a498-489b-4a5c-a3cf-afc9f633910d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         61ff335c-9cc5-4854-8389-a4b54fbac8c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96f15eab-c64a-4bdf-9f43-4d257c296f98)(content(Whitespace\" \
         \"))))(Tile((id 55ad89be-a115-4ef3-9d67-4822b66b2adb)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0171f98d-3672-4ba4-910c-ec144e659725)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bcf7337b-b7d2-40a7-b8bb-52416362bca4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4700653-3e75-4916-ab7a-40a8d8525c4b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9baa639-5c60-495a-9620-bc2037d8c28e)(content(Whitespace\" \
         \"))))(Tile((id \
         b8c9d118-7847-476e-8ce7-c0e3ecac3f9b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         41f61a66-79e0-474b-9c3b-7f6daf390774)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b365b04-e9fc-4e6c-97ae-71764253bb54)(content(Whitespace\" \
         \"))))(Tile((id \
         482c2315-d1d8-4d59-8cbe-ba9e3cb9d9ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2dd931b2-d2c8-4324-9314-bdb8f8dbc412)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8dd2466d-6b54-4bd7-8943-7d0a323c369d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e73e90cb-fa5d-4397-ad32-a1ff9e20e078)(content(Whitespace\" \
         \"))))(Tile((id \
         acc0e798-1ddf-4303-9b76-18962d1de422)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e6c4306d-bbfa-42f1-b362-228fd693fbf2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d90f5fe-0d5a-4bc4-bcd6-c3dbf962b49d)(content(Whitespace\" \
         \"))))(Tile((id \
         962f6cd2-0b22-474f-a409-cd5642a11073)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         666ef02d-40a9-4864-9c44-452b7fce7a69)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         591653bb-44d6-47c2-931b-81a1972b174d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6d7824a-538f-4cbe-a8ce-f40036d76f0f)(content(Whitespace\" \
         \"))))(Tile((id \
         8f5762c5-b474-4fdb-a194-4a9ab03bcca4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         f9616b55-f09c-4f5c-a0c3-e10c40905046)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8139f724-b6a0-41de-8c96-8620f040448b)(content(Whitespace\"\\n\"))))(Tile((id \
         ae176a1b-e725-4df8-8a11-776e734f4354)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a956bad6-62a3-4c72-8bbb-577298165406)(content(Whitespace\" \
         \"))))(Tile((id \
         61402033-ea1d-411b-8725-5937c14586af)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dcbbff5d-3837-4a42-964e-445b78628a0f)(content(Whitespace\" \
         \")))))((Secondary((id \
         627f359e-4750-43bf-b2b3-b59793666169)(content(Whitespace\" \
         \"))))(Tile((id \
         65153ebf-1555-423e-a2f6-fbad9d0e2abf)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11ffca4e-0c60-469d-a4bf-b7b826f4ace0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1aad2566-e683-4c02-9560-891021131a88)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46ffd7db-a5c9-4284-9ac3-fc3096933ec5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68e89fae-917c-493c-86bf-4c66904179e4)(content(Whitespace\" \
         \"))))(Tile((id \
         0a7fb14e-bcf6-465d-b730-fc837450d30c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         602312fc-674a-4607-bc65-4826082bbc22)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         60e032b3-97ac-487c-9d26-6565ef2bc759)(content(Whitespace\"\\n\"))))(Tile((id \
         ddef4a21-0e58-4d38-97a2-d22e70fc996e)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1ef85d5-2299-4c57-8a85-fbdcca9bda72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         939f8863-fe8e-4b3b-a152-19178e20c920)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6daf60a-7f31-4ba3-a17c-1fe24a2f32ee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f33940ea-5f5c-41f6-ba51-28d11b8562cd)(content(Whitespace\" \
         \"))))(Tile((id \
         1a2f7f1a-c903-46aa-b0a6-6e2cb44adfcf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e12f1da-ef74-4940-b2ff-749b6ff0e93a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b122d81b-7bc0-4df4-ab21-2f0b6721229d)(content(Whitespace\" \
         \"))))(Tile((id \
         d2ad243a-880f-4dfb-a447-6058da122e05)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b854a28-31d9-45a6-a183-e27650ea318a)(content(Whitespace\" \
         \"))))(Tile((id \
         87cbf61f-9669-4ff2-bc8e-2136aa3a07b2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad334b15-3190-414d-8a95-0f6c73850497)(content(Whitespace\" \
         \"))))(Tile((id \
         3da04827-791c-41a1-98cc-e8c378988ef4)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         72316594-2c02-4277-ad82-8587a675cd8a)(content(Whitespace\" \
         \"))))(Tile((id \
         08658b25-678b-436a-810d-6c7a775ec10d)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57927d11-8c63-4fca-9361-7dd87f003efe)(content(Whitespace\"\\n\"))))(Tile((id \
         b43789ab-6dc3-4f18-99d5-34090d0b809b)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38f933f6-93b5-466d-a25f-4834bea097ec)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4db19670-50ca-47a6-8823-4098fbb3d0a9)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f15ffbe0-ad9d-4328-8e21-0442b8cd5ceb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6fed2907-05e0-4fd2-8ab3-f0f70e39e92a)(content(Whitespace\" \
         \"))))(Tile((id \
         e34ed754-dee0-4fbd-a642-118abc4b2f45)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b6d8276-74e0-45e9-a9a1-d1236dd640f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         290b1a82-4d1a-4b52-9c7f-e2d4e97a62f0)(content(Whitespace\" \
         \"))))(Tile((id \
         acaba5ed-17c1-4f6e-b726-3eea05d992e9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eac14c43-bd0c-4c53-86b3-f3426c1c1120)(content(Whitespace\" \
         \"))))(Tile((id \
         5cf583a7-2918-46ab-970d-e1d53c34d0eb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd5acb6d-074a-408f-8aff-cd05f3376de8)(content(Whitespace\" \
         \"))))(Tile((id \
         7e1c6751-154a-4e08-b400-0fdcedd093c5)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         288a334f-0e58-4a8b-8f2b-6edb33048ef8)(content(Whitespace\" \
         \"))))(Tile((id \
         3c389048-461e-48d2-b399-97773e4e518e)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         627b3c95-a827-4bfa-9ecd-909cd5717061)(content(Whitespace\"\\n\"))))(Tile((id \
         31025ad6-f24f-401a-beab-f93544ce4096)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         624f3519-8ea8-426b-badf-378363bd8466)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb2a8cf5-522f-4726-bc89-7ca41fe70672)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18055d6b-a4d4-4ac5-b9b1-9aec02868c61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28c51e75-13e7-40e4-b2dd-7bb8c28779cd)(content(Whitespace\" \
         \"))))(Tile((id \
         f4139cdf-8c77-4f70-b857-b3a7a8305799)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd963c36-c571-4ee7-91fd-9094807950a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa34f7a9-414c-4b35-b9a7-ea8165b58638)(content(Whitespace\" \
         \"))))(Tile((id \
         d41aa1a1-3d8a-4066-b9ad-63bb68072ef2)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3d38ecb7-582f-4962-9a6c-fef7f68a97b0)(content(Whitespace\" \
         \"))))(Tile((id \
         ac9dabd5-788f-4bbb-a073-41c9f36f6659)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d197c30b-f695-4760-8285-2b1351fb6004)(content(Whitespace\" \
         \"))))(Tile((id \
         0330d2cf-165d-4f38-8d6c-2f8765a6e18c)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1df728a0-9853-4509-a2c3-a9629766f15e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b6fa10b4-7912-4feb-abe7-9a3592178284)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3402c1f1-c6a2-49ef-b9f0-daab16b70cf8)(content(Whitespace\"\\n\"))))(Secondary((id \
         4718afaa-37f8-4596-adce-0b200d7a4726)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7a196d3-bc93-4f0d-950d-1061c43252a6)(content(Comment\"# Block: \
         stable 2x2 square #\"))))(Secondary((id \
         5a0a96d3-55e8-450c-baf0-26095e7568ed)(content(Whitespace\"\\n\"))))(Tile((id \
         ada15f92-48bb-4217-9600-46bad1574ac1)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2c69c0f2-5d80-4490-a048-a0707a50ffeb)(content(Whitespace\" \
         \"))))(Tile((id \
         cc0ae651-7de5-4000-a2b1-e05791637a51)(label(\"\\\"block is stable \
         (still life)\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b55995d8-2393-407b-89e2-1b23e044f50a)(content(Whitespace\"\\n\")))))((Secondary((id \
         b8d38f19-d946-4b43-993c-dae2399da1b8)(content(Whitespace\"\\n\"))))(Tile((id \
         5d616594-80d7-4acf-be32-79521793d0d1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         034ff3e2-231a-4763-b8dd-6673847d1537)(content(Whitespace\" \
         \"))))(Tile((id \
         2c2c4859-4f92-4325-9cad-c58ee40f6337)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         72929704-21be-4ea5-a799-780e76ff9e9f)(content(Whitespace\" \
         \")))))((Secondary((id \
         182a56d5-ec51-48b8-92eb-3ab8ee52443c)(content(Whitespace\" \
         \"))))(Tile((id \
         cdfdaf47-597e-4346-a333-67daa12fa711)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed0c7fd6-4166-4daa-ab86-0c7fbefe3362)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         333dc830-7f4a-45fb-9903-32def969f3f7)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7a9ef8b-431d-4342-a035-e620e2587995)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3710edd4-1f46-4134-b0ce-090eea35ffa2)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a17557cf-eb45-42cb-834d-2057c3e82f6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41babd54-41e1-469a-b8a0-95041647a7ef)(content(Whitespace\" \
         \"))))(Tile((id \
         d2ac8c07-513a-42a3-8c4b-16d7d6525d67)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         920cb1d4-55a2-4cc5-bfd0-42625fe48f4c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47538f23-0c74-4e97-a51a-ffe52f338b1b)(content(Whitespace\" \
         \"))))(Tile((id 1943b18c-46a9-4640-960a-e5ca2b833c30)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58973d3a-06f3-4436-aeac-81bb30ce598a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fc918456-6e93-4986-ac61-6a5ce4fd0459)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eeef2f64-afd3-4a7a-9f85-d5ec4819811e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4454fde8-c0cd-4383-b132-8a164c3ec48d)(content(Whitespace\" \
         \"))))(Tile((id \
         88b4510c-56cd-4ded-8e1b-9ae876777fa7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3291788b-cfac-47b0-bc5d-b784101c6f14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ac3ade2-4f0b-497a-a4f4-23669fa44df0)(content(Whitespace\" \
         \"))))(Tile((id \
         64503c0c-60c5-4439-b539-b2a0a85b813e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         58685edc-e18d-4001-80be-9c32d4d64943)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d0953fa-a3bb-465f-98c7-8e4ae84ec349)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         650b3aa5-70ab-4470-b3dc-1069395aa433)(content(Whitespace\" \
         \"))))(Tile((id \
         f3ed1b3f-a8ea-44b1-8501-a76bb54eec58)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         71524e07-d4e4-43f9-9512-b287bcc80454)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d024c025-9285-439c-8d98-65bdba24d476)(content(Whitespace\" \
         \"))))(Tile((id \
         cc569766-328a-4519-a4bb-8229a80eda9a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         792192ab-6f30-42e8-9f7e-2bb89420134b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f93f402-2df7-47f2-8a00-e3e8a77ef716)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cd3522b-fe66-4aa2-b158-78b30bd0fe0c)(content(Whitespace\" \
         \"))))(Tile((id \
         a6f4cbb8-5d13-4c3b-8760-5a2724825d43)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d7afb5cf-3eed-4848-9989-e5e74dbca3f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         329f09be-af0e-496d-a467-b7931dffaf83)(content(Whitespace\" \
         \"))))(Tile((id \
         84327199-15b2-48de-86a6-f51c1e21aef9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         929dc1cd-d351-458b-abef-8e3bf31fa655)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4181a873-d40b-4346-8ba9-b6d9cb7146a8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d9d8554-312c-4a82-b179-62796dd2ca2a)(content(Whitespace\" \
         \"))))(Tile((id \
         28195c9e-3f7c-4191-8f32-500810071764)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         934f7f82-a673-43ea-a25f-1fbfbef8578e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         09485fc3-44b7-4df2-958f-3f0fe60a286e)(content(Whitespace\"\\n\"))))(Tile((id \
         f11cfd18-bb28-4821-ae6c-4f98b3baf453)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         af39f538-e549-43b4-8243-4107a61993e7)(content(Whitespace\" \
         \"))))(Tile((id \
         685b8161-6523-4c54-9d34-f2c67569f6dc)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         90f2a609-a2b6-44c2-a529-33e309e07455)(content(Whitespace\" \
         \")))))((Secondary((id \
         48c4a60f-1776-4133-9ed1-bd98b4bc7ee7)(content(Whitespace\" \
         \"))))(Tile((id \
         4f18554c-f3b4-470e-b442-450b5e4b684f)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eef17031-6205-43de-97cd-77040b1397aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2e255635-7e78-40f4-b81a-4acd1bb5e92b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         84aebdfb-9306-4b3e-9d08-11f2cbbc15d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be9db327-ada4-40ac-b405-c3b6cb750821)(content(Whitespace\"\\n\"))))(Tile((id \
         8aa299fd-56ff-4734-b3d2-cf9d2797d519)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72b016d7-0221-4bde-be66-b57090278798)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5f106601-be87-402a-827d-f78c5396061e)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0cf717f9-2182-42c7-b16f-0018ef5b033a)(content(Whitespace\" \
         \"))))(Tile((id \
         608c1c25-93ae-4b29-99a5-82b43ff6d9c4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f21b5cfa-1ea8-4c64-b399-ac16a828065e)(content(Whitespace\" \
         \"))))(Tile((id \
         673eedf7-e8c0-4c6e-82f4-8a27d76efc5b)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f107047-42f2-43b7-90ee-be5a38df9fbd)(content(Whitespace\" \
         \"))))(Tile((id \
         e3962ff8-60c8-44a4-b099-8ae92d5d4c51)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcb26f21-5715-4b25-9676-26483b942b47)(content(Whitespace\"\\n\"))))(Tile((id \
         13accd91-4405-4087-a15d-be64641bcc29)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebf30435-008e-4684-83b1-4b802a7a4826)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cfc3406d-09b2-4590-82b4-53d01053f009)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f0820c4-fd30-4cf3-9441-519b01a1511d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac779e52-2bef-4da8-8424-ebbd84191aea)(content(Whitespace\" \
         \"))))(Tile((id \
         3071e8bc-9ed1-4a17-8a96-01d2a4df7ca5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc9a9fc9-5a57-4bbe-8f9f-fdc71c94bd1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45ba9ef3-8735-4917-81ab-cb78bca00e48)(content(Whitespace\" \
         \"))))(Tile((id \
         ac853ca1-ab46-48f0-b1d5-3f440980a408)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ea549cb4-347f-40d6-992e-509fc6593faf)(content(Whitespace\" \
         \"))))(Tile((id \
         55521d70-1f98-4803-bd03-fe195d257583)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f482ac1f-fd0b-4edc-8301-ab92007cbce7)(content(Whitespace\" \
         \"))))(Tile((id \
         812cdda1-b4ee-4333-9bdf-42466eee4302)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         612e886f-0ff4-4218-a252-8a348a1d9fd8)(content(Whitespace\" \
         \"))))(Tile((id \
         0d619db4-3f47-45e1-8b68-bc8e15842f2a)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96913e17-0d70-440f-8582-b44e83cb54b5)(content(Whitespace\"\\n\"))))(Tile((id \
         56e98773-d906-4796-b10f-4bbec5a67703)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67c76257-9abf-4eb4-b3ca-70b53a0c0e55)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7598c6e-3057-4ea4-830b-96568148e7c0)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         736b809f-ff9a-46c9-8969-ac1e6262bf08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77850891-f2ec-4747-92d8-3e7b48bff11d)(content(Whitespace\" \
         \"))))(Tile((id \
         0f162206-73e3-43a0-b8a8-e0f6c3e6ba61)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74e383a8-36e1-4b8d-8f61-32f04ccfb781)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24047636-6d5f-473f-b7d0-35ec201ae5d9)(content(Whitespace\" \
         \"))))(Tile((id \
         46777130-2c0f-4c86-9a0f-559104dcd214)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1ef3bd9b-5321-4d10-81fa-8349e77e6fd6)(content(Whitespace\" \
         \"))))(Tile((id \
         c22f2d6c-4c8e-4756-84f8-4b0435cdc847)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98ab95a1-a1cc-4cc9-9938-c8b6d4174fad)(content(Whitespace\" \
         \"))))(Tile((id \
         539902cd-dcc6-4f31-a2d9-46c36dd4d644)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15bdd693-0ffa-40cc-9219-97272f4ed3f8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         02d74ffd-893d-46cd-9a3c-15bd663cbf17)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         197b7ed3-e3c1-4e97-9c71-387f9fd7b10a)(content(Whitespace\"\\n\"))))(Secondary((id \
         615f6710-0ae5-4a55-8923-6bd0043dc32c)(content(Whitespace\"\\n\"))))(Tile((id \
         6916c2cc-a635-4bff-86cf-073eb20dc9bb)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         74f2d085-ffb1-48f3-8401-6255f3c22207)(content(Whitespace\" \
         \"))))(Tile((id \
         fe661553-9931-4da2-a3a0-c54b2f114287)(label(\"\\\"block remains \
         stable after 5 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f53af104-58c6-424f-88a5-afa2cb08b337)(content(Whitespace\"\\n\")))))((Secondary((id \
         303be2c4-70a0-4a17-a197-5d1d6e3da56c)(content(Whitespace\"\\n\"))))(Tile((id \
         7cde694b-ae9f-4923-9790-72da99f58895)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bb04260f-b85e-4b49-85b3-d02a883a827e)(content(Whitespace\" \
         \"))))(Tile((id \
         4db66be9-0631-415b-bef4-cf79fdf3f90b)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1d4f68fe-2e92-423d-bfa7-0f12b09ffd3c)(content(Whitespace\" \
         \")))))((Secondary((id \
         dba4da5f-0216-4ba4-91d7-a023f571cecf)(content(Whitespace\" \
         \"))))(Tile((id \
         7a3d1558-a13f-4342-b59d-bfe242708e05)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3707e9e4-fb14-4be4-bc80-8699f739194b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1806a8a0-a607-41d3-b54f-eca6a3dae0b7)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c49c2610-2d2c-46cd-a790-a47bbc719e0a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6acaa9a7-de49-44d9-8568-7596f52e6756)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e1ed923-5bd2-41ef-ba23-929ab3fe8c1d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b701b444-8040-4c14-930f-4ccca69a54ed)(content(Whitespace\" \
         \"))))(Tile((id \
         93b5b440-d8be-48aa-80ec-554268032044)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ddc092e6-c325-476e-b0e9-88922df24c01)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07ecd052-2dd9-4ce5-9d41-4073d87e3f26)(content(Whitespace\" \
         \"))))(Tile((id d8f35776-c686-4f09-b243-ff3556e49f55)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c6ff64c9-fbfe-4b09-9e20-aa01c6c9939c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         eaeafb0d-5ebe-441b-974d-4f2df0f580f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf8cbbeb-6f00-40ae-a442-9c78a0e0e76c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91f5ad1d-c4ac-4262-84ab-a0137d2a1f18)(content(Whitespace\" \
         \"))))(Tile((id \
         ac216d99-80a1-4332-9d73-e4ca770377c6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5e87326e-80df-4a84-87ff-126e5d20defa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5942a7a8-8eec-414a-8bc0-8ba873d5e041)(content(Whitespace\" \
         \"))))(Tile((id \
         0a8dff0a-1ea2-4f86-81f3-f8d539f4fa47)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8c6e1de6-c33c-43d1-8293-da3f1054b5ab)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb3a1af0-e762-40ab-8cc9-74f6182b2a02)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea5da7f9-bff3-48e7-a5c5-b64745eca970)(content(Whitespace\" \
         \"))))(Tile((id \
         cebab9cf-0d70-403e-9bac-a2a3b40d371e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f06634b0-1ac1-463d-a748-4669225fa3b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c0c30e9-1153-405e-8b45-7ca1f62ffc4c)(content(Whitespace\" \
         \"))))(Tile((id \
         baeb78b6-19ca-4c2a-9aa9-fd832282f052)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f47cecf0-ff42-4b7e-98cb-01512f743a1b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b72e0ecf-be79-4b26-8836-ac7aad9cd13c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b29dc49e-f327-41b4-a329-2c0a1338aaaf)(content(Whitespace\" \
         \"))))(Tile((id \
         f2b97fd5-ca86-44fc-9211-1d3da3df99c8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9dd8b997-ad07-4bc5-a683-61006d392e57)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13a443b5-080a-4fa2-b2de-cb3354234d57)(content(Whitespace\" \
         \"))))(Tile((id \
         95a647e9-596f-4125-a3b5-ea2ba95d00a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         86bf6603-bd69-4c20-bb72-283527c6cf73)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e476692-f31f-4939-87c9-7e647e8f7ab7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8055b862-9507-42b0-bce3-0f84f6cb9fa4)(content(Whitespace\" \
         \"))))(Tile((id \
         b1afa14f-4c5c-4173-bfda-197d1f714c42)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         31b22867-1dda-4899-b8f3-6c33b4bf072c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e304066-68b6-4a80-a11b-96e0fa1d43a8)(content(Whitespace\"\\n\"))))(Tile((id \
         b0fddfa3-269f-4e16-8523-66c1b9fafe73)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fc5d90e1-53a6-4aa5-8332-b3fa9ca2cfb9)(content(Whitespace\" \
         \"))))(Tile((id \
         d79af311-d56c-48fb-a701-5d9636f0a648)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         49a7c56f-fbbc-4231-8fa3-b4f51332f601)(content(Whitespace\" \
         \")))))((Secondary((id \
         69a20bc2-2ba0-4041-9f43-f42147d83958)(content(Whitespace\" \
         \"))))(Tile((id \
         4e6d97c5-07e0-497e-8e30-28c4f496cb79)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e31767a-5f0b-44c0-a9cb-e732184bb333)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         228d5cc0-5a65-4ebf-bdd3-f662c4fdb2ef)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7924dc5a-69e6-4f57-8863-92348ab9170b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f364246-d9fd-41b3-ad2b-5d7c8f338c72)(content(Whitespace\" \
         \"))))(Tile((id \
         c512ba85-8b40-4e9e-945a-3d0a9215512a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d5522abe-766d-42e8-a4d0-052763c6f453)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bee176f9-5ae1-40f6-bf67-001cc4632817)(content(Whitespace\"\\n\"))))(Tile((id \
         2557b313-f791-4ffa-8da3-c0a23a7837b0)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73c9b286-3c13-46f7-ab03-16a966a91017)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         349dae81-ced0-4ff1-bd60-c28d455d3cb6)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1edd635c-436a-4aa6-91b4-87427986174d)(content(Whitespace\" \
         \"))))(Tile((id \
         9407ea76-829a-44ae-8d9a-b270a5ef70af)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         747ecfc4-c518-40e7-8684-1a200cf7da34)(content(Whitespace\" \
         \"))))(Tile((id \
         1e7ff6ea-9335-41af-b86f-21d18697956c)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1c0cd67-e7c1-4c91-a38f-c0e5ad04a050)(content(Whitespace\"\\n\")))))))))(Tile((id \
         862ebc6c-77c3-41bd-8bf3-7879b6e3e512)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e06ccc36-eae1-4453-b43e-00126fe98fac)(content(Whitespace\"\\n\"))))(Secondary((id \
         549b642a-f507-4efe-a67f-7d38c18f0e2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc12a3d9-9c7f-450c-87d2-1c341796dc6d)(content(Comment\"# Single cell \
         dies #\"))))(Secondary((id \
         3913aab2-1609-4601-925d-83a46b211bb0)(content(Whitespace\"\\n\"))))(Tile((id \
         81e99c44-a7dc-4b9b-b461-9ab277dbc8c0)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         42c981d7-015f-4e60-a3f7-6a2bb3683e13)(content(Whitespace\" \
         \"))))(Tile((id \
         8db24f18-4843-42df-a4f4-c42ef1b0ecbc)(label(\"\\\"lone cell \
         dies\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         66f51a89-7de8-4c1b-ae0b-7aa3960886eb)(content(Whitespace\"\\n\")))))((Secondary((id \
         3189dc73-6367-4308-86fc-212c480c4dfa)(content(Whitespace\"\\n\"))))(Tile((id \
         334e90ff-b777-4ba7-af6b-ffae848a4daa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0265c3b2-7526-425c-9375-7c905e0b409a)(content(Whitespace\" \
         \"))))(Tile((id \
         d77dc12d-c935-4eaa-b821-536a85563f4a)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         feab639f-14e2-4a28-8476-fd1b1aa11aa2)(content(Whitespace\" \
         \")))))((Secondary((id \
         8c7b5a15-63c9-4d3b-9c18-5a147b8bbef2)(content(Whitespace\" \
         \"))))(Tile((id \
         8f49daa8-11b2-4c50-b7ab-eb3a41a608e2)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6fde68e4-08df-4562-9640-eb094c50165f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4351bedc-589c-42ed-96ea-7a9dcf94a309)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a93a97d9-75c6-4de2-91ed-c888ca0f9cdd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25c7b129-9e99-4c22-960f-ef9b1fee6b28)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06abe4b8-cce2-4937-88a4-68e4ec27c1ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47c3bd94-1576-41ac-9e6b-059a64e20eec)(content(Whitespace\" \
         \"))))(Tile((id \
         2e180cb9-bafc-4e71-95f1-cc22972d45fa)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         33421bb6-16cd-47b8-9aeb-3f982d5b17a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80b74ec8-785b-4c01-8b0d-c6c3c62dcc5c)(content(Whitespace\" \
         \"))))(Tile((id a3c04f5a-7fd2-4bfa-8e27-ea2394be59d6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87b5fa9b-e7da-44a8-8de5-4345be3838d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fdc56d51-a486-4bee-8d1c-e708b17cad12)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b0fc379-5bbe-4576-be8f-0084b8ffaa17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e4bb5837-961b-4d68-be2f-1e6e9bd51b40)(content(Whitespace\" \
         \"))))(Tile((id \
         35778f1b-ce08-457e-8787-0ba0922cc6ca)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         c38dce9f-5590-4b04-a174-423ebee9ad38)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         01e7d0b3-b33b-4c47-9ba7-ff46e54571e1)(content(Whitespace\"\\n\"))))(Tile((id \
         2f8699b8-10a7-421e-9f1b-cefa1cdfc0d2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a08e113a-47ec-454c-9f2b-00eab8421ab7)(content(Whitespace\" \
         \"))))(Tile((id \
         cb7cc9a8-71e7-486f-99a7-c90adb941879)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         05c13774-e652-4454-a6e9-51169f7bad44)(content(Whitespace\" \
         \")))))((Secondary((id \
         17675243-87ff-4038-8fc1-4d376614cadf)(content(Whitespace\" \
         \"))))(Tile((id \
         9c1841a1-aee2-4987-bcaa-e50dc31992aa)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ffa12823-89d3-4470-980b-8d3e67ff437b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         56686a33-13e0-4057-87db-8fb238c5db5c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         618b1833-b2bd-4bd9-8827-bd67a9a8aead)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8be893b9-3145-4161-bd68-266d3bc0125c)(content(Whitespace\"\\n\"))))(Tile((id \
         246ae5a4-a42c-4342-b3a7-af1a75b366de)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43973549-5404-4c47-9af9-1ff44c435421)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c9ff034-9b67-41cf-8c9c-4a3b704a5d1c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b352298-b8b1-4c09-b03e-78f6cfe672fd)(content(Whitespace\" \
         \"))))(Tile((id \
         ec9c4cbb-c74b-40b5-a2dc-83720b72ef44)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55e040ea-4190-4013-bc6c-ed6cfa12b6dd)(content(Whitespace\" \
         \"))))(Tile((id \
         f8ef8757-c609-44db-b81b-e0cc0fed16e0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a8f73d57-20f8-4bee-ac04-b65676ed7d41)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a207b69c-f92b-4786-a2b8-15397e9d72de)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         914daf9a-d27c-4658-9603-7666c9f4bc28)(content(Whitespace\"\\n\"))))(Secondary((id \
         307be687-c23b-43e8-a482-7afaa0fc08b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         75a5ecfd-1422-41ac-8374-6fc5a75f9484)(content(Comment\"# Two adjacent \
         cells die #\"))))(Secondary((id \
         bffe3852-6f4c-462b-a3cf-971b3eff4f2f)(content(Whitespace\"\\n\"))))(Tile((id \
         89fa34f3-54b4-4dbc-887a-3098ce374cb4)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         88b00f91-fa71-496d-ba89-458b4d161bc7)(content(Whitespace\" \
         \"))))(Tile((id f42d2eb6-a2a5-4020-8685-8a99bc117f20)(label(\"\\\"two \
         adjacent cells die\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         669ce338-a418-402a-9771-a03358074a0c)(content(Whitespace\"\\n\")))))((Secondary((id \
         924199bc-77da-47ae-9066-aefb54c51168)(content(Whitespace\"\\n\"))))(Tile((id \
         a2808cc6-8aa1-4231-b7fa-2303e0644cc9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a5f50e9-1658-420f-a58e-cf5d0dce8eee)(content(Whitespace\" \
         \"))))(Tile((id \
         d57db2bb-95b3-4032-a86d-8ea2dc326311)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4f5ddd08-bc68-410a-9fc7-4f0dd4c9d2e3)(content(Whitespace\" \
         \")))))((Secondary((id \
         4ec760f4-ad65-4599-b0f6-bbaeb70473ec)(content(Whitespace\" \
         \"))))(Tile((id \
         6a0067d4-f818-4a8d-9e1f-e793d9da3863)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a901a8e-6f33-4251-8d24-3fb0f78043cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b897389b-c7f7-47b4-a004-884d24f7fddd)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         816c002b-c898-4dae-a5ae-894c0b5b72d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc436439-7460-479d-b4f4-edef8f0368d3)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f8f99a5-df65-49b0-8e5e-cb6d13b3d1a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ba35447-2bb9-409b-9c67-98fba79e3aac)(content(Whitespace\" \
         \"))))(Tile((id \
         744e4c14-1bb6-41ab-adcb-5577528b53f9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         90ddb292-5e5b-458e-be1c-8d91606b0f83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b651157-2229-424c-8eed-829dbc5e6cba)(content(Whitespace\" \
         \"))))(Tile((id adfd5760-5bb5-4c97-aa4e-6e37df49865f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c7d4908c-4ebe-4c34-b076-a55273989bda)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         080db952-5f27-4533-9f05-3cbd4c7a822b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77b57b2b-8f15-408a-ba2e-37a660ff4a15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4026b99-5a0f-47b2-b58b-c0566bac0e36)(content(Whitespace\" \
         \"))))(Tile((id \
         35c9bf27-0507-4447-a50e-a6004973db6e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         10006a74-9d3a-4342-9189-709aaf1be72f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         180a0fd9-979f-43fe-9286-c958a29ebfe6)(content(Whitespace\" \
         \"))))(Tile((id \
         62b0ed4c-8cd1-4614-b6ec-995a222a5924)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7fa49f11-5a91-4ad4-b842-91a69a49c2c2)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d3a8434-8ade-4a5e-955b-51a4c8174003)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc8b8986-2128-4330-bf23-92f20fd51199)(content(Whitespace\" \
         \"))))(Tile((id \
         5a2426d3-cb23-4c7f-96e8-671c15667bd9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         fae269b7-1f7e-4021-b329-c02ac6af1805)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6250de4d-bc24-4fae-8eab-150db684cb18)(content(Whitespace\"\\n\"))))(Tile((id \
         f1783e1c-615b-47a3-943c-7e13222868ce)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e83c18a2-3e33-4e53-bd4d-83af8024b9a4)(content(Whitespace\" \
         \"))))(Tile((id \
         12915f84-f9d4-421a-9ee2-fb4135adf73e)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         20c5f70e-e14b-4f7d-a508-6997e6a8da90)(content(Whitespace\" \
         \")))))((Secondary((id \
         bdb7917e-a76b-4ce0-863b-a315742d131b)(content(Whitespace\" \
         \"))))(Tile((id \
         b136e97a-c99a-4945-8ef6-1b7c9b26f70d)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53d21d82-f1f6-4eb0-8edf-c2ca301ffb6d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b10c9cd2-9990-4b50-bffc-36f1b1b9a759)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9d4d3113-0587-40d0-bd78-0ec58ec2f6de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ffab90ad-5c37-414e-a688-f3fb945b864b)(content(Whitespace\"\\n\"))))(Tile((id \
         e13798d2-22ba-4c86-ba0e-eea07536b6b7)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f46d3c4-b009-4527-a6bb-2a25301a1269)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         30bc6e3b-b9ec-4fc9-a6ba-d9ab04ced80c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0e52c13c-5f85-4037-aee1-b6c44c306e07)(content(Whitespace\" \
         \"))))(Tile((id \
         008146ba-350e-4620-aaba-eb730d11657d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dff782e8-f56e-4a91-85db-7526a7c58f2c)(content(Whitespace\" \
         \"))))(Tile((id \
         827f053e-549d-43e3-a150-89d50200489f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         abafdefd-083b-4340-b3bc-3b399c1eda43)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2a9971af-60a4-47c5-b109-62c013fd4db2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31d97e66-b432-4c31-aed9-93bd1608f0d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         9cce113d-0e30-46c8-8fd9-2b198730fc8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4feb504f-c46e-4131-a56f-b9ba08e3aad3)(content(Comment\"# Simultaneous \
         update test #\"))))(Secondary((id \
         4d4d388e-db65-4fb6-9a30-7ffbcbefc2d7)(content(Whitespace\"\\n\"))))(Tile((id \
         64af445b-3b88-4067-abd2-36ff3a271881)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e89600a8-ff01-44d7-96b8-1386720ef3ed)(content(Whitespace\" \
         \"))))(Tile((id \
         91e98d8b-56ad-4030-93ec-41f7560e61ef)(label(\"\\\"updates are \
         simultaneous not sequential\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0865165-d9a7-4a92-bf8a-605982f23ca0)(content(Whitespace\"\\n\")))))((Secondary((id \
         259131cd-481e-4910-bd26-b291d5ce7931)(content(Whitespace\"\\n\"))))(Tile((id \
         203cc536-c1e0-4132-8cda-5e5b514fa03e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5cd730b3-a1a5-4f68-ac9a-3cfec1169403)(content(Whitespace\" \
         \"))))(Tile((id \
         749d61cd-fab4-406c-ac25-414d451bb3da)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e7683adc-a330-466b-81c3-00798149c433)(content(Whitespace\" \
         \")))))((Secondary((id \
         4a275036-3c49-4e70-8276-78c00b353455)(content(Whitespace\" \
         \"))))(Tile((id \
         dae599be-0b5c-43d0-b967-1faa7d912278)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d3b1238-b845-4417-84f8-c9b0be57217c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5053c739-5a92-41bf-b0b3-517671516192)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb557212-9d0e-4e02-b6a4-f27f11ed94e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0bdf9164-324d-46eb-b653-5569b4f73219)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cdc66075-1514-47c1-bdc6-c5efe66c9e7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37e18190-bed6-464e-bc61-35eb42696378)(content(Whitespace\" \
         \"))))(Tile((id \
         5e8f8aba-5707-4f35-8b95-ae95f075e409)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0c472b10-c881-4e29-9731-0654073dce92)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1c7b18d-d7c1-47b6-8796-95706bf27421)(content(Whitespace\" \
         \"))))(Tile((id f83e7731-2dca-4fc8-9215-0d5636124219)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0dac0b8e-7c26-494b-ac26-f9bf1638363a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7d80fdc7-0597-4f57-a0a4-04190af00e3b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         824cbff4-368a-4326-9353-d7fd9e2443fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f10b3c04-e3e0-4fac-a466-b055f5f63a14)(content(Whitespace\" \
         \"))))(Tile((id \
         414dd19e-d74c-41dd-9039-ef20fdc1aa6a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         20268bbc-fc7f-42d1-a5e8-5eb3027fde72)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3efc3e5-1f57-4aee-b6e2-0c109e59852b)(content(Whitespace\" \
         \"))))(Tile((id \
         e5b3fc69-0663-40d3-a56d-ebf1f90707cd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         87ca9020-66bc-4fea-9715-6115819b60b3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d165798a-0867-4ca3-a8af-1bff4a281af5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         138c5065-39cd-406f-ad39-a8257877a4de)(content(Whitespace\" \
         \"))))(Tile((id \
         886b3842-9881-4cd5-8a30-6069debec4a9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0d3d168d-827f-46dd-859b-6087e8c9d475)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4306a7e3-286e-4a5e-b254-ea62a91a9ee9)(content(Whitespace\" \
         \"))))(Tile((id \
         69208976-b60a-4d03-bffa-8dbfd6879c51)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         37a29e5d-e335-4a64-8640-19ac43e27401)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26bb82d3-55a9-4446-a5cf-ac653e01c5e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         088cffb9-31ad-4111-a21c-7fbe957ac981)(content(Whitespace\" \
         \"))))(Tile((id \
         b6782fdd-a524-4de4-a07b-43003776f9ca)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2d4a5f68-62a1-47e4-8be9-1d981a4d7ff4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3239b418-20c6-4dfe-9746-ab29e0ba67e9)(content(Whitespace\"\\n\"))))(Tile((id \
         9540a01d-7eef-4ad6-90ed-1175fa092ef0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9cff8c6e-018a-471e-8bbf-68b91293b41e)(content(Whitespace\" \
         \"))))(Tile((id \
         ad6dc980-a53c-4bef-bad8-46f5f9547b80)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f76838d5-0954-49b3-bcfe-a72b0c6cd925)(content(Whitespace\" \
         \")))))((Secondary((id \
         0d780aa5-7a2e-4a6a-97cd-9ca3470198c7)(content(Whitespace\" \
         \"))))(Tile((id \
         1e8f5183-a1c3-4cd3-8680-d44c97d8deca)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         262e5524-f177-4549-8d65-ef9b5fb75b24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da0a3f86-3dda-4599-a43e-85eade4bce40)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0379dd8d-e22a-4ba3-94d5-b7f40d885d05)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         23f44321-a5d9-470f-9411-8867ccf60514)(content(Whitespace\"\\n\"))))(Tile((id \
         65db36ea-b2cb-4a6b-a391-ed10f6965012)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2583643a-8432-49ef-8a2f-299bcdd0889f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         328dfbf2-08a6-4d78-99c3-536592730112)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adb34ef0-5244-4fac-8c03-e178c87db941)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26f3c058-204f-4cbc-8326-f4596abd853f)(content(Whitespace\" \
         \"))))(Tile((id \
         3b3af175-58d0-43e1-b660-3cee07eb81d8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfa3c182-e408-47bd-a023-4c15b421b5b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f29d78fb-0d46-4d50-b6e3-31e5ea2e0f45)(content(Whitespace\" \
         \"))))(Tile((id \
         bc1277db-9e50-4d9a-9267-b73a5ea80433)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7270e563-7949-49d1-b649-0f8a6ddae1da)(content(Whitespace\" \
         \"))))(Tile((id \
         83b96dc7-648e-408d-a3e4-4c58d19bf262)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47060443-f53b-4f4d-8ca6-a07a81f60f30)(content(Whitespace\" \
         \"))))(Tile((id \
         4d4f520d-d1d9-449d-9d16-0294a5a4ac2a)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ccf33bb-85e0-4f95-8f9a-89d1a66a1c9f)(content(Whitespace\" \
         \"))))(Tile((id \
         bc6bd8f2-136b-4af9-aa94-21fbbfd6b65f)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27d30618-8fdd-490d-a085-9076ba78e386)(content(Whitespace\"\\n\"))))(Tile((id \
         1eed0fa5-cb96-4c28-af1a-55a86454be2c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96096920-7e57-4462-b005-20d2e9aff8e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         74d0eff7-bfbb-4b2b-b0a5-c7222b384f02)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9a50015-a981-477d-a07e-50be0d558741)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b7e3025-dafb-41b8-b925-6b4dde027278)(content(Whitespace\" \
         \"))))(Tile((id \
         eed26f79-6335-4f83-ab18-2d9026e4f4ba)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74fd3d65-c842-4c38-9a18-d7b6c460dc18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73aaddea-ecc6-45d4-9f5f-1598dd67d866)(content(Whitespace\" \
         \"))))(Tile((id \
         a3f8795e-4290-4bd6-b02d-e3947e3b6994)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         96f4b964-3113-4abd-8d80-84e6c772d55f)(content(Whitespace\" \
         \"))))(Tile((id \
         36f8611f-d5b0-4af2-8a15-363db1c9928f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7fb3903-3d63-4fa7-a87b-4fe8c633886b)(content(Whitespace\" \
         \"))))(Tile((id \
         50741ac0-a929-4c4e-b981-5ee4cc8cf74d)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e71c09fc-b9b5-47a5-8b3e-d37cd9858f87)(content(Whitespace\"\\n\")))))))))(Tile((id \
         21012849-868f-4b90-aa4d-4887c6ad6413)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e24da0f-4cad-4410-80a2-aa4537e4a589)(content(Whitespace\"\\n\"))))(Secondary((id \
         45e1e110-4a29-4bb2-b6b6-8dd9cb6d90e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         687f91f5-ea2f-4f30-9deb-cca10f3e9ec9)(content(Comment\"# Edge \
         behavior #\"))))(Secondary((id \
         b8d5d163-9f66-4b93-b3ba-dbe11887d637)(content(Whitespace\"\\n\"))))(Tile((id \
         4573333f-7847-458f-97ca-668360c8b80a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13b7e197-fccf-4c70-952e-8c9aa7b1dae3)(content(Whitespace\" \
         \"))))(Tile((id \
         eae34c39-9898-4828-ba26-7cd06b672a5b)(label(\"\\\"edge cells count \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eecd24fb-e7f0-49b8-9065-6e1ccc1228e6)(content(Whitespace\"\\n\")))))((Secondary((id \
         3b2b09dc-07aa-4e54-85f0-50c4bdafc77d)(content(Whitespace\"\\n\"))))(Tile((id \
         a8fe5eda-d87e-476e-932e-5944fedf9047)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21b1a14a-7fbe-423f-801a-30ed9b0df84e)(content(Whitespace\" \
         \"))))(Tile((id \
         490dfb4c-3223-4cef-968a-ad7c2d025b6d)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a8d55857-d566-4079-9b3e-0406df5527c9)(content(Whitespace\" \
         \")))))((Secondary((id \
         2ef7b26c-5434-465b-829b-a8a61a874876)(content(Whitespace\" \
         \"))))(Tile((id \
         80ff50bd-8cef-46d4-91bc-c59900f78ba5)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7c613bf-ce7c-43b0-87e9-6f00325ecf30)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5076f321-f931-4092-9be9-2d2c28ca27ad)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e844c953-0cb8-4c1f-b5f4-95f7c00d2a9a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         784ab8ec-4ac2-4e1a-a683-41ab53224059)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8850bbf5-7004-49b2-a77b-da91cd5af82e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8738a5db-89e0-4b35-a881-b48a881976f5)(content(Whitespace\" \
         \"))))(Tile((id \
         8c21bea3-9a82-4dd2-b160-ae9a3e6869b2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c530e497-bad2-484d-9dc7-58c077f77106)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc5dd137-9cd6-49d2-903f-8326687221fa)(content(Whitespace\" \
         \"))))(Tile((id 023e72bf-4f42-4682-b31d-41988fe56caf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0622125b-08af-4844-9e17-4466ab485c6d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bfa58b87-0ce5-4dce-8d3b-06ac3f0d1fec)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecba4830-a8a1-405c-b862-ea79b85ee212)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82eb0fea-b293-464f-8303-9d44abf0e870)(content(Whitespace\" \
         \"))))(Tile((id \
         7be48775-f857-426d-b1d4-aa1009e8b36b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6450aeb1-fd25-4f29-be1f-a7e4f47e8a64)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8128d8f-ab85-4be4-9f1a-216d20ef8449)(content(Whitespace\" \
         \"))))(Tile((id \
         a4118701-bc23-475c-a831-b5af2e4eb340)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cca16b70-542c-4d26-9222-c019138dbf11)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f7c17d5-419d-4392-84b9-0314b9ca3861)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1eff6379-dc21-499a-9306-3e53fcb7b4e7)(content(Whitespace\" \
         \"))))(Tile((id \
         265bb5a4-4acf-46af-9304-76376e9d2ba3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         da29dc89-b08e-4724-a3ae-e254ef050c96)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75b83c34-c509-4b6a-822a-2ac12793ef73)(content(Whitespace\" \
         \"))))(Tile((id \
         8960d020-2102-4377-899f-722041c58f26)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         25a1e477-d943-43f8-a764-8740cf63e95f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1f49f1a-a8e4-42df-bada-8a7efacdc81f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbb08884-d3f3-4deb-83c4-3209df6f8b36)(content(Whitespace\" \
         \"))))(Tile((id \
         a0e68020-6d21-4f7e-b5db-6da79abecf23)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         7b4897dc-835b-4b19-b372-5f6e4232d7fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5cd68958-5bc4-4ca6-a070-b62b8817aef6)(content(Whitespace\"\\n\"))))(Tile((id \
         ecbae15f-68a4-44d7-b824-c944f5eb0cbb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c40ea6e9-d4e0-446d-9f04-bb0767453ed4)(content(Whitespace\" \
         \"))))(Tile((id \
         594cfcb8-d02b-4c43-a315-0083313b8ea6)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         313216e7-a02d-4f8e-804f-dfc9f8ae1f4c)(content(Whitespace\" \
         \")))))((Secondary((id \
         e1b224f8-738a-4603-a9da-9e5c987e6492)(content(Whitespace\" \
         \"))))(Tile((id \
         ac3b30ee-1bd0-44b9-a2cd-9aff39932b2e)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4303d536-5f94-42d6-a0da-3bec4195b9f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72460d4f-d948-4535-93bd-d3a505000f50)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7d752e1a-5e28-42f2-8a86-80646d598866)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         19d0076e-dbb5-48cc-8c39-af92de6639c1)(content(Whitespace\"\\n\"))))(Tile((id \
         8936879a-5f11-4079-8f4f-d6678001049c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         12f25f3f-f254-4ec3-94d6-16ba4acf4c22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6635c742-1f46-43fd-86e5-9a6c7357e64c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01ac6bad-1f81-48b6-887b-f0b163aea2dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e3e2500-8f9d-4452-bb03-e24f9b63bca4)(content(Whitespace\" \
         \"))))(Tile((id \
         f6a735c1-acd3-415a-9d05-456651ff853c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         257d2d42-632f-4a2b-a878-26fa552885ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20a428d8-c7e8-4a52-9ced-80136e8f3a98)(content(Whitespace\" \
         \"))))(Tile((id \
         9fc3981e-65e0-4c6b-a9ad-f6e007ea7be4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3218867b-6bf5-4d83-a6dd-20e41394ab67)(content(Whitespace\" \
         \"))))(Tile((id \
         a3285519-836f-4eeb-aa74-8b964169f9a6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6c04266-4658-4255-8540-1e4d2922cc36)(content(Whitespace\" \
         \"))))(Tile((id \
         a78226c0-67c4-4267-831d-7ab90fae071f)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7fb0281b-c69d-448f-af4b-cf4c6781bc52)(content(Whitespace\" \
         \"))))(Tile((id \
         3fc0c54e-432e-4315-8dc2-7bec49ed549a)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbfbc30e-0fdc-48b2-a7ca-c3c5a852b229)(content(Whitespace\"\\n\"))))(Tile((id \
         6076314f-6f9a-4d33-93fe-b88d646f40e2)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ece7245-b248-4c56-ab3c-baa89675255e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34a6eacd-5125-4b6e-ae96-23ef4b0f43d8)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46db9709-d0a2-48d9-a171-2b6936946412)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3351365-e9b4-4ee7-979a-2a43ed202870)(content(Whitespace\" \
         \"))))(Tile((id \
         3e73a545-cda5-415a-a030-2c464582bf06)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc67e256-ce13-45c2-9a9f-fe85161542dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6535c433-aae7-42c7-a71c-2e055eb439bd)(content(Whitespace\" \
         \"))))(Tile((id \
         4a1d78b1-83c2-41ed-b04a-4f67bf5340f6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d671c066-42f9-413e-945a-76b78e351abc)(content(Whitespace\" \
         \"))))(Tile((id \
         cebef397-f732-4366-a4f3-00d797e3bfbc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d39deb0-f42b-4a28-a2ea-19424843cb22)(content(Whitespace\" \
         \"))))(Tile((id \
         837e0e8f-65fd-4d84-b450-ac196c04db29)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         13aa4632-ea5e-4faf-9e59-b7b9067a8be3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ea568c4c-2719-4a08-9f89-ee3dc22cec1f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b1349b2-b844-4a4c-8740-05c337c6030d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d715fb56-0df4-4501-8bf8-a4a373000627)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4c5e9dd-a948-4b4b-8d4d-9437d7cd4db4)(content(Comment\"# Demo: \
         Blinker evolution #\"))))(Secondary((id \
         745ab81b-b7fc-4198-992e-5a30e1800e75)(content(Whitespace\"\\n\"))))(Tile((id \
         3853c9c1-d253-45ba-a4ad-f717812a2cff)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3db915a2-d6bc-4c89-b3df-fc7697f987cd)(content(Whitespace\" \
         \"))))(Tile((id \
         e4164c58-69a4-4530-a961-0e7e14844062)(label(blinker))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8757b8a3-c7d8-4725-9b5a-b1a5912a41ff)(content(Whitespace\" \
         \")))))((Secondary((id \
         b43a1808-cc92-4c0a-acbe-0d7c67b1590d)(content(Whitespace\" \
         \"))))(Tile((id \
         a3dec21f-8f65-4f7b-9b9d-2a2fbe66888a)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80c10bde-f052-46c2-81a3-204af3de39ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         994cc839-3583-43e9-b37c-829a7f900e34)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8e55afa-a383-4b61-93ba-c64045b1a3b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7289e15-254d-4d57-bc11-212edfc2cb3a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5661d4ac-d445-4ea3-98b0-2f1658f81bcd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63213502-bcd0-4bcb-8daa-4a6298cb5aee)(content(Whitespace\" \
         \"))))(Tile((id \
         eb030e87-6adf-4373-9afa-06d9ca72a3f6)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         89380fe8-e889-49bc-8da3-ec10aa471044)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7bf921b7-de5d-4abe-8301-0cec2c190a17)(content(Whitespace\" \
         \"))))(Tile((id 7e297493-6494-49ce-b7e9-eb3b11a0e435)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55d06ad1-0b6f-4218-a0cb-b35d722bd13e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6dc09d19-c3a2-497e-97d0-e34a60e05fc8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f02a2d91-d564-49b0-bef7-479e42372913)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5496367d-ac51-4760-bc4b-7ecfee3abd1b)(content(Whitespace\" \
         \"))))(Tile((id \
         8e59f775-2c2e-429f-ab3c-a96754276629)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d1b6d3a7-1e01-43de-b1b4-96e70b976409)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e180a68-9a2b-4d02-b08e-634708be753f)(content(Whitespace\" \
         \"))))(Tile((id \
         b1c4f290-a710-4aff-9b7c-357feb161154)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f1a2578d-c84f-46bd-a9f8-5d812840452d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecebca8f-aba9-4d6f-b6c6-2559ff9cd1f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35832129-f613-41c6-aa15-d4f677d15958)(content(Whitespace\" \
         \"))))(Tile((id \
         45ed91b3-76b1-459d-8867-aa22c6761709)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f95de04b-827d-44e1-b19c-5b3c7fe77ecb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f2de1f3-26c6-4143-b58c-6c0c87a3e912)(content(Whitespace\" \
         \"))))(Tile((id \
         5ed5dde1-2b05-4754-8884-4a557e698bf4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e905b4b3-61e1-468d-b577-c960130338ec)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc766a8c-4145-4b24-97f8-c3473a7dceec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01bd4cd1-687a-4049-af77-19877a92bfdb)(content(Whitespace\" \
         \"))))(Tile((id \
         6a385843-fa79-4025-a9d6-5ae5bbc06f84)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         577e0854-b361-4cf2-9da7-937fb45c7f7b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0c555127-2b6e-444d-a1dd-6399dfa1519b)(content(Whitespace\"\\n\"))))(Tile((id \
         780e5bc6-fe41-4c90-9835-340dc4753e54)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b8f36646-25c7-466a-9074-f9fcdff15556)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea80e708-8155-4d05-8407-93ddb1e94a02)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24279e0a-ae41-4947-9a3f-5b2ad3b808e3)(content(Whitespace\" \
         \"))))(Tile((id \
         c5afdabc-b671-49eb-9ab7-144916f9d173)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c353e9b-76b5-40aa-88ad-a68ce7c88bc6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         486177cc-73fe-4895-88a2-9017d4dc87c5)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e3173b3f-0cb9-4e45-9e7e-7ac68c4f5b12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07c31729-8198-44b5-a6f0-bdfbfe2858ae)(content(Whitespace\" \
         \"))))(Tile((id \
         26484b9a-2c16-4b87-9d43-eee304ddaf16)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eae03aa1-4478-467e-9815-cb896a10d828)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6d089be0-d834-484b-87f9-85aede3ef05f)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7bf8ba5-1e4b-4bb0-8a71-b6258222c31d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ede8a995-23b0-4717-89c7-f6146b859c6b)(content(Whitespace\" \
         \"))))(Tile((id \
         7dd5d86f-011e-466a-ae23-08eb764067aa)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4e84ecdd-7595-40ee-b717-e337dfe81d84)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# CONWAY'S GAME OF LIFE #\n\
         # Cellular automaton with birth/death rules #\n\n\
         type Cell = + Dead + Alive in\n\n\
         # Grid is a flat list with width/height metadata #\n\
         type Grid = (\n\
         cells = [Cell],\n\
         width = Int,\n\
         height = Int\n\
         ) in\n\n\
         # Create empty grid #\n\
         let makeGrid : (Int, Int) -> Grid =\n\
         fun (w, h) ->\n\
         (\n\
         cells = map(range(0, w * h - 1), fun _ -> Dead),\n\
         width = w,\n\
         height = h\n\
         )\n\
         in\n\n\
         # Convert (x, y) to index #\n\
         let toIndex : (Grid, Int, Int) -> Int =\n\
         fun (g, x, y) ->\n\
         y * g.width + x\n\
         in\n\n\
         # Check if coords are in bounds #\n\
         let inBounds : (Grid, Int, Int) -> Bool =\n\
         fun (g, x, y) ->\n\
         x >= 0 && x < g.width && y >= 0 && y < g.height\n\
         in\n\n\
         # Get cell at (x, y), returns Dead if out of bounds #\n\
         let getCell : (Grid, Int, Int) -> Cell =\n\
         fun (g, x, y) ->\n\
         if inBounds(g, x, y)\n\
         then nth(g.cells, toIndex(g, x, y))\n\
         else Dead\n\
         in\n\n\
         # Set cell at (x, y) #\n\
         let setCell : (Grid, Int, Int, Cell) -> Grid =\n\
         fun (g, x, y, cell) ->\n\
         let idx = toIndex(g, x, y) in\n\
         (\n\
         cells = mapi(g.cells, fun (i, c) -> if i == idx then cell else c),\n\
         width = g.width,\n\
         height = g.height\n\
         )\n\
         in\n\n\
         # Count alive neighbors for cell at (x, y) #\n\
         let countNeighbors : (Grid, Int, Int) -> Int =\n\
         fun (g, x, y) ->\n\
         let neighbors = [\n\
         getCell(g, x - 1, y - 1),\n\
         getCell(g, x,     y - 1),\n\
         getCell(g, x + 1, y - 1),\n\
         getCell(g, x - 1, y),\n\
         getCell(g, x + 1, y),\n\
         getCell(g, x - 1, y + 1),\n\
         getCell(g, x,     y + 1),\n\
         getCell(g, x + 1, y + 1)\n\
         ] in\n\
         length(filter(neighbors, fun c -> c == Alive))\n\
         in\n\n\
         # Apply Game of Life rules to a single cell #\n\
         let nextCellState : (Cell, Int) -> Cell =\n\
         fun (current, neighbors) ->\n\
         case current\n\
         | Alive =>\n\
         if neighbors == 2 || neighbors == 3\n\
         then Alive\n\
         else Dead\n\
         | Dead =>\n\
         if neighbors == 3\n\
         then Alive\n\
         else Dead\n\
         end\n\
         in\n\n\
         # Step the entire grid (simultaneous update) #\n\
         let step : Grid -> Grid =\n\
         fun g ->\n\
         let newCells = mapi(g.cells, fun (idx, _) ->\n\
         let x = idx - (idx / g.width) * g.width in\n\
         let y = idx / g.width in\n\
         let current = getCell(g, x, y) in\n\
         let neighbors = countNeighbors(g, x, y) in\n\
         nextCellState(current, neighbors)\n\
         ) in\n\
         (cells = newCells, width = g.width, height = g.height)\n\
         in\n\n\
         # Run n steps #\n\
         let run : (Grid, Int) -> Grid =\n\
         fun (g, n) ->\n\
         if n <= 0 then g\n\
         else fold_left(range(1, n), fun (grid, _) -> step(grid), g)\n\
         in\n\n\
         # Helper: set multiple cells alive #\n\
         let setAlive : (Grid, [(Int, Int)]) -> Grid =\n\
         fun (g, coords) ->\n\
         fold_left(coords, fun (grid, xy) ->\n\
         let (x, y) = xy in\n\
         setCell(grid, x, y, Alive)\n\
         , g)\n\
         in\n\n\
         # Count total alive cells #\n\
         let countAlive : Grid -> Int =\n\
         fun g ->\n\
         length(filter(g.cells, fun c -> c == Alive))\n\
         in\n\n\
         # ===== TESTS ===== #\n\n\
         # Basic grid operations #\n\
         hint \"empty grid has all dead cells\"\n\
         test\n\
         let g = makeGrid(3, 3) in\n\
         countAlive(g) == 0\n\
         end;\n\n\
         hint \"can set and get cell\"\n\
         test\n\
         let g = setCell(makeGrid(3, 3), 1, 1, Alive) in\n\
         getCell(g, 1, 1) == Alive\n\
         end;\n\n\
         hint \"out of bounds returns Dead\"\n\
         test\n\
         let g = makeGrid(3, 3) in\n\
         getCell(g, -1, 0) == Dead && getCell(g, 5, 5) == Dead\n\
         end;\n\n\
         # Neighbor counting #\n\
         hint \"isolated cell has 0 neighbors\"\n\
         test\n\
         let g = setAlive(makeGrid(5, 5), [(2, 2)]) in\n\
         countNeighbors(g, 2, 2) == 0\n\
         end;\n\n\
         hint \"cell with one neighbor counts correctly\"\n\
         test\n\
         let g = setAlive(makeGrid(5, 5), [(2, 2), (2, 3)]) in\n\
         countNeighbors(g, 2, 2) == 1\n\
         end;\n\n\
         hint \"corner cell counts neighbors correctly\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(0, 0), (1, 0), (0, 1)]) in\n\
         countNeighbors(g, 0, 0) == 2\n\
         end;\n\n\
         hint \"cell with 8 neighbors\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [\n\
         (0, 0), (1, 0), (2, 0),\n\
         (0, 1),         (2, 1),\n\
         (0, 2), (1, 2), (2, 2)\n\
         ]) in\n\
         countNeighbors(g, 1, 1) == 8\n\
         end;\n\n\
         # Cell state rules #\n\
         hint \"alive cell with 2 neighbors survives\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(0, 1), (1, 1), (2, 1)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Alive\n\
         end;\n\n\
         hint \"alive cell with 3 neighbors survives\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [\n\
         (1, 0),\n\
         (0, 1), (1, 1), (2, 1)\n\
         ]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Alive\n\
         end;\n\n\
         hint \"alive cell with 1 neighbor dies (underpopulation)\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(1, 1), (1, 0)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Dead\n\
         end;\n\n\
         hint \"alive cell with 4 neighbors dies (overpopulation)\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [\n\
         (1, 0),\n\
         (0, 1), (1, 1), (2, 1),\n\
         (1, 2)\n\
         ]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Dead\n\
         end;\n\n\
         hint \"dead cell with 3 neighbors becomes alive (birth)\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(0, 0), (1, 0), (0, 1)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Alive\n\
         end;\n\n\
         hint \"dead cell with 2 neighbors stays dead\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(0, 0), (1, 0)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 1) == Dead\n\
         end;\n\n\
         # Classic patterns #\n\n\
         # Blinker: oscillates between horizontal and vertical #\n\
         hint \"blinker oscillates (horizontal to vertical)\"\n\
         test\n\
         let g = setAlive(makeGrid(5, 5), [(1, 2), (2, 2), (3, 2)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 2, 1) == Alive &&\n\
         getCell(g2, 2, 2) == Alive &&\n\
         getCell(g2, 2, 3) == Alive\n\
         end;\n\n\
         hint \"blinker returns to original after 2 steps\"\n\
         test\n\
         let g = setAlive(makeGrid(5, 5), [(1, 2), (2, 2), (3, 2)]) in\n\
         let g2 = run(g, 2) in\n\
         getCell(g2, 1, 2) == Alive &&\n\
         getCell(g2, 2, 2) == Alive &&\n\
         getCell(g2, 3, 2) == Alive\n\
         end;\n\n\
         # Block: stable 2x2 square #\n\
         hint \"block is stable (still life)\"\n\
         test\n\
         let g = setAlive(makeGrid(4, 4), [(1, 1), (2, 1), (1, 2), (2, 2)]) in\n\
         let g2 = step(g) in\n\
         countAlive(g2) == 4 &&\n\
         getCell(g2, 1, 1) == Alive &&\n\
         getCell(g2, 2, 2) == Alive\n\
         end;\n\n\
         hint \"block remains stable after 5 steps\"\n\
         test\n\
         let g = setAlive(makeGrid(4, 4), [(1, 1), (2, 1), (1, 2), (2, 2)]) in\n\
         let g2 = run(g, 5) in\n\
         countAlive(g2) == 4\n\
         end;\n\n\
         # Single cell dies #\n\
         hint \"lone cell dies\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(1, 1)]) in\n\
         let g2 = step(g) in\n\
         countAlive(g2) == 0\n\
         end;\n\n\
         # Two adjacent cells die #\n\
         hint \"two adjacent cells die\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(1, 1), (2, 1)]) in\n\
         let g2 = step(g) in\n\
         countAlive(g2) == 0\n\
         end;\n\n\
         # Simultaneous update test #\n\
         hint \"updates are simultaneous not sequential\"\n\
         test\n\
         let g = setAlive(makeGrid(5, 5), [(1, 2), (2, 2), (3, 2)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 2) == Dead &&\n\
         getCell(g2, 3, 2) == Dead\n\
         end;\n\n\
         # Edge behavior #\n\
         hint \"edge cells count neighbors correctly\"\n\
         test\n\
         let g = setAlive(makeGrid(3, 3), [(0, 0), (1, 0), (2, 0)]) in\n\
         let g2 = step(g) in\n\
         getCell(g2, 1, 0) == Alive &&\n\
         getCell(g2, 1, 1) == Alive\n\
         end;\n\n\
         # Demo: Blinker evolution #\n\
         let blinker = setAlive(makeGrid(5, 5), [(1, 2), (2, 2), (3, 2)]) in\n\
         (blinker, step(blinker), run(blinker, 2))\n";
      refractors = "()";
    } )
