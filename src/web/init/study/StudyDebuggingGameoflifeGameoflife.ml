let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / debugging / gameoflife / gameoflife",
    {
      segment =
        "((Secondary((id \
         ccb6bba1-c9cb-4712-a2f8-f3bc0490a0fb)(content(Comment\"# CONWAY'S \
         GAME OF LIFE #\"))))(Secondary((id \
         d5959b1d-9d64-46c4-bc4d-ba878121eeab)(content(Whitespace\"\\n\"))))(Secondary((id \
         edd97700-3ae3-414d-bfb7-25f5b093a90f)(content(Comment\"# Cellular \
         automaton with birth/death rules #\"))))(Secondary((id \
         dcee8855-7754-4a34-b246-c632a03a0340)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e457ba8-995e-41d5-ba14-ee3dfa21561d)(content(Whitespace\"\\n\"))))(Tile((id \
         efd4ddd0-4414-446b-8b12-45f07a84e1bc)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be2b3fda-813e-44fe-b733-4080f8c7d902)(content(Whitespace\" \
         \"))))(Tile((id \
         ce19bc8a-231c-411c-9ce8-a92b9b593298)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ebf5f036-9f86-4c52-836f-f52493a3a040)(content(Whitespace\" \
         \")))))((Secondary((id \
         9528f63a-d772-485d-8cd7-b17992557acb)(content(Whitespace\" \
         \"))))(Tile((id \
         709e4697-566b-4fd5-8f27-857c1329fb1e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2ac404b1-b4d1-4a17-9ebd-b17204d23702)(content(Whitespace\" \
         \"))))(Tile((id \
         8079efe0-89df-4966-8936-2be5f87083b0)(label(Dead))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a13c3af2-acff-45bc-97e5-19c59a3b21a2)(content(Whitespace\" \
         \"))))(Tile((id \
         63a5c6a5-0269-402a-ab48-cfa7d7b7863d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         add6247c-664e-493e-9398-4e1a5d51192d)(content(Whitespace\" \
         \"))))(Tile((id \
         20da5838-ac95-4dab-83a1-ab0de0bc0c9a)(label(Alive))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8013e0f8-213e-4d93-87d8-ea751c2049c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43f82fa5-479e-41c8-b5af-77ef21dee955)(content(Whitespace\"\\n\"))))(Secondary((id \
         07db5024-c83c-4ea4-ba79-0662107943e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         18832df3-d6b6-4292-88b7-1f1b2e41f6fc)(content(Comment\"# Grid is a \
         flat list with width/height metadata #\"))))(Secondary((id \
         afc01a40-ed3f-487f-88e5-4220914e7a3b)(content(Whitespace\"\\n\"))))(Tile((id \
         1c1684ed-e4f2-4d77-b610-4dd0e8921104)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a390a5b-4957-4a14-9935-3064d9ac9b08)(content(Whitespace\" \
         \"))))(Tile((id \
         08134691-54a5-41e3-9d37-8ea479b0c883)(label(Grid))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4ed46b69-b60a-4f02-a24b-7e1e1fab6e12)(content(Whitespace\" \
         \")))))((Secondary((id \
         615d4a72-8a76-4fc3-a8f9-4991d8da8f90)(content(Whitespace\" \
         \"))))(Tile((id \
         fd6b3cf2-7fa9-40ee-88aa-01f1e3f59f1d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         a6de139f-3175-46a4-a9aa-2a2d74fae7c0)(content(Whitespace\"\\n\"))))(Tile((id \
         6c957440-e7c0-4ac4-966e-cf03e3e2ebdb)(label(cells))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         80c67f3b-269e-4b72-8b52-3144f76236d3)(content(Whitespace\" \
         \"))))(Tile((id \
         cd7d8e1f-683d-48a7-8dbf-2dbe24dfa892)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         94892ff7-791e-4b82-8230-6cd4b8d0d55b)(content(Whitespace\" \
         \"))))(Tile((id 8a01cb44-f034-4b70-bef2-510facd55371)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9f5ed2e6-b19f-4f2b-9d91-434d5096a8d1)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         3191cfed-2bd8-45b0-beb3-70b5d11d11b6)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ef541fd3-e07a-47cf-b68b-cda7513d7386)(content(Whitespace\"\\n\"))))(Tile((id \
         dd0b4c53-3520-4890-9add-e1432794c442)(label(width))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8e5e898f-7d0b-4607-b851-e578b13c13ee)(content(Whitespace\" \
         \"))))(Tile((id \
         0fd8167f-4c50-437e-ab87-9117a68e9ac0)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dff9c1c8-859e-4ec3-9211-665a6cd62c78)(content(Whitespace\" \
         \"))))(Tile((id \
         2ab8ff96-ce8e-42f6-a4ec-6823e1d4e4de)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         464d3413-851e-47b1-a8aa-63cdb0bb45f5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         af5277a0-2b02-41c7-9582-8d47f4735a87)(content(Whitespace\"\\n\"))))(Tile((id \
         a686772e-fceb-4fba-8ba6-1984a3adf741)(label(height))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d3c467b5-9dd6-4792-9f1d-9a7df90d967d)(content(Whitespace\" \
         \"))))(Tile((id \
         137a27e3-b8d4-44f2-9713-5f54817eab6e)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         71270e8d-1bf0-4094-ad60-9e212667a9a0)(content(Whitespace\" \
         \"))))(Tile((id \
         4fba0394-1952-4a6d-8289-a24d43e4bf4d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         438e8883-49bb-456d-a42d-9586fa7003d8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fb933fa7-4a2a-4bf1-9961-7f650e016ebe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f49a994c-e9d4-46f6-a271-bb5e15574c8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         064b17e9-6474-45d7-8de9-da13e77b4af6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8468d06f-a547-4f64-84f2-fdc5bcea366d)(content(Comment\"# Create empty \
         grid #\"))))(Secondary((id \
         e8518891-a2fc-4c98-abaf-dc209982ed8e)(content(Whitespace\"\\n\"))))(Tile((id \
         c2d150e8-ff49-47ce-aac9-5efb5748b54c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7d16fdab-3a21-49b1-bb60-54f7f9cfcad1)(content(Whitespace\" \
         \"))))(Tile((id \
         0dba6b54-a4e3-44eb-8f3f-aea09b9de16b)(label(makeGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c5dde9d-b1c0-48fa-ab8e-6968fad7ab0d)(content(Whitespace\" \
         \"))))(Tile((id \
         dab1a472-c8f2-4b42-8991-749fa3b5c60e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         78eed40f-34e6-471c-b812-83a29611b0a5)(content(Whitespace\" \
         \"))))(Tile((id \
         2492c0b5-dca1-4d91-a362-7cf4dbabc0c0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f7913e75-684b-46ac-a54b-54515fc80b02)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b540c605-39d9-4d8c-8f36-39285c8f5764)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15e030ca-7063-42d0-a8bb-f1e6770845a9)(content(Whitespace\" \
         \"))))(Tile((id \
         f5d409cc-6f60-45ac-8869-b2f35e9ee1fc)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5262430c-6c15-48f9-9388-c2bacf849e27)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d6a1bd-fc2d-4f49-ae21-0f622d6c33e3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7ead1af5-528b-44bc-a6bc-1eaa50f5974e)(content(Whitespace\" \
         \"))))(Tile((id \
         8c5b0000-c2e8-4a93-91e5-8f548ede086d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         94d48734-167e-4f61-8871-1c7171863139)(content(Whitespace\" \
         \")))))((Secondary((id \
         c84c7aec-e3a7-4f75-b506-884f758c9e48)(content(Whitespace\"\\n\"))))(Tile((id \
         44875cd7-5ca4-43f0-9947-847797864e57)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ec72c540-b988-4f5e-a932-6d76ea2196ca)(content(Whitespace\" \
         \"))))(Tile((id \
         f12f3015-f432-4d14-a2a0-1e872f32928d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4839b559-672e-4e57-b87f-5641efe51834)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3104e259-b044-47f4-8f8c-6a6586569ed2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e11be03d-49cf-441f-9ed4-8d591b5779c1)(content(Whitespace\" \
         \"))))(Tile((id \
         4ceb7e82-faf3-47b0-907b-b6603c379abf)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7fb6d27c-f41c-482c-bbbd-df5543d5711d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b09ce91e-8c7e-4b40-880d-8b4f2734b430)(content(Whitespace\"\\n\"))))(Tile((id \
         5c5a4a69-ea48-46ef-be00-55e81fe5ce60)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7eecbb9c-45db-44a0-bf20-10390ed085fa)(content(Whitespace\"\\n\"))))(Tile((id \
         d4bd6f08-c72e-4063-9635-d287d98f973d)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1ce9bac-eb20-455a-a2b6-45f2601ca30a)(content(Whitespace\" \
         \"))))(Tile((id \
         67fd04ae-e462-47db-9c10-97c8da6895a0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fcf8f7a-4845-4836-a68d-5a51299820ef)(content(Whitespace\" \
         \"))))(Tile((id \
         84be5fa9-d05f-43fb-a974-f89d0a6b5a5e)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8b57967-a18a-4c39-a993-fcb6ba11efc3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         69842c34-2cc6-4a6a-befc-266abf329c96)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85833abd-4995-47a5-80c5-e9ba0bbfed3b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         05648e49-996d-4c94-b160-3f7b254715df)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         818d8f88-1a0a-4173-a698-b9c0a1ff53fd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a6ede33-305c-4592-8efb-d612a67a5570)(content(Whitespace\" \
         \"))))(Tile((id \
         d8643137-b1fe-4b43-b870-814de034063c)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be8f6f01-772e-4a13-970f-895b1ac27103)(content(Whitespace\" \
         \"))))(Tile((id \
         8c740cf0-0b3d-47ba-881d-fa82dc061bc3)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20bac0e4-01fc-441c-90c7-3e226ee48a73)(content(Whitespace\" \
         \"))))(Tile((id \
         54678acb-045c-414c-b91f-aea1a36c834b)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4066328-929e-44b8-9483-7161db2b00df)(content(Whitespace\" \
         \"))))(Tile((id \
         2964303b-2db1-4fb3-b5bf-ab8237320070)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         542c84ed-0a24-42f1-ad17-545908172207)(content(Whitespace\" \
         \"))))(Tile((id \
         2cd356ff-1432-4553-942d-66ecd5e4e1d6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2477c056-4892-4043-8e7d-3b076e623221)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8eb6815f-143e-452d-ac5c-b59e72e448fd)(content(Whitespace\" \
         \"))))(Tile((id e550154f-e69d-4a1f-9abd-891de91c4d60)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7dd15a95-df12-4ed7-be76-2f5cd93bf625)(content(Whitespace\" \
         \"))))(Tile((id \
         77665092-a4b3-4c02-a0f9-dae5d92a6fc8)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         69183a52-1193-41d1-ac8e-8a7d4ddd062c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b462d213-c34b-44fc-820c-be622a72caef)(content(Whitespace\" \
         \"))))(Tile((id \
         48bbc27a-808a-4aff-ae99-275c0552b1ac)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         46778313-8c99-4687-85a0-c8c70530d446)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b62a29d2-b4fb-47a1-b677-837d44360a4a)(content(Whitespace\"\\n\"))))(Tile((id \
         371b6049-1927-4d55-ba1b-5f5d782c52ab)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17d90a8b-85bb-40ed-9d81-ac7e1ed250aa)(content(Whitespace\" \
         \"))))(Tile((id \
         4f73a321-9977-4ead-801b-66c86ec6ad2f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51a4453d-9cc1-4226-9f45-822570819e2a)(content(Whitespace\" \
         \"))))(Tile((id \
         675874e9-5b69-4949-a796-034af20da05d)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b530a350-1bce-40e6-9538-28bace43a27d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46498fa9-1a2e-4104-b58d-060b8d1fb72b)(content(Whitespace\"\\n\"))))(Tile((id \
         ac376992-483c-4d3a-b81d-7bbc461daa8e)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a89bc716-717a-4f7f-98a8-9fe782073b7d)(content(Whitespace\" \
         \"))))(Tile((id \
         9197842d-f51c-478a-9ef9-f7acc997b2e1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb12c456-3c11-4438-b50b-33b3f6275c8a)(content(Whitespace\" \
         \"))))(Tile((id \
         671a55a0-820d-4ebb-a21d-6799de24a77a)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c57e83b-9f67-4900-a652-07ed3eb51a19)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d401b938-2c69-4714-af3e-ecaa7b4c6111)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         742440d7-8bb9-4b63-bceb-9be8bbc3adfa)(content(Whitespace\"\\n\"))))(Secondary((id \
         a499132f-8208-4f27-8e7e-8258a938a0e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5da303f2-20eb-4411-9946-5cebbfefa702)(content(Comment\"# Convert (x, \
         y) to index #\"))))(Secondary((id \
         03be4cfb-226e-4670-8aab-1ebab65599e3)(content(Whitespace\"\\n\"))))(Tile((id \
         e9d7631f-925e-4b57-8103-1dd98931369a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5bacb77b-368a-401a-9bfa-7561755fb1cc)(content(Whitespace\" \
         \"))))(Tile((id \
         7125b8a5-25e6-4857-8e1c-5cbc7ecfa18a)(label(toIndex))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9d05916d-d139-48e8-af76-907c1bb9583f)(content(Whitespace\" \
         \"))))(Tile((id \
         bba17664-36f6-496b-9f36-71c24fcf454a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4b12df21-c3bb-4b2c-9291-5ea2fdbac4be)(content(Whitespace\" \
         \"))))(Tile((id \
         fa6db34d-a46b-4835-a12a-ac5be69f372a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         db688457-751b-4731-abd5-630ee19269be)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6a62e4de-aa66-4704-a483-7ef28a3849af)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d41ac315-1ea1-4c98-a0c2-4c4c41a3b2d4)(content(Whitespace\" \
         \"))))(Tile((id \
         1f4084ab-04e2-4fc4-bc2c-7fad18f26c62)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         127dde5b-e9fe-47b9-aefa-c07b04264e8e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         84809264-93cf-448d-806c-47ea55c3bbc0)(content(Whitespace\" \
         \"))))(Tile((id \
         e937af56-e9ca-4063-bb95-b820cb8ef081)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         68527816-94bf-47db-b83b-c29115cfe1d7)(content(Whitespace\" \
         \"))))(Tile((id \
         dafb1caf-174b-4336-82f5-a5bb70570e90)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c3f9b0f2-b42f-44ed-8539-be6e5453c6b7)(content(Whitespace\" \
         \"))))(Tile((id \
         375807c9-1503-47c5-87ee-813ded88d0bf)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         831d5664-42df-419e-880f-b06f7187b153)(content(Whitespace\" \
         \")))))((Secondary((id \
         aa023627-a129-42e9-b0bd-43cdd4fbb685)(content(Whitespace\"\\n\"))))(Tile((id \
         6e964efb-8777-4321-8ee8-b4a2a3ba75c0)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cb1b91e4-35c0-4ac9-89c6-c71e574f071e)(content(Whitespace\" \
         \"))))(Tile((id \
         696dc6d5-691b-4873-b09b-61a2666e19c5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a5055cc3-0340-45be-ab72-8fd749b461c8)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ff8c18eb-8ff7-4480-ad0f-56aefde8788e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         18c9b8b1-a0d5-4108-8d1e-eea61ca33c08)(content(Whitespace\" \
         \"))))(Tile((id \
         700dde90-e60b-4be6-af47-b84a528caabd)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9fe0ea2d-7a09-401b-8450-fd4efc862359)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9b6c6394-95d5-4c95-82f3-01d25ed253a1)(content(Whitespace\" \
         \"))))(Tile((id \
         3666da43-c955-411c-a119-3b13174e76e4)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f6846baa-0ffd-445a-89e5-7e7067f46a9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8f58c84a-c7f8-423e-9651-a4ffe6cacdea)(content(Whitespace\"\\n\"))))(Tile((id \
         804af67e-2d8e-41d9-9b55-b598730f2036)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b4ad780-4bc2-43b6-81cc-e616368f4fd0)(content(Whitespace\" \
         \"))))(Tile((id \
         d2a83443-2d13-4d51-928f-fdd007ada43e)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90e2bba3-5e33-4eb5-9c26-7d98efede707)(content(Whitespace\" \
         \"))))(Tile((id \
         58138e2f-fabd-47aa-afc6-2afc1f529147)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09a90582-7379-441c-98d1-8e626e540593)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         aa9dbb95-4152-4b1c-b0b7-09630d0990fb)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd569608-6477-443c-9f0a-a12821b43691)(content(Whitespace\" \
         \"))))(Tile((id \
         a7ab9b60-23f0-465f-84c0-bb17ce64d694)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2852b9e-3f78-4e97-8fd6-ed514d593d9e)(content(Whitespace\" \
         \"))))(Tile((id \
         ee588f05-f2b6-4a1b-8efd-19ef350d6c81)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e868abc-752b-416b-b05c-c13b8b0b5646)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         41357f3f-aebd-407b-887a-00f56fc93b59)(content(Whitespace\"\\n\"))))(Secondary((id \
         136c13ce-771f-4fcf-b52c-bbd6e059baba)(content(Whitespace\"\\n\"))))(Secondary((id \
         9533ed30-ca02-4d24-b5d7-5fad47acdba4)(content(Comment\"# Check if \
         coords are in bounds #\"))))(Secondary((id \
         7901ce00-39ed-4b82-8d32-e5b563e9a207)(content(Whitespace\"\\n\"))))(Tile((id \
         e444b145-d678-44e0-bc93-dbfa62a15a93)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3cc60a7f-f197-48ca-93f0-2740a6a8eea4)(content(Whitespace\" \
         \"))))(Tile((id \
         eaf44c80-d159-4f52-a59b-80eabb8dd83b)(label(inBounds))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73bb7ce4-2f1b-4934-a30c-1bfdea80b1dd)(content(Whitespace\" \
         \"))))(Tile((id \
         542e03ee-2ec3-420b-a18f-65dc6fb3903e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e8a0958f-7f6e-435b-ab72-7f69abdef94a)(content(Whitespace\" \
         \"))))(Tile((id \
         2ed96b6c-a3bc-40fd-8977-53b34f95908a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         a625ae2c-9a7b-4941-a86c-d67b547f760d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4baef2f1-99b9-43c6-9609-c1e9fdefc858)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         11e961fa-46ae-4b40-a5c3-e272fa6b3bdc)(content(Whitespace\" \
         \"))))(Tile((id \
         e6d4c1ba-8d42-4286-9dce-f01078b3658f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         80084b8f-b1f5-4778-bbf6-30f5a0341e00)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         84d5208b-e1b0-42f1-acc5-84d0cedb8f3b)(content(Whitespace\" \
         \"))))(Tile((id \
         d40710a6-7ce5-45f9-bec6-133561e4776a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         43a4254f-0598-464f-8ed9-81ec0f669cfa)(content(Whitespace\" \
         \"))))(Tile((id \
         06d2da89-d078-46b8-b9a7-327309a1e26e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7bb86706-298c-4f70-b726-dc41e6f13beb)(content(Whitespace\" \
         \"))))(Tile((id \
         1a300885-c0b1-44fa-b71e-2a36ba871c3a)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3f55e31b-426b-4207-9a15-44e4ade3fae3)(content(Whitespace\" \
         \")))))((Secondary((id \
         45a1c054-45a7-49cb-87e0-cc67d3b280f1)(content(Whitespace\"\\n\"))))(Tile((id \
         c0b798a5-c26a-46e1-a568-c709a8f9b2e4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3e220be6-4cc3-4c20-b586-7ec6a3d81e65)(content(Whitespace\" \
         \"))))(Tile((id \
         59b63926-7bf4-4bac-ac60-b167b68607d9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7e06a44b-03c3-4432-85c4-fd7d86797859)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         edafb0e2-13f6-4e8e-8a57-b3b3ef25abbf)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e3648eb5-1fb5-4575-ac7b-01fcdfbdbc62)(content(Whitespace\" \
         \"))))(Tile((id \
         4ba01dad-5be1-48a7-8af2-c43fd74fd440)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0f344bb5-cf57-4b73-b397-95afdf67d23c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6beb2db9-2f13-46ae-94bd-2b65b3fcca86)(content(Whitespace\" \
         \"))))(Tile((id \
         f118fd6e-38f3-40ad-9b02-2650b68282c0)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         243017e0-4083-4ab4-ad3c-f1e7c2c3a7ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2bf0ff19-d2a3-40a8-acb0-183ed9253140)(content(Whitespace\"\\n\"))))(Tile((id \
         4e8cba05-3915-4092-a124-25a537d2d39f)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d94b1166-5fe1-4758-878d-4474a5d28798)(content(Whitespace\" \
         \"))))(Tile((id \
         19bc6b7f-978a-4965-bdef-bfb288f52b7a)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85340d33-ec39-4cdc-be4c-d6c52f284934)(content(Whitespace\" \
         \"))))(Tile((id \
         022b86ee-e938-40c8-82c9-a6806253ad63)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         603883b7-4135-4cf2-9702-d59c533286f7)(content(Whitespace\" \
         \"))))(Tile((id \
         4e43cbdb-7b78-4b2c-ad64-c2e3ff63ac20)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         433a2243-4b35-463b-ae31-eeee56e44ca8)(content(Whitespace\" \
         \"))))(Tile((id \
         596f94a0-7496-4fc0-98c3-d1e6a23c6da0)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5cb66b17-0196-4efd-9638-4098bb7b0560)(content(Whitespace\" \
         \"))))(Tile((id \
         ef160b1f-bffe-4cdd-8764-d50d3e263ea4)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47690b32-d527-40ce-a84c-25ac8e2a7ca0)(content(Whitespace\" \
         \"))))(Tile((id \
         ce394d16-1608-441b-9f7e-9ac1dd893f13)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cbe4ece-eff4-4775-8481-a5efcb8be587)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         92fe600f-811e-4c12-861b-14d97d54d35e)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61fa87df-6243-4d2c-bd98-c6656d48641e)(content(Whitespace\" \
         \"))))(Tile((id \
         2e8ca780-def7-444d-8fb8-090dea8bf388)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6f5cd53-b30b-4a7c-9b7a-76b3f03b23c6)(content(Whitespace\" \
         \"))))(Tile((id \
         ab6115b6-2a17-48a3-91cd-7bcd7a53058d)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e6f85da-3e26-4e52-b3ba-996e80087d3e)(content(Whitespace\" \
         \"))))(Tile((id \
         c794db24-1305-406e-833d-c65bc4c9f565)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         086ffc2b-598d-43e8-9585-c1652aa45d5a)(content(Whitespace\" \
         \"))))(Tile((id \
         bc5ae8b6-78d1-4dbb-9d8d-a7a009e00625)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb70abf4-a261-41d4-898e-a4e2b376f06e)(content(Whitespace\" \
         \"))))(Tile((id \
         5b0c6cdc-d5af-4244-85b5-08ce5547152d)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9944ade5-c8dd-4451-b7b5-265b0a632ba0)(content(Whitespace\" \
         \"))))(Tile((id \
         e8db56fc-238a-4f4f-bc10-4cf3ed87c691)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02f47939-8331-4edc-8c57-09df5c745b84)(content(Whitespace\" \
         \"))))(Tile((id \
         9ffcc44f-a4b2-4d01-9aca-a302e5790d1b)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fbab008-d4ec-47ac-a431-31abf02849c6)(content(Whitespace\" \
         \"))))(Tile((id \
         7825db78-c7f5-4866-b74d-78beab5b98c7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         477a979a-c882-4b8e-ab65-8f4e17d0e29d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         05fc9da7-8da2-402a-be2e-d033d00959e4)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4127a980-61e4-4905-bf81-de63da2a4c60)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         497be532-6e03-4a0e-a956-b3c9760ea390)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ed42dd3-fc2f-4b76-bf11-88f09a78dbec)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d057563-76d4-40e0-8e00-06b90387deaf)(content(Comment\"# Get cell at \
         (x, y), returns Dead if out of bounds #\"))))(Secondary((id \
         84c55906-0aad-45e9-9c7e-26bd07facbba)(content(Whitespace\"\\n\"))))(Tile((id \
         b359f72c-be56-4bd9-97e7-3d13cf8a0eeb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e2c4f967-e5c2-45fe-9ac1-a038c9306258)(content(Whitespace\" \
         \"))))(Tile((id \
         a6479d38-bc03-4652-a19f-8eab3a2964e0)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c387a53e-8100-4a15-b39b-4cf53618a041)(content(Whitespace\" \
         \"))))(Tile((id \
         5fd6edc5-b0d9-444a-8e84-9192a2263a4f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0e7a62f8-728e-4ff3-b6e2-b0c3c2821a82)(content(Whitespace\" \
         \"))))(Tile((id \
         ae96e58a-ef53-4075-9c0d-02f991f73a96)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         fb6582e4-3923-4bb1-9635-83ac3f57b943)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fb00754d-621c-4168-a0a2-5831d03589fc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         adac0cb9-fcb1-44b1-93f7-f14c4ca553b4)(content(Whitespace\" \
         \"))))(Tile((id \
         90884554-da74-4609-b49e-d571e9f910af)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         24710ab1-8d34-4e57-951b-9abef5f1f069)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9cb33411-a9a1-4be7-addf-b4162629ce7f)(content(Whitespace\" \
         \"))))(Tile((id \
         e26a098d-1775-48e1-995a-11bf2647ce0f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7ea93393-8e22-4c38-a846-e1b7e390a5c4)(content(Whitespace\" \
         \"))))(Tile((id \
         c070b7e2-dbc6-48c5-b2d9-3f8e63e528d2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         734a8e96-be2e-4fd3-91db-76b750ed108d)(content(Whitespace\" \
         \"))))(Tile((id \
         00893d6f-9f21-4d5c-8670-5a930b3a1d90)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5b445592-2657-4f1f-b9b8-3e651f383350)(content(Whitespace\" \
         \")))))((Secondary((id \
         192b0d0d-6643-48ee-bdfc-4b654805fc91)(content(Whitespace\"\\n\"))))(Tile((id \
         0bcc4fa2-cde1-4abc-969b-aeb838d6a33f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4fb8437d-53e1-4457-a758-b1b486dcc426)(content(Whitespace\" \
         \"))))(Tile((id \
         71e56c9d-736c-4201-a990-69d4a30376d7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         88f9b6de-23c7-4136-be6f-efab6c930d65)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5e6d062e-1bfc-4665-a0bd-e6b7527d31a3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         75b3326f-cdf0-49a8-92e4-06a52bbb8275)(content(Whitespace\" \
         \"))))(Tile((id \
         9862acf2-c2d4-4791-a21f-b55e5af3adca)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5bc2fe55-bd5b-4f20-86e7-b34fb41f4330)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c509f66f-277f-4ba1-9508-d628321ab9df)(content(Whitespace\" \
         \"))))(Tile((id \
         dbfe185f-53cc-42e4-bc79-a243cf40db03)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         8c51c8ab-ae33-47c7-82cb-241d0f4d0b0b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c5ff0216-9bc0-499c-b1aa-14636981c168)(content(Whitespace\"\\n\"))))(Tile((id \
         b42ec3cd-2644-42e0-842d-ac6dfd44fbb4)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         50c8c0d7-5547-4c88-b916-cbf4f19ab4ac)(content(Whitespace\" \
         \"))))(Tile((id \
         89dabe76-e9b5-4a68-9435-feb00ee781e4)(label(inBounds))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b5fef2a1-4bf9-4923-8551-eb6a2e9eb31a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         40e1ba85-8769-485e-ae01-fc6b88ce1d9b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72ac6b9d-4e35-4d3c-afa2-23e1a429bbd9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9b09499-f6d9-4eba-8dbe-6f4bd556b7d0)(content(Whitespace\" \
         \"))))(Tile((id \
         f2cd93e2-dbdb-46da-a134-c4c0b77af9d1)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2aeaa546-f9ba-4e72-a8e4-612e1588fae7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6497172d-3cd3-46b7-be61-ed1b3a336698)(content(Whitespace\" \
         \"))))(Tile((id \
         c27d4419-4663-482e-81df-a037de706f10)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f2b3476a-dc73-4bb7-ae8a-1b047de9da77)(content(Whitespace\"\\n\")))))((Secondary((id \
         209afe5e-0d9c-48ae-afdc-2b576ed9a192)(content(Whitespace\" \
         \"))))(Tile((id \
         ef93faa8-a6e0-4aa4-8230-28743688c0ed)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e6172d7-2fdc-48c0-a40c-27e0cb6ed8db)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5684bd63-eb8f-4432-b03d-6785101f7f75)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cd3e3cc-5c89-40aa-beb4-35283b30314c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f866a5a2-de95-4813-9bb6-c37925020660)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc9f5dec-325c-42bb-a261-c1082bbdaef9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa1ac2d7-88ca-41b5-a697-99d4d3a7eb4b)(content(Whitespace\" \
         \"))))(Tile((id \
         dc54aef2-ad3f-4e89-855a-60747a18a53e)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ca05fcc-2422-49bf-ac53-c1fc4de400df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fd39ea58-891a-4737-846b-658a4a3ae01f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1d2b32d-7b0a-48c5-afde-d02b8ff6f4f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb9be000-fb84-48ec-b966-117e41d0f24c)(content(Whitespace\" \
         \"))))(Tile((id \
         da02d885-cdb0-4be3-bb64-f93c6712e796)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2b284fe-93c0-4cba-9319-723e3c008498)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6b055a8-aea1-4e29-a5cc-469a7a6a3af3)(content(Whitespace\" \
         \"))))(Tile((id \
         891fe819-7de9-420a-b7d9-30b22e783ea0)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         301a9a94-463b-47cf-9d3e-156ad0a01d69)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         67c6978e-9156-497a-8b45-c31b2fe0d443)(content(Whitespace\" \
         \"))))(Tile((id \
         78ac4789-d3fb-4fd7-b91b-909d7182d16a)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d72ef0f0-399d-4bdd-92bf-26ce1b93c2f2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9f746a7c-e116-4bdd-8c69-3971802233db)(content(Whitespace\"\\n\"))))(Secondary((id \
         68cddb2b-74cf-4673-9a70-dfc1425decac)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5af60d4-20c8-466c-a7c1-0f482e01f48d)(content(Comment\"# Set cell at \
         (x, y) #\"))))(Secondary((id \
         ca814bb9-3612-47ee-97e9-16d2a197c4c4)(content(Whitespace\"\\n\"))))(Tile((id \
         aaf815ca-d163-4caa-ab92-f9b50027b48a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         911efb3d-23ea-450a-b47f-e9c25c66b3c5)(content(Whitespace\" \
         \"))))(Tile((id \
         09d5fb74-0996-403a-8cd3-d4cf4c69941c)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f18dc100-26b1-47ef-86c7-6ab00f7e40bc)(content(Whitespace\" \
         \"))))(Tile((id \
         dded91b4-cfdf-4a54-a156-62abd80a326c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c53785c4-63a5-4243-812e-4524c5002615)(content(Whitespace\" \
         \"))))(Tile((id \
         50869a24-25d5-483e-9d48-1326d23b37db)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         359fb5ea-c97e-44ae-9d5e-82bd4e2d951a)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         17d045f8-eb27-4178-b74b-bc433b0e0854)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4d879104-ba32-4d69-aab3-989dab662ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         bb5ae3bb-b00f-4e67-aaf2-54637e8ffd2a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0fe8bf4b-c1fd-4b0a-84c5-3f11e7bfef2c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0cf2504c-85b0-4765-85fa-28e5b6b95c1d)(content(Whitespace\" \
         \"))))(Tile((id \
         c37df59c-279a-4959-9587-407c2617943d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ca0b261b-dfaf-45d6-80eb-d2886a6a403d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9290ec4d-7152-4e6f-8934-1e5135f465bf)(content(Whitespace\" \
         \"))))(Tile((id \
         98ee7e9f-9841-42b3-9924-0089b50a48d1)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         70806149-53c7-4a93-8afc-0c6b3fd20df2)(content(Whitespace\" \
         \"))))(Tile((id \
         9fc4fe07-37c8-4422-b22d-ebcfd5983e20)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e288b61d-6853-4731-99f1-0cd46561a944)(content(Whitespace\" \
         \"))))(Tile((id \
         62fc56e0-3e9f-46f8-a11d-4283414fca18)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         15542c1d-f6d0-4c73-a455-f52b2c0308fb)(content(Whitespace\" \
         \")))))((Secondary((id \
         28f7edfb-9c9b-4ec8-b67a-a135c3de29dd)(content(Whitespace\"\\n\"))))(Tile((id \
         c48f9376-d38b-422b-9d63-e39560df2986)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         869e5105-f353-4e87-a953-f8bd495c2cd5)(content(Whitespace\" \
         \"))))(Tile((id \
         d960a2ff-8671-422d-956d-64a14bcdfa90)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2d076525-ce50-4597-94b1-a6462ac532eb)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b8d4fb12-4057-4d07-b6d0-e68a16994580)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         70faa9cd-5b92-4bf7-b7cb-9200be0c5a11)(content(Whitespace\" \
         \"))))(Tile((id \
         c78fcd72-b720-46ce-9670-1b1d153da651)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         86dd2772-2c71-4962-961b-e4e2a6d3ad69)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9b800f44-c9fe-4f23-b73a-175f9f560b87)(content(Whitespace\" \
         \"))))(Tile((id \
         aef7eba7-3656-4334-9518-680f8fea9944)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2cf7f892-5931-4f3a-adcd-be846ae32882)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d87c8348-f32b-4474-87e9-f23e9c10a95e)(content(Whitespace\" \
         \"))))(Tile((id \
         7715401c-81bf-49f3-95c0-a8e967baf2ce)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         59b950ea-e372-4e40-91a0-daa01743b357)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7fc13fd0-7ae5-4d36-98e2-de0dae8ed95a)(content(Whitespace\"\\n\"))))(Tile((id \
         bf66f370-b5fd-4869-8a8a-c0e7df45bfce)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6906e70e-0509-44d3-bbd7-0134e522c715)(content(Whitespace\" \
         \"))))(Tile((id \
         dd0cce51-9b56-49d7-9222-48aae42245b7)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         06894a57-cb1f-47e5-b967-d62328dc73f3)(content(Whitespace\" \
         \")))))((Secondary((id \
         677d5236-9f49-4a65-987e-2ec0c2d6ed8a)(content(Whitespace\" \
         \"))))(Tile((id \
         03d9033d-6748-4358-930b-8cd8b4618889)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a169ca77-715e-4204-a07c-efb27be7a3a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         076c76b3-3b29-47c6-9908-bf0ae60e7e3a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         650291e9-d989-433a-9a54-b8ced005e073)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         877b4178-6bab-4afc-9d94-cacf66b42a60)(content(Whitespace\" \
         \"))))(Tile((id \
         6fa59135-8a8c-4111-b522-d55cde6cd989)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70d04b1d-2b63-4d14-be85-54de66752aa9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e00afdd-40f9-41c0-aab0-ab5e7f0a2b5d)(content(Whitespace\" \
         \"))))(Tile((id \
         e9bbc5d2-13b2-4d8c-9b76-4b18c124629c)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3e53c35a-5b74-4f48-bda6-a7d5d9bb519c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e1161b2a-bb29-4715-9a4c-d77242564288)(content(Whitespace\"\\n\"))))(Tile((id \
         c0b081ad-82fd-4224-b9d0-938c23997403)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bb0cb479-46f3-4cef-bae4-81199472ab28)(content(Whitespace\"\\n\"))))(Tile((id \
         7a788234-a2a3-43b9-b8cb-07738f9e6ebc)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0ae7ca87-2f7b-455e-8c27-fa43aaf6f974)(content(Whitespace\" \
         \"))))(Tile((id \
         fcfbf2d1-2225-4b49-a970-8f1d9247060a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6409aa66-8e82-46c3-ad9d-5c46f371ed01)(content(Whitespace\" \
         \"))))(Tile((id \
         0bc8beec-20cb-423d-925e-5ccd7e689ede)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e65d641-ad00-492a-8be2-7c8e85f724de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         715e8e42-a36c-4521-8d6f-d8289b6074f3)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f65030a-5dbf-4b7a-9d44-c864db825696)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         05a16ce9-c094-4a76-af47-c712bd12a263)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f9739110-bc7b-4e91-9b11-184296a36210)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b01a95a3-1fce-4ed3-876c-5a30acdf014d)(content(Whitespace\" \
         \"))))(Tile((id 392a0f94-c264-4c8b-b6be-341e5d33460e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c4b9b009-8bcf-43c2-951d-f07cde0ca343)(content(Whitespace\" \
         \"))))(Tile((id \
         a76357c7-b836-4c56-badb-264d7d86d305)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6d2d1dbd-5407-4d94-88f5-8565861c70d4)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e388cf85-2cc6-46fe-9469-c734642cb54a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4ea18cfc-6c7c-4dae-bc9f-1ffba92bd68b)(content(Whitespace\" \
         \"))))(Tile((id \
         e3176e05-5921-4e1b-a41b-faa14ad6e238)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ddf8f54e-bef0-46cd-b9c9-6285b28b8c5e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         99ffeefe-3565-42a7-9d7d-32c9784a63e2)(content(Whitespace\" \
         \"))))(Tile((id 91419486-a033-4214-99fe-7df753dbe8c5)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         8a057bc8-cf9e-413c-ab74-fbf70df9e663)(content(Whitespace\" \
         \"))))(Tile((id \
         e6c6bee5-6f53-460b-a985-e159cb61641f)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3be418b9-54ac-411e-a663-8039d6964734)(content(Whitespace\" \
         \"))))(Tile((id \
         f72d7ce4-f65d-4d9a-b17d-7241916b9c68)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00e6c69e-b7f3-4469-9efa-147fadac6621)(content(Whitespace\" \
         \"))))(Tile((id \
         a6b4b677-de8f-4565-95b5-159c807099c1)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         76e7bf84-efa5-4b8c-9b24-53e107917d71)(content(Whitespace\" \
         \")))))((Secondary((id \
         e0a17578-3ae1-43bf-9112-e8cde06a15be)(content(Whitespace\" \
         \"))))(Tile((id \
         d0da4852-bb8e-4b65-8581-7d95e8e44211)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         921b3b87-100e-4081-8982-fdb10369caf0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         708fc525-a4e2-4662-9009-17d8630988b9)(content(Whitespace\" \
         \"))))(Tile((id \
         bb48028a-f05d-4c2c-bc29-cf8b5d122046)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         92a25b1b-ae16-4989-b251-6fc6440a8444)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ac6f0c4-a079-474f-9b23-9a3171447d86)(content(Whitespace\"\\n\"))))(Tile((id \
         4b2978cc-cb26-4c51-b03e-5e2ed37bd6d7)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2be0d1b1-3b03-42b9-93c9-9286afd0bc07)(content(Whitespace\" \
         \"))))(Tile((id \
         8f9e919b-3dbb-47bf-a1cd-94b536eeb69a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         563eed59-1f63-48b7-8d2a-37deda789b06)(content(Whitespace\" \
         \"))))(Tile((id \
         b4a4eb35-08a7-4e65-9cd8-624823904e84)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da7303e6-26e4-4c4c-bb90-7fb361adae4a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         60cd0a9c-9b7d-4c81-b394-b87e00045e14)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43660785-3e35-4449-8f60-e3b6f2e588fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5b3f787-4a2f-4ce1-ad52-7039196c087a)(content(Whitespace\"\\n\"))))(Tile((id \
         9b16bea4-41bf-4a62-b2be-49722dbf9ac1)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f487dc71-7980-4474-8b04-d7a6e73958bf)(content(Whitespace\" \
         \"))))(Tile((id \
         a7e18c88-fdc6-4a49-b103-aa42de44933e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e665d16-6f28-4b41-a502-d20e65d53803)(content(Whitespace\" \
         \"))))(Tile((id \
         1344a695-6782-4a88-a53f-e1a49f38b6f0)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         476379b4-670a-4534-ba58-b7bd442a66be)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e1455127-0fe2-4773-80fc-85038b8c527d)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         65898cba-04be-4a42-8f2d-4bbc87cb8fc2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         94e83b2a-2ab2-4089-a182-0c63cb43890b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6d8a86bf-599e-43b6-8af4-cca2862fed1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         109d7e1c-a848-4dcb-b40e-28a7ddceb849)(content(Whitespace\"\\n\"))))(Secondary((id \
         8004318c-47bd-4fa5-a41c-9802a35d17e2)(content(Comment\"# Count alive \
         neighbors for cell at (x, y) #\"))))(Secondary((id \
         9c790ceb-9497-4a2e-ad12-5701c1ac4f56)(content(Whitespace\"\\n\"))))(Tile((id \
         9091af14-8d53-4f9c-a679-cf32dd989cf7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ab869be6-4023-4ca4-b7d9-f130cdffcec9)(content(Whitespace\" \
         \"))))(Tile((id \
         32c2c835-d6d4-43c3-a0b4-8f04299d042d)(label(countNeighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         398c9443-7386-430d-9f97-b00f385f723c)(content(Whitespace\" \
         \"))))(Tile((id \
         d700ccaa-11a1-4d10-aae8-d1d174732cc5)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a49c761-f848-438f-9082-3e1f54ba41a4)(content(Whitespace\" \
         \"))))(Tile((id \
         608cdb2a-5e45-43eb-b04c-0b6e572ba2c3)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         891ba188-2b1e-4e1f-afe8-31045aa85e36)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         43d7e2ed-7563-4835-9ae4-f6e0b1993601)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         525cca6b-c037-477c-86eb-38ccb4841304)(content(Whitespace\" \
         \"))))(Tile((id \
         a2fc3086-8703-43d6-be51-6aff37529af6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         706937f4-1130-4893-b4c9-b7a083fc1b83)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c7d05beb-01e9-4a99-b021-bf1b7ec54e95)(content(Whitespace\" \
         \"))))(Tile((id \
         aa39cd1a-a26c-41a8-b2fe-2360bdc2dd0f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         362e4f29-34fc-4d2e-8d8a-f38c9d2f7c75)(content(Whitespace\" \
         \"))))(Tile((id \
         7222fe76-42a0-46b7-96a3-dc138118c82a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         efcf74ee-19d2-498d-abed-ea6fc425e7ec)(content(Whitespace\" \
         \"))))(Tile((id \
         4a7b1eee-1a08-4844-a9c1-a91d0d19341a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b0bdc0cf-3b7e-48fc-aeca-9010f84cf6fa)(content(Whitespace\" \
         \")))))((Secondary((id \
         bf70db16-bcbc-4a62-ad41-0b17e0afc7c9)(content(Whitespace\"\\n\"))))(Tile((id \
         d9a10a69-ff46-4c5c-a902-58765af1f331)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d2a2e6cc-bd55-49e6-9875-1c3d832e5c65)(content(Whitespace\" \
         \"))))(Tile((id \
         759e12db-ed10-4f2d-ba71-0234fdcab603)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6d64ee5a-9d4b-412a-927f-6ddd852a6a82)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5d285302-57b5-47d3-aab4-cb7f71240f96)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6a1b26b2-75b5-4c2f-b58a-9d02c52a27eb)(content(Whitespace\" \
         \"))))(Tile((id \
         dc90c9a5-c899-4770-87c0-5347cb0abba2)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b7c1fa22-8e70-46dd-b1c6-a5cbbbc73597)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f3de29a1-26e1-4682-893a-ed9bbdffc38a)(content(Whitespace\" \
         \"))))(Tile((id \
         d1f3a095-0b2e-486b-a5ca-0656fe25ae61)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         8aa5d370-6d11-4508-b91a-b8c56ec1f553)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ad23b843-9a32-4461-b136-bc28604942b1)(content(Whitespace\"\\n\"))))(Tile((id \
         e04554ad-d736-41ff-8d22-6a109cf403e8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         162bc3d3-0ebf-4d9c-9325-3088c3f08430)(content(Whitespace\" \
         \"))))(Tile((id \
         5ffd6577-627e-4f5f-a55a-5f990d460ed3)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4fec07dc-12da-4b7d-94cd-d66c901dccf1)(content(Whitespace\" \
         \")))))((Secondary((id \
         e01c41d3-400d-4ae7-ab11-263399a02f17)(content(Whitespace\" \
         \"))))(Tile((id deddc2e7-4051-4ee4-b2b1-725325e536f4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e4851f13-51cb-4b8b-a70c-44e165bf0b6a)(content(Whitespace\"\\n\"))))(Tile((id \
         5e16911d-7b7d-482d-a316-7ab2ab1809be)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf613471-f0d4-49e1-a156-b95fa0dd75ef)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         86f05214-a0fd-4c72-86d1-6d5fb185dd25)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6e0417e-6331-404d-8991-1dfb0f44523d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de6576d6-3d14-4f46-a5fb-a2cb584ff092)(content(Whitespace\" \
         \"))))(Tile((id \
         26903411-8435-4cfd-836a-93ecc0b919b9)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         70c56f44-6e64-4e25-81af-28f3ae261b3c)(content(Whitespace\" \
         \"))))(Tile((id \
         c76ecf80-a32c-4368-909b-64663bdc3c67)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7470a0f6-d8e2-4d82-a774-76df19ea801c)(content(Whitespace\" \
         \"))))(Tile((id \
         804ebf8f-37db-45ee-a467-edadcb7326f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5de4a0b-0a84-4383-b49d-66828df339ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5593fe91-1d57-4521-b274-e8932088ae7d)(content(Whitespace\" \
         \"))))(Tile((id \
         b476fc36-1c56-412f-a1b8-b6e2fe410527)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9eda3603-0811-48b5-b017-91a7ad9a621b)(content(Whitespace\" \
         \"))))(Tile((id \
         92d7e86d-709d-4a7e-a3cd-9dd3f2b2e32d)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3acea4da-74bb-4418-8afe-8aad3a4f8403)(content(Whitespace\" \
         \"))))(Tile((id \
         0905b5da-6400-4756-a6cd-7f4aecc64053)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fb28ac6d-671a-4930-b4ac-47f410917a89)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efc4a93f-78f7-4df5-8c8e-06876bcf1543)(content(Whitespace\"\\n\"))))(Tile((id \
         b692ebde-f231-49d8-ba1e-ffe2523dfcd7)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7679c183-601f-473f-a152-26f48b0fa2d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55d517b2-d402-459e-9b8f-69b05381bd7b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8688d407-3d26-43c4-9c5b-f8f56bf09860)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e4af717-bc5c-4c60-ae4f-3740c97d331e)(content(Whitespace\" \
         \"))))(Tile((id \
         5ca5592b-fcf8-450d-a34a-79e0cc626e52)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd1a0914-9e10-41a6-ac1d-8cfcf68f424b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e22ca28f-901c-44b8-87ff-fe9540c5e9f7)(content(Whitespace\" \
         \"))))(Secondary((id \
         376de6ca-0556-48ce-91b8-5a9515a32a3f)(content(Whitespace\" \
         \"))))(Secondary((id \
         65f0ed15-9c61-4914-86c3-de51430fd83e)(content(Whitespace\" \
         \"))))(Secondary((id \
         8f99fb8f-b700-4aeb-81ad-1ccc2e11ec14)(content(Whitespace\" \
         \"))))(Secondary((id \
         34a1f850-8b09-4b95-a9a0-202495166716)(content(Whitespace\" \
         \"))))(Tile((id \
         364c097e-cef5-4c3f-98f9-1df9e79f5f6e)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17a0fa8c-4654-41c3-8b35-bd0354e5c23a)(content(Whitespace\" \
         \"))))(Tile((id \
         52b41a32-b491-46f5-9545-5eb0dad94475)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         970cf511-7150-4157-ae8e-31d64a1c402a)(content(Whitespace\" \
         \"))))(Tile((id \
         99091b95-6e1d-489e-899f-2d66094156b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e3365381-0710-418e-ac3f-d5627b6d5b72)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2eadd172-80bb-4aad-9242-f7a08dceab4a)(content(Whitespace\"\\n\"))))(Tile((id \
         ec193d38-04a6-42cb-8db3-c65d5a1dd3cd)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86caf7ae-3f3c-4b6b-bac8-564bbdd3814d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         adbdcc70-646f-4ff6-81f4-294fc8e6902a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c82d62f-885e-464a-8749-e5cc7f9b31e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b870391-df03-4fd2-8f94-ecd8f793e7b1)(content(Whitespace\" \
         \"))))(Tile((id \
         abb418fa-47e0-4d88-9557-8ee7deb52212)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32096007-3cbc-47cc-aff1-82e2f14f437c)(content(Whitespace\" \
         \"))))(Tile((id \
         25a129a9-f21e-4898-b01a-05bd2d083f5b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1da0d75e-6d59-417e-ac1f-df924c70548b)(content(Whitespace\" \
         \"))))(Tile((id \
         951ae24f-3b99-4be5-a474-6ba5e5f71d99)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59ac9497-1ea7-44fb-b0ab-bd2806835d9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c141090-2182-48bf-81d0-d473e2551428)(content(Whitespace\" \
         \"))))(Tile((id \
         92098f28-a608-46c7-ae46-b570391a4577)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4dab132e-3c5f-48b0-ba3f-48971ca74924)(content(Whitespace\" \
         \"))))(Tile((id \
         cb85ae34-46e1-49a0-a0a9-4ffb0c72f188)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0233e4ee-7956-4f6c-a366-2d765d3c1c2b)(content(Whitespace\" \
         \"))))(Tile((id \
         156ba5b2-224d-46c2-91cd-857460ac87b0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9115229f-0e42-4ada-a8e0-0a3ff3c3e60a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d6876d0-41b1-46a9-9bf9-d4d915e97004)(content(Whitespace\"\\n\"))))(Tile((id \
         9e37862d-65c2-40a2-8536-ca2823be16f0)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f14ab82-5b42-43db-a798-01620b4e9d35)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         89b4a109-00f0-4948-8f46-c39b39c8277f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b402ec5-443f-4be1-b66e-e53d126d47b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9df4221-ccfb-46c9-9bf6-b90e929b3e27)(content(Whitespace\" \
         \"))))(Tile((id \
         c79b22e6-0395-4a11-8db1-85746c350f96)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         511022bd-6c11-485a-b8c3-4eeb4bdba7ed)(content(Whitespace\" \
         \"))))(Tile((id \
         577cb60b-c68c-4536-9dba-d2092af44f65)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3712bfd2-6955-42f8-8f1c-87e55aab4b73)(content(Whitespace\" \
         \"))))(Tile((id \
         b22020ac-9402-49de-a4a7-ee52d898130e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c904a9a-1967-490c-a5c2-cbe4fba7ab54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5811f2c-4114-4722-a037-7e05e9df32c2)(content(Whitespace\" \
         \"))))(Tile((id \
         1cdde84b-9e2d-47d7-bd58-e553a95a7a56)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c3a7a5ae-44d4-48d6-9e56-3db719328218)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cbe92d7-d133-4d6a-a96f-8cd6f907e43c)(content(Whitespace\"\\n\"))))(Tile((id \
         aefbc820-9daa-4fcc-bc60-09e69b2ed468)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e010f06-d3fd-477b-a17e-25be0ecf34e8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         56a3c04d-4658-442d-9375-361045654190)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af150c69-4626-4f40-994c-9ac6464242dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9125d1e-c123-408a-8c69-7c24e6399a33)(content(Whitespace\" \
         \"))))(Tile((id \
         5e84e08e-4975-4d39-980f-0b6d3a187cb7)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17b22cdd-4666-4b7f-8ef8-040869320c3d)(content(Whitespace\" \
         \"))))(Tile((id \
         1a5c1ec6-20e6-4216-ac17-1207cdd1f679)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21057062-1307-4e79-b07e-cfc54bceedfc)(content(Whitespace\" \
         \"))))(Tile((id \
         bacfa9c7-ca8e-4e23-ac0c-3b7328d9a1c7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc0eaa63-499c-42f5-8419-acbb551eea7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1278ca2d-2ad8-4259-ae58-22832b672e3b)(content(Whitespace\" \
         \"))))(Tile((id \
         890e370c-7221-46e7-9f9d-a7c306eee16f)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         46fbc855-a1cb-4d16-aa6a-fa3a3fe50ba1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4284395-e4fe-48a1-98b7-4139f3657c37)(content(Whitespace\"\\n\"))))(Tile((id \
         d15bcac3-3d45-42e9-a855-f1ca3181a1f6)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c275fa4-816e-495a-b880-8e71d7d07e73)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac588fab-9d60-43b7-8bc8-60e2b0be7003)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         964a6781-3a0f-4b62-a777-e39b52f34f74)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e42d10c-9a7d-411d-a407-7ffb5f5e1e71)(content(Whitespace\" \
         \"))))(Tile((id \
         83e47f73-cc41-4b23-9048-91766b969c6b)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74f4465d-155f-42e0-9165-4158a4f90364)(content(Whitespace\" \
         \"))))(Tile((id \
         e77100c8-7fd4-41d9-8a43-489d460175d1)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b2a2071-e16a-4735-bbf9-c6c45e0a926f)(content(Whitespace\" \
         \"))))(Tile((id \
         bfb15641-6b84-49f1-9802-edfe7d018b7c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42558f81-c66a-452c-9e72-837a38bec34e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4902c30-c1f6-4847-a8ea-edbae243f540)(content(Whitespace\" \
         \"))))(Tile((id \
         4cc37efb-2e84-4b34-877c-de42f8fef720)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         826cc9a5-c746-4ebc-bd07-9613a1c3818c)(content(Whitespace\" \
         \"))))(Tile((id \
         c52e4dfb-013a-4874-9f10-66d7e06e6e29)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         067bb896-7ec7-4a17-9444-05115ad9b78c)(content(Whitespace\" \
         \"))))(Tile((id \
         bb42151d-be39-468d-8932-32b3297b9079)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4e090f20-7017-4257-a1e8-a31b3194b7da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a29ad7e5-1441-4770-8909-355de40bf9a2)(content(Whitespace\"\\n\"))))(Tile((id \
         0fe8616d-ffa6-4e80-a2bf-02c8073de27b)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         751f7f8b-bc4c-4799-92eb-b622a09777ec)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6c5c8cd-2e7c-47ea-8f48-6923cf417a41)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16156c8b-6a48-42c0-8580-d7324e516f88)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4346a65-585e-4b2f-8da6-17dc32835aca)(content(Whitespace\" \
         \"))))(Tile((id \
         1b2a0cab-07bc-49b1-9633-a290670b1799)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed9762e7-6a77-4629-acc1-cf7e208e3119)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         825d1125-21a7-4fbc-88c0-c85098c23ff9)(content(Whitespace\" \
         \"))))(Secondary((id \
         89738710-66b8-4e55-8400-dbd5b1407df8)(content(Whitespace\" \
         \"))))(Secondary((id \
         f1a18d9a-41bb-4bd0-83b6-d2321ddddc81)(content(Whitespace\" \
         \"))))(Secondary((id \
         b06367df-8fe2-41a7-a258-fe0047cd97de)(content(Whitespace\" \
         \"))))(Secondary((id \
         c1ba4ae6-9b79-434a-8615-63a30809d195)(content(Whitespace\" \
         \"))))(Tile((id \
         ba2b7fef-c3cf-4fe9-b802-590241900788)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91834f26-6804-4d1a-9a17-b4e6ec46f995)(content(Whitespace\" \
         \"))))(Tile((id \
         732b5712-cad6-47ff-9aab-936fc1403ff1)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bff5ea5f-1ae9-454d-85fc-ff0b34633110)(content(Whitespace\" \
         \"))))(Tile((id \
         139d17b5-c112-4aff-837f-4d078dea73fb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f6451622-3e2c-4608-9f4c-eb3cc1ab4f8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59157d7a-0ee3-46d5-8516-3a91a1e75e32)(content(Whitespace\"\\n\"))))(Tile((id \
         74b6826e-b24b-4e1b-bc98-4c4a86463083)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09dfe8eb-03da-4fef-9123-61e43f49bd5c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2de0f055-6c11-435a-888c-35a9841e69ef)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         36f575fc-e7f4-4be2-8238-4d179e7c714b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3337bf3a-bcc2-4a5e-b84a-0e319229e433)(content(Whitespace\" \
         \"))))(Tile((id \
         9b2eab06-6ae9-40c4-902c-d32c3737ff9a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2ced86b-d061-4e87-b984-8deedcd0f321)(content(Whitespace\" \
         \"))))(Tile((id \
         50f7d938-3bd8-4d7f-bf12-ce87375a5169)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccd55084-723a-4b26-8692-3410e924b1a4)(content(Whitespace\" \
         \"))))(Tile((id \
         08ad41fa-4c39-4633-8c6b-e42bf3bbe804)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6f5a962-33c8-4183-95c0-5e670c35162d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eab8f706-4836-4743-8404-00f0b397174d)(content(Whitespace\" \
         \"))))(Tile((id \
         22093736-ec3b-44c6-b220-ee75964c7702)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         367f23e0-1b16-4001-8ddf-ade124fc69b0)(content(Whitespace\" \
         \"))))(Tile((id \
         8b79928c-bc7d-44f5-ac2f-f315c65e5cc9)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         413b4a71-1633-4bcd-a453-2e5b28a966ef)(content(Whitespace\" \
         \"))))(Tile((id \
         45d34c54-af4c-4a3e-8380-67bcbd9ab771)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ae0ce2a1-9423-4179-bee9-6b4e19321cf3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9e6fc2b0-b368-4188-8696-c823e03296e4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c219ed71-3633-4aad-902d-9b9ffc3b676e)(content(Whitespace\"\\n\"))))(Tile((id \
         c2af50a2-9170-4a93-b2b9-13ef876a2f5a)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78aaf90f-f6c8-427c-9068-1adc153e5c90)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9100cf19-3974-4ab1-a1f9-b2d70b24430f)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed4c554f-bb2e-42c3-b2fb-f338af90da7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b2075a22-de34-4416-bb28-00e60be6a1a5)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7666630-0e4e-4d97-8845-4d7925e0e93e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1642401-6a52-44b0-b7f4-802b8690409c)(content(Whitespace\" \
         \"))))(Tile((id d17cb800-a9d2-49dd-8dbd-450dc9ebd6df)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1bb3d7bb-25c2-487c-a2a9-4b18dbc96dff)(content(Whitespace\" \
         \"))))(Tile((id \
         f9fd3b46-e4c5-45b3-b515-fcf6d6ae34d9)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5dadf471-40aa-4cb5-8bde-f1241003862b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e899dbb4-2081-4ede-95b2-32b82046b574)(content(Whitespace\" \
         \"))))(Tile((id \
         76da8fd0-1d72-4722-b6dd-76a8f71d693b)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08cdda91-5601-4566-a572-007cd0ffaa1c)(content(Whitespace\" \
         \"))))(Tile((id \
         74251f4d-4c72-4925-9b71-3f9344e459e8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0ffe7d6-0700-4620-a06d-a75cca7fc82b)(content(Whitespace\" \
         \"))))(Tile((id \
         756412bd-b3f2-4328-a4de-44d6b3fc0ba1)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         72238c0f-05b1-4805-b5fc-acdaabfc73d3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f8ae8dea-9f56-46fc-8031-acf0d729d0ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         30e0a27f-9ae7-42e5-9675-ed4e41eff7f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         97136b71-5500-418a-9627-7d4157dfb6b9)(content(Comment\"# Apply Game \
         of Life rules to a single cell #\"))))(Secondary((id \
         9dc730b6-49be-4800-97e1-c68560d78e9a)(content(Whitespace\"\\n\"))))(Tile((id \
         331c54e4-2164-4405-9e81-f8fe0a1510be)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         12710716-c875-4f42-bfba-fab7e5ff8903)(content(Whitespace\" \
         \"))))(Tile((id \
         73901775-0e2d-4ebd-9b78-0e7f784ed606)(label(nextCellState))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         375ebba2-d598-4881-86cf-2512be96056a)(content(Whitespace\" \
         \"))))(Tile((id \
         14f937d3-45d7-4329-af9c-52a5fee169a2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb3462ac-9e37-43b2-a299-9b95a0dedbeb)(content(Whitespace\" \
         \"))))(Tile((id \
         5400d127-576d-4324-b912-175f4cdb32d9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         a1617ae8-51c7-48b2-8554-b58fb4ca3287)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ab701c3c-a507-4b74-b64e-f94d08830eb9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         59260e61-40db-4b95-b8bc-2d81366a54f6)(content(Whitespace\" \
         \"))))(Tile((id \
         c446ddb6-b9a8-4e33-880e-c0fe017cfc3f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ab3c67d6-b453-4c33-aea5-0c1ba48c1a37)(content(Whitespace\" \
         \"))))(Tile((id \
         055ea3f1-c016-4b34-bd74-426eab29050d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4bd054f5-a3ba-4aa0-a3f0-2871d93f44bc)(content(Whitespace\" \
         \"))))(Tile((id \
         60b9d671-36c8-437e-b93f-73dd1ad8ae2f)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3d439513-9127-4548-ab50-72c055779449)(content(Whitespace\" \
         \")))))((Secondary((id \
         1032c4ae-aeeb-429f-a828-7ff1794d98e2)(content(Whitespace\"\\n\"))))(Tile((id \
         c9946b68-e6e1-4283-817f-901a3f32586a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cb5acf1c-c861-4d34-a29f-797e5d040e19)(content(Whitespace\" \
         \"))))(Tile((id \
         e1ee947c-f55a-495e-bf2f-9cd6f0c039d7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b7636c26-9347-44fc-bc9a-bdafe854ea8f)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         29caae39-32e2-4069-96cc-fbc6bfb0ab89)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b3885c32-551d-42b1-b3cf-b2fe4fdca6ed)(content(Whitespace\" \
         \"))))(Tile((id \
         e8df5a25-dc5b-4055-8e2e-792fedbfbd29)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c6e67cc7-e693-4d74-bc31-d75adf724952)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b445743-2413-458b-bedc-4ab5fc628621)(content(Whitespace\"\\n\"))))(Tile((id \
         012280cd-e099-49a4-8815-4d157dcf49b5)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3c4956cb-f6ce-4016-ab45-8e4313b2c55a)(content(Whitespace\" \
         \"))))(Tile((id \
         29bc4769-cfe8-42c0-b4ef-d37427b74dfb)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c584ccf-4943-4b07-9ce5-b4f4ba7b3494)(content(Whitespace\"\\n\"))))(Tile((id \
         be7c9838-f57b-4124-bad0-6d5fa97d3dde)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ae01a220-c981-41ba-85fd-f40fff173fd4)(content(Whitespace\" \
         \"))))(Tile((id \
         0d19b669-e3f8-46eb-bc6c-a0e35c0d2c20)(label(Alive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         824875b2-def3-4872-a70c-9138916b842e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         22aef063-6463-4ed6-8593-40f3ec7b55d5)(content(Whitespace\"\\n\"))))(Tile((id \
         1275a804-dfab-4648-899b-243378ac5b83)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         141c8c16-f4f5-412d-b8ec-005313bd671d)(content(Whitespace\" \
         \"))))(Tile((id \
         caa85105-9182-4846-8c83-7471da571531)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f6d94990-1d49-4f82-91fe-6c0de1d0cf4d)(content(Whitespace\" \
         \"))))(Tile((id \
         17aaec29-4966-423c-b685-a1b3fd66e76a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5efa7f80-220e-453d-b40b-988f204dae1b)(content(Whitespace\" \
         \"))))(Tile((id \
         ecfe036d-aa95-4fd2-84fd-52ae00ab06e1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         79ccfe8f-49f9-4040-89ed-f3170aaa7d53)(content(Whitespace\" \
         \"))))(Tile((id \
         24ec0af0-41f4-47d7-a580-94f2f394a7a1)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0eef054-8b45-4f07-8c5d-da5d731e25dd)(content(Whitespace\" \
         \"))))(Tile((id \
         73cc7cda-1bc0-4d67-8280-9987508b3abd)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a82c633f-3877-49b7-a3ba-1fefca4bf8a5)(content(Whitespace\" \
         \"))))(Tile((id \
         73a46141-4f03-4561-96b5-e33d04719be6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fbf0a7f-e84c-4b13-b2e6-dc38ce4e75f8)(content(Whitespace\" \
         \"))))(Tile((id \
         f3df5c29-562c-48be-bb3d-18b9f1d7ec0e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a64fd6f5-ea0a-4941-b0a7-9134cf586771)(content(Whitespace\"\\n\")))))((Secondary((id \
         67bfe210-83ba-48d6-8ffa-d04203c8f429)(content(Whitespace\" \
         \"))))(Tile((id \
         c6b61178-031a-4f6a-8238-39a3458905c5)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         855bbc63-e0e1-43be-93db-6fe5fe6d837a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2bf18dfe-7140-4436-9e74-eeb1458e0d5c)(content(Whitespace\" \
         \"))))(Tile((id \
         090c2b27-b908-4080-8783-b05dcda20d06)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2e05d6f-72ae-411b-a9d6-175681fec2b8)(content(Whitespace\"\\n\"))))(Tile((id \
         3e301495-1638-4391-9870-d6d68d8410d0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         68e9ec25-5b79-48e9-a49c-d51963cd668b)(content(Whitespace\" \
         \"))))(Tile((id \
         8c274333-bdd8-47c0-bdc3-0816e48aa646)(label(Dead))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c74deaa0-d7e9-4f4a-8619-f6ba2afa3b3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5e35f74d-124e-4d71-83ba-8f12c825ca70)(content(Whitespace\"\\n\"))))(Tile((id \
         91fb4d0f-36dd-4b95-8534-a8a857146aa6)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         af3a3570-9851-46fe-88a0-1ad0e196279c)(content(Whitespace\" \
         \"))))(Tile((id \
         f679c9ae-a82e-4d59-86a3-7e1657e959ce)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c45b2bf2-2e67-4cd4-9030-378f642962e7)(content(Whitespace\" \
         \"))))(Tile((id \
         4b56620a-3d08-41d3-a995-3ffedc3f359e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec1597a3-02f3-478d-871b-4cf4244728f0)(content(Whitespace\" \
         \"))))(Tile((id \
         3a6d17a0-0229-48cb-a70e-43abca73cee4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         683b1fde-4f31-4c0f-81d6-64b41181502e)(content(Whitespace\"\\n\")))))((Secondary((id \
         58e6efd6-86da-49e1-a77f-5263fe4002de)(content(Whitespace\" \
         \"))))(Tile((id \
         f1bd25cc-8b99-4444-8cf8-1b6ca350e738)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fdd8f25f-c54b-40f7-8d50-069acdce9512)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dace08ac-51b6-46b1-9c75-fe66dfba5c8f)(content(Whitespace\" \
         \"))))(Tile((id \
         2cbd5238-4670-4890-a7e3-6161a4382df2)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6238cce0-a5af-475f-8d0c-b2a65f9da00d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7c3a5492-5261-4e82-83a6-febcbc461b8d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         23402164-817f-4d46-b40f-95f180e2c92f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee0400d9-24c5-4989-92c7-37cc778467af)(content(Whitespace\"\\n\"))))(Secondary((id \
         83186578-9ed1-44bd-9927-d519c75d24e5)(content(Comment\"# Step the \
         entire grid (simultaneous update) #\"))))(Secondary((id \
         8d158517-3870-46ba-9322-439e671ba361)(content(Whitespace\"\\n\"))))(Tile((id \
         9e1ef1e4-e91c-4c3c-8a6d-ab87aa482522)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a828b9cf-84be-4290-9b11-336f9d5a8a61)(content(Whitespace\" \
         \"))))(Tile((id \
         11a6c670-8768-42ab-81db-6643b62996b9)(label(step))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         57772223-6b33-4c2a-bc9d-1a8e0a6fd5e7)(content(Whitespace\" \
         \"))))(Tile((id \
         86d7ff01-82bb-4407-b816-c01e32122061)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3b87d47d-556d-46f0-9ab0-75186ca4308e)(content(Whitespace\" \
         \"))))(Tile((id \
         ad608eac-2dd3-479d-9ff1-62e4b915e217)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5e2c271d-6da7-42a0-9b41-00d7a0c9ded6)(content(Whitespace\" \
         \"))))(Tile((id \
         a8feda78-11a3-4f36-bf5f-75cc93fba847)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2854449f-12cb-431e-b8cc-bf60b6c7c2c8)(content(Whitespace\" \
         \"))))(Tile((id \
         ee17364a-2e84-4cf7-80bc-92247f75846a)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4d00ce14-5f0f-4cc6-a525-cb3ba7caf6cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         3ef2fa41-5522-4489-bf0c-1d55c9887e53)(content(Whitespace\"\\n\"))))(Tile((id \
         fc882ce6-d395-43b9-8149-30b01faba5c2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9e4a0e18-beea-40a3-a92f-6d00245f9d8e)(content(Whitespace\" \
         \"))))(Tile((id \
         6048ad2e-e02c-4dfe-9059-eb6700782e9c)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3f32ea49-d377-4344-91c6-687d05c4022e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         902adcf0-b65c-4128-8e8e-37b5a58dfe8f)(content(Whitespace\"\\n\"))))(Tile((id \
         f9d3249f-c707-4899-8fe3-2e4504e15c89)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ac12b019-e69d-4ccc-b4d2-be213668ac61)(content(Whitespace\" \
         \"))))(Tile((id \
         13c03c3c-074c-4b67-b2fe-f12851ab1fbf)(label(newCells))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         642099e3-de07-43a2-a36e-20c6cc18375b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a76cc321-1187-4ec7-92e4-26536c64a116)(content(Whitespace\" \
         \"))))(Tile((id \
         0c352ec0-8dca-4343-9022-d71a3f3cbf32)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a604ecc-4940-4a47-a684-c4803b19eb71)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         27e18787-8df1-413f-af41-25e5b47a6bf4)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89a972a8-5515-496e-8995-7a3feff2eef7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         78d3343e-2d78-4ed5-a939-76ec603b4836)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c2a27ce-e762-4576-b9af-8e8aee8ddb82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07cd3126-be16-46b7-82f9-b577abf193ca)(content(Whitespace\" \
         \"))))(Tile((id ea26b183-e7dc-4d72-8caa-3bcd6a0b8d8f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         42dff936-f663-40b7-9e4b-dce2476d2e19)(content(Whitespace\" \
         \"))))(Tile((id \
         c753a9bb-ccbc-484f-9578-33888f81a166)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         636d42c4-e0b5-4e03-9829-ea8b8cdae7f1)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8531564a-4fba-4732-abf3-5bd8ba9fe281)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a90479f8-e131-4801-95e7-5f482fb70d0c)(content(Whitespace\" \
         \"))))(Tile((id \
         eea914e7-47d8-445c-a2d8-8c9e0974c03c)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c8a6b05f-6d06-4b99-88d4-43de4d02cc61)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1db9bad7-5349-4593-a187-407049cf1ecc)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f9c097-074f-4e5c-8c60-65dcebdf016c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         410413b6-1b44-4bb4-9210-c588825aeae4)(content(Whitespace\" \
         \"))))(Tile((id \
         76ea9361-6abe-4b1b-844f-b6f10accfe9d)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         02b6e0f2-c03b-4028-baea-5ed1c07aa906)(content(Whitespace\" \
         \")))))((Secondary((id \
         111604f6-b75d-4ee0-87eb-0dc87506eeb4)(content(Whitespace\" \
         \"))))(Tile((id \
         3e15c76d-3228-42b2-bfed-c7ef8ad9e1a5)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         70e28a5e-48a6-4873-b6f7-5ffd16d33548)(content(Whitespace\" \
         \"))))(Tile((id \
         20195efc-f963-45c3-afdb-1c42a5e0f87e)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fedc2228-b36d-4a6f-823c-ada323c43bcf)(content(Whitespace\" \
         \"))))(Tile((id \
         3572fdf4-7ac5-44bb-847f-e0def20b2c21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9d335c0a-fc96-4613-8ee1-5e5b30ee7fd0)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1aebd9e-f17d-4de5-9e51-5fcf2561df83)(content(Whitespace\" \
         \"))))(Tile((id \
         d0eed099-5d93-49b6-974b-3888d2e41172)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce9c9eea-1919-4f1a-a61d-f8047cd5656e)(content(Whitespace\" \
         \"))))(Tile((id \
         8d80090d-fb6c-425d-b717-fdf864d2d87d)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2db9513-471a-4713-ba0a-25e24b461ef0)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5628559b-c052-497e-9536-c5115c38f888)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         87a33499-4152-4a0d-a65d-3f5442819737)(content(Whitespace\" \
         \"))))(Tile((id \
         e1ccbe4e-1709-4099-8a7f-97e6c9ff7ed3)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bb03d17-1e5a-4348-bed0-90e892839763)(content(Whitespace\" \
         \"))))(Tile((id \
         a453807a-ddad-4f60-a859-abf64772ec18)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8367166c-a457-47ad-8678-dbb853935fcb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b67398b7-60fa-4512-b64c-5f75d0d47d75)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d46c6395-64ff-47bc-9958-a952c65ed14d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5d77e62a-fa96-4d99-84a7-1eba908d8487)(content(Whitespace\"\\n\"))))(Tile((id \
         8588509a-cc2f-443d-93bf-afea4ab34bdc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7863b0ba-e376-426e-9ae5-11820a89ca54)(content(Whitespace\" \
         \"))))(Tile((id \
         bda12dff-1bd6-4761-8c5b-22e96255ccad)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9a08a8fa-c00b-4ca9-bde8-f8fb82817d94)(content(Whitespace\" \
         \")))))((Secondary((id \
         7d235ed4-c499-4748-b4d3-6525e3351824)(content(Whitespace\" \
         \"))))(Tile((id \
         8d6830b1-fcd8-43b5-8003-c5625424956e)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef12fc3a-fe04-4ed1-a6f5-005b86c4f670)(content(Whitespace\" \
         \"))))(Tile((id \
         dae1d57d-3666-45ee-8bdf-f683c2e206c5)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eca22c9b-f484-4cf1-a213-2250c3184a95)(content(Whitespace\" \
         \"))))(Tile((id \
         5c7d68a8-3457-489c-a1df-6b6c3f444e98)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b08045ca-ac47-499f-ba19-898d59b10f26)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a9bde67b-0aff-4ad2-a94c-c0e7a75fe5ef)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9497d967-8ee4-497a-b49b-a2771e23ae08)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d750f893-ed09-4f52-8ea5-33a89293d9e3)(content(Whitespace\"\\n\"))))(Tile((id \
         30c2edad-3705-435a-8c5c-62974b18064c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         85c6da22-1b4c-4472-9e0d-06c3b5cf3996)(content(Whitespace\" \
         \"))))(Tile((id \
         4d072d87-7b1a-486c-b5aa-63b408423612)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8cf87713-1893-4e2c-aded-997a5f3af119)(content(Whitespace\" \
         \")))))((Secondary((id \
         207f6aba-768e-4b4f-aeab-1e530dc907f8)(content(Whitespace\" \
         \"))))(Tile((id \
         befc7bb5-286d-4458-a9b2-ace8a264ad6b)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b639cd20-b82c-4141-9336-3f6cce0daec9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3bae036-17e9-4c10-beaa-312e8b12a9e6)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2891628a-9a24-47c7-a6e0-73b794b807f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c0bb4f5-28c5-4695-8c7a-b54a3ba6fc3e)(content(Whitespace\" \
         \"))))(Tile((id \
         5f528523-e15b-4487-ba49-befe09126e45)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8925291b-b4c2-4949-80d0-c38f6d8ddc9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4464ffd-3bf8-4f51-8f66-fa389b1d830d)(content(Whitespace\" \
         \"))))(Tile((id \
         c8d84269-6d4e-4f87-be95-de597daf347a)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         79a78798-bc54-41c3-aea2-ccd5d6bcad85)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         719e94b2-dc7a-4a0f-bfa6-35dd11eefc46)(content(Whitespace\"\\n\"))))(Tile((id \
         173cab97-1634-475a-b2a6-971e80dba5cc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         adde3be1-4ea1-49e9-91b2-279e8dcf7c8a)(content(Whitespace\" \
         \"))))(Tile((id \
         0baebdad-eced-43d5-8926-cf36b8d626c3)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         85c079f0-f3c0-4a03-b1ac-b4f30f588d8e)(content(Whitespace\" \
         \")))))((Secondary((id \
         20aca2a1-3d83-4fb3-8cd5-f587a1da75d5)(content(Whitespace\" \
         \"))))(Tile((id \
         c86ca761-6409-468c-ab06-1ceca148fdaa)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94a440df-1e2c-4a64-a0f4-5172649e50a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4df00ea5-46f9-45f5-9776-38006263f8c2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7e08480-705e-4f6b-994a-6e5e5e74aa03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d49f39e6-9228-493f-a199-38eca87961ba)(content(Whitespace\" \
         \"))))(Tile((id \
         3c623eb7-bc2d-452e-baab-a29fdc304463)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca777b3c-8f8f-4da5-9967-eb32e8eccdcb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0802e3b8-6c7e-4da7-a5ed-a66b96b17e12)(content(Whitespace\" \
         \"))))(Tile((id \
         2329d2d4-ba42-4545-a518-695bca3315fa)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a97bbe00-9338-4535-8c3a-a5f177113a29)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ed33fa96-4010-4501-8757-874da8dd266e)(content(Whitespace\"\\n\"))))(Tile((id \
         51747f5b-7c27-4dd0-a210-78946090ecc1)(label(nextCellState))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c17069dc-04b7-4f80-992f-aae605044007)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         78743f90-2951-43e8-b3fd-ca548dd0f99b)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a51a10bc-7aca-4261-88bf-01d920bdf77e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30ea4c40-ed93-4baf-aa68-f13aa149824a)(content(Whitespace\" \
         \"))))(Tile((id \
         a091a71a-8851-4652-b0f3-fe6aff7efb6b)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         166b4d1d-2566-40a1-be33-770472451f9a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e289cec7-420a-486c-a2c4-0ff7c6dd7ba6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0d91d77e-10d7-4da6-82e9-7d551b3b5838)(content(Whitespace\"\\n\"))))(Tile((id \
         e47ceb10-b1d9-46bb-b807-b0dffa36d602)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6dc7533a-4d89-4087-8805-3f93d1cb2f99)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8fbcb48a-e2c0-44e0-9ccc-a3f146f07ae3)(content(Whitespace\" \
         \"))))(Tile((id \
         e5d47828-ba88-4e7a-b578-c30f5e159a13)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec6d74de-e80f-40ba-be3a-9858058916da)(content(Whitespace\" \
         \"))))(Tile((id \
         2d4b748a-c03d-4dbb-a0b4-62893ccd2217)(label(newCells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97d9d663-54b0-4950-8b6e-d1c096f9f2ad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efff2c5a-a5e4-483a-a0c8-6311c93ced9e)(content(Whitespace\" \
         \"))))(Tile((id \
         217e12f6-01ac-4b3d-81c0-d3fe4ab645e7)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8d311bd-280f-4365-a127-f6a8d8dfe52e)(content(Whitespace\" \
         \"))))(Tile((id \
         24c52d53-ac20-419e-bb4d-2a4faf264887)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b3f10d4-95e3-405c-a4f8-73c804c7fdcc)(content(Whitespace\" \
         \"))))(Tile((id \
         eb509bab-f5a5-4d52-b86d-6825c6fb7b9f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aaa10c3a-c269-4543-b681-0435ebb0b8ff)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3252bae7-88bb-4962-9263-b5038d8593c9)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65f86427-9525-4d3d-bd2c-fca4337c1935)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a3bc15d-bf45-4683-ad08-4a19beed9e52)(content(Whitespace\" \
         \"))))(Tile((id \
         b9a74a23-2cc5-4e67-ae45-236507323a53)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         acaab618-1f3f-4e6c-95eb-b635236f6837)(content(Whitespace\" \
         \"))))(Tile((id \
         227c251f-f2bf-4f97-b445-73727635d96c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86a676a8-e787-4afb-94c4-15b2a5bcd522)(content(Whitespace\" \
         \"))))(Tile((id \
         23ef70bf-80a9-488c-9f1b-4f09a3f7af0a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2128ac33-4d02-4207-ad7c-d20ff5c9213f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e8a76d09-fcf3-44ab-a47f-3fdce845b8b9)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d95c9eb4-c1c1-4091-a9e9-6ed51d9ea3f5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         352da1d2-f808-4445-94c8-6008db99d34a)(content(Whitespace\"\\n\"))))(Secondary((id \
         52abd385-3a43-4311-9619-a52bf703d7e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         89bccea0-5467-4349-89f6-7568fd7e9f1d)(content(Comment\"# Run n steps \
         #\"))))(Secondary((id \
         f9cb3b1f-03bd-4846-bd8a-1838b2979ce5)(content(Whitespace\"\\n\"))))(Tile((id \
         eb10609e-1347-40f4-9e51-b6b1dd14206f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb7a9c62-c8a2-4f0d-90e8-8a9d52aad2e9)(content(Whitespace\" \
         \"))))(Tile((id \
         e25e2e80-1b7a-42c6-8935-252acc66b51f)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8c35ed34-3335-4117-86b7-74cd519c3341)(content(Whitespace\" \
         \"))))(Tile((id \
         d91dfecd-6fb6-4f98-a1fe-2dc375d03013)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         054e7df0-5d76-4106-909a-6bda45e9552e)(content(Whitespace\" \
         \"))))(Tile((id \
         11b87805-7312-40fc-abd5-f8d185b52057)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3b6e1572-a06a-4fc5-ba70-fc7554e106b8)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         febcf098-ef92-4f57-b5c8-d3821dffc156)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6fd05a82-97be-4014-b150-8a54ae4c7143)(content(Whitespace\" \
         \"))))(Tile((id \
         02a172fb-e3b6-4e2b-aa3a-062bd57d601c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5118e049-a816-43dc-9c2c-549332335a16)(content(Whitespace\" \
         \"))))(Tile((id \
         2950f4db-1ece-4c42-998d-e998f6eaeb2d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b10e277-4356-44d0-86b9-6b7274c6cbed)(content(Whitespace\" \
         \"))))(Tile((id \
         183a276c-ec03-47d8-a2f0-ffff403c76d2)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4ace9dc6-b9bb-458e-b4e8-1c3b9b1dec93)(content(Whitespace\" \
         \")))))((Secondary((id \
         ed90bb1e-e080-41ee-938e-be4dbd8947de)(content(Whitespace\"\\n\"))))(Tile((id \
         c218e28d-b1fc-467c-878d-c554cd3ade19)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0f20ece6-ca80-4dcb-b5ff-b9513e768e25)(content(Whitespace\" \
         \"))))(Tile((id \
         c6cdd3df-f58e-4c9b-9a9f-600af670ddea)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         deaf8d15-c29e-4fcb-ae72-0542653da435)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         84febc95-5ca1-4078-9b0d-6a8827a43f1f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7e90723f-69b3-4a6e-9e13-194f0d605cc8)(content(Whitespace\" \
         \"))))(Tile((id \
         71df6e93-c5af-47b5-9626-b3938831975f)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2a12e4a7-78cf-43d5-834a-476e2ffb9729)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         09cff7a8-e883-4475-930c-08077b949b93)(content(Whitespace\"\\n\"))))(Tile((id \
         746a5432-ac83-4dab-ac3d-f3961495a29d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         236a68e8-93cd-442b-b24c-7e732bcd5c63)(content(Whitespace\" \
         \"))))(Tile((id \
         1ca8e9ed-3d67-4e9b-a6f5-3e606266aa86)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea9ea219-63f6-41ef-bad1-095f8d56cf6e)(content(Whitespace\" \
         \"))))(Tile((id \
         63f08cb5-e3e4-4002-b1ac-3426fdfdd19a)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22c74a08-a0fd-449f-a0c8-1b354275a552)(content(Whitespace\" \
         \"))))(Tile((id \
         987777a4-0b78-4bd2-9781-7ae47177174a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18417b48-3430-4db8-8b45-7d0cc1dc2a66)(content(Whitespace\" \
         \")))))((Secondary((id \
         79087513-855e-4719-8c1b-13639e2d1c92)(content(Whitespace\" \
         \"))))(Tile((id \
         9cbc488b-109d-43c3-9ea8-b6bb5d73e26b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24126052-5bfd-4327-836a-3ff4551ac8f5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e6c6dd6d-7268-49fe-912a-58990dc32615)(content(Whitespace\" \
         \"))))(Tile((id \
         7e1e2923-70dc-4377-ae19-30406654f798)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8650fc4c-e355-4115-90e1-7640ea5c5f5a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         92d7dd41-58b8-4683-8150-af39cce000c5)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59d3d991-d7b9-411b-8a1d-c33b8b49cbb7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         70a5d29a-1a0a-4fb7-80cf-83377aab3b99)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ab93d6a-7160-4e4f-9a61-65ea1e1ad2d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a92d8f13-cf75-41f7-8852-635e842e2a62)(content(Whitespace\" \
         \"))))(Tile((id \
         e3f14293-72d2-4630-84d8-aecdbf8a9047)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         124c200f-784e-4174-9485-9884608a5aa4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92153512-1b62-493b-a77a-2a718c735ef5)(content(Whitespace\" \
         \"))))(Tile((id e7fdb5a0-18ce-4d26-bc8f-6e81844cc2ec)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cdaa9b26-9322-4826-ae5f-60d41955dd3c)(content(Whitespace\" \
         \"))))(Tile((id \
         c19064d9-5245-4f62-8876-30dd89f4c108)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         83fcd461-4f77-4e0d-9330-dbc7b7b584ef)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         03ae60b9-72fc-4fa0-a7bb-fc2042999248)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6f8a4402-c9da-42e9-ba6e-7637ac5d991d)(content(Whitespace\" \
         \"))))(Tile((id \
         f591346b-a71e-4845-a852-d00f4062ead2)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         bae58ab8-605c-4c40-9958-99acedb64a9c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b56900e-9201-47d6-bf74-f7c96f5f7495)(content(Whitespace\" \
         \"))))(Tile((id \
         7ebe9769-2bca-42d5-b4ad-c5160f1328f1)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81301f66-5fba-4ec2-90e5-a3c928c47220)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1495012-63e3-4d7c-8192-937b01fee05d)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e5d70a4-12c7-44d1-ad06-f99cac3da672)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f425b3c3-9754-4ee0-8232-8804a344c4f5)(content(Whitespace\" \
         \"))))(Tile((id \
         9344efc8-a91e-4fd3-a6fc-4ffdd3114123)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0b484bf7-cbfd-4bdc-89e1-4252c2efcbd6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a993b593-cb5f-4545-9d80-d71b7c6096fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfc24dea-c8c6-4189-b7a6-8b9a82301f88)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef3f8bc7-f611-46da-9cb1-30e5c69548c3)(content(Comment\"# Helper: set \
         multiple cells alive #\"))))(Secondary((id \
         c58ceeb5-528a-4274-96c1-7a9378b903e6)(content(Whitespace\"\\n\"))))(Tile((id \
         152ac0f7-54e3-4b2e-a8fd-c86aa0e8a320)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e2a094e6-5cbb-4a89-9030-1d68b132cd66)(content(Whitespace\" \
         \"))))(Tile((id \
         003dba62-f063-43ec-9ed6-7669f6dc3223)(label(setAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1e25a5d7-e200-4dc3-97ae-156a80e4c56b)(content(Whitespace\" \
         \"))))(Tile((id \
         e79c56a6-613a-4eb0-bfee-3dfa24b0eaa6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ca61813c-58d6-47e5-94ae-35d4630f3266)(content(Whitespace\" \
         \"))))(Tile((id \
         f7c377ed-2515-4735-b77f-7243daff4e2a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         2b0841e6-480d-468f-bdaa-a3fcf72635fc)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         68f2e3d8-4f60-4996-b18e-1f8e5b764b45)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eb5e4d9d-8847-4827-a3da-3dae5d74838b)(content(Whitespace\" \
         \"))))(Tile((id 23f6ccfc-5b5a-471d-8639-066720781a98)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         26aea3ed-c22b-467c-8d9e-471f06cb91dc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b7b88a8a-ada7-41f7-a360-bd05bee47097)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         90fc749c-f780-44a7-8882-a5a179cd1d9e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eb8bccbe-361f-4562-9839-eb802832fab0)(content(Whitespace\" \
         \"))))(Tile((id \
         845f2433-ce4e-46a3-9ef5-ad285bfa8d18)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         07d4be95-b893-4710-9ee9-4b9df827541e)(content(Whitespace\" \
         \"))))(Tile((id \
         c77446df-c69b-4b90-b70a-288f5dfc6b4b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb513bd3-2520-40fe-b3f7-7a348d704fed)(content(Whitespace\" \
         \"))))(Tile((id \
         a1f987a0-34b9-4030-b0d3-b413936d648d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         365d9d8e-766c-498b-ab1d-95bcaf4f5ad0)(content(Whitespace\" \
         \")))))((Secondary((id \
         8d85bb4a-e9a3-4bab-8957-d95d823154ea)(content(Whitespace\"\\n\"))))(Tile((id \
         6b719581-36d8-45a7-b3df-e8879953cfe1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ae1a9bd0-d346-4916-8912-53766acb8f3f)(content(Whitespace\" \
         \"))))(Tile((id \
         50de166b-cae3-447b-a849-01febdadfb9d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d7d62574-6dd5-4f3e-a442-d54e15571549)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b178a89a-31ae-4254-a266-449084d3fbbc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         552f7bb1-4516-4c8c-bc9f-f583b263c4a6)(content(Whitespace\" \
         \"))))(Tile((id \
         c3c5c37b-e184-4fd5-a4a2-82fd5f4ee9c2)(label(coords))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         70f7c5c8-f30e-4acb-ab2d-023b442b464b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b27f9396-74d6-4c70-9818-0d039a3e470d)(content(Whitespace\"\\n\"))))(Tile((id \
         e1d08881-d152-40a4-b365-93716eb2dffc)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb26925b-2aae-4cfb-9ef8-3449c339e0df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08496010-c20e-4cfd-a9e7-48faa2480888)(label(coords))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf540b8f-18f0-4b93-bc70-fba2ce6fd35e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab49e608-d5b0-45bb-9cd0-e30f716d729e)(content(Whitespace\" \
         \"))))(Tile((id f7fac66b-70e6-4494-9b33-f760718bfcc3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         515045f5-9cb1-40a1-a92b-5005f23815ac)(content(Whitespace\" \
         \"))))(Tile((id \
         f85f7f9a-fbf7-4164-9463-c0c5022ccf3a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         22eca6fd-3c26-4402-9099-65639df8ac60)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8986957c-e51b-4752-a396-fe556a4b7ad4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b6da375f-9fa1-4f03-884d-64d53586091f)(content(Whitespace\" \
         \"))))(Tile((id \
         7b293708-491c-44d6-a1be-17e8698201f1)(label(xy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         51088323-c923-48b9-a5c9-5925163f61a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e321307d-ab75-4e7d-b6a5-90eba3b38958)(content(Whitespace\"\\n\"))))(Tile((id \
         52e8049e-4cc3-4132-a110-f7058d62be57)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         58598416-2a61-4693-b4a3-72ae3c614cf1)(content(Whitespace\" \
         \"))))(Tile((id \
         8a851ba3-8fa4-425b-98b0-873609502dc5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a74a0cc4-6677-4263-ba22-4c217fc916fc)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ca563dad-e492-4f76-9539-f71b5c0c354e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f462af47-4ae7-47fb-8400-69fe0a8861c2)(content(Whitespace\" \
         \"))))(Tile((id \
         a91b10db-1940-4119-bb12-d9c7b7284371)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a3851d58-1310-43c3-bd07-21589b407772)(content(Whitespace\" \
         \")))))((Secondary((id \
         5ffd9930-4dd7-4eaf-99a0-4a40cf1bb7db)(content(Whitespace\" \
         \"))))(Tile((id \
         4d080096-25a1-4cac-9620-4b55f8592e8d)(label(xy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         67384f92-7ea6-4273-96cb-9c8cf70f2636)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f80d81f7-bb15-496e-8264-130ab410eab7)(content(Whitespace\"\\n\"))))(Tile((id \
         57ce6c4a-6d72-4ca9-8ed2-60367506385e)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24500a07-84db-49f3-9203-98ceb30ca5c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de25120b-2133-4953-b50c-272838fb3218)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24d2f989-b7ae-4df5-86c8-120329f9c506)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b99b62f-da10-4ecc-b628-3136006d26da)(content(Whitespace\" \
         \"))))(Tile((id \
         9631c736-b9a8-492d-aaa6-6b7675b95a85)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52bc1fc7-f6b9-4a5e-9ee5-7247535c971c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         172fa50c-e67e-4712-923e-29218351c4d0)(content(Whitespace\" \
         \"))))(Tile((id \
         b4d5cd0e-a0bc-4998-a764-f01405b06cd6)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ce63bd5-82e6-40bb-9cf3-fe8277083618)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b609a6f5-c25a-44a8-bed6-249ff011c833)(content(Whitespace\" \
         \"))))(Tile((id \
         b4cde874-f4ee-4986-8505-1486eb787011)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac094a4d-bf49-4a99-bdc5-587d0b30226d)(content(Whitespace\"\\n\"))))(Tile((id \
         1edb6d70-c8b0-4bc9-b977-225a6003bd9d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc658e68-aab2-48c9-beeb-9906065e3c44)(content(Whitespace\" \
         \"))))(Tile((id \
         9178526d-1135-45e9-9df1-5f6a6ca32c17)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3d4d554c-44a6-4e50-85a9-42b52eaa1176)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e6c1b2d6-a679-407d-b3d7-6dbf12aed852)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d3f0c09-6aec-46b1-84dc-d947b7559ad4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d022af5e-4a16-4e90-907d-55d69f703d7a)(content(Comment\"# Count total \
         alive cells #\"))))(Secondary((id \
         5d6f4a69-d558-44c3-b388-c0ff545ce77c)(content(Whitespace\"\\n\"))))(Tile((id \
         1dfd08f9-d8d8-4ebc-be2a-45ce12e712c2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8ac313fa-ec76-48d6-b54e-950598b1ef31)(content(Whitespace\" \
         \"))))(Tile((id \
         e8b8ab8b-c6c0-43a1-9493-f6710e22ad8b)(label(countAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1193f94b-1a56-4cb7-8841-004cf14da2ed)(content(Whitespace\" \
         \"))))(Tile((id \
         ca333191-9bc6-4027-bb2f-bebbda0da943)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         983ddd68-e737-4140-9654-5ac912e850eb)(content(Whitespace\" \
         \"))))(Tile((id \
         9fe1c7ec-0cc3-4c3c-b27d-2c21602f6fd1)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ecd4c8cc-5a20-4ea2-9130-dda9c55fb1fb)(content(Whitespace\" \
         \"))))(Tile((id \
         3cf1ac07-721b-468e-a845-70f764be93d3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         79151d23-3e6b-4863-a8b1-bc81e65f7fe6)(content(Whitespace\" \
         \"))))(Tile((id \
         cf2d10f8-85f3-495c-bb3a-acd2a9e4fb31)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a867bd1b-3243-4255-93f4-ceaa224e20cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         47269e62-e36c-4968-a7e2-04fdfc332011)(content(Whitespace\"\\n\"))))(Tile((id \
         7393a6d2-44e3-428c-b443-7e6dd7cd8668)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9ef91fc3-2ae4-4f50-a495-fc9f8804d2e1)(content(Whitespace\" \
         \"))))(Tile((id \
         3ed69391-8d4c-4edf-a9c9-0e2a388d9d87)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         df760181-f392-4161-9f12-286da571a705)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a54550d6-c2e0-4aae-9592-5667f7d73bbc)(content(Whitespace\"\\n\"))))(Tile((id \
         b77484b4-0b7a-4340-9b1f-1b98769dd7fb)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21a27fab-aef6-462d-a996-5920904e4d72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da077611-5817-4fa8-baa4-3ff1ab9d5192)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cae23766-6032-4455-8456-73e414f1026c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7384fdbd-d71e-4770-988a-83b4deafb947)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0adb2b80-c7d1-4532-b3d7-5a1922693e84)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         83470b87-c8fc-44f6-a44c-41ae97b5b70e)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c9f0d9c-438b-43a9-9f5d-d6b6bfa2df2e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1fe815e-3aad-4789-bdeb-c7bb5733a542)(content(Whitespace\" \
         \"))))(Tile((id ca4b9974-f52a-4434-8d08-717c7a6bc51b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1e9c5877-3513-4911-9b4c-09674b007cab)(content(Whitespace\" \
         \"))))(Tile((id \
         60c41108-52b7-4b94-a597-ed1db7bb7168)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         760bf00a-ffaa-4207-a0d6-f125b39b11d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         959f6531-fd58-4524-a07a-33551866bc00)(content(Whitespace\" \
         \"))))(Tile((id \
         9f84531e-26a6-44dd-95f0-cb65952cb188)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df5cfaf9-3855-4294-821c-ec89d5385f14)(content(Whitespace\" \
         \"))))(Tile((id \
         21c6514c-0dcc-444b-9156-b01bb61c60e9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8271a1f-42b5-46da-8cc6-dc3186aa8acf)(content(Whitespace\" \
         \"))))(Tile((id \
         74fc6e83-5a92-4076-89be-da20c2559b30)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7e52f03e-2dbb-4551-93c1-0d422e23ed38)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         420781eb-a191-41d8-826d-354dbddf4da7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7594cee-5736-4e96-8008-1b320a6a0012)(content(Whitespace\"\\n\"))))(Secondary((id \
         8bcf38c5-c92f-444a-8954-c2916e5550fa)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         dec6b638-809f-4846-9924-0f2eafd27b28)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4403a2d-2dee-44c3-95c3-4bc757440c74)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3dcdcf1-0115-4869-b1b1-3e1b04a028b0)(content(Comment\"# Basic grid \
         operations #\"))))(Secondary((id \
         789515d9-ef73-4d7b-a69c-8f37b392c602)(content(Whitespace\"\\n\"))))(Tile((id \
         c05014ab-151c-4e71-8f04-db27ee3b5b97)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         25e33f64-9a62-4ee0-baab-d0285a13cd63)(content(Whitespace\" \
         \"))))(Tile((id \
         1adff2de-31e9-43de-b31c-5d8351cb6113)(label(\"\\\"empty grid has all \
         dead cells\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3d356e24-3059-4ef1-8c33-aae3d1b44140)(content(Whitespace\"\\n\")))))((Secondary((id \
         bc13a874-5f31-449d-8e20-8f3bd39f61a4)(content(Whitespace\"\\n\"))))(Tile((id \
         d13a9954-9fd8-476e-937c-a2811677e00e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fa6f354f-196f-4226-a4d0-470602e058da)(content(Whitespace\" \
         \"))))(Tile((id \
         508bf5c5-8d2b-42ac-8424-df11b3502678)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         050f0af3-9bd2-4d1e-ae36-6c1699234ca9)(content(Whitespace\" \
         \")))))((Secondary((id \
         c9f8fa0a-0c75-46b2-91ef-3c11028336a5)(content(Whitespace\" \
         \"))))(Tile((id \
         6326e2d1-ba5a-4b26-9d5e-b9f83f340f13)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5903effe-84ab-4edb-8dcf-9a028a8d32cd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1250fa88-a55b-4111-b86d-0159019f463d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3486419-14b5-4448-bcc8-3ed3c53b97bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d3e10b9-fd10-42c2-9775-655133d7be16)(content(Whitespace\" \
         \"))))(Tile((id \
         7653e3ec-077a-4ca5-904b-0349b62a4a59)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e8dee87e-03ea-48a9-9d4b-8c5e0f5f683b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         39e872be-a222-44bf-8852-4a33812da27e)(content(Whitespace\"\\n\"))))(Tile((id \
         23aaef91-5101-41c1-9de1-eba80aa81d5e)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b8ebec1-e4e9-4f6b-a54e-2e89e724d43e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac3f73c4-32da-4613-97e0-b40d4d8c6fc1)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1df7da95-8d7e-4630-ba5a-aed662f959aa)(content(Whitespace\" \
         \"))))(Tile((id \
         a599cb67-7780-4953-95c1-a38a733770d3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f1d595b-c1b0-4fa4-aba5-bc3a0881c84c)(content(Whitespace\" \
         \"))))(Tile((id \
         72968d6d-bb28-4625-8133-6e144c6cf588)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31998ceb-c5b5-4093-8ca9-e6a56ffae96f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8f475e93-824a-4450-9030-94121d4f5d0d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3c8f9a1-497d-4338-b948-70b759db409f)(content(Whitespace\"\\n\"))))(Secondary((id \
         37872790-d467-4bfb-bd2f-f7079db55740)(content(Whitespace\"\\n\"))))(Tile((id \
         7c023208-1d7d-4b62-a714-c24277ffff86)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         32cb781f-929d-4ae2-9024-c7c033dbe8a7)(content(Whitespace\" \
         \"))))(Tile((id 401fdd47-b9dc-46cb-b3dc-09a49a815a22)(label(\"\\\"can \
         set and get cell\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5bba0302-bf0c-4f86-807a-a27c166c81b8)(content(Whitespace\"\\n\")))))((Secondary((id \
         3abda1f1-2a01-4d5d-b837-9be8852767bb)(content(Whitespace\"\\n\"))))(Tile((id \
         006e428c-1aac-4d79-bc0f-db9f93aaa059)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b8075a83-1a58-4862-8dc2-24cd7de5d163)(content(Whitespace\" \
         \"))))(Tile((id \
         7d6ff71d-1b54-433b-8c56-20d13568debd)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6ada1402-c352-47a0-a8e0-4dec686cd070)(content(Whitespace\" \
         \")))))((Secondary((id \
         476016b6-01b7-453a-af5d-86b86e9c748f)(content(Whitespace\" \
         \"))))(Tile((id \
         9a113e8b-1452-467c-bd04-0daf08886517)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         518ecfcb-81a0-457d-808f-e0f07ca94bd6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48a404b1-d7ed-49a3-afc9-2ab56a0805f1)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e7ccd14-1e06-4bba-9cff-be46889040f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbfe278a-dd91-41db-ada2-8cd8a903039e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed3205cd-e5ab-41c3-acf3-76aaf7a7fef1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         984ea87c-716f-48de-8d2c-07244352ac7c)(content(Whitespace\" \
         \"))))(Tile((id \
         7ebfd9b5-7cfc-4da3-9e91-59be8c12e94f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         acb39fbf-b96c-48c9-a295-56c9ee9a59a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6b53d9b-e533-4be3-8e2e-90741c01e8c8)(content(Whitespace\" \
         \"))))(Tile((id \
         379fc580-307e-4f03-a2d3-d045db7d02a9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         790b3b56-801e-48c3-86b4-e819ae874a8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b32c96b2-fd0d-4f9d-bddf-7131b9950308)(content(Whitespace\" \
         \"))))(Tile((id \
         b4acd218-bcf2-4db3-b6b4-c4a38d3994a9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45051783-818e-46e8-b963-d5050a40bf87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1dd8675c-75c6-45b8-b82f-664ea38a092d)(content(Whitespace\" \
         \"))))(Tile((id \
         d2c8d2fe-fe1a-4604-ad33-9dc13ca00221)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b719ff5b-107b-4cc2-ada5-9e11137a2c3b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2f83b0a7-6c33-4b18-97be-d84df37a4091)(content(Whitespace\"\\n\"))))(Tile((id \
         6af276ad-8f0e-4cc6-8195-d3144a4eccef)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         882d19ec-c834-496b-8d52-b6cf906fef08)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08870b79-4618-4176-94ac-54540bd27291)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40970377-6445-4ac3-b872-d2b883198915)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e62497bc-5abf-4c72-bd5e-f3d61bf8c534)(content(Whitespace\" \
         \"))))(Tile((id \
         37437f79-80fc-4c24-b212-b1d11b2a4724)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76903ee9-fe05-4790-b760-1af6418c79f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47aac2df-9c69-47e9-bf76-26f8dcedd31c)(content(Whitespace\" \
         \"))))(Tile((id \
         99a51906-d3a7-4f2e-81db-eb9f8f075010)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         de1843fc-5149-4cff-b798-0bf30923add0)(content(Whitespace\" \
         \"))))(Tile((id \
         6ffad02d-1e4e-4d77-92c1-4225c48b775e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         321e0531-ad26-4e39-b11a-27dda714b356)(content(Whitespace\" \
         \"))))(Tile((id \
         945aa8e8-d28b-43e0-90fa-5f14587b4605)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8bc6ad93-71f9-42e5-a81b-9f2166659d12)(content(Whitespace\"\\n\")))))))))(Tile((id \
         99660d57-4bf0-459f-9b97-4476bdcd6be6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69989af7-5363-47b6-9e27-d2d3ab8f2554)(content(Whitespace\"\\n\"))))(Secondary((id \
         41362b39-ead0-4e47-b7b1-47c0135bc3df)(content(Whitespace\"\\n\"))))(Tile((id \
         175e65ce-d8fb-46df-92b7-c09ced98be70)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6036cac3-c247-43f6-b6e5-6129653712c3)(content(Whitespace\" \
         \"))))(Tile((id 5b3d2aa5-2fd6-4c0f-b90e-7f3deb324688)(label(\"\\\"out \
         of bounds returns Dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a57bc102-6a0e-45c1-906e-75cdcbe3fa8c)(content(Whitespace\"\\n\")))))((Secondary((id \
         6425dbab-b030-45fd-8a38-1a479ffec5b3)(content(Whitespace\"\\n\"))))(Tile((id \
         bdb26339-f33c-4f6f-8bdf-4a5be0e6cae4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         58396a27-7483-4da9-ae12-48d1c703b18d)(content(Whitespace\" \
         \"))))(Tile((id \
         62a269d4-5b4f-47a1-a439-4d8364cc58e8)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6aea2298-b2ab-4c41-aad2-946beaef308e)(content(Whitespace\" \
         \")))))((Secondary((id \
         5083dd46-07ec-41ab-90bc-aaa7e1a137f3)(content(Whitespace\" \
         \"))))(Tile((id \
         2e548297-b7d7-4702-9fc9-bba9237447b3)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac5dac78-675e-4d99-8508-99dc16248189)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2164d84c-f2e3-454c-adad-7263621c388b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         10292b7c-0605-4981-919d-b5933b341f6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4245845-36f9-4705-99dc-df7716d9b53e)(content(Whitespace\" \
         \"))))(Tile((id \
         855ac723-67ae-460d-9adf-b233ab053252)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5a72f12-f2ba-4e13-8402-ff9dc630b3cd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a930e946-8bab-486a-ba4c-c5849cdb62f1)(content(Whitespace\"\\n\"))))(Tile((id \
         87ad0f07-0c4b-41b3-b614-9db0959b09f1)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a02263b-56ee-4837-90ae-3d758d42f3b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29d02275-3c90-442b-9c08-0d0f39c9f513)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a21598e-cb26-4c2d-ab33-9aa9eca5e7b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31417bfc-5d4b-41da-b7a6-6d7617090180)(content(Whitespace\" \
         \"))))(Tile((id \
         de3872ce-1deb-4591-9df6-6be19acf3f50)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c63e335-9bf6-4ed0-b398-3939336929ee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         116db7bc-385b-4281-87c4-977570083c5f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f90b268-b488-4f31-a8bb-2ccfb08deace)(content(Whitespace\" \
         \"))))(Tile((id \
         9f9c9cd5-1d00-4d19-b152-a4770b75a824)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cd439172-ff0f-42fc-8caa-dc63152aa9dd)(content(Whitespace\" \
         \"))))(Tile((id \
         ad256809-12e8-47fc-b04d-c429a325b4c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         722249e2-05a5-453e-8251-a7af49ef26f5)(content(Whitespace\" \
         \"))))(Tile((id \
         b1b2c9ce-efff-403a-a672-669013d76d46)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3c4fed4a-120b-42c3-9063-3ab69ef8ecbc)(content(Whitespace\" \
         \"))))(Tile((id \
         89918636-e03f-4b38-997e-f63a2b262d0d)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         816e0fa2-8d2a-4b35-a12a-748badbeb573)(content(Whitespace\" \
         \"))))(Tile((id \
         44594da4-abf9-4e71-82b2-b490f28b2ce8)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a79f4e6-d3f8-4987-970b-9bffa47f3390)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c316ce2e-add3-4395-90b8-6e1c830e33bc)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69e958a5-bfb6-4096-967c-6e4e15322bb1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec524d4f-3fee-4405-8f65-abfb50dfeee9)(content(Whitespace\" \
         \"))))(Tile((id \
         96209605-ece2-4f12-8f4d-03c3a821a549)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2dd0e4d-ac77-4354-8ff9-a60b2bdf8b6f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1f1db81-e782-483f-9894-37c1d1e23c1c)(content(Whitespace\" \
         \"))))(Tile((id \
         82c7453a-d963-493e-9317-6cdd9ed8b25a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9b242c2a-8461-4488-8217-d70e5ce1e698)(content(Whitespace\" \
         \"))))(Tile((id \
         3581b66c-b7e0-4735-8ee2-f68213546166)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65c26b8e-3b22-47a6-9e8b-b396718159b3)(content(Whitespace\" \
         \"))))(Tile((id \
         294cb7da-432c-4d2b-9f6c-b22c8b50a426)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         93746fa1-7f4b-44ac-96b9-6bfcbe20aacd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fcdbb763-face-4665-874c-53e3a5ca20ed)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c03ec605-bed8-4fcb-9aa9-83e4e4d586c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b26cb11-f1c4-48fb-965c-f509b104821c)(content(Whitespace\"\\n\"))))(Secondary((id \
         43127381-298f-437a-afa5-95570d7a86fc)(content(Comment\"# Neighbor \
         counting #\"))))(Secondary((id \
         d3a38de5-a428-4164-87e5-a0ab27be7b94)(content(Whitespace\"\\n\"))))(Tile((id \
         73a33a27-7395-4876-a583-5147ffd24e36)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         032ce89b-d308-4b0e-bfbf-cb0f01c78df3)(content(Whitespace\" \
         \"))))(Tile((id \
         3e34eb68-5420-44a5-9425-08c26f817e91)(label(\"\\\"isolated cell has 0 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c913821c-9039-4551-9623-3bceeb59aae9)(content(Whitespace\"\\n\")))))((Secondary((id \
         0901082d-31cb-4508-b679-8e2d52d4a7da)(content(Whitespace\"\\n\"))))(Tile((id \
         233dd79d-3585-4b19-94bb-2c89cfc658ca)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07fc474d-81d5-405a-93ec-488c1ae9d0cd)(content(Whitespace\" \
         \"))))(Tile((id \
         4022c678-6d49-4b21-8030-8a1dd270a1f8)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f4110f23-4fcf-4aee-810a-64b5d3de9e53)(content(Whitespace\" \
         \")))))((Secondary((id \
         a0075ce1-f72c-4bca-a4bf-76e16603e505)(content(Whitespace\" \
         \"))))(Tile((id \
         22b83315-ac3c-404c-856b-00f982d05680)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f400353-22fa-45ab-954f-9ba000ccff22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         626c6dbf-e4bc-4968-88a6-8ad67a6d6191)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5b6fca4-f08a-4603-a7a8-fdc05e47d333)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c28e2112-af14-42e2-9f17-64b321b632a0)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e70149ca-96b0-4b51-81f8-822d84c7ab9a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0c193de-0e47-4ea7-96b9-946346387b0d)(content(Whitespace\" \
         \"))))(Tile((id \
         d67f0e63-4b79-446a-bdb0-d7ee1c40bb8e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3077ae96-d432-4b63-a84c-cc8179d4e019)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14a738bf-8c75-4eef-b6be-7c2485d583b7)(content(Whitespace\" \
         \"))))(Tile((id 14e102ce-a44a-4c02-89f2-9d102ad8e861)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         61f99a23-ce38-476d-af3a-f39bf5a5301f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc7d6f2b-5596-4a57-a08e-78004d635979)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b2e2470-fbd2-45dd-86f1-4670a466b1e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c743e99-f814-49e0-8a06-96c2ebe63ed9)(content(Whitespace\" \
         \"))))(Tile((id \
         58145bc9-6617-4138-ad02-95f7459e823e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         496419e4-7a0e-473b-a4c4-ea965b9c089d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7e6b4919-5eba-4211-9af6-553041120f78)(content(Whitespace\"\\n\"))))(Tile((id \
         2ff791b0-61b2-4d1f-ac32-1fb753a27350)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97a531fa-8ad3-431c-a04f-bf4aa3627e57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3fca2be8-7d72-444d-8487-583b16dc73d7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3437ccbb-abf9-4094-b4d4-9e754888686c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e18802e3-7eff-4ee5-8f88-09dc2219760e)(content(Whitespace\" \
         \"))))(Tile((id \
         3b1904b6-0c04-4595-8cf8-86c85cee982e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52275325-fcdb-4918-85cd-73c51a2ffa7f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07a909ce-9351-4891-a861-a09d77ec0d12)(content(Whitespace\" \
         \"))))(Tile((id \
         75835988-0e32-421e-ad24-404a2bef3e32)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e1c4c5ee-51df-4b1f-be92-8fdc26a4f582)(content(Whitespace\" \
         \"))))(Tile((id \
         522e6c62-9842-4700-aa90-cca7557ed3d1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c3fdae2-67c6-4f61-b360-4b61f0e1b326)(content(Whitespace\" \
         \"))))(Tile((id \
         f902ffea-3a4b-41c8-9aad-b7509242762e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1d2fb4ba-abe7-442b-89a3-ad6a21408c0c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         55de7441-d8f5-4740-a4c8-f2991f466084)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95988d58-ebac-4355-8c83-62c58be7afc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         cea0bc0f-34b1-4103-a105-c8e64bc74c51)(content(Whitespace\"\\n\"))))(Tile((id \
         87621af4-507d-4c62-b4d1-fa2563988f19)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         10dfff12-5ce3-4740-a7a0-212a58f3b09d)(content(Whitespace\" \
         \"))))(Tile((id \
         55aa7adf-1fef-4c5a-a0df-2c6d90f1dcb0)(label(\"\\\"cell with one \
         neighbor counts correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4845f91b-ca99-4718-971c-ba21803d0ee1)(content(Whitespace\"\\n\")))))((Secondary((id \
         7a847bde-cb4c-4483-9edb-8fefb39da705)(content(Whitespace\"\\n\"))))(Tile((id \
         68346022-a5c6-434e-802e-5487e33b24ce)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ce952e13-8423-4768-9971-be23c78c08a8)(content(Whitespace\" \
         \"))))(Tile((id \
         36be3b84-be15-478f-8354-01420c9b47ca)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         067fe739-11d6-423e-9883-5ad672844160)(content(Whitespace\" \
         \")))))((Secondary((id \
         23897cc6-f69d-48ad-8624-f48bd4428941)(content(Whitespace\" \
         \"))))(Tile((id \
         8706da4b-a88e-499d-a921-f452cf66b7ae)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e53565f-3490-4cf7-9e05-1af6eef8ae21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         921b2db7-6e0a-45a7-870d-62208928a937)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8a6e1a9-1034-4f45-9755-71b9413117f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         11ad4b70-ebcb-43d6-aef4-437b78472f55)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         188f655c-a0c0-4fba-9637-9a14503bcc7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0320b99a-af52-4b98-9761-b0c135f2f09f)(content(Whitespace\" \
         \"))))(Tile((id \
         f1cf78d9-9f4b-4865-a91d-9ac7149229f3)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ffa84df7-9c73-4a90-8e51-12c74e02e606)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c14c5d65-d92c-4015-8b73-dee9dbee63a8)(content(Whitespace\" \
         \"))))(Tile((id 5d5425ab-32ac-49f8-bf22-32661e0367b1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c9d9c308-1cc2-4181-9a6e-8502a939966d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8ee45ea7-9424-4482-883d-57b2f09d7567)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35489ea7-1975-47e1-8643-7e26d8e02be6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acfd6e11-381a-436e-90a0-d1e286484d62)(content(Whitespace\" \
         \"))))(Tile((id \
         9e46ed94-ec2c-4ae8-9e85-569c7ad25345)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9d9f6488-1c70-4d23-8ca7-6cec28e05f9c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24eb0d18-86c0-4c93-b574-8cf9b14de497)(content(Whitespace\" \
         \"))))(Tile((id \
         6c8eabe4-fec9-4bd8-b96f-ce4541b6ae58)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2bf44d79-758d-4461-8b10-bdaa389fec0a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da8612ae-309a-48ee-96ee-e0a86b979b51)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2457bf7-dde4-45be-89b9-2da5d96a14e8)(content(Whitespace\" \
         \"))))(Tile((id \
         d65cfc71-6a73-4e89-a75f-3b27f7728b70)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         b792d578-eca3-476a-980a-c738b292a092)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         513218be-caf8-4aaf-829a-435e744feafa)(content(Whitespace\"\\n\"))))(Tile((id \
         81b33e0d-a5ec-44d3-b48f-6aa2e08f5912)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77079eaa-083b-405b-b3b0-4ba19d4ed670)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fef00de4-688b-4a3c-a556-f3888fe264be)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         102f50e9-87f0-4a76-9857-37fb83fced8e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1296a95-290c-44fb-9f74-a1c5070b0cda)(content(Whitespace\" \
         \"))))(Tile((id \
         2892f891-a200-4c1c-8732-6839d41659c7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc890c8f-4a9e-4653-8aa8-9825bbda8d52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a8fdd5c-73e1-415d-852a-01a1add61bf4)(content(Whitespace\" \
         \"))))(Tile((id \
         9cff1bb1-798e-4e97-9c54-4098a693df0b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         552e33a3-a918-423b-84ea-51a3bbd62aa8)(content(Whitespace\" \
         \"))))(Tile((id \
         2fcacfac-13a9-4631-b6d3-7f5ae8cbc149)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7474b53-4fc2-47b2-ba8c-d8e88ff0c71f)(content(Whitespace\" \
         \"))))(Tile((id \
         219a30d4-b04b-4b27-8e80-5ece5df5e359)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aee16263-af26-42bf-acd0-8af9cdf653a8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         032b78e9-1682-4aa2-ad8d-371ff97b8a4f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0788b6b5-ae37-409c-ba89-cc970f32b347)(content(Whitespace\"\\n\"))))(Secondary((id \
         831291f3-2901-479d-acbe-e1f656122438)(content(Whitespace\"\\n\"))))(Tile((id \
         502544e4-6858-4d6e-90ba-796ee75ebc93)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b3c52f4a-543e-43e4-a2ef-bafe3db08da4)(content(Whitespace\" \
         \"))))(Tile((id \
         b1109f5e-abdf-411c-936d-6c574190faeb)(label(\"\\\"corner cell counts \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2e81cc63-9370-44d7-b3a9-61fe4fded79a)(content(Whitespace\"\\n\")))))((Secondary((id \
         c40e6226-8089-46f1-9d7e-a43246c3abce)(content(Whitespace\"\\n\"))))(Tile((id \
         ddde8301-63c0-4d96-a609-8549a1d2a226)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d3af8c5c-b13c-4d40-a139-aa9b843cdb14)(content(Whitespace\" \
         \"))))(Tile((id \
         50491681-a172-4ad6-9a20-cb713d280f71)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af1d6bbc-7b41-4c0c-a083-c85ab5dfb292)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff4a4c1d-e2f2-4789-afeb-eafa9088909a)(content(Whitespace\" \
         \"))))(Tile((id \
         0834d527-137c-452d-8f18-feb46d7ade7c)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dce78383-919b-4f48-9cdc-31a96dc78b12)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f4006753-3737-4b9b-a441-e56bd71c6281)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b9e91a7-5405-4365-8c6d-50d23739b096)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de56cca2-6991-4e3b-a9e5-9d0430244610)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af1cbbf5-d556-4997-a813-4a0fd78dafc6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf8f378d-3645-47b9-904a-4801be40e4ae)(content(Whitespace\" \
         \"))))(Tile((id \
         c86a2975-88d8-42da-81b6-0e41d18a7129)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d8b3201e-5adb-49de-8564-422f4cfcab10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         684b9670-9030-4995-86f6-cf970781bb6f)(content(Whitespace\" \
         \"))))(Tile((id 508e96fb-3c42-4ce8-8ced-f6c96a33b6bf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7f71c4a-9788-4536-a9b3-5e03167b4c41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d594deef-2007-432f-ad2a-7e6a840adcae)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcecd68b-916a-4978-aad8-b9b1efd8e76a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6436a40c-48ea-4d77-b461-52baa98024a0)(content(Whitespace\" \
         \"))))(Tile((id \
         4cb6fbf1-714e-4f8c-8491-369aa360e8ef)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         506fcd46-b090-4003-af22-eb475e78f831)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1794ab0c-fa0e-4a93-ba23-e9028affdce1)(content(Whitespace\" \
         \"))))(Tile((id \
         8241d287-30fd-494f-bce9-dd6c7828ce7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         06f0d1cd-b23e-4e21-bd69-8ef64506f644)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa91c1c2-3bf6-4b18-b7a3-8b3da0351a2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b02ae4a0-9ac7-4222-a4eb-81eb26516c66)(content(Whitespace\" \
         \"))))(Tile((id \
         e6ecc228-09ca-444a-a948-520f98a9f5d6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1d00474d-ee89-40ae-9cbb-fe69eedd91e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a2e75ff-7b55-458d-96aa-555c48c62601)(content(Whitespace\" \
         \"))))(Tile((id \
         e8bcca14-0f46-4dd2-8860-f0f2de3fb61b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ce8e4925-ab57-499c-8fa2-b8c47626f6bb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc57a57a-37f7-44a8-80c4-9106b50eb450)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4a303e0-3a5c-46bd-855b-1424ec11f7c5)(content(Whitespace\" \
         \"))))(Tile((id \
         866c48ef-e214-4066-89b9-65e4bac017d5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         b4cd5b06-fd00-46a6-8d25-1661105142be)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9705fd4a-54a9-4a93-b2b7-5c8df40a1e9a)(content(Whitespace\"\\n\"))))(Tile((id \
         d1c4c99b-05b8-4fac-b048-f898ee17409e)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0733e46-d8ba-4147-b78f-dcc908986ce4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b38a15ac-6694-46b1-98c4-2548a98e2431)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         746b8391-e5af-4260-9be6-5f510c3208d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ec19040-69b0-4f26-8839-c55c0abdabb6)(content(Whitespace\" \
         \"))))(Tile((id \
         f85fc337-022c-4611-bd28-5d3bcb1fa249)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a98aad2d-282b-46d2-acdb-facf5c4452c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ce81ff1-6a45-4a93-b0e5-5495ce0e99c5)(content(Whitespace\" \
         \"))))(Tile((id \
         70123acd-6f2b-42b1-9d8a-b400371ad203)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eea2a601-d8db-45a0-8839-ca561b2628a4)(content(Whitespace\" \
         \"))))(Tile((id \
         44098a66-c690-4d80-b3d0-d0d6c408fed3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba2e9a83-f669-4423-ad8f-dc494e7f902c)(content(Whitespace\" \
         \"))))(Tile((id \
         58831a3c-27d2-4b5b-8060-45592a00194a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3bba13f9-73cb-4320-ba20-958d20fcb2cb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         57841e85-3693-4f4c-b50c-817df1ec95b9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27476925-c36b-4311-9929-61f65a779067)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d752182-0c5f-428d-8cbc-a6d2e68609b2)(content(Whitespace\"\\n\"))))(Tile((id \
         f2857a64-4255-4f43-ad9b-1e65ad1a8f38)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a85c8de-e65e-4853-90be-c685fba2d517)(content(Whitespace\" \
         \"))))(Tile((id \
         0d8bf9f4-4beb-42ae-b7b2-b97c0dada1f8)(label(\"\\\"cell with 8 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6a9d860f-6525-4b48-9d33-219f4e9bd13c)(content(Whitespace\"\\n\")))))((Secondary((id \
         abc4e9b4-c989-4c5d-b60e-04519c940b5b)(content(Whitespace\"\\n\"))))(Tile((id \
         061a3891-c1c8-471d-893d-d6087e06cca1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e610017a-bcf2-443d-806a-27cd9ba8adec)(content(Whitespace\" \
         \"))))(Tile((id \
         7e647ff1-93c8-489b-ae3b-e8852393327e)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dad308b8-d11e-406d-bdaf-31a154822157)(content(Whitespace\" \
         \")))))((Secondary((id \
         0136f062-65ae-4548-816b-18a1686b04e2)(content(Whitespace\" \
         \"))))(Tile((id \
         4c33cba0-6566-4b8a-a0ad-3100be7002b8)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7285e1cf-1264-4506-84f5-ee4dd97dc196)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9f64ff8-c3c7-4ef7-b858-d0fa590047f6)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f55ef95-2064-426b-a0e8-48fc53133f2c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91429af2-b048-4789-85b7-5a6df9559f8e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f675afac-0846-4618-bafe-cfbff132b562)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b2ca766-7031-483e-bc39-c27e8cdfaef7)(content(Whitespace\" \
         \"))))(Tile((id \
         af87f7b9-0256-44b6-9148-dca79a2d0486)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e7f06799-c1b5-46d1-a5fe-7c15c88152a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e22b5ca-6b5b-4857-b1d9-b3356f1864a9)(content(Whitespace\" \
         \"))))(Tile((id 81a786d8-1229-471d-9141-e1cea0a42e59)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b8a6129a-a7c0-4c7c-a804-761d8d9d5245)(content(Whitespace\"\\n\"))))(Tile((id \
         8e8b39e2-8831-45dd-ac40-b853b579c10a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0de25966-aa02-44f5-bb26-9dc67088ffea)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ac45862-c7b3-4dd4-b884-b4ecb5fbc381)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c79b9fa9-4b5c-4941-a5f2-e806f2787307)(content(Whitespace\" \
         \"))))(Tile((id \
         679d16a7-dd8a-4516-892d-00b9aac380af)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         eea05273-e8f3-4ff0-9421-2b45830ad1e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a54d099-e9e5-4e21-856a-efabdf549a36)(content(Whitespace\" \
         \"))))(Tile((id \
         3acdb80f-70fa-4ac0-b65f-748a1bb10276)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9e9b71da-c29e-4e01-9629-a5bc5865fddc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4662ed9-47ea-4277-baa8-b534e34066e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dbb2944-dca8-4d38-b65f-fce54a3ad7f3)(content(Whitespace\" \
         \"))))(Tile((id \
         b6191706-a2c2-4b49-bcd0-9690f3cc4798)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e7aab50-db94-4f65-a1ba-1ddb41f8cc0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a528a8a8-62d2-4ff2-8e05-27cffc38f197)(content(Whitespace\" \
         \"))))(Tile((id \
         49cad654-465d-4f9e-9aaf-37b8d8e0fa3d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10fe70a0-f9de-48f0-ad00-5b5da159ddf4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28f52654-ee8b-4512-9456-59b2e727b739)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2da38950-281c-411f-a74b-ba13d6886476)(content(Whitespace\" \
         \"))))(Tile((id \
         9f989f96-2b32-4815-8d65-ed521845c601)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         db18070a-78e8-43cb-b5bf-593a8744e2a9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78cb2277-2c66-4656-9123-59fe6c793bf2)(content(Whitespace\"\\n\"))))(Tile((id \
         566c4f4f-d7a4-4b89-9fba-f36ac86abce7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7c1e3a2a-0a6e-4eca-af96-86a8eb6f04c4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25b3983e-4960-417a-bd8b-8bafbb9aecdb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         330c2f8e-d662-4133-b552-b36d5172dea6)(content(Whitespace\" \
         \"))))(Tile((id \
         556f0f52-d493-4cae-be2f-97b0c2fca885)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ca54563a-425d-45ac-85b8-3e0e461f9e39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0ed6243-2132-4db9-88a0-741a1a12a80c)(content(Whitespace\" \
         \"))))(Secondary((id \
         1f4cab45-0741-4750-b744-cf303cec59cf)(content(Whitespace\" \
         \"))))(Secondary((id \
         8007b1f8-1197-4c36-88c2-a58958397715)(content(Whitespace\" \
         \"))))(Secondary((id \
         972249c3-ec00-4e19-9302-1a32df0500f1)(content(Whitespace\" \
         \"))))(Secondary((id \
         b14597c6-07e5-4811-9a8c-4fcdca7a9bf1)(content(Whitespace\" \
         \"))))(Secondary((id \
         6c060be8-cf96-440f-9269-9d9134d1b510)(content(Whitespace\" \
         \"))))(Secondary((id \
         713b8dcc-a092-47d1-a9af-626dd6238555)(content(Whitespace\" \
         \"))))(Secondary((id \
         b53032aa-8bd2-43b3-a746-25b6d63ec4f6)(content(Whitespace\" \
         \"))))(Secondary((id \
         8a0f6ada-82da-4580-967f-831a5c046728)(content(Whitespace\" \
         \"))))(Tile((id \
         57409c0e-b8d6-4edb-a891-0247d1e7fd5d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         99a2acf1-156e-4a55-a5ed-ab7ef81c6531)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18269bef-68cc-49ee-a2b0-8ab27394d2f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bf86199-f5da-4439-866b-58ca11fabe52)(content(Whitespace\" \
         \"))))(Tile((id \
         75b58ca1-5bb2-4504-a04b-ccef72f5a989)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         346c9614-d5f9-4698-97be-fd3155494ed0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14592d7c-aa66-41fb-8a4f-7825a008dffa)(content(Whitespace\"\\n\"))))(Tile((id \
         f2571b81-3bb0-47c3-865b-8ac0309f9daf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         db54d7c4-f33b-451d-b930-9a32424f0cb7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdad9b5c-79a7-48aa-ad7a-58fed1682d1f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c752ceb-d5a8-4b4b-81cd-49b537d4a461)(content(Whitespace\" \
         \"))))(Tile((id \
         b3e28d82-9e60-4329-87bb-19b3e97b8392)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f683f054-2be3-4c2c-b215-f4e8ed391fe3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e044e70-f12a-4a3b-af63-892e55f4f0de)(content(Whitespace\" \
         \"))))(Tile((id \
         04eba665-614d-4554-9e51-eb0b4caa3ea5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         204c7f23-fb07-4fc6-b592-542d419e04af)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94cdafb8-8834-4188-86fb-e3d644d46114)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8e92329-b024-43c0-a9fc-84df31bb6da9)(content(Whitespace\" \
         \"))))(Tile((id \
         db223f37-a549-411f-8f64-13bbf6142e3c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         68b80cb6-2413-4ebd-a60e-71c9fe974e60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c88ddd7-a278-4225-8d73-575b6049eaee)(content(Whitespace\" \
         \"))))(Tile((id \
         0ae5e618-06ea-4036-9cd7-8b14534827b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d292b97d-ef16-4d92-b385-4ee875544906)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90e617be-2a35-4b15-b79d-79beb31d1689)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21fed4f3-8aaf-40fd-93c6-6cca09642d3c)(content(Whitespace\" \
         \"))))(Tile((id \
         b4cd9422-0aeb-4e0a-a982-f3459b25087b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7af8ffb3-d040-459d-90bd-b09ffaaaeb60)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         c632aaef-1a69-479e-84d5-260ab46231c5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f6182ef-d73c-47da-b507-21b9a4bff619)(content(Whitespace\"\\n\"))))(Tile((id \
         3b4ec170-926d-40af-b875-2bc0d20a3d86)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b62c9c61-378a-48bd-ad07-420982018b65)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         371bb611-9eda-4f2e-b53c-46d6d523e79b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5166e679-18bc-4253-b3bc-08c5bb503d18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37cf2b60-0e40-4f59-8a28-165fb9c3b5e0)(content(Whitespace\" \
         \"))))(Tile((id \
         6b953fc2-b32f-4006-a417-58347367abe2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93b7ebc5-1e3a-4e82-a732-dbbc1d3bd974)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d07bca9a-86e6-4b1a-b96f-58c21d63eb7e)(content(Whitespace\" \
         \"))))(Tile((id \
         b6f5447d-a4d9-48fc-aafc-52b07091cc89)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2cd93fdc-57a0-4ad3-98d6-c4be935ca517)(content(Whitespace\" \
         \"))))(Tile((id \
         e274ce3b-e701-42cf-b875-147b3b81a0b4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee7ea5ba-0928-4585-8625-9f1e022f957d)(content(Whitespace\" \
         \"))))(Tile((id \
         42ea24fd-209c-4554-b958-cbb97bb33432)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         50d106c3-60dc-4b91-a4a5-f94bf9717715)(content(Whitespace\"\\n\")))))))))(Tile((id \
         dff84917-1667-49b9-805f-ce7a4bd7fd2b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b48b8c0-32d5-4fe5-a55d-dd23dec88da7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a725f04a-247d-4eaa-987d-d300e6221600)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca932977-14a6-49f3-ba41-1fd66c5512d5)(content(Comment\"# Cell state \
         rules #\"))))(Secondary((id \
         1e9dcdaa-add3-4299-b08f-6ae74a972a5b)(content(Whitespace\"\\n\"))))(Tile((id \
         7085ed0d-bfb3-47fc-8c82-3e8cc0cadad7)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f159eb12-02cf-44cf-83b4-09bfef469e39)(content(Whitespace\" \
         \"))))(Tile((id \
         2154a3e7-92fc-4f8c-a989-6e093f6086f0)(label(\"\\\"alive cell with 2 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         03754ad1-6310-4c43-b9f6-8759f341f3b0)(content(Whitespace\"\\n\")))))((Secondary((id \
         6948c279-5f8c-494d-8868-1eff607691cb)(content(Whitespace\"\\n\"))))(Tile((id \
         db1d5adc-8ae1-4fbb-a0bf-e4c48113a930)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3acdb8e2-eaf6-465e-aacc-607d616d038c)(content(Whitespace\" \
         \"))))(Tile((id \
         97f1d74e-6e2a-4662-a608-9a7dc6805ac1)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         59385b3b-7635-4e68-b1b8-dba2fc1b4317)(content(Whitespace\" \
         \")))))((Secondary((id \
         b98c834e-c71d-44fe-b19a-2463f9e15b7e)(content(Whitespace\" \
         \"))))(Tile((id \
         44118904-e96b-4916-98ec-ffdb2554f6c1)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7904fe7b-f935-4c36-9161-e5578f30911c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e74211fa-c9c7-44db-a2e1-2e194ad81a04)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3376448f-e468-4965-acbe-d1418c977d01)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e1da916a-ddc5-452f-b669-c704569a129e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         468163e4-3a62-4a2c-a11f-2b8af2880444)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90c36b57-db16-4c1d-9ec7-7cf979e47bda)(content(Whitespace\" \
         \"))))(Tile((id \
         c5743737-5c42-4a96-a421-1013773fb824)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3f221f24-d3ef-45fa-9885-a3b5da3b0426)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         892cabbe-c40e-4874-8b80-ed2bcdd74be0)(content(Whitespace\" \
         \"))))(Tile((id 4baeb518-d3c8-4f20-8b93-87c75c6f3bf9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a69f30cd-79e4-463a-9c70-2198eb41d04a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a7261c17-1504-4525-897c-e86827b2cbf7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67035b0d-2d25-4344-b8ef-6b8fa51f956b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c4ec9d7-21a6-45ce-a6be-e4a0468b1e39)(content(Whitespace\" \
         \"))))(Tile((id \
         087c8eac-f63c-4a2c-bfbb-4a920dc5cb24)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         64ea1b6a-3737-485b-8f5c-6fd3a5a203ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34984dd5-b560-4d6c-bf63-9721627998d2)(content(Whitespace\" \
         \"))))(Tile((id \
         09d7d119-3d1b-45f9-8d58-10ad01bda2b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1c3884b8-7fdf-4a0a-b720-15782f24198a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c297a07c-7efb-46b8-8ee6-56b6da963f1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d45fd4a7-ccb7-46db-ad15-696812d08717)(content(Whitespace\" \
         \"))))(Tile((id \
         43b6fa11-7fe1-4e0d-8a5e-23b44055a787)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b7355528-b1a8-4700-8258-eb7f6268d8de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17d4d30b-1af2-4334-ae8d-c233f56bd493)(content(Whitespace\" \
         \"))))(Tile((id \
         501baec2-f0fe-4b09-a52b-9fe874d4d664)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6b0f5bb5-3f17-4b61-9727-9e97bdcb1fef)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c19f8c86-f3b3-415d-b656-e73c83dfeb94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f46eb2d7-3cdd-4d58-9354-919258dd1589)(content(Whitespace\" \
         \"))))(Tile((id \
         ff6af878-6573-4c60-a9f7-afe5900b24cb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         ac3245dc-01e5-4cde-a528-62d7f63f0f08)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         da61211e-b7c0-487b-83e4-8698686558bc)(content(Whitespace\"\\n\"))))(Tile((id \
         8f2ec348-726a-40c4-89aa-f3f7a2e89a00)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         112a7c3e-acdc-45d2-a3da-9dc458135dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         e9a8b96c-4983-45ae-a2b6-555454545013)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         22b999e1-4309-4ae5-8755-2554f66ef775)(content(Whitespace\" \
         \")))))((Secondary((id \
         2e0827d1-b433-4a02-9281-90a263bad07e)(content(Whitespace\" \
         \"))))(Tile((id \
         f2468bf5-b14e-4045-9e95-b8c79481b0f3)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e3b4799-4087-4892-ab20-7e45ff0bbc38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2163fd28-4d21-4249-8828-36ca0813229a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         94a736cc-64eb-4ff2-8b17-68b269558478)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         00d8b8e7-07af-4920-badd-dafb23046c21)(content(Whitespace\"\\n\"))))(Tile((id \
         522a0027-f1e7-4f9f-926f-1bcba4ba473c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da33a4ec-b3f5-4057-93fd-bd79e77d48d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72501544-076c-4d31-8c47-0d0aed5a6d35)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37f99395-c423-4e90-88ea-fdecfdb23c5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f20e48f-1979-4084-b385-8efd3d511456)(content(Whitespace\" \
         \"))))(Tile((id \
         433ca9b8-efc6-4dbc-b55a-433509601ba0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13c016ec-c6bc-4839-9352-40a6f6f9d992)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e7b1089-d681-4674-997c-2fe7faff2fa4)(content(Whitespace\" \
         \"))))(Tile((id \
         ce1d0019-3340-47ff-abc8-55a659adca29)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6f24001c-4fee-464c-9046-88bd7c9c04f2)(content(Whitespace\" \
         \"))))(Tile((id \
         1eb48f11-b4ee-4df3-b4c2-21c0237dfe07)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         091d22e5-0041-4656-a9bb-51fc92c0148d)(content(Whitespace\" \
         \"))))(Tile((id \
         b3b48d76-12a2-4ff5-88d7-9f98044f7655)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         01b6db84-0187-4ecd-96f6-604b387d50f6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         497ed6ff-e559-47ff-a4dc-87a3fdf0474b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5651120-6f0c-4f03-983a-1f1226e11c7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fa6ebb7-51b5-40d1-9654-41e209aa6729)(content(Whitespace\"\\n\"))))(Tile((id \
         03f75f8f-d527-4c8e-9a0d-45d22a0e159e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f106e95-296f-41e8-a0e2-bf5c24a628e9)(content(Whitespace\" \
         \"))))(Tile((id \
         c3782b52-b0d7-42b3-8f30-c7cd6f228385)(label(\"\\\"alive cell with 3 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ceb27179-adb9-42ee-a76e-c062378b73da)(content(Whitespace\"\\n\")))))((Secondary((id \
         e496bcf4-4e43-4fa8-aa12-98b685b9ba8d)(content(Whitespace\"\\n\"))))(Tile((id \
         a9a29058-f630-4da9-805d-bf82e1b53ac4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a4b1be9f-e2c6-41a5-b2ba-f2ed4e9de757)(content(Whitespace\" \
         \"))))(Tile((id \
         e323fdab-97f9-4ef9-a527-5374db19ec77)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         01c2727f-6437-46c1-bf43-003d8bbad363)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca1064d8-7129-4d3b-b7de-ffb8f7e9b224)(content(Whitespace\" \
         \"))))(Tile((id \
         ff73c2f6-f3de-41d9-9a0e-00303f7691d5)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89f9bfe3-08f4-4236-9237-35669db51922)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c7c75e44-954a-4fa3-8347-6482588572fa)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b1cae40-e8b6-4a35-b37c-cf7c9aefbe96)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         52ccfd80-4ebb-40b9-9a2f-2c0e9bc4c895)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa6f171f-95b2-4392-b407-48a4e1e91b58)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         481cc32d-cfbb-41bd-b835-12db25672508)(content(Whitespace\" \
         \"))))(Tile((id \
         300258c0-d2fe-4315-acbd-3feb94872f6b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         66299e4e-9333-471d-8aca-fe72d112a696)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f442d61-f199-40d7-a580-99bb002e5fb3)(content(Whitespace\" \
         \"))))(Tile((id 63b564f8-c61e-47a2-b204-6627738eae39)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c9db6cee-d5f1-44a7-b5ce-663dd1f9b6b2)(content(Whitespace\"\\n\"))))(Tile((id \
         102cf1be-2d38-447c-b51f-43f62c8ddb57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ce08adbb-af41-4788-8c7c-415ba180800c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f310bef-04b1-48a0-8ff2-af48025acf08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0233e32-2b2a-49dd-9d40-9e2f084b85c8)(content(Whitespace\" \
         \"))))(Tile((id \
         8daf4ce9-aeae-4a1b-81d7-0303a0c77375)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         62b3f30b-2d8b-4b03-bf33-7187278efed3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90ae8164-6adb-4a16-a9c1-4bb386057eec)(content(Whitespace\"\\n\"))))(Tile((id \
         64548616-044f-40c6-b19c-6f1f5f29d585)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fad6fea6-3b18-4b90-83ef-26bee4e83157)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eab69ce9-9b4b-4df7-830f-a79ad7412429)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9411e51-6fd8-4fd8-9ad9-f1c97a699d6d)(content(Whitespace\" \
         \"))))(Tile((id \
         8e634f89-e85c-47ec-ad40-fc669b9bfef9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fc60ef18-e0b5-4157-8fc6-f6b57c3ab977)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3914154f-1689-44f1-8e31-4e7c854061ab)(content(Whitespace\" \
         \"))))(Tile((id \
         51365d4f-dafb-4be0-b62c-e748f1f629b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         87ec993d-0e2b-417d-a63a-01614d5f361a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc0b92f1-a29a-4ef2-b235-e7bcc53d81c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1940dd1b-0e41-4db3-a435-b8d31a1a30ce)(content(Whitespace\" \
         \"))))(Tile((id \
         87bcbf30-2319-4f67-ac6d-049e58f7ac67)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9e7c7e33-0d43-4674-a92a-58c6ed6e07a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e14bdfb-dde6-4c1f-b78c-4a90fd522d23)(content(Whitespace\" \
         \"))))(Tile((id \
         30dfcbb8-4884-4a1a-9672-6fc0cb194d4c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7959b163-cf76-4dc2-855c-48dcd79d724f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c34abf1d-6e88-4e5d-8fd4-df9334ffbf01)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4420edd2-60b0-4857-95e4-e621509d7c3b)(content(Whitespace\" \
         \"))))(Tile((id \
         4cf3c23c-6b6b-4334-841f-ed51c034a7c7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         724eda8a-e1d5-463f-983e-460dbc895334)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         cb95dced-1c51-44f3-9c10-afdef190d077)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         63a94c71-b96c-4dc3-a81b-e82a0e766814)(content(Whitespace\"\\n\"))))(Tile((id \
         917b9b97-43a2-4fc4-81a0-e25132e693cf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c46bbf0-d159-49b0-bdc7-87ed6b839c20)(content(Whitespace\" \
         \"))))(Tile((id \
         ef8df84b-83d2-4433-9c62-8a676d0e2ea7)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2b22fcd1-1a5e-4a6f-a46e-ac162b2799c0)(content(Whitespace\" \
         \")))))((Secondary((id \
         702b701b-b3a5-4443-883c-7b6e3c450d4e)(content(Whitespace\" \
         \"))))(Tile((id \
         be98e321-391e-407a-848d-53ea7e7b2454)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ecbd25e-ccb0-46d1-8111-2f053740f6dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1d46015-da36-4b57-becb-49496e4dced7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2438179c-78e6-4339-a340-45b2cf0b1cd9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3dea8998-d5e7-43c0-befa-1a923bbba91e)(content(Whitespace\"\\n\"))))(Tile((id \
         dd974f73-c4c2-4c5a-a23d-491b40986b79)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a13e987c-606a-4144-b876-cd4d6b11ef3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b1fec86c-e2d9-4c69-985b-89f9e4627159)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1ec2f51-997d-416c-bc73-f0d08bb208c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c9c214f-0868-4752-b6c8-891ec0897b96)(content(Whitespace\" \
         \"))))(Tile((id \
         1c23c93c-8624-46ec-8f8e-196e544e6eee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65df46ff-543c-4589-9fb5-c4bf3d25458e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e898e658-f988-489f-b305-59b767775054)(content(Whitespace\" \
         \"))))(Tile((id \
         87756d49-e070-4d7e-b3b4-64762e35c75c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f68e9e7b-5828-4ccb-8d99-24b70eb7de15)(content(Whitespace\" \
         \"))))(Tile((id \
         29d8c7b4-65b8-4c36-8a96-b5be54b5511d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df7fa1e2-0edf-4850-8ae5-e9143e5085f9)(content(Whitespace\" \
         \"))))(Tile((id \
         eda99274-29e1-4983-8962-9b78a0e3cc90)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ec0f5420-ce67-4e1e-b444-f02b7d380ea7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e5aa14a7-46be-4c9d-a9c6-ed5e41abd334)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37067503-4396-47d6-98fa-7f0cd6760cde)(content(Whitespace\"\\n\"))))(Secondary((id \
         e33ff4f7-48e5-4da2-8446-83e31a0093ed)(content(Whitespace\"\\n\"))))(Tile((id \
         df2678cf-f27e-40a5-979a-509c7c89ea8f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c6fe3bb8-44d2-4b01-bd60-70934d7cf3f3)(content(Whitespace\" \
         \"))))(Tile((id \
         7f16231c-4760-45d5-a8c6-9b448006abda)(label(\"\\\"alive cell with 1 \
         neighbor dies (underpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a82373a-5148-4159-8365-0e08d71bd430)(content(Whitespace\"\\n\")))))((Secondary((id \
         ad11317e-74cf-4a9d-bfca-8ad189f3342b)(content(Whitespace\"\\n\"))))(Tile((id \
         f29a7f37-2397-4fa8-8a0b-cff9ab41e13c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7985da6a-cf7c-487c-ae93-9599772d9df3)(content(Whitespace\" \
         \"))))(Tile((id \
         f3d4994e-79b3-44c8-b8a8-663a321c8717)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         87dfba45-e359-489b-ab24-fcc45d99da2b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a6db9f41-9d68-402d-9f5d-a3a7504021bb)(content(Whitespace\" \
         \"))))(Tile((id \
         cb2a79c0-9a95-442f-b3be-5e6defe3b057)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98daaa3c-2261-40bc-a39b-57c8642898ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1f1e4f33-ab6d-4856-b4ac-360ea16ec672)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e53387f-df54-4e71-b28a-006249f6fbc7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         02d7b3c1-9578-4dee-91de-13ce44e27449)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96445a68-5aac-44ee-b1f5-66b957c6659b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52c6d3d1-0726-4cdb-8b00-a359ea31e04a)(content(Whitespace\" \
         \"))))(Tile((id \
         b6ef0933-8a50-4574-9dd2-326ebb13a80c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b96ad8fa-36b7-469d-b8fe-62049abb91e3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71269041-6c40-4258-bc5f-1020e63c16e2)(content(Whitespace\" \
         \"))))(Tile((id fb700f2e-b022-44ab-906a-f12b2fc352f7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de8b46d0-1d25-463b-9439-285349282a82)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8706ffcd-d15a-4df3-9b9c-75aef10f57db)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bca7817-a591-439b-a8a0-ad36a7e1bbcf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ea39096-2295-4a4c-883b-94edc987d387)(content(Whitespace\" \
         \"))))(Tile((id \
         5ceb8af1-a100-4e74-9cd6-9af7a58a2f2c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d21ea016-eb48-4315-a2ba-c6332dfdc647)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         505b9e40-6d9d-4ff3-814b-501d06b70961)(content(Whitespace\" \
         \"))))(Tile((id \
         18242dfc-e3e3-4ead-925d-bd6f20de29b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3a1dd2db-95c1-4f85-adc9-7fcbedb63424)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b572774d-d0b5-4cb8-a0f9-98ffd35a3c7b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71e2160b-b4ca-48ff-9517-be19ab6373e5)(content(Whitespace\" \
         \"))))(Tile((id \
         c8786b36-addd-41c6-a6bd-102bf26f910a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2cf09342-7ba4-4745-9f6f-abd2c66c8933)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         348609b8-44d1-4795-9c46-52e8fca2097d)(content(Whitespace\"\\n\"))))(Tile((id \
         fd3f77e3-7cd2-4717-bf0f-df7512a790d8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b83abc2a-d559-4dad-8436-73cc8394b16c)(content(Whitespace\" \
         \"))))(Tile((id \
         3275a108-0896-48c7-b885-6c416025cf14)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         469f02e7-947b-4de2-b7a9-6a786d0d4401)(content(Whitespace\" \
         \")))))((Secondary((id \
         5e9f77d3-3f1b-4d9d-8417-29b29a14bd6b)(content(Whitespace\" \
         \"))))(Tile((id \
         2f1ac903-31b3-43db-8169-0361fb6c49a2)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7ab0012-07f6-4adb-ae3e-4f4dbe5fc08b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         32ea17d3-42e2-4620-8713-b00d3138f286)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ee0f8b58-f330-4aff-971a-5519245dabe3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0ae622f-7d4f-4f47-be37-2b5ff47b44bc)(content(Whitespace\"\\n\"))))(Tile((id \
         4f6a6171-422f-443e-956a-3c65567534ea)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4546497f-041a-49c2-a942-efb8267460a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6a8b2a1-5897-4414-a6b4-a0d365daf07b)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7272d7fb-5fcb-4e42-8c5c-8b099da18d40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99b8cdca-85f1-4792-a4b3-1b33eaa0d375)(content(Whitespace\" \
         \"))))(Tile((id \
         bbeecd74-fff4-491a-a34b-6c65512eadaa)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b4acedb-d674-4f3b-9736-23978a4ddafa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2848ceb7-a89c-4544-9162-a615cd1c673b)(content(Whitespace\" \
         \"))))(Tile((id \
         a6cf4811-651d-4aa9-8be3-de323fe858b4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         00461d45-3959-438e-8fad-a5aa92b94210)(content(Whitespace\" \
         \"))))(Tile((id \
         742175cd-b791-4b71-8315-fca0a81df295)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db6fe206-61ed-4bc7-a286-09ae48e92c1a)(content(Whitespace\" \
         \"))))(Tile((id \
         1293d128-92b3-4bb4-99ba-0a0e98231f38)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         493567e0-65bd-4678-95e2-fda6b46baa37)(content(Whitespace\"\\n\")))))))))(Tile((id \
         01130b28-0ee9-433c-92ef-468e278b74fd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8aa06917-b6f4-4c79-b09c-1f0539226949)(content(Whitespace\"\\n\"))))(Secondary((id \
         21e517ad-3f76-4aae-a2be-064b2cd78c81)(content(Whitespace\"\\n\"))))(Tile((id \
         96f187d7-f37b-45c2-8b24-ca730c511fd0)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5496829c-781f-4a53-8fbf-3fde9211d2e6)(content(Whitespace\" \
         \"))))(Tile((id \
         e5032637-91aa-45a3-8fb9-e408a67a97f9)(label(\"\\\"alive cell with 4 \
         neighbors dies (overpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         db0e2be4-529c-4c21-9755-998b97fccb7e)(content(Whitespace\"\\n\")))))((Secondary((id \
         0b56ec0a-3538-4b98-893e-1e271771d9eb)(content(Whitespace\"\\n\"))))(Tile((id \
         ab166970-2b29-4981-ba70-ec0bff5d71f3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a82e561-c144-406e-af8b-2bae407565a1)(content(Whitespace\" \
         \"))))(Tile((id \
         8decc54b-f521-427a-b568-2ad623a45c90)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6e3711f-c0d6-4063-99cf-1b3068e1e543)(content(Whitespace\" \
         \")))))((Secondary((id \
         f979c475-85dd-4e9d-8840-d8f6693584e5)(content(Whitespace\" \
         \"))))(Tile((id \
         20c19e63-b623-4512-a990-937fed416940)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5991fd4-9507-4c46-a6ff-a838867b44f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         413fdcb1-dced-4d95-a88d-c1aac58d164e)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1db66f4b-6fbd-4482-9555-147d805f14d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f28be1f0-124e-4789-a870-744e08239ceb)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d675358-f662-4098-8686-edd62772a37b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e7a819ef-349f-46e6-b2c7-e779e71debef)(content(Whitespace\" \
         \"))))(Tile((id \
         f58a2ad5-7476-4513-a950-851c77fd8426)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b0fa60c0-b6b6-4ac2-b481-2873864ed260)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8361898b-78ff-42b0-9077-21679f9b3100)(content(Whitespace\" \
         \"))))(Tile((id 5e092547-a575-443e-b6c9-2798772aacc3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2ce1d13e-dc5b-4002-b43e-7a99a65be25f)(content(Whitespace\"\\n\"))))(Tile((id \
         1348e305-5952-43c1-9e00-34806dbf1cda)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         778026f0-2d77-4ee9-b9b6-9f1941b13bc2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a70ad65-ef24-44f8-bf64-8f8652c09b27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fd0fb90-a7a7-4aa5-8a0a-1b1b6058fb20)(content(Whitespace\" \
         \"))))(Tile((id \
         7d35bf70-c648-4002-afcb-7862b1a7c333)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fd8e7221-d4c2-4faa-b4d2-9fbec3a53021)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31d581d1-1c32-44c1-a9d6-e292deb380d9)(content(Whitespace\"\\n\"))))(Tile((id \
         442b3d33-af8b-4732-9b69-201f05a3c5e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7c747253-f72e-4438-a0ee-6ecff293dd78)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89780c9e-a44c-4fe1-bf0c-ef24718092f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89ae766a-a148-4e5e-abd9-2b35b1f3249d)(content(Whitespace\" \
         \"))))(Tile((id \
         454fd8ab-bac4-4f66-b1ff-2863ce0f5766)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         97414b58-e40c-443b-af54-37d37f299c34)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98863fdf-a8af-4cda-99e5-9a150a473623)(content(Whitespace\" \
         \"))))(Tile((id \
         8eef6bab-0fd7-46da-8a66-90fc3672c8eb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3d107a56-f489-4add-9900-e4ab6398ef03)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e3dcb02-91c7-4423-82de-78f1f5dbb160)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58a568fb-9f76-482c-97ab-cc83c91605bf)(content(Whitespace\" \
         \"))))(Tile((id \
         cdb0b92d-015b-4a9b-9bc7-18d102e20205)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         eac9d784-8303-47ec-af30-46c0a6dcbf99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a200fcbd-9296-4d45-9dc3-421b8c28b65e)(content(Whitespace\" \
         \"))))(Tile((id \
         5a6f1c50-f64f-4376-aa30-18afcb621827)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4acf2bc-d48c-4fd5-9ee6-53738e0743f6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         835526d4-fb54-4180-8156-25681b4af15f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25c01dd3-0bd4-47ee-85bf-56a1fc81e60f)(content(Whitespace\" \
         \"))))(Tile((id \
         701f2be0-1e51-4b0f-8bad-8c222b1fde08)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         50a9d16c-a06d-41ff-b5e8-cb7883e3cfbd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70fb121b-164a-43ae-82b9-740ceb629a63)(content(Whitespace\"\\n\"))))(Tile((id \
         34d6c9db-b3f7-42e7-9cc4-ed5d3fb64fb7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         547a4009-d653-4f2d-8e80-06b65287deda)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35a9a2da-3be1-4513-a9fb-ee31fbf51f56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29c0aedc-b575-4f5c-bbca-48f755250dfb)(content(Whitespace\" \
         \"))))(Tile((id \
         5aa4c534-b2e7-44db-9a2e-f08c045b2a90)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         82e2cc68-c75c-44d9-8856-9202416595bb)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         9df83e02-a4fb-4dc7-8fcd-dcc05f2dcd81)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15b4398f-0528-4709-906a-0128ece90c1f)(content(Whitespace\"\\n\"))))(Tile((id \
         a62a5de8-0fae-4a59-8207-3c4bbf464c47)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         111170c3-4120-4d7d-a68c-6bec331cf31c)(content(Whitespace\" \
         \"))))(Tile((id \
         27797782-847c-4cf0-9bb7-ea227f1cb0b8)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fe0c3654-ccba-4ae6-af5b-c2caa789301b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a7357be7-b2c2-47c4-b79a-6e61f58ab373)(content(Whitespace\" \
         \"))))(Tile((id \
         6877a8ae-4c19-4ed8-8864-35213643ea9a)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c278bf2-3130-432c-9a2a-f08e49079fcb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ac971f4-20a2-4ec7-9b9d-18addd4b6c67)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2abff9df-b02d-43ca-95e9-18e799153ed3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d7fac822-60c0-4047-93b4-8ba68bbb50c5)(content(Whitespace\"\\n\"))))(Tile((id \
         50abe0a7-9bfa-425e-b392-723cb4ec68e8)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5af593a-1309-4289-8ee1-6738ef32fd2a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f4836731-3c58-48a8-8554-1c30b962b1ef)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78bdc0dc-5b03-4eae-9c00-7fd90381f069)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbccae8b-6dde-4168-b148-08a99e741c02)(content(Whitespace\" \
         \"))))(Tile((id \
         b105c74e-01c8-4a6d-a788-496ec9fdd976)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c95d9dfd-6b82-454b-b079-31e22991b87e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         541ec0e8-9ae3-4642-a555-ccdaadb31ddd)(content(Whitespace\" \
         \"))))(Tile((id \
         665edcae-0dbb-487a-b288-c07bec9c093b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8fd42eab-0fba-4bbf-ac92-90d3826de785)(content(Whitespace\" \
         \"))))(Tile((id \
         71d75c8b-4242-4b97-a164-54cf2edfd954)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f01ab265-6634-4ab2-af45-734060ff0aea)(content(Whitespace\" \
         \"))))(Tile((id \
         e095fe3f-8968-42c6-afcf-be655a64fca7)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         315abc49-3015-40f0-969b-74fb8291f481)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ff95c22e-4262-4ed0-bb6a-7da9b6d433ea)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32c73556-5f5d-4e28-898b-4aac3ade1b63)(content(Whitespace\"\\n\"))))(Secondary((id \
         26bb5380-8bd4-478f-a0f1-48210faabc5c)(content(Whitespace\"\\n\"))))(Tile((id \
         e82d5bb8-0401-4743-9043-93202b501b5c)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c805c07d-6ec2-4634-b8cc-afd7fb8d9bea)(content(Whitespace\" \
         \"))))(Tile((id \
         aaa0ac5c-4504-4364-9c80-9871f5205cb7)(label(\"\\\"dead cell with 3 \
         neighbors becomes alive (birth)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb0493b8-3d65-4134-b5c8-181343311795)(content(Whitespace\"\\n\")))))((Secondary((id \
         075abf37-bdd1-428f-9ed6-9aaad1ab2647)(content(Whitespace\"\\n\"))))(Tile((id \
         65094018-0e5a-4519-a1e3-c691be310492)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         540f0c79-580c-42c6-80db-e9d0e9b1777c)(content(Whitespace\" \
         \"))))(Tile((id \
         4752e6e8-cf7a-4b0f-8b97-0ddbc0dda83f)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8cc42ead-aede-4095-94d4-c16f3d53ff99)(content(Whitespace\" \
         \")))))((Secondary((id \
         b61a4c92-bce7-4e27-9e3f-a1da5278bb7e)(content(Whitespace\" \
         \"))))(Tile((id \
         06210169-8d65-44b9-b43c-44bcc6fb54d5)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e748d0c9-64df-45a8-99a6-48d03214bf77)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6900679b-d6e7-40b4-98d4-aa9984d8709a)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d252d061-cd35-4792-9e98-2fbe3904e02e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8be7f1e4-f7fa-43b6-973c-7715f68eef95)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86454c87-4f28-4226-82f1-84f8bbb18977)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b1fb66a-1213-4429-b652-f314483297c7)(content(Whitespace\" \
         \"))))(Tile((id \
         aebd37a3-edbf-4bdd-be1a-d90784525e75)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e585e7f4-2aaf-40bc-a876-27d204b67c7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         242efd61-154b-43ce-a5b4-3be4fddff783)(content(Whitespace\" \
         \"))))(Tile((id 8e821def-93fd-4307-bdfc-a635289d9044)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d3d8728b-7c9f-4c31-b873-3c7ec098792e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aa85b0ba-1377-4886-a798-7bcced610ccb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd643d11-5333-43e2-9cd7-f40f121e7cfd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ae2338d-5405-4478-9c20-61129da3990c)(content(Whitespace\" \
         \"))))(Tile((id \
         02f589ac-fe3a-4dc2-9bb5-0cd42340084c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6ac604e3-07c6-40fb-a994-f8eee5be77dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a41f7b5c-d065-4bb0-a8b6-6ce931d6d1c8)(content(Whitespace\" \
         \"))))(Tile((id \
         a0ec6b04-833e-4899-a0ff-47316d7a9d83)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6feb8d77-e247-4660-a77d-e4715aaae5c0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15e51338-5006-4b32-af4c-631aa318a7ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1fea85c-4756-4bf1-8d6a-fb5b9df4fc36)(content(Whitespace\" \
         \"))))(Tile((id \
         316fbfc3-d602-4dbb-89c7-9a454c2009c1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b2a86c33-ff56-440d-8163-114e1471f8bc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55134a2c-b4d8-4e74-852a-0334ec66b691)(content(Whitespace\" \
         \"))))(Tile((id \
         b361b1d8-4c4d-45cd-9484-5a9a3c6a5271)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c13d4361-9bf3-4f5b-881d-da9bc385474d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e0bd3fa-718f-4304-9866-8d6a5258f788)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9e81098-cde8-4971-bc9b-e5cdc3d435b5)(content(Whitespace\" \
         \"))))(Tile((id \
         10bee3f8-da59-43cb-b853-6eca18cb3761)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         eb7693ed-16c6-434f-833e-c2505debed1f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         091f4352-99ae-4711-a24f-344a50d8ca58)(content(Whitespace\"\\n\"))))(Tile((id \
         6b04f1e4-23ba-402a-8148-83b0e5e82327)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aca5a6d1-caf5-49f2-bcac-c562a9431959)(content(Whitespace\" \
         \"))))(Tile((id \
         36df1265-38e2-45f0-8d40-15c9d69c55ed)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3b21aeb9-0043-43ae-9aaa-193940777b2c)(content(Whitespace\" \
         \")))))((Secondary((id \
         99dae973-d2d7-4709-b444-453bb69cf5c8)(content(Whitespace\" \
         \"))))(Tile((id \
         025f954d-37e5-4608-8ee1-2cc6c4e13cd4)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e0e1764-8ad1-428f-ad39-6c8be32a0ab9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14fe9a52-6132-4ed7-8b55-802b5ab3a683)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3beabc4f-67fe-473f-8019-33cacedc710e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6cb47460-09a2-429c-a13f-2da805cb02a5)(content(Whitespace\"\\n\"))))(Tile((id \
         8c6ee3ad-6a1d-4e64-b728-1f3b3bf6ba06)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d99e74fe-b2ba-4390-89d8-b327682fec1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ceacf71-b53a-4ceb-8267-a4c2ac340cc3)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24853926-eb68-498a-a73f-e47f92030b8e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff7d058e-a80d-4c24-97bc-39761f3f19ac)(content(Whitespace\" \
         \"))))(Tile((id \
         cd9e3c3d-7c2f-45f8-9fcd-53f79b721021)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f9fb529-6589-49eb-81bd-98c8ba34814b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce37604b-22d6-4416-ad09-f070a590a392)(content(Whitespace\" \
         \"))))(Tile((id \
         42543a24-c45a-41f5-b320-4209227df770)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dd07d4ad-f3f6-4f42-840e-b5f937811a66)(content(Whitespace\" \
         \"))))(Tile((id \
         3e832406-11c0-4db9-ac39-7f8b00bb74e7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73d96801-5f15-49ce-b12b-13ac8d3390f9)(content(Whitespace\" \
         \"))))(Tile((id \
         7bd4edf2-f225-4372-98df-6aa83dae832d)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7bdfa8cc-acc5-4278-98a0-2bbbb0f58c16)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1ee2a470-c0c6-4d33-a7db-21bb6ed49f13)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cad8ffe7-5e21-40c1-9c33-441e32a558b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8b966c2-5020-4a51-b5f4-08c2d04a7fc6)(content(Whitespace\"\\n\"))))(Tile((id \
         f267bfb8-a9a0-4e69-a3c1-d15b53788c59)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b8ca88f6-f462-4bce-b4de-d73c3dfbe6b1)(content(Whitespace\" \
         \"))))(Tile((id \
         9e9d3c3c-500c-4cbd-aec9-13c7956bab64)(label(\"\\\"dead cell with 2 \
         neighbors stays dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ed02d740-e9eb-490e-b1ec-263bbf3f6f1b)(content(Whitespace\"\\n\")))))((Secondary((id \
         71ee3238-8480-4ff5-89a4-b50e8b5a5059)(content(Whitespace\"\\n\"))))(Tile((id \
         0bd846bb-01ca-47fc-9728-63ffffcb1a5f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be324b95-cbc9-455e-8cd5-4c24223e33a5)(content(Whitespace\" \
         \"))))(Tile((id \
         18244227-11ca-4f33-aca8-3fbf86728428)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         857ecf8b-a3c1-4968-a59c-dbb9aa451174)(content(Whitespace\" \
         \")))))((Secondary((id \
         e8a243df-ad5d-42ee-ac3b-fa456ee57aac)(content(Whitespace\" \
         \"))))(Tile((id \
         ba07fe12-6845-4329-b99c-ac45d9f20b10)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         faec457b-6f1a-4d77-9261-e0421a11987c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eee6fb49-7398-46da-85c0-b89b9996c9ef)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         570978c3-1e4c-4a56-a105-5a5deaa7448c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bd92c523-02a2-4dde-b158-5e105de02464)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9330eb6-8bdc-4504-b1cb-0a89b43abb07)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dad4b61b-55e8-46d1-8fc1-3e178b014c24)(content(Whitespace\" \
         \"))))(Tile((id \
         a252fc4c-8f73-44ae-9ed1-9eb6b5f5ab30)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         58c4eef6-7c6a-4822-8458-a262f56d057b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b75cdd1b-3f4e-463e-96d0-c3bc108f1599)(content(Whitespace\" \
         \"))))(Tile((id 3824afd0-82d1-4282-ae89-371163191157)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         06d343b8-dfd5-40cf-bb24-edf6145eafb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         341ff674-4a50-40eb-bb64-bf4a47bd7a3a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         684c9be8-780a-42ef-9ed6-32fb32e6a902)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52df6bdd-0706-4298-a524-96382f2e4bc3)(content(Whitespace\" \
         \"))))(Tile((id \
         98df2364-9859-4c6f-ae36-a0b720b83f6d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ba39eee7-6957-4894-bb66-3cea6ef831b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4447dae-ffa8-4859-bcf3-6428d84db33d)(content(Whitespace\" \
         \"))))(Tile((id \
         f6950618-4e02-45e8-98ec-dbd17fe2e3e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f060d009-a983-433c-8bbb-a7ee62dff2f0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2082bfe-59b4-40cf-8a1c-bd82e4ccc2eb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea50dde2-eedd-45a6-a00e-0e408322ad9a)(content(Whitespace\" \
         \"))))(Tile((id \
         28e35ab6-6e62-41be-b4cc-c36cf66a33d4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         6d63a965-846c-47e9-a24b-b7daeeb6f26b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8eacf75b-1ee5-41bf-9aef-516442bc33fc)(content(Whitespace\"\\n\"))))(Tile((id \
         3892155f-154c-449c-8036-6248553eb579)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         460e58a2-7503-4401-8f7e-9ccf0b8565e8)(content(Whitespace\" \
         \"))))(Tile((id \
         4005abda-6887-49dd-a34c-054767687ef2)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1508a4c2-861a-45ca-bbf1-b52b72d9490f)(content(Whitespace\" \
         \")))))((Secondary((id \
         e7af0b5c-3f8b-4473-8143-e023579b832a)(content(Whitespace\" \
         \"))))(Tile((id \
         a05dc4a8-7809-4c20-838b-35257f3bee40)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b6b934e-371d-4229-b7c9-d41c73e89b62)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af4b2028-67f1-42ea-8c4c-fcc99b26d438)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b125f95a-b011-4679-a584-a4f7fa52a08a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         66aaa8ba-0a91-4917-b1a9-76e38847820a)(content(Whitespace\"\\n\"))))(Tile((id \
         fbd3a38b-e386-42f2-aeff-2aa9be9ab89c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e04ad34-8d3c-4ea7-9b12-80d835b41d7d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3331b1b-9fc3-4b02-a658-edb5c3d4e232)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7fd56d9d-d31b-447c-9ef1-b25624f5c7a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fe2c414-b662-41e9-b1c8-9601909e24a6)(content(Whitespace\" \
         \"))))(Tile((id \
         e72c1ecf-2517-4f23-bd06-917a200996e3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe4550ef-bd83-47d7-8686-f5703597f901)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b164e1e-c0cb-4a6a-810a-41f9ea31dc41)(content(Whitespace\" \
         \"))))(Tile((id \
         0fc18bdc-bc54-48f4-8e90-2e88db91baa8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f298302-8d82-4cbe-ab41-63d45a2b78ff)(content(Whitespace\" \
         \"))))(Tile((id \
         23c4f799-5734-4180-9a87-aae4405af883)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e7144be-98d2-43f5-93e0-3b66c45e1c4c)(content(Whitespace\" \
         \"))))(Tile((id \
         7d10eb06-c772-4714-83e0-68788cadf6a1)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         66b8ad52-cbfb-4a28-98e5-781d084c2c3a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4fb7601d-6e70-40cb-8c92-586eb14d788c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         989a71fb-0dab-4d38-ac48-8989c02be117)(content(Whitespace\"\\n\"))))(Secondary((id \
         70d1c258-eec8-4c85-8b17-6658f1cccadd)(content(Whitespace\"\\n\"))))(Secondary((id \
         97a1d2a1-1eca-4bd3-9798-ae82e5c3458b)(content(Comment\"# Classic \
         patterns #\"))))(Secondary((id \
         f0401d71-086e-47d0-8cd2-a39a94ec37f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cc75dc5-508f-4237-bc47-23e6cba4f102)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ccb7c86-9d3b-4823-9bf2-7b7d03384b54)(content(Comment\"# Blinker: \
         oscillates between horizontal and vertical #\"))))(Secondary((id \
         32a1e6cf-b596-4a77-a5ea-05b1318b81a7)(content(Whitespace\"\\n\"))))(Tile((id \
         5661ec0e-5344-4b86-8ede-3588305e4aeb)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e26399c0-602e-4ed7-8f47-c2ffc5473993)(content(Whitespace\" \
         \"))))(Tile((id \
         f84bd4c0-6131-4641-aad5-8b46bc89d391)(label(\"\\\"blinker oscillates \
         (horizontal to vertical)\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cea28a32-1ee3-4604-8a89-aa9e00df600c)(content(Whitespace\"\\n\")))))((Secondary((id \
         78564507-8d61-475d-a10f-52091555c459)(content(Whitespace\"\\n\"))))(Tile((id \
         334e59de-b222-4beb-bff8-44c8799cadd4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e76a9e6a-bb6b-4ba8-b2b8-f62af06bee52)(content(Whitespace\" \
         \"))))(Tile((id \
         912ba466-c55a-4f56-a5cc-c5624cbe9a61)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a3c2af6a-15fd-489a-8440-e84f481457da)(content(Whitespace\" \
         \")))))((Secondary((id \
         e97a2027-9f51-4e9e-b0f2-fc364b0b2706)(content(Whitespace\" \
         \"))))(Tile((id \
         888e8050-4aa2-4953-ae39-c284fa35608e)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31d71b67-3803-4fb7-a881-163e534a4204)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f59b1bd4-1532-479e-86bb-e82af4ed8f74)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         581a6414-4b0e-4ec5-89a5-83644e5b85dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6435661-5e0e-4306-bece-414600c3064a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e25cd68a-449f-464e-b2bf-43f4f5423619)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c8d0631-bc8c-47cd-9f5a-056f90f31202)(content(Whitespace\" \
         \"))))(Tile((id \
         7738335f-df1a-4401-8573-eb2e45738b6c)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         43030111-06bb-437e-98f1-daee1f34fe6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f701aa8f-e855-4c43-813e-b513c641bcc1)(content(Whitespace\" \
         \"))))(Tile((id 2f0ff91f-b986-4d63-a32f-040fe1027003)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c59cb47f-881f-4c8a-972a-b522f8181c43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cbc3aac3-dff8-4cd5-828d-d7cc7464976b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         342c0b98-776b-4550-8b6e-a51653bdc222)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96958136-9e8e-40a9-b079-f370c0db37fb)(content(Whitespace\" \
         \"))))(Tile((id \
         093b856f-fa3d-4b4a-b2de-e72aa2d31795)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3e8ace69-8d04-47b3-8416-4a0375358f0e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10ce998b-b9b8-42d7-92e0-d5b417cda155)(content(Whitespace\" \
         \"))))(Tile((id \
         00914496-80a9-4852-8cfc-04ed2997d183)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4b30db12-29ce-4108-999b-e1b7289acbb4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b21f4983-f602-4778-b97c-0dcaedafe008)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20307b0b-ea51-4c62-a186-90f39ed85daa)(content(Whitespace\" \
         \"))))(Tile((id \
         e928101d-0e62-4377-b202-46bd0c50143f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7a047ba4-d70f-415e-9943-7941e92d6da6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0de512f-8812-4dfe-85ac-fcd77cab69d4)(content(Whitespace\" \
         \"))))(Tile((id \
         ed27a2aa-eceb-425d-a8fe-f7990826ba45)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5d4a177c-09c7-4a4a-af26-1774d31ba646)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2656beb5-8fa0-4878-b4ea-fa34ac563fdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85275556-b584-4666-b601-483c429c2ab3)(content(Whitespace\" \
         \"))))(Tile((id \
         e2bf4f67-50e3-4068-98b1-11cdbd4fd942)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         1e75fa6d-1ee5-4cf1-a31b-8c271da1b60d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8c4b1583-25da-4698-9ee8-64431150669d)(content(Whitespace\"\\n\"))))(Tile((id \
         1346d3ef-dbaa-4fb3-85b0-ebfa31f0f711)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         39d5aae4-64b6-4cb7-a8ac-77f81b4071b8)(content(Whitespace\" \
         \"))))(Tile((id \
         288e2dd0-9882-436f-84ff-d553c111cbdd)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c4f99f37-84a9-4fd3-8b18-4aa9ebba150c)(content(Whitespace\" \
         \")))))((Secondary((id \
         96b15a15-2c76-4eed-95e1-36ece9e3c03a)(content(Whitespace\" \
         \"))))(Tile((id \
         949f42d3-c708-474d-8658-4b605d6e4514)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21260850-aa8b-4c8b-8a93-6463eb4b6619)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         66c4ddb0-7665-44d9-9b8a-e4f650289c99)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8b179600-c31a-48df-95b7-363f02d92c43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7163a27a-1c24-40b7-813c-43ba65b21767)(content(Whitespace\"\\n\"))))(Tile((id \
         8d1f15e8-2fc6-4374-9894-ff34af4a90eb)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0fb72c51-6944-4ba9-a7c5-f3a858e7e5c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d0bb024-13f4-4f3b-ac0c-5008ad98883c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2606f72f-e8dd-4230-93ef-0dadb7cf0f69)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e22d6a9c-2804-42d0-94b0-0ab75f08ee93)(content(Whitespace\" \
         \"))))(Tile((id \
         c9e7a422-145f-45f5-abb2-cc48ee56b763)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0c05622-8dd1-46db-b17d-0dc9e56ba81e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1126bec5-720e-4fe9-8356-4b6af82ebb35)(content(Whitespace\" \
         \"))))(Tile((id \
         38bd503f-4d69-4792-b9ae-401d20782d0d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b20773c-1805-49ee-a971-b52803d9f4b6)(content(Whitespace\" \
         \"))))(Tile((id \
         3a332121-db7c-4598-838f-5964b190e8f1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9c65584-2e45-4e79-ac81-b06d66d4752f)(content(Whitespace\" \
         \"))))(Tile((id \
         b324b198-6fdd-4f71-9e2c-74c177aae87a)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb85b239-b435-4631-9059-82c2c57f9b2e)(content(Whitespace\" \
         \"))))(Tile((id \
         9bcae476-2a53-46d3-929c-0f5c450c8392)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b358c85-4e6d-4237-81c0-42c73220f1b8)(content(Whitespace\"\\n\"))))(Tile((id \
         ae078134-c8a0-41e2-aacf-ed83a75a42cd)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc1cb307-0fe9-4196-b1fa-4cc3c75be89a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b10e474-bed3-4d28-b35d-f3a352683741)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79f0cfdd-0fe6-4a40-bf59-f9aeb7f0b432)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9eeeccf-4cf3-4573-a6a5-82fbf09c27ba)(content(Whitespace\" \
         \"))))(Tile((id \
         45f2e4da-853b-4667-b693-dae6beac89a7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81176105-3246-4747-935c-a4e8ad578df9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3699eba2-1616-41b1-b80b-9a367d8996af)(content(Whitespace\" \
         \"))))(Tile((id \
         2090db28-d94f-4e13-9aec-d1a988de3ca2)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c1609b4e-ff7c-4ab6-b428-93624510bc09)(content(Whitespace\" \
         \"))))(Tile((id \
         4437089f-276b-4252-b23d-4dedc45ae3a8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af2799c9-38f8-486f-a9a1-ae3e4414f408)(content(Whitespace\" \
         \"))))(Tile((id \
         0aeddb3b-0a06-4b35-b100-b4562ed96206)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92d98712-f36a-4eca-9a06-35cfdb8d3f32)(content(Whitespace\" \
         \"))))(Tile((id \
         6413f49a-253b-4412-ab01-740ee783e187)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c306710-923a-4d74-be57-e3f4de782459)(content(Whitespace\"\\n\"))))(Tile((id \
         63659e9e-93a8-4aa6-a6d1-130b3eb4f147)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4543f10f-bbf4-460d-b16a-16afa6fd4318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ac713c6-8d51-4783-993c-d31927d674e8)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4cf4cbe-05b6-45e0-83d3-03394222697c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a25b091a-b251-48b0-8527-68c55cdffe8e)(content(Whitespace\" \
         \"))))(Tile((id \
         84b678fa-c47c-4067-bd0f-c265089eaef0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c63b5509-79ef-412b-922b-a064177cd076)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f14072f7-3192-4906-9f7f-85cae89ea10a)(content(Whitespace\" \
         \"))))(Tile((id \
         0448a41d-85f1-4c60-9b8c-e7231c520872)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2104a465-0b8b-41bb-ad01-60dfcb0ae925)(content(Whitespace\" \
         \"))))(Tile((id \
         8f327f52-50e5-49a8-be2c-9f9f31484a8d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e13877a9-0697-463e-9c5e-b36e47e0cc33)(content(Whitespace\" \
         \"))))(Tile((id \
         205a2d8e-239f-426f-b12c-11a9488e7643)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cfb947ad-8779-4233-99e9-ea366994bd4f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4cccde2d-1dec-453d-bcce-64c07427a473)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fc6117d-5ea8-47b4-bf1e-98dd2cec2864)(content(Whitespace\"\\n\"))))(Secondary((id \
         81674d7c-009d-488c-bfb6-871d05967767)(content(Whitespace\"\\n\"))))(Tile((id \
         54410fe2-284d-473d-b721-82bcd6dcf7c0)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1ac5daf4-a3a5-4528-a2f8-35c17ff5f889)(content(Whitespace\" \
         \"))))(Tile((id \
         0090ef86-b473-40ac-92bd-8bcf9a7b301e)(label(\"\\\"blinker returns to \
         original after 2 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dfc15f40-ec87-4a06-9158-65543ba04fac)(content(Whitespace\"\\n\")))))((Secondary((id \
         5779e693-c3e5-4b94-ba52-c60533a7c063)(content(Whitespace\"\\n\"))))(Tile((id \
         72a89d15-b6b7-4fdd-a28e-f60eeba9fac2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7e3adce7-e892-4925-9096-42e14c949a18)(content(Whitespace\" \
         \"))))(Tile((id \
         c43bef0d-7751-4411-9900-d1de9a44c606)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b13b5452-b156-4c23-8b1d-6d43b929bc7d)(content(Whitespace\" \
         \")))))((Secondary((id \
         fc9450cb-a7d0-4d19-97bf-749870b624fc)(content(Whitespace\" \
         \"))))(Tile((id \
         cc284f5c-4e70-455d-821b-fcdbcf486d45)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         553e78ed-418f-4196-94f1-43b02e00b8c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bad43cc1-c789-4402-8d2f-b0381915f63e)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ca4af9d-d451-4903-bea5-4aacad0a3007)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7412e948-93dc-4b31-bae4-be41e15a43e4)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         acf33198-bafb-40ad-a27b-1b0a6cee5a91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0820a666-6661-4b61-9207-22092dca5245)(content(Whitespace\" \
         \"))))(Tile((id \
         350f3bad-82d8-43db-be99-eb62a23bb82e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3c99329e-73f4-4408-b887-d73ee65fab02)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcf7a679-9613-44bb-b235-2c8617dd8a0b)(content(Whitespace\" \
         \"))))(Tile((id ba329d56-127c-44c8-9ba4-5e5329a2f600)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9b9b354-df42-4a60-85b0-83b72bac2112)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         63437f2d-4c23-4ffc-bfb0-e618b8cc489b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d3775e1-3957-4e4b-863c-8563c2a9b15c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a74d6000-f54e-4780-b987-2225f8b84fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         c6866a3c-4989-4b86-90ac-6e9ac57e3825)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         eb141d72-dc89-4b45-977e-e3ddd56e906e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44af8e8a-e6d2-450d-9f9d-fa413b1a6af0)(content(Whitespace\" \
         \"))))(Tile((id \
         f1d9a63f-a953-499e-8763-7d69318fd9ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4635bced-64a0-4389-b3b6-c21a1596829f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b1994ed-a86a-432e-9427-86922c602e64)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe944466-80bc-4156-8855-bce97146789b)(content(Whitespace\" \
         \"))))(Tile((id \
         584596d6-d40e-43e4-8e4f-fc04485ef24e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         84019562-ee4e-4ab1-8e48-f47acd92d10e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42319d9f-ae43-4739-8f41-36873aa72989)(content(Whitespace\" \
         \"))))(Tile((id \
         7768b9e8-3cab-436c-afe4-79105d5a63cf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         11abc9fc-5984-4551-94ab-ec508ea64114)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6708ab30-94e7-467f-a4df-66470ea81d7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1596b5a-b26a-4bfe-8236-ae93f1fcd088)(content(Whitespace\" \
         \"))))(Tile((id \
         e0cfa2d6-cce7-4b3e-8d7e-ce4464425c1b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         554e9549-edbb-497d-86c9-6546d6cd0a98)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46edbe6a-c090-4a42-aaf1-fb7b9bf3b8f5)(content(Whitespace\"\\n\"))))(Tile((id \
         3c6b6e1a-d90e-43e7-b3c8-7d3f45ea140c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         983071bf-01df-413c-99fb-9ab409f1def6)(content(Whitespace\" \
         \"))))(Tile((id \
         e839aad5-d8ed-4962-98fe-3a338f330ada)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d6f9f5e-c44e-4696-8b71-373e770d970f)(content(Whitespace\" \
         \")))))((Secondary((id \
         97b25bd0-4a5d-4ff0-bb56-8e4fe4fea9bf)(content(Whitespace\" \
         \"))))(Tile((id \
         e9bf9e57-df94-430b-be29-6cd12ecc5a20)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4ef3525-a50c-4e10-ac1f-93e5efc24216)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         11dbe142-9949-44e5-b12b-675dba08bf2f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0069a65-7b55-4975-8049-f2060b0c7633)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6924fda4-d3d2-46f4-befa-d20fa76fc531)(content(Whitespace\" \
         \"))))(Tile((id \
         e1dcae38-8793-4ae4-8a91-1ad6f5db51a1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         239f708d-c74e-49c7-b97e-4c580f39c278)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b3861ff-6fc0-4fff-b8e3-fc7916a3daca)(content(Whitespace\"\\n\"))))(Tile((id \
         5fb49f6a-444f-4f66-974d-0a2d7737b117)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fba3e74-74b5-4679-9343-37b85d5697b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c396d55b-0c9d-4216-9161-1bc19da8aa5b)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         241b47dd-788d-4741-b7fc-c2e965110ce4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3dd4b58-a1c2-481b-8c34-83f2a94b54bf)(content(Whitespace\" \
         \"))))(Tile((id \
         86e888a4-3488-488f-8cfb-ff126779d68f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9372898c-4ea7-40fb-9804-d15ce41a22cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50b4d64e-a37c-49ed-aa8e-964ddd62ee2b)(content(Whitespace\" \
         \"))))(Tile((id \
         ad52458b-c608-4ad4-a3fb-e3896c7ec52a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d6e2c02-3655-4e0a-adae-2dd6fac10e39)(content(Whitespace\" \
         \"))))(Tile((id \
         d759440b-b803-4a87-955b-8596acc29347)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0038da98-036b-4853-9c82-2f304f638064)(content(Whitespace\" \
         \"))))(Tile((id \
         1266aad4-f2ff-4916-b0df-71d4257dcfc7)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdc64d53-61c8-4e1d-8e33-353a8e1b3ed0)(content(Whitespace\" \
         \"))))(Tile((id \
         6ec76ac9-1fe7-4a58-a7bd-853804666c05)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df773fb7-696a-4af9-8312-02c154995290)(content(Whitespace\"\\n\"))))(Tile((id \
         fb9c8649-9e0c-41a7-a62c-bc7b18187a94)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1594a37-97ea-4648-9212-3c0dd960a019)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e6a64928-7d75-4729-8a5a-53992c0238b7)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e252db5-caa1-4ec1-9e7d-2b862ec17e7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c600e46c-aa5b-4cd9-afe3-469bc465081c)(content(Whitespace\" \
         \"))))(Tile((id \
         c668f491-a1e5-45ac-9c6f-8cbd84efeaed)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a8654c0-496e-4b0f-87de-3dab58a37b96)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4394b51-05d3-4c1d-addd-c020ab9e7ba4)(content(Whitespace\" \
         \"))))(Tile((id \
         b41093b7-dee0-4255-9cc1-42ee97f1f56f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         705a4a4f-98ab-41d4-9fec-73bbd13cd109)(content(Whitespace\" \
         \"))))(Tile((id \
         c2c2af7a-24bc-4e90-8441-9e39c31ca273)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40134e1b-e98f-4189-91ab-fab356a7e422)(content(Whitespace\" \
         \"))))(Tile((id \
         18c1ce46-f9a6-4192-ae3f-7efef0b3b2b4)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d2ddb115-5afc-4c02-b847-0877df1c5d25)(content(Whitespace\" \
         \"))))(Tile((id \
         e55b9330-9859-4275-b3cf-f91fb5640733)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c60d3c25-5ab4-485f-b0bb-4b91052bf283)(content(Whitespace\"\\n\"))))(Tile((id \
         f0f4860e-846a-4901-bf69-9629a2efb260)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e459f85a-bb8c-405c-a704-bee9a944cff4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d4e646c-2721-4f2f-a170-dd63455d1b09)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cdfa3ce-da69-42d3-83f4-ca0ce8c418cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd020af3-80ee-47b9-8f09-f88e5f3303e4)(content(Whitespace\" \
         \"))))(Tile((id \
         38bc93ff-2961-49a7-9576-be1625cb7cea)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         044c827d-680b-41a6-b1da-f4767eb24412)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         606626a0-251d-4a2b-b20c-4b8be8a51833)(content(Whitespace\" \
         \"))))(Tile((id \
         1993020e-bb4b-4ad7-b820-109caed3ada5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         52b9973d-813a-4bc4-8454-826a28e07c59)(content(Whitespace\" \
         \"))))(Tile((id \
         9e0dc789-09a4-4d21-a3f3-68013e3212b6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d65b9205-ab28-4d2e-9974-56739aea7f83)(content(Whitespace\" \
         \"))))(Tile((id \
         a83906e9-bc56-4f4a-adc5-68078ab77e36)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         11c7b21f-6f7d-4829-9f16-3eb1832944b9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9d59dac8-7729-483a-b4d9-b882c818c838)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         621206a1-afd6-4512-ade6-1c9d7d50effd)(content(Whitespace\"\\n\"))))(Secondary((id \
         694cb9d5-8f14-4f2d-a859-c7bf3ef657fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d0a91f7-b19e-4507-af33-417c3a318efa)(content(Comment\"# Block: \
         stable 2x2 square #\"))))(Secondary((id \
         d3c11937-f7fc-497c-a276-0cadefa0f51d)(content(Whitespace\"\\n\"))))(Tile((id \
         146a1930-4a3e-4e64-a53f-d3de2e6da5d4)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         71ad35b5-b37a-4e70-aaed-63b3af6bf581)(content(Whitespace\" \
         \"))))(Tile((id \
         e7c195c2-3c98-4ca0-a09b-e21bc0863048)(label(\"\\\"block is stable \
         (still life)\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b8e56dc4-ea84-4286-aa0b-e72b3b01c19b)(content(Whitespace\"\\n\")))))((Secondary((id \
         0d7782d6-2fcd-4422-a21c-7159465a9881)(content(Whitespace\"\\n\"))))(Tile((id \
         71f0dad0-2172-48f4-96df-98c191526faf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d9a0546a-3742-4af3-9c81-6aae1e5b3588)(content(Whitespace\" \
         \"))))(Tile((id \
         cb1ae95f-4c7e-449e-8674-3d492a5a0869)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         81697332-f4ce-4d4d-b782-aa17d7c19ca3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7a23acde-d70f-4e8a-a1d9-ec8dae8c5db9)(content(Whitespace\" \
         \"))))(Tile((id \
         6a710f39-3734-492f-9deb-003ccca7d9b3)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfdeb3a9-6bc6-4faa-ac43-4f919031c57d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1511d370-be85-48c4-9162-d136261d5818)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00a52aae-4024-4ec2-aa2f-02d509be6011)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         374e5cf2-826a-4841-9f31-c6f0c0d81cb9)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9655c1d-e1de-420e-88c0-8be6db29fb9d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80f1eff2-5e20-4fb7-95ca-365a528dc565)(content(Whitespace\" \
         \"))))(Tile((id \
         6d497a1b-b8be-4840-90f8-e38ccfb77573)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         50815b74-ddea-4e3a-817e-ae70b139dbf2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         608b4867-77bf-49d1-a85f-2ad0f7526254)(content(Whitespace\" \
         \"))))(Tile((id a96c8017-4f6d-425f-bbb4-25ac17d779a8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         53428ac6-4a95-44db-952a-16b6a3d6d34e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a8940e6b-7bb8-4cb8-a112-dc6f3d52a01b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         854300e5-5158-445b-8597-3d0ca750d1f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d7b946a-dd1c-4257-b139-d23f16d8a122)(content(Whitespace\" \
         \"))))(Tile((id \
         f33d68db-d872-4463-946d-900d119b729b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5f2f25aa-719f-43c6-9cef-9ab01bcbb859)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d873c44e-b699-4265-b43c-566d274eaafd)(content(Whitespace\" \
         \"))))(Tile((id \
         db3864bc-ceb6-40a1-948b-e7de83ccc4e7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e42d56a5-7ef6-4b45-b6b1-198851928b93)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a46a9a23-5385-464b-8909-ab88b2d84b62)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a94469d1-2c50-47d7-b226-aabadd083e04)(content(Whitespace\" \
         \"))))(Tile((id \
         3ce548cd-783e-4014-bf4f-0f7e5e8679d4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2fd9f4e2-3bf6-4201-8c02-6e60d281ad63)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23f271ff-e298-40a3-9db7-1be98f05570d)(content(Whitespace\" \
         \"))))(Tile((id \
         eaee2b17-80f1-4ed5-9007-240db3bce37b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         92caafbe-c617-4988-82ac-9c6963a41a2e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed746cc8-0372-4423-9564-846dfd3a270e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         caf3d7a3-1d64-46f2-a2be-287eabaf28ff)(content(Whitespace\" \
         \"))))(Tile((id \
         f9c36204-aa01-4d84-9e6d-2774e54934e5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3d36fd7a-40a4-47fb-985e-038503a653ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         898108c9-cf64-472f-b43e-6e5e3ecdc16c)(content(Whitespace\" \
         \"))))(Tile((id \
         93653121-10b6-4af0-9854-a1223ddc7007)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5f449027-9736-40ac-9c5c-b95ecd2975e8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         533c2ea0-a9c9-4f81-8b7c-7724ce03f394)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc3707b1-e5f5-474c-90d6-b7d2ffa625ae)(content(Whitespace\" \
         \"))))(Tile((id \
         0f3932b9-6905-4b74-b330-b177c49e22fc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         c8a630bd-4f14-4bec-bb79-dfceb1d35d7d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         098818b9-4db0-4857-b819-fa8db0e412d5)(content(Whitespace\"\\n\"))))(Tile((id \
         2856e0eb-00f1-4c56-96f9-f53ceb746280)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d6260f06-ab5f-4d78-9b33-c0aada81ea6d)(content(Whitespace\" \
         \"))))(Tile((id \
         f1cc9ed3-8c02-43fd-a1ed-9ed67b10ae38)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cd640e77-3d99-4c95-9491-47e501982bc8)(content(Whitespace\" \
         \")))))((Secondary((id \
         f480e231-cc90-4ba8-bb78-0a8eacc5cf13)(content(Whitespace\" \
         \"))))(Tile((id \
         28a2b979-afd5-4fc3-861e-d4f4d129837c)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d385a75a-04a7-44db-9d3c-9e98dda2e4cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ec39ddae-24dd-4a4d-a9d5-2a8457662a1f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0a823d24-6526-4c31-9566-5f2a7b243230)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         247eca5c-93be-4065-a0c1-f779b78bb86f)(content(Whitespace\"\\n\"))))(Tile((id \
         771b0b37-ab3a-42e3-9d03-4ce24533499d)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fbedb31e-08df-4c40-a3f3-b8d903b3fb62)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4f8edd3e-efeb-406c-bde2-56514b62fc82)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eacb3322-7079-4bb7-ac45-88a421f3bcdc)(content(Whitespace\" \
         \"))))(Tile((id \
         7c9f3993-69cb-48d2-8ff6-4a9a954be6e2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17572927-220d-4547-b111-7c94bccb0a4d)(content(Whitespace\" \
         \"))))(Tile((id \
         2c0139a9-5537-4601-b430-aaa098338262)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         afb7f777-5433-4b5f-86c6-039e2e6f35ee)(content(Whitespace\" \
         \"))))(Tile((id \
         533e1fa2-8a0c-41d7-a4c4-b6b7acf95641)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11b87038-8796-41ca-9d10-b532e73c9ae9)(content(Whitespace\"\\n\"))))(Tile((id \
         4d194218-b0fb-414f-a34f-8a80b055a529)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0055a6fd-83f5-4f20-943a-82637fdc6cde)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         033fec7e-6b36-49bc-a8c9-3b71f61e8893)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1759c757-2504-46a6-b3e2-35fba469d26e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af702e09-8338-432e-9430-9837bb691f69)(content(Whitespace\" \
         \"))))(Tile((id \
         342e65e7-907c-4732-bb07-dfb431806ad3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5526a31-38bc-4dbb-b489-00036e040dce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2adc8923-828c-4fe6-94b9-89f6463e2497)(content(Whitespace\" \
         \"))))(Tile((id \
         e26415d1-fd44-4f5b-8e0e-f94facc15928)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         799ed857-cfb4-4a03-a8cb-3738e831ebe3)(content(Whitespace\" \
         \"))))(Tile((id \
         3d53b247-6fa8-43f5-beee-81bbc44369bc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b592331-4c03-45ae-bc72-ea92a0aaa0ab)(content(Whitespace\" \
         \"))))(Tile((id \
         bca93133-4eda-4210-aaac-b2ae5a73d97b)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a2672ed-8296-4454-ac28-ccf518dd7dc2)(content(Whitespace\" \
         \"))))(Tile((id \
         4a1d3d28-5cd5-4bb6-9b77-d3c9bc725c16)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb14c395-1cf5-4655-957e-618bb15f3ae7)(content(Whitespace\"\\n\"))))(Tile((id \
         229f0f19-500e-4573-80e4-eda58c9d7e4c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         923e7577-4dc2-42dd-9347-8fa4fbdff20d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1f2a8ea-383b-4483-9518-2e079efb5896)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2192ae25-d141-4bdb-87e1-a86fe5258f58)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9061ad8b-1117-4b4b-ad45-9bac00113601)(content(Whitespace\" \
         \"))))(Tile((id \
         3c720c93-3d30-413f-af71-aad15237da28)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8dd4642-c7a7-4d88-955e-964e465d2210)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7cb1aba-a647-45fa-9f3a-2390260e53a4)(content(Whitespace\" \
         \"))))(Tile((id \
         1146d738-cd36-44c3-b67e-0b5ec94aca57)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eb22854e-b51c-463a-a836-70303531fe42)(content(Whitespace\" \
         \"))))(Tile((id \
         ad79c31a-93f2-4fae-9188-21690121cd1c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9f35933-2b02-4012-9059-529f4aa285ff)(content(Whitespace\" \
         \"))))(Tile((id \
         6179185f-c6f7-4e36-abf6-7c837c3966c0)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a05bdf83-acd3-4e79-97a3-2e4f8504ed5d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0ed91815-715a-4fcf-95a9-53ecf443ba64)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c740e7ca-9c4d-4674-9a07-a0fcd55fc738)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3af83ca-d421-484d-8c13-02255f93ce30)(content(Whitespace\"\\n\"))))(Tile((id \
         5a406814-dd26-4730-8f5f-31bdc283ea6b)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8f35bb25-1f12-470a-bba9-f2a65fba89cc)(content(Whitespace\" \
         \"))))(Tile((id \
         850307dc-0cf8-414e-84a1-a0327fb31aab)(label(\"\\\"block remains \
         stable after 5 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         744c61b5-29ec-4388-a953-7f12afc8b3af)(content(Whitespace\"\\n\")))))((Secondary((id \
         860a9f7d-d737-4f28-a9f8-ed71767cb2e4)(content(Whitespace\"\\n\"))))(Tile((id \
         dd7d1f45-ba0e-4130-8248-369d3c9dd7dd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3203e95e-36c6-455e-9f97-da09fc6cda9e)(content(Whitespace\" \
         \"))))(Tile((id \
         8aa0e095-844b-4aac-9005-e45675037339)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73d8970a-44d2-4bc6-bf75-6612f49d1f43)(content(Whitespace\" \
         \")))))((Secondary((id \
         b25f1605-c8ba-4d86-a92d-28ae322ad1ba)(content(Whitespace\" \
         \"))))(Tile((id \
         f55f55f9-1c57-430b-9c89-258554a458cb)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7df08231-afbd-4239-8e2d-7608c3e8b3b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         07ea0378-7f47-4aa4-a4a8-09a0e9459a2c)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf83f92a-119d-457f-be59-23caa7915b0c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         44f4b5a4-1c00-42fc-84d9-9ecfc940611e)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8f100aa-30a7-49cd-ac57-27d43b8ad171)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4602ae36-e81f-431b-992d-c981a8a4ff14)(content(Whitespace\" \
         \"))))(Tile((id \
         52439663-9ce7-40f6-90b0-8025610837f6)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4484ef7f-2518-4cc8-8105-9080b5884892)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2fbb840-2e4e-47df-879c-df80127ecb9b)(content(Whitespace\" \
         \"))))(Tile((id f8188665-7bf7-4b8d-b91e-e462f13472b9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         867d6b51-8624-424c-a2ef-36cbedee34d2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d9478006-3ba0-4af3-a82f-34eb4e88d15d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aaa9bffe-c87d-4d8b-b189-6e6a3fa6a97e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dfcbe8f-873d-4157-9ae2-e4d960254212)(content(Whitespace\" \
         \"))))(Tile((id \
         fefe73a2-fe59-4d94-afdd-aca05eaa0f96)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         36798f19-ee2d-490b-9c6a-23c4e7a67c19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         883fd94d-3ffb-40fb-b67a-cc77ebc5372e)(content(Whitespace\" \
         \"))))(Tile((id \
         b280e784-5181-4cfc-a2c1-cbef7076bd7b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         548c3391-2429-42c3-904c-dad7dc6a639b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         591bfc00-a478-409a-b58e-4a7711e819e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e02aa3d-fad6-460f-a21e-99fc225d3b71)(content(Whitespace\" \
         \"))))(Tile((id \
         c7d16154-f920-408f-a560-540e81929ed3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         10978160-64a1-4c9f-8ae3-214c2249f6c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20c5b47f-5f03-47fe-b93e-65ec8e771028)(content(Whitespace\" \
         \"))))(Tile((id \
         ea7a6c36-672d-4762-87bc-fb42456bd9b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0909bc4e-fede-4d78-8ffd-716a7ec8a871)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         268d8178-53dc-4b43-9140-0f32f146813c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4515d431-35c7-4711-b3c3-d760db741fbe)(content(Whitespace\" \
         \"))))(Tile((id \
         6e43efba-50fe-413d-a5e8-c780d97bd3e5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c860b657-7b56-4c50-a1e0-f8cad78310f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91ae12a4-1e56-4abb-a3db-558ecd55a275)(content(Whitespace\" \
         \"))))(Tile((id \
         6272905b-c667-4271-83b5-0e93587b3156)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         06792397-4a3d-4e7e-b54e-0c4ee7f64c27)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d3e8556-b926-46f3-b595-e9f027d1530e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95dfd575-eab9-40ac-8032-1b715c4edab3)(content(Whitespace\" \
         \"))))(Tile((id \
         16c6b7b8-27dd-454e-ba77-af638e9b742a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         63031a50-281a-4308-8f6b-770815825ddd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cbd9a4d7-917c-4edd-b715-1e0ca3620ba1)(content(Whitespace\"\\n\"))))(Tile((id \
         86edd189-59a8-4d72-8a57-463752099e34)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81af3934-2044-499c-b8fe-610b9f998e0a)(content(Whitespace\" \
         \"))))(Tile((id \
         aeaf3295-d777-4735-b15f-86a1119f2702)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6ccb101c-30db-4d35-a907-4dbaa6f9c046)(content(Whitespace\" \
         \")))))((Secondary((id \
         bbcff3da-c436-432b-8dd9-77192e3362aa)(content(Whitespace\" \
         \"))))(Tile((id \
         7e6ae6ee-4ee5-4a6d-9995-a170f45c135e)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b668533-cbec-4ecf-bfe0-0d4803b91c5c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bddffac3-4cf6-48cd-b838-ecf4c5393a4a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50e4b10f-fc79-40e2-8be6-4b52e09cd843)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be0b8da8-bd44-416b-b1a6-e0f935dd4fbe)(content(Whitespace\" \
         \"))))(Tile((id \
         105dba5e-9d4c-452b-a418-bceeef995288)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4bb718f7-dec8-4bc4-b220-74eabe12f5cb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4731fa95-d36d-4c87-92ae-7ce7bad9a360)(content(Whitespace\"\\n\"))))(Tile((id \
         7478964a-0a57-43db-ad1a-ab065dc14402)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29a99435-de6f-4387-a36f-24413285716f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ab5bd96-9f0b-42cd-9ecc-87f9ee64abc3)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         79f675f9-9e13-4839-8da9-e55e06b71d98)(content(Whitespace\" \
         \"))))(Tile((id \
         d46dfc97-4122-46e0-9c85-77d701cb5df8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba771cb5-42ed-41db-b042-ef6c13aa704f)(content(Whitespace\" \
         \"))))(Tile((id \
         4d751744-37e1-475f-9bb3-f907cd051117)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         baadf506-d945-478b-9712-50eb920f5e9f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         605763d7-0398-41b9-8645-34e06041d59a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3153898b-e00f-48a5-bea3-2535709a47b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         87753768-6fc1-474b-8ffc-8f88ed00ddcd)(content(Whitespace\"\\n\"))))(Secondary((id \
         e87dd5fa-440d-4ce6-a7e3-f6798f53b8c6)(content(Comment\"# Single cell \
         dies #\"))))(Secondary((id \
         da74bd24-a276-4c6f-be8c-0d0e94370622)(content(Whitespace\"\\n\"))))(Tile((id \
         1c742264-93ad-4082-8742-c0bfa68bb7b0)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b28b0853-575d-4565-a765-b5bc5241e100)(content(Whitespace\" \
         \"))))(Tile((id \
         806e79a7-6e2a-4886-b609-c6f5257c28e5)(label(\"\\\"lone cell \
         dies\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc82d3fa-d07f-4075-9ad2-29303fc68f97)(content(Whitespace\"\\n\")))))((Secondary((id \
         41077d34-023c-4e1f-9310-e59c4221f47f)(content(Whitespace\"\\n\"))))(Tile((id \
         dfd09624-6df1-410f-8c48-4aa8e3eeff86)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         123f8dc6-c4e9-4ea2-95d2-1a92de01dd90)(content(Whitespace\" \
         \"))))(Tile((id \
         efb2a500-7c7a-4335-b794-c5fee911f904)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         15f4162b-a164-4be5-ad48-829508897c28)(content(Whitespace\" \
         \")))))((Secondary((id \
         f30fda96-0744-457a-9ea8-b5cc4fea536e)(content(Whitespace\" \
         \"))))(Tile((id \
         f4d0fc58-ebc2-494b-ac92-3165b213217e)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         396b793d-9b6a-4f0c-8709-11236f596741)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dd449204-d84f-4c00-b6af-519941a757fe)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc7a297f-d790-4293-ae6a-2e549bb69791)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5cc3a321-cb84-4a18-9a15-f44249193c3c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77e392c5-f66e-4cae-b406-3e885b9ef2a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53ec0d7d-f57e-461c-b326-cc65a5e2ae98)(content(Whitespace\" \
         \"))))(Tile((id \
         0d66589f-684d-4463-b3ca-34ca20229df6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6ef68966-c6d2-48df-9161-1888b0d038ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88c67d3d-1529-4133-a866-7f8403afbb5f)(content(Whitespace\" \
         \"))))(Tile((id 2f67b950-e108-47ad-a5b5-78e562e4c04d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         99a096a4-a497-445f-8832-4d0adf8ccf9c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         922683e2-cd11-4be8-9c61-7ba17c49d0a4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9422a5b3-ef20-4241-9398-775f144b87ee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eec57d4c-208c-4fec-a8d8-c5c702a7ceac)(content(Whitespace\" \
         \"))))(Tile((id \
         b7b49f0c-6b54-4b7b-a2c1-f03da517ae44)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         9bf39d9a-cce6-4967-aae7-bdc47bfce1c9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         28b04b34-49d2-4221-b690-d1239fe42e33)(content(Whitespace\"\\n\"))))(Tile((id \
         bdb0ca34-5753-43d5-8e43-71816bdfe0f6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         818fe7d9-c844-4389-815a-09e5b37d8679)(content(Whitespace\" \
         \"))))(Tile((id \
         baae0e99-e557-4544-a5e1-b4c70e776b17)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b6901030-ddc9-46bf-ac40-a7053c42b49f)(content(Whitespace\" \
         \")))))((Secondary((id \
         3221cf79-0e2f-4abb-a896-e5fcec682241)(content(Whitespace\" \
         \"))))(Tile((id \
         21e8312f-0c3d-488d-8f07-3179837113c0)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba73f247-852a-4c78-a84c-20b94e86d7e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f381a77-6c94-4bb7-895f-888081602139)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70a90b4d-d34f-47a4-a4d3-a56cef932b43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         264992f4-6366-4ac3-8c77-e682c066f4fa)(content(Whitespace\"\\n\"))))(Tile((id \
         e34ab2fd-cb8b-400e-bb7b-90c62c7164c9)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34917813-7554-41c7-8979-6b1e2978c8c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34d7f053-65f3-4ba6-b3b1-38fdc5df7738)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         278dec17-ac40-4868-a9de-06628edc4a18)(content(Whitespace\" \
         \"))))(Tile((id \
         2e5ded23-7896-455c-9bf0-828c119538f0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a191f5c-6d27-45bd-9686-5029fd7f8eb6)(content(Whitespace\" \
         \"))))(Tile((id \
         01edaefb-7a19-4ca5-bd46-3d26b9f24bc8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b634f40f-828c-4754-8a54-a41d45705879)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c2db79df-ac4f-4c96-ac1f-f61a24c1c34c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         caa341e4-d0f1-4327-9311-95631cc1c233)(content(Whitespace\"\\n\"))))(Secondary((id \
         11a60c37-8057-452c-bd0c-4937bd280c06)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b1478ad-3e4d-4b8c-a41e-b94ce402bd24)(content(Comment\"# Two adjacent \
         cells die #\"))))(Secondary((id \
         4b8fdb0e-3097-454c-aa1f-6318071f3d7d)(content(Whitespace\"\\n\"))))(Tile((id \
         ef2f4ea0-9247-446b-8f96-46ac7ddfcce6)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         578cd377-f3b4-41c4-b92d-83451b10661e)(content(Whitespace\" \
         \"))))(Tile((id 9eb9a14f-fcd5-4f9f-9802-f3adfaa248e2)(label(\"\\\"two \
         adjacent cells die\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57cb308b-eb91-427e-9dcb-c77d49fd7ba0)(content(Whitespace\"\\n\")))))((Secondary((id \
         1a9bc9d4-e461-44ca-849b-59433be38098)(content(Whitespace\"\\n\"))))(Tile((id \
         1eb4191d-8a79-4834-80a3-77fd5e0f16d6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a878b3e-24f3-4d2d-8da5-93130f6bc8a6)(content(Whitespace\" \
         \"))))(Tile((id \
         08312960-1dc6-4cbb-baac-437c65c3ced9)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef9da9a8-adfc-453a-9421-cdc880104ce5)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff8fd35a-45c0-4856-8d0b-02e42131debd)(content(Whitespace\" \
         \"))))(Tile((id \
         8d4803b8-280d-414e-9795-f2215c9d5c98)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b5613b9-9931-4b7a-a567-9c31eda2cd25)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cfe743ce-dbca-420a-82f2-a1feac11cbea)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9630cb09-afbb-4b3d-9b75-4049ff963c95)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         73fed4c1-08ed-4e63-a1a3-6ef9da5a467f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b48390b-6782-4e09-b327-64b945e2a355)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a8f963c-0034-44cd-8bbb-d7c7b2573214)(content(Whitespace\" \
         \"))))(Tile((id \
         e8f2c890-2e19-4709-8cab-c0a789eb1486)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ae4af887-c16b-437a-b8f5-2d4da0824b95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f567fb1-c9c1-41af-a010-e3b67f2da3af)(content(Whitespace\" \
         \"))))(Tile((id 6a1841f2-105d-46a1-8ab1-b207186b4e0e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         811bfb27-6e76-422e-93c8-49044e99518d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         466915a8-72d6-4dd5-b624-1273b158aa7c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef474d44-419d-49b0-86cf-42fad2777e01)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02ed9496-0ee3-427c-aea6-92e7e7b7f370)(content(Whitespace\" \
         \"))))(Tile((id \
         2b181987-bb41-4c77-b219-4a5255840693)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         45688a8d-fc18-4442-9f21-491a3a6dad82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         575c954a-4c97-4228-b5b9-03b08d135b15)(content(Whitespace\" \
         \"))))(Tile((id \
         b4712859-6554-42b4-865a-a86e4494989b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8acf2b47-c3a0-45d1-800a-964e81a3eca8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4e226f6-b57e-40d4-8dc1-d134c9c14b3e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77854825-13e9-4386-a143-6191cfa476be)(content(Whitespace\" \
         \"))))(Tile((id \
         7e7f0e00-a087-42ab-861e-f0cff58e0c62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         d93a202d-21e7-4e25-976e-65d5dd36c9a3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         594dc5da-3719-4b74-8ba0-44250bf64c01)(content(Whitespace\"\\n\"))))(Tile((id \
         ccfbf31d-8864-401c-90b8-28149dfb7cba)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b75f67e8-91b2-4b72-86f9-4870958a84d1)(content(Whitespace\" \
         \"))))(Tile((id \
         10f84e58-1895-4c92-90e1-2de62375ebfe)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         37750753-5fed-4857-905b-8dd6ebd6205f)(content(Whitespace\" \
         \")))))((Secondary((id \
         7f4d1d8e-17b9-4ede-8c4a-ed40d549ccb4)(content(Whitespace\" \
         \"))))(Tile((id \
         b94c56fb-015f-4f7d-8091-3e8c392aa56f)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1dc4d15c-ac4a-4e53-8239-9e97fe3cc985)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         119660d3-2b36-406b-8c7e-51c5700b7410)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dcf3410a-4d92-4f87-9771-8b302b7890c2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ed5d627-f62a-454f-9c8a-d1c4c0904f8f)(content(Whitespace\"\\n\"))))(Tile((id \
         16aa99bd-71a3-456f-beae-59724c37500a)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1645917d-252a-4d34-a0aa-cde3a82e7595)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a042a505-0d41-446b-b641-dcef5b1a06e5)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         83d01d5c-f064-439b-ba8e-dbde39eb136e)(content(Whitespace\" \
         \"))))(Tile((id \
         0f75db5f-837a-4b74-97e2-1b2b45b8c891)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e4c9926-d8df-426b-a699-2a2d9173aea6)(content(Whitespace\" \
         \"))))(Tile((id \
         7722b826-cbc0-4106-ab23-5ba3f41f0fe2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a9cf26d7-6266-4619-9537-36ab60575b00)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0859e825-ede9-4fa3-8472-500ae79061a6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54609081-ea7a-416e-a239-7b8ad00cff3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         33e673b4-31f0-4c94-84c3-2fdf7377dbe8)(content(Whitespace\"\\n\"))))(Secondary((id \
         339d1291-bdf9-434e-8c78-0a384ccb9e03)(content(Comment\"# Simultaneous \
         update test #\"))))(Secondary((id \
         71a1be1b-90c3-4f13-949d-3b3a158c06a9)(content(Whitespace\"\\n\"))))(Tile((id \
         d3a21fce-7284-4fd5-a6f6-7de2f7e5d5d5)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         016d15be-ba00-41d2-a9f4-5ed282cdfd59)(content(Whitespace\" \
         \"))))(Tile((id \
         2a05533a-85fa-49c8-90e3-ec5b3e6295a4)(label(\"\\\"updates are \
         simultaneous not sequential\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         adcc9c36-473a-47af-aaa7-64a544d5dabe)(content(Whitespace\"\\n\")))))((Secondary((id \
         5676bb35-03c1-4326-8096-9fc83f38bc46)(content(Whitespace\"\\n\"))))(Tile((id \
         10b43055-8e8a-4330-bb59-652aaba48e9a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         56568766-5ed2-458e-a6ba-9d40c8c79731)(content(Whitespace\" \
         \"))))(Tile((id \
         873a99dc-64d6-465a-9288-09242058268a)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6752dfa1-b40a-4a3b-8629-0ef3c9a20003)(content(Whitespace\" \
         \")))))((Secondary((id \
         2c7456ca-93e8-4db3-93f0-ef9a85c6dd97)(content(Whitespace\" \
         \"))))(Tile((id \
         826829f8-6985-45fc-89ef-39d08df9af94)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3718582-735e-4ab3-9e5f-dc193e98e196)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8aa11211-0fa1-4d71-84a7-32079ee4d4ca)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ca8d39e-b658-4208-91ae-212e07c19b45)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4bbe93a8-2f22-46c4-b287-61e1b2ca2723)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c3c75ea8-9d73-4e2e-80ad-ace7febb9b2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17418d29-b460-422e-b202-5b4e8722d42d)(content(Whitespace\" \
         \"))))(Tile((id \
         43d0b997-66e0-48d1-b033-be742858b430)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2aeab1b8-be64-4d39-9667-456836263925)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54f615ab-f989-4ac4-9ce0-2521da956bd9)(content(Whitespace\" \
         \"))))(Tile((id 5a237156-c2bb-447d-9a9a-fdd89106ac69)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9871145e-9a69-48f9-9d0b-14f4e2bdb696)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         66707cdc-a488-4191-9bb6-e5afa76b9474)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed6e01f9-e702-4861-b6f0-55f4581b975f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0575eeb6-5615-40db-8c6e-bd89a54fd3dd)(content(Whitespace\" \
         \"))))(Tile((id \
         d774e8b8-90b3-4934-a7e8-d2cbc7e1d2c7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dcfc45ac-6654-4820-a5ab-18352b80f73e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         514627e8-e582-4bf1-abef-ad2a645e9cca)(content(Whitespace\" \
         \"))))(Tile((id \
         e67814b7-e8b1-49a4-a324-0535f2ad87fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         57400b75-3bff-4315-b982-3b832cf79336)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0330223e-c06f-4c4f-a0a9-e76ff84856b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3af1d98d-e9d7-4f43-9a38-c52f8b79e579)(content(Whitespace\" \
         \"))))(Tile((id \
         142cee4b-8429-4860-b7d5-1dc8653abd68)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9ada1ce1-8a61-4dc8-a3ef-e664cc677b46)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c7a271c-de34-4f11-b385-57c8f67ca44a)(content(Whitespace\" \
         \"))))(Tile((id \
         6f54feaa-60c1-4187-9caf-1faa3d2f2f82)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         637c75b7-8e9a-4ef6-aa28-9ff81b8ec143)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a65c7492-79a1-4220-8230-07ce03d54b4b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ebdc428-1941-4ef6-ae91-1e931bad8712)(content(Whitespace\" \
         \"))))(Tile((id \
         46624397-656d-4dc5-b1c1-6d7f2b9e4dd8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         67c35a16-2646-46e2-aa89-fc3041d67f6d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0e688b3f-3776-4128-baa7-18f1ece1ac04)(content(Whitespace\"\\n\"))))(Tile((id \
         42bc96d6-3495-4a2d-8d89-15a91a55a126)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3d683f36-f0c0-40db-9a2e-58a5e63a31bd)(content(Whitespace\" \
         \"))))(Tile((id \
         b56c4ec6-b71b-40ef-9ec7-de8383791425)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b332af1e-49b2-4154-9a95-5bf715a0c232)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8dbac56-c203-46a9-9438-0f6992316639)(content(Whitespace\" \
         \"))))(Tile((id \
         a0c3f5c7-1a64-47d5-8b14-f79feaf0dd0f)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7e7a284-2588-4cf5-a5e6-5f53b7a183b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ae37484-6655-4c1b-828e-907cfc61356c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8fae1f60-7e67-4cec-9174-70b05c4da905)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1eec5a33-e6b0-4f56-b012-c2a4ede02d51)(content(Whitespace\"\\n\"))))(Tile((id \
         4ed1a5da-9e38-4b61-8bd2-a24698f2d339)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1211ec5-5cbe-42ea-bd7b-4eb95a8165fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e2464c76-dee1-4363-a27d-86ebd125ae4e)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa07301d-10a1-44a4-b634-e41fbaa09fb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e823ccc2-f706-4763-91e2-4661805877d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0673d11a-63ba-4e51-bea1-fbed3b833919)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dea81cdd-9a7b-4aff-9049-5c0b1f83bd5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e162d1c-aa23-4b66-ad68-5490e2ff6c52)(content(Whitespace\" \
         \"))))(Tile((id \
         70d172af-2057-453a-b8a2-e2d48d4e5077)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         574884f0-a660-4a0c-aed1-f5333c5dc0b2)(content(Whitespace\" \
         \"))))(Tile((id \
         9cdac9fa-513c-41c3-bbf6-65eb36708d48)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2845bcf-2b1e-467e-91f9-6631c9ace0b2)(content(Whitespace\" \
         \"))))(Tile((id \
         48901817-401e-4c17-acc5-b75acb285ed7)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a964b89-d48a-4ebb-884d-da45a5088a2b)(content(Whitespace\" \
         \"))))(Tile((id \
         189eab21-3966-4585-8ea4-b2b9e024ba36)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c26c5c4-85f4-413d-928f-c06e01242d20)(content(Whitespace\"\\n\"))))(Tile((id \
         84b36f12-3569-49d3-a3b2-fc32ab8e2e5a)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d60bffb-bbdf-4a43-8b63-868d7d3aba43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         451e3794-c5f0-4d52-b19d-bf78ef1126b4)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2a15d75-51e5-476e-a49f-a0fee726c81f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccebb614-0520-400e-92e8-2fe9a168ce77)(content(Whitespace\" \
         \"))))(Tile((id \
         d452b278-5ddc-4808-bd99-1e20197e1197)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61882a45-4222-4672-be65-9978b24803f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f07bb7c6-4938-45ac-af16-0815d8860319)(content(Whitespace\" \
         \"))))(Tile((id \
         d7e9c90f-5084-40c6-9463-e53d7b228561)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f631ab0d-6c82-4f1a-bb53-85bb2bf69d52)(content(Whitespace\" \
         \"))))(Tile((id \
         809b6307-3306-4881-a9e4-8c28521ca0c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5cbc670-7fb6-4f3b-aa03-602c9b19ee4a)(content(Whitespace\" \
         \"))))(Tile((id \
         0345e357-fe62-4d9b-a84b-d303c0a84ae4)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a61978e8-4a8f-444b-bc9d-7d86198be5bf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         525fbee2-1b59-4c86-a156-2d737aaf0578)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f179f951-a28a-4844-b9c9-d335673d2081)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b508e56-05be-4864-9664-5eee8a05b281)(content(Whitespace\"\\n\"))))(Secondary((id \
         3208e059-b9f3-4caf-9558-cf57dd2d67a0)(content(Comment\"# Edge \
         behavior #\"))))(Secondary((id \
         1191d34a-6dfb-4fb9-809b-6e4661929e45)(content(Whitespace\"\\n\"))))(Tile((id \
         4172be1c-b796-4402-aed6-ac70a58b0c1f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e355be81-04e4-440c-a313-e790677e76ed)(content(Whitespace\" \
         \"))))(Tile((id \
         76f8dba9-dea4-4386-bfa1-99655750ba80)(label(\"\\\"edge cells count \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f655bd10-9b7f-43f0-a77c-fa7011134d98)(content(Whitespace\"\\n\")))))((Secondary((id \
         58c166a6-b022-48ca-aabd-ce4ed2a42196)(content(Whitespace\"\\n\"))))(Tile((id \
         8ecab864-a95c-41a3-bfbb-a239745bb600)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f01b83ca-933b-404f-be58-466815065240)(content(Whitespace\" \
         \"))))(Tile((id \
         aed2bb26-f750-4902-ada3-d6452f34f04d)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25a40540-19d4-4558-9532-f4d5e4fbbda6)(content(Whitespace\" \
         \")))))((Secondary((id \
         41de46ef-9a84-4b90-b22b-f813dc43c84d)(content(Whitespace\" \
         \"))))(Tile((id \
         3c60c966-4b07-4359-ab12-b7819b898454)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0fdac25-f80f-45fe-b3b9-1eca1207198f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         83f870a3-54df-4ad1-bea8-01d58c52729c)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a872f522-2893-49f2-a402-ba4bdc28b12e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b56f235-fc89-493d-8c82-5cc93188a554)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         974ddefc-d45d-4dd8-809b-f20bcbeeef7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23bf06a4-e834-46d1-bb0d-6ed248676c92)(content(Whitespace\" \
         \"))))(Tile((id \
         e5e64340-73aa-482b-96b7-41b7cd2dfbc2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7b3c8fb0-67b8-4076-8a05-132bcef998de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eddc7307-27c7-4bcc-b1ba-88d0c72ecc89)(content(Whitespace\" \
         \"))))(Tile((id f914771d-cbe7-4b8b-af69-7049f208f13b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d93a30d7-bf0e-44af-b5f9-a3342483c554)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d36ae7d3-f617-44e5-b773-c40e4a5477c9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8e6dee2-7d28-4cc9-9f78-5f6f3049cd73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fee83d3f-f5d1-42b1-9326-ad23068ad8cb)(content(Whitespace\" \
         \"))))(Tile((id \
         46c19fc3-946e-40df-8d7f-abfb8e0507cf)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         73240c8b-88b9-4ade-969f-fe4cd86c5f9c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c96443c-0185-4ebd-be8c-0676c70cc4c8)(content(Whitespace\" \
         \"))))(Tile((id \
         af26929e-864d-451f-9894-7087d1ebe398)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3b9097d6-f38f-439b-a7fc-306fc89b00fc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74152599-7d2a-45e8-b729-054cea4c66b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37376b26-d257-4961-a196-a262acc641d8)(content(Whitespace\" \
         \"))))(Tile((id \
         fae50bb1-0ace-4baf-b6ec-9a9a391eee08)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f9325842-055f-4295-9cd5-2d6bd00677aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a06b4c1-71d9-49c8-9bbd-04bab303ae20)(content(Whitespace\" \
         \"))))(Tile((id \
         4d110224-1d06-408b-84dc-b6c70f4271b9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff66af9d-85a2-4a61-9d7f-23dc487e97f9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         431a13b5-11f3-4eff-8af2-c7e0fc638302)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         460f65a5-3bc2-475d-95a7-967e809d9a8f)(content(Whitespace\" \
         \"))))(Tile((id \
         73d34b61-42ed-4568-917c-6808dfae3527)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         db2768c0-29d5-4347-9545-7148e7ca7411)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         18f94cd5-4b6b-4ede-8a63-0a7567042089)(content(Whitespace\"\\n\"))))(Tile((id \
         382dbf53-7583-4fb0-a050-ff6a1564c8eb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         919fa30f-63e1-409b-aaad-455a71d35b3d)(content(Whitespace\" \
         \"))))(Tile((id \
         24206e35-816f-4f08-8558-4b18c6b565b6)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc20ccd8-c632-4479-9472-ef2a5e9cd612)(content(Whitespace\" \
         \")))))((Secondary((id \
         85c63fbc-5ae4-4982-8d95-dfc8dd6ccb3b)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb1d8d9-94cd-4f9f-ab88-9502a703f23e)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d443947-7c09-4f33-8f9c-d4219647c23a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3a1ee0b-04a7-4268-8260-8fb84c33432c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e4625d2d-fee8-4c89-93bb-36364993b451)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ec65697-bb89-4b11-baad-8209715d19a3)(content(Whitespace\"\\n\"))))(Tile((id \
         ba51a9e6-e98f-4927-a42f-2be225c0b034)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b94a7da-c029-4147-b46e-f40a6942d907)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03020b56-6a52-42f2-84f1-6a0cde06aac0)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df3f12f0-3c16-4c87-9c22-e2fb8e8b6be8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         469b6ad6-b817-4a4c-a4ae-91ec18588641)(content(Whitespace\" \
         \"))))(Tile((id \
         3016495a-74cd-467e-b283-1f0a7f3d85d3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dfcd4de7-5c88-4509-a6f6-2ecc67551d92)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89178caf-a930-4f4f-9440-387b4b2cd11d)(content(Whitespace\" \
         \"))))(Tile((id \
         4b7d43eb-c1a1-47e1-9713-b179374ab020)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9ae07449-dbb4-4d61-b879-cdb8857956d4)(content(Whitespace\" \
         \"))))(Tile((id \
         35a4ef03-b264-4ce6-8d71-7b7080c20f69)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b87adaa9-0e4a-4602-ac78-b1c384312fda)(content(Whitespace\" \
         \"))))(Tile((id \
         1a77bba0-96a9-4c48-b672-9c8a82f6f310)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f8667ad-c849-43b3-a4db-8b7488b0e17d)(content(Whitespace\" \
         \"))))(Tile((id \
         f9930b05-ece1-4e36-bd92-9c2f6a45af1d)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c15e8f15-3bac-41b6-b2a2-a8dfcbd080ec)(content(Whitespace\"\\n\"))))(Tile((id \
         d3cf24db-4b60-4bed-9b1c-e31495fe9d3f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b160ebf-1b92-46ed-823a-7b9c8fb8c821)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b558465d-34ac-4771-8818-3dae9f97f825)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f947717-64e0-453e-806e-ebbe5f7e9d26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc26414e-09e0-4e7b-91f9-b566efa5dd52)(content(Whitespace\" \
         \"))))(Tile((id \
         92612dba-438f-4796-a11a-04865cf938fe)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dee03ba9-beb8-49db-a52e-0190cb453619)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb658b06-5544-4514-9cd8-db4d33d90725)(content(Whitespace\" \
         \"))))(Tile((id \
         82f525a3-f9b5-4d79-967c-0a916faf9c9c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         901c23b4-59a4-4a61-871c-9eee01edd4d7)(content(Whitespace\" \
         \"))))(Tile((id \
         f5268f2a-e61b-446e-b905-bef915542d20)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aaa5b1bc-34f9-46bb-a1a4-e06d0f3d21ea)(content(Whitespace\" \
         \"))))(Tile((id \
         a8cbb898-5186-4851-8185-0d9cb34b4d52)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3be61288-ee41-415f-b9fe-6145d3980dbc)(content(Whitespace\"\\n\")))))))))(Tile((id \
         eb6f5c5f-30f2-4b8e-b516-4f664ef8be56)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c66543f-d60e-4b31-83fa-f4da50cb712a)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf0b0230-941b-4d4d-8fbc-9451c58fb2d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bbfa965-dee2-4d35-a5b0-7cd20469d315)(content(Comment\"# Demo: \
         Blinker evolution #\"))))(Secondary((id \
         0ebc750a-98c1-40bc-b61b-50b9c695dc66)(content(Whitespace\"\\n\"))))(Tile((id \
         4112bd5e-e4c5-473b-b853-e959e0bf8d87)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         869c9939-ec56-46e3-a216-04d7c183e8b5)(content(Whitespace\" \
         \"))))(Tile((id \
         1ceda18d-6022-40b9-83bd-95f422490e05)(label(blinker))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dcd981f6-29a2-4c2c-ba4a-45ddfeca2c3b)(content(Whitespace\" \
         \")))))((Secondary((id \
         2ee8fdf7-1431-4100-9ec4-567516a12c29)(content(Whitespace\" \
         \"))))(Tile((id \
         62071bba-a26f-4e60-8af9-1e63744f3d21)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba449708-a65e-4ef5-9fef-396637a80008)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ad2f147-753e-4a38-a01a-a3cdbc6c972b)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da7d0215-4db0-4b90-b902-4f7cba6e384a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         174d2fa1-77f0-422e-b923-e90a0a33820c)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         463f4e9d-5e16-4b28-8b01-dbb0a9ad9aca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         619de228-e9c2-4026-9244-bcc7f9da4c35)(content(Whitespace\" \
         \"))))(Tile((id \
         a5454b7e-b358-4437-9a79-9d3f1a409202)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         95ab11f1-4f2a-48d5-8213-c60c2bbd0f10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10b79ad1-3bf1-489b-89d1-6482abe78b12)(content(Whitespace\" \
         \"))))(Tile((id 0a853072-1936-405b-ba42-b1aa722098e5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f6a1c7c0-ba70-4c85-b865-51eb7e365cdc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2f722f9a-66a2-4965-80d0-e7308d6fd41b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc6b6ccb-d392-4044-b414-295cad1bafb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd6b06a1-9aa5-406c-a39c-5878a5ab20d7)(content(Whitespace\" \
         \"))))(Tile((id \
         e25d9765-09e8-46c5-91de-ab8b00698018)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         18694a59-e109-4763-aa1f-7b3374888717)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83311dc0-6be2-4616-add4-27b710c918c1)(content(Whitespace\" \
         \"))))(Tile((id \
         eb842e68-dbe7-46bc-baca-7a31a4e102fa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         194addaa-b118-4de7-983b-2e604e592805)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c3e0fbb-f232-4905-8eb6-f9a1e6bd4512)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52573fe5-9849-49cf-af72-6437e136afa0)(content(Whitespace\" \
         \"))))(Tile((id \
         28c41d82-980c-4fff-b313-87bdfb627736)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fcfb5d41-7534-4855-a1bf-0595676cd5b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22957846-79f0-4115-aeb6-c16790ec2c2c)(content(Whitespace\" \
         \"))))(Tile((id \
         9c8dde30-2f08-43bb-98a6-1f7ea04b0130)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c0c2a350-63ea-4606-a5b7-8dbd2d5e0a39)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1d10431-366e-4360-9411-ab62540d1b12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9bef597-705e-415d-9ac6-b26a2c0e1cb3)(content(Whitespace\" \
         \"))))(Tile((id \
         54f27666-17a2-4a32-b8a5-ebaa51894eb6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         7359cfdd-a8cc-42e9-a20e-90f6308257bc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9720921a-86f6-4136-b459-b8d3285f6233)(content(Whitespace\"\\n\"))))(Tile((id \
         e2899b94-f61a-4a2d-92f2-50343bc55fc5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         75608272-77ed-47c8-b14c-00956862e273)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93c1dcb7-00ab-41e1-82ec-057e6accfe2a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         543afcfd-4f2f-4730-946a-6466baca0287)(content(Whitespace\" \
         \"))))(Tile((id \
         3f85a537-ebc9-4cd8-b0ac-c0e60749eceb)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ba717a5-5614-46ee-8b6c-124a93079568)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f03bdaf-0a62-472f-9a25-3e7095267334)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         349e722f-75ee-4b1e-a656-f13e1a73142c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cde51b6-4d75-47ec-af90-e11b51883870)(content(Whitespace\" \
         \"))))(Tile((id \
         b826182b-b345-4267-86fe-b0ca5fd988ac)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         131cbde9-b0ef-40a7-b1d5-f79c8e933a77)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b157892d-09a7-4956-b215-fb0f324f3125)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16893fa0-ccfe-4b66-9b11-ec43297f37b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a844227-0e70-4b71-9ac1-8396d7122796)(content(Whitespace\" \
         \"))))(Tile((id \
         563c65fa-e100-4d00-a3a3-d4189bd0d596)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f99226fe-02b9-4e7d-a1cd-c60461d22127)(content(Whitespace\"\\n\")))))";
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
