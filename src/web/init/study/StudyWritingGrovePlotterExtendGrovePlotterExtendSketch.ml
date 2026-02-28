let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / grove-plotter-extend / grove-plotter-extend-sketch",
    {
      segment =
        "((Secondary((id \
         580402f7-73f4-43ad-9b9c-5ca7ae121a39)(content(Comment\"# GROVE \
         PLOTTER EXTENSION TASK                   #\"))))(Secondary((id \
         bcaa19e4-4626-4fab-a760-bd56496f9a53)(content(Whitespace\"\\n\"))))(Secondary((id \
         4641e876-ac55-4258-84b5-b0fce2cd0292)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         7aa0cc1f-5800-4b94-ab68-d96836f51fad)(content(Whitespace\"\\n\"))))(Secondary((id \
         086d21fc-e76d-459b-8ac5-e32651016c85)(content(Comment\"# The grove \
         plotter app lets you plant seeds on   #\"))))(Secondary((id \
         55eff779-f363-4b20-93a9-b8bb75747cc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d09ad1d-e1ac-4728-959a-09995a2e57a3)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         1cb08153-9c8f-416f-a709-7ee6318f0635)(content(Whitespace\"\\n\"))))(Secondary((id \
         b74b3290-e28b-4a1c-b625-50a1f827902b)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         c2f332b4-0535-4e61-85f9-1355cc30043d)(content(Whitespace\"\\n\"))))(Secondary((id \
         875857e3-0e0e-48ab-87f9-6da9b2a5b60d)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         87acf008-b329-45e5-8ab8-815ee8e6073e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b0cee7e-b091-46b7-9c72-ab7b21bf2d2a)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         25569960-254c-4573-a608-0010af840dee)(content(Whitespace\"\\n\"))))(Secondary((id \
         58130c9a-b070-430a-b37c-cb59189082af)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         7f9ab8e0-d602-46d2-ad54-00e032fd4bd7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a02afcaf-2a9b-4e1e-9355-8d8a65c0906c)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         fa75bc8b-0bf5-4053-8d51-4d3dd2ca652f)(content(Whitespace\"\\n\"))))(Secondary((id \
         96eb1899-4ac7-4b12-b0dd-88bcb75c8630)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         f827468b-ac29-478a-acc2-b6ef826d1d09)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f22f153-0f85-47c1-9fdd-b4ace426e1d9)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         d627c002-c70f-4701-aba9-e54430b8e957)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5234afa-b3a7-4280-b3ee-b77d39329e97)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         b525bf06-3516-4364-b672-4f9ee5110ffb)(content(Whitespace\"\\n\"))))(Secondary((id \
         16a8426f-d7ff-4ece-a4ce-c8c49d1dbc6f)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         590bf096-be8d-40c8-9822-660207b1ae45)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec213a5d-6ec3-4407-b3a6-696382537b5b)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         c4968d6a-e209-4fea-ba71-51170ee5a90f)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f782c08-24e4-4740-9d4d-e364812bd77a)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         8aa22912-9bbd-4758-8f92-6148ee6193f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         02eaaa30-87cc-4b63-a02d-28fddf8ac898)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         ec26cc82-f408-466b-9899-3977f057cd29)(content(Whitespace\"\\n\"))))(Secondary((id \
         0dd74e58-30fc-419b-956a-58b3b08a9f13)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         d69088ab-6936-4cb8-8b26-a70582c0bf62)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5caa30e-9ce4-472b-8f38-4edfe027adfd)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         4f58bd45-ddb5-4009-86a1-75055ecf3849)(content(Whitespace\"\\n\"))))(Secondary((id \
         5acd0be7-baaa-4641-817b-58d1e6d3f5de)(content(Whitespace\"\\n\"))))(Tile((id \
         cac5ba08-bf02-4d11-bc4f-335f0216e82f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         de6547ee-e7d8-4491-b20c-b627c17ea7d9)(content(Whitespace\" \
         \"))))(Tile((id \
         fd79bc6f-86f6-46da-a19f-5e163025f529)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         189ec397-3e51-4136-9788-22d727426cdb)(content(Whitespace\" \
         \")))))((Secondary((id \
         8fb18d09-6c2a-4255-8041-4332e6f866ec)(content(Whitespace\" \
         \"))))(Tile((id \
         0ffe49a3-1a4d-42fc-bf1e-6b0e2923cf8b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         55e46acb-e034-4f5a-8701-019fd135fbf3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         88ab9aab-fae9-47db-97a0-0e901145f653)(content(Whitespace\"\\n\"))))(Tile((id \
         42e916f9-89ba-4c3a-a6d4-0c83b658d762)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         727c8708-474f-4c97-98ee-41a4398f3670)(content(Whitespace\" \
         \"))))(Tile((id \
         2957829e-c49f-4fc6-b1b3-d28837b9e321)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         682a1918-db35-44e9-9282-35433d4593e0)(content(Whitespace\" \
         \")))))((Secondary((id \
         2100ba40-1d35-46fd-b2bc-0a3534c04512)(content(Whitespace\" \
         \"))))(Tile((id 8f06a919-8146-47a4-a580-ed38c367984a)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         33a4baa5-dde4-4a34-887c-d61423b96706)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         08a5ad6e-45c4-4ed0-ab2f-c648608fedbd)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         66b79e28-5833-447f-9e43-0ae2e49b1f9c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a65cbf7-caa8-43a0-8dd1-e9356d3e7904)(content(Whitespace\"\\n\"))))(Tile((id \
         571d14e4-5af1-464b-95b3-3497191c975e)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         de953000-ab0f-464d-b98b-12b44e4ba854)(content(Whitespace\" \
         \"))))(Tile((id \
         aaeca3f1-08d7-450a-b516-e17db945ab06)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4a682725-5fa4-45d8-8c18-21096aef6f2a)(content(Whitespace\" \
         \")))))((Secondary((id \
         84789072-5ff3-48c6-af4d-e221d73af953)(content(Whitespace\" \
         \"))))(Tile((id \
         2ee09da3-8087-4bda-a78b-921054475682)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         233bf6dc-4e26-49dc-94bb-bfdba9eab540)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2505cca-7207-4be2-85ac-65753e0d1092)(content(Whitespace\"\\n\"))))(Tile((id \
         5ed53467-03bf-4dca-ab0f-348bf95529b6)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6a39e874-374c-4a3c-805c-5edd847f093c)(content(Whitespace\" \
         \"))))(Tile((id \
         68d9351c-cd6f-4c5b-a31f-bdc85d39d3eb)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         2cf6573e-04a8-4540-8fa2-eecc0457c788)(content(Whitespace\" \
         \")))))((Secondary((id \
         008aa2a7-f963-4d32-8a6d-8e5b5eff3eb9)(content(Whitespace\" \
         \"))))(Tile((id \
         d289b742-ca45-464b-bb8b-c14c6793c954)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         96454616-cb0f-4280-acbf-49c6b0fcea60)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b31446da-29d4-49e0-bb39-5215c1322877)(content(Whitespace\"\\n\"))))(Secondary((id \
         57ed7e51-8322-4d40-8a7c-32d0332558cf)(content(Whitespace\"\\n\"))))(Tile((id \
         a47e2ea8-c7c0-4f1c-80af-422fdec0f45f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         23db16b5-a050-461f-ad74-6071d24f4f86)(content(Whitespace\" \
         \"))))(Tile((id \
         1430f2e2-1fff-4475-89ff-09233d57558f)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         fe4386e6-d7ec-41dd-84c5-3b31de75b47b)(content(Whitespace\" \
         \")))))((Secondary((id \
         28598f4e-3910-4a1d-b11a-044d5ef37df6)(content(Whitespace\" \
         \"))))(Tile((id \
         4934e90e-05f3-4d1b-a7a0-8e6bb6dca078)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         660a677d-6474-4e3d-9ea1-3fbddd7a35b5)(content(Whitespace\"\\n\"))))(Tile((id \
         214879f9-e2c6-47e6-8ae0-bf4225e85d76)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         891abe40-7004-4b87-9300-f4a5bacbd1fd)(content(Whitespace\" \
         \"))))(Tile((id \
         3c713983-aed8-4ad3-bb5d-66d211371ab4)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d295e446-0657-4841-9275-83eb10640171)(content(Whitespace\" \
         \"))))(Tile((id \
         8c1ee279-f05d-4846-ae34-c09e6d8599c2)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bddcae5d-b192-4d57-8437-f83d1832354c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55e07a56-995b-4af5-a3f9-bdb50df0d580)(content(Whitespace\"\\n\"))))(Tile((id \
         7df6a1ab-9b68-4b32-a1e7-33b79a3336d7)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         569f25ef-4640-4152-b782-0aa4e3d51768)(content(Whitespace\" \
         \"))))(Tile((id \
         7da02794-a2c1-4ff3-ac46-527c0b4488a5)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e13ab85f-aa27-4496-b484-dbb46b801a36)(content(Whitespace\" \
         \"))))(Tile((id \
         7346fffc-214b-4e45-b771-acab75b3d423)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9366b891-6751-46a4-ad0f-c948d68190a4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         54a69fc3-b5d8-4786-80b6-38ef14f77d4e)(content(Whitespace\"\\n\"))))(Tile((id \
         19048349-6d07-42c1-a314-ef1c296af2ac)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8d464b6a-34bc-4a9d-b2ff-6e58e691cfa9)(content(Whitespace\" \
         \"))))(Tile((id \
         bc866443-e799-4f38-8323-2d501e3827c0)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         725bb2a8-a3c3-4427-9666-b4fe699c4532)(content(Whitespace\" \
         \"))))(Tile((id cdb60b02-e8a3-4845-be69-c69fcdcf20e5)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         31a34dc7-0a15-4340-9ee9-d6c188fabf1f)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b232121e-c982-46c6-a897-3bc2e814f882)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9f3f95df-5fec-4e95-a069-879bef38b21a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bed6172c-59c8-4edb-929e-409652775390)(content(Whitespace\"\\n\"))))(Secondary((id \
         998541b6-8d3e-4c2c-b942-ad8f8e2ecd97)(content(Whitespace\"\\n\"))))(Tile((id \
         9e0f4418-1278-45db-906a-5e5afdbc4c77)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ccea9317-2e17-4a94-9839-71dccb0831f8)(content(Whitespace\" \
         \"))))(Tile((id \
         910b3b12-2dfa-4918-a543-90729cc66caf)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         fd4a973e-ee5d-482e-aa45-c174f78e02de)(content(Whitespace\" \
         \")))))((Secondary((id \
         fedfb38c-c959-4e80-ba4f-38bb58fabe9c)(content(Whitespace\"\\n\"))))(Tile((id \
         94028d86-8767-4f0d-8c6c-4ea977fdc764)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9d63c7ce-8d97-47f7-b7f9-273161041466)(content(Whitespace\" \
         \"))))(Tile((id \
         06535f72-f88e-4fe8-9213-fa9f80b9b6b7)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         23b936e9-349f-40a1-b9be-fdca16e027d0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9830b84b-77b2-40f5-8d94-e1932a22c00b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b42d7eb2-9e10-4136-b932-4d4b585e99d3)(content(Whitespace\"\\n\"))))(Tile((id \
         c7140d33-d67e-4842-82cb-8c8356869ae1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7d4eb838-f1e2-48fd-ab3f-9d21db204c7a)(content(Whitespace\" \
         \"))))(Tile((id \
         69677598-129a-4ad3-be3d-c61be8e7e800)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         93695c03-b669-4f8f-9220-052fca3abc69)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d2d4c118-090f-4761-965d-1383e0eed406)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         741a0f31-e820-4e7b-aa21-ab09fbab4bed)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b950358b-cd37-4628-b567-497bf60516bc)(content(Whitespace\" \
         \"))))(Tile((id \
         4bb55269-da02-43c2-a4bf-a849cdc7c328)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5cd8f7b2-c465-418a-8b2d-b204c8ee956d)(content(Whitespace\"\\n\"))))(Tile((id \
         cb4ffacd-bf53-432f-8635-2c25c17bc60b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         06dcd65c-8d0c-4f99-870c-57b65bf0a755)(content(Whitespace\" \
         \"))))(Tile((id \
         52d9ae80-cc23-450c-a7ae-218a6f8a28b6)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ebf1084b-bcdd-492f-970a-2a9e98bf88d0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         bcf463b1-fcd3-4a7b-864b-ef4220a9e5c6)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         295483af-ab22-4f20-bfef-a38ca06537fe)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         97453ea3-b5df-48af-ac3b-638d7313374e)(content(Whitespace\" \
         \"))))(Tile((id \
         0c77feda-8453-406a-b920-28413fcc42cd)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e9fb4466-d8e1-412e-89fc-5e2b49014274)(content(Whitespace\"\\n\"))))(Tile((id \
         943b8a61-583e-4b59-a021-5f04823c58c2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         58b02589-7dcc-4bca-9c4c-b1ed91ae7213)(content(Whitespace\" \
         \"))))(Tile((id \
         78ea6e4e-9a08-4851-8766-af18bb080ad1)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e216dfe3-66fe-4b56-ae1e-a73871d28094)(content(Whitespace\"\\n\"))))(Tile((id \
         f66dd1c0-8678-410b-8b3a-975bd45554b0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4ccae45-9060-4aaa-bec1-d7679370025a)(content(Whitespace\" \
         \"))))(Tile((id \
         4ba681a7-c4f4-45ad-8e6b-47e821e409a6)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1530ec89-4591-445c-9e1c-6f572225e15c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         63d9510c-dece-4114-a285-5eb0971c29bb)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ca848764-d564-42f0-8a22-e6dc1a171d96)(content(Whitespace\"\\n\"))))(Secondary((id \
         a565b443-f1ad-4893-8bc3-c015c337e92b)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         f40b04ef-7927-45a9-8581-bf77fe9cea4b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3a2e3b1d-d8b7-4070-8f95-c6305bfa5a78)(content(Whitespace\"\\n\"))))(Secondary((id \
         4081fa3b-841e-4d82-be79-bd5d372123af)(content(Whitespace\"\\n\"))))(Tile((id \
         16e65acb-028f-4771-aae3-e1c09c2d0b2c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5f5fbd6-c2d2-4330-931c-f9b450cc7ace)(content(Whitespace\" \
         \"))))(Tile((id \
         d2abbd6a-6b30-4f31-a378-246b77e66854)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0cf86f0a-fc59-4c9a-aa07-0134f08e545c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b992acd7-2969-4391-8f79-e809c2c824d6)(content(Whitespace\" \
         \"))))(Tile((id \
         b92aa913-66da-4726-940b-10057b21d46e)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         06d28997-71e2-46c8-9285-9875fd966e74)(content(Whitespace\" \
         \")))))((Secondary((id \
         34756f35-02b5-4b12-99a3-093986ce2098)(content(Whitespace\" \
         \"))))(Tile((id \
         dbad555c-496f-4c88-8e2b-52dddd4c8f67)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b2ce71e3-10e3-4b17-9cf9-787910dc9e4f)(content(Whitespace\"\\n\"))))(Tile((id \
         39825ea0-99e6-4c96-a6db-683064119b2f)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e7f2b7a0-1ad5-4271-a526-84f940b96aaf)(content(Whitespace\" \
         \"))))(Tile((id \
         ab5a4be6-9b34-46eb-b7d1-0bf4fe46f530)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         475d3369-a2c4-4947-b2d3-2c57937c1f02)(content(Whitespace\" \
         \"))))(Tile((id 801106c9-bda6-414b-9dbf-f3f0ebdf8412)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         836b50fb-4979-4f18-8222-09fb286f89ae)(content(Whitespace\"\\n\"))))(Tile((id \
         a02ab606-aa59-4bc9-929c-80cb83add8cc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         99068883-741f-4704-99ce-b80b93230d59)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6daa69e7-593e-4ccd-9079-0831caa3767d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da76f771-4711-4e73-be3e-b56e60334eae)(content(Whitespace\" \
         \"))))(Tile((id \
         4ee1f36d-4a4e-4685-8422-5378357fd37a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6be5ee67-7f05-4b9a-be06-0bf92b6df0d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41bbcc71-cfd8-4324-8cdf-c43a2c2df94f)(content(Whitespace\" \
         \"))))(Tile((id \
         edce3061-ec77-42f0-a259-ec10267da6d7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         920b84db-8fe0-44c5-ad9b-18fbbdb8bf95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e01bc7e3-df5e-4142-bbca-a68551ae1653)(content(Whitespace\"\\n\"))))(Tile((id \
         900d49b6-c24e-4d4c-9be5-fe8bf3def0f2)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1e328c74-0a52-44ec-9a47-0fc6f0fde9b2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75e114ea-330d-459a-a424-5333c3577fad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9cbc4ea-7ec4-413c-a450-67c76f2578c4)(content(Whitespace\" \
         \"))))(Tile((id \
         d3955824-a8bf-401b-ab8c-1555aa0825d5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         137d3cf4-22e0-4e8a-8d74-54b98c17dfb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8b8b180-4d37-4ef3-af03-daeff424040d)(content(Whitespace\" \
         \"))))(Tile((id \
         586dfee7-775e-42b5-9e80-3acc3b99e8ff)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         932004eb-bbf5-4f32-b6b1-b7fe4f1efe9d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32a329d4-c128-4c1c-878f-76125954cffd)(content(Whitespace\"\\n\"))))(Tile((id \
         c9844dba-fc5e-4470-b4ae-1261713bdca2)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4323753b-97a4-49d5-95f7-ada16a9344f2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         db3cb7bb-77e9-41d8-8f5d-ef44507e98d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbec7dd6-8b9c-4c11-a41e-04965f5325d9)(content(Whitespace\" \
         \"))))(Tile((id \
         7a01f809-6f5b-42cf-9bd8-75b18ae7ecc5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6ee188c-17f2-41e1-81c1-d4538641b82f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca7ec202-78c0-4496-8393-305bb693c653)(content(Whitespace\" \
         \"))))(Tile((id \
         cfa38cf3-d4a4-45b6-9798-fc4caa3cab3b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         28695b3b-4ef5-45a0-8ec5-7256e6cca78a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fd957871-58be-4aca-b69b-7cbca82dd803)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39274bf0-74d4-4640-abbb-560562b14440)(content(Whitespace\"\\n\"))))(Tile((id \
         67d2b46b-8c29-4e8d-aa6d-a3574921ab5c)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fe6d2c40-3136-4e8a-bb02-c57bd797598f)(content(Whitespace\" \
         \"))))(Tile((id \
         c7c206e5-da4f-4c3e-b298-5dad142c2955)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ac449a4-4da4-4361-adba-02acc021b44e)(content(Whitespace\" \
         \"))))(Tile((id \
         546f0e52-9241-42f3-9d16-efb35cd99c56)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df6e378b-9e4a-4489-bf1d-2d7ac1789b14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         287d9b08-83c2-4c27-860a-b0de6a8c7aca)(content(Whitespace\"\\n\"))))(Tile((id \
         21ff2af5-56a1-4106-8597-1d1e0207060d)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8eb2b771-2bb8-4ecb-a765-b0dc4189c60e)(content(Whitespace\" \
         \"))))(Tile((id \
         aab5f5e4-679e-4065-95da-fad7fa3b6ac1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9c1c1ae-fb90-42c5-89be-10293630f329)(content(Whitespace\" \
         \"))))(Tile((id 94c652f1-d47a-472c-96aa-fa44c2230aa4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db52c337-cb1f-4eb5-a3dc-97f4449c337f)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         968f32a2-8826-4424-bfd0-054cbc899c95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d17c7f9-7358-44da-a4c9-f8651223370a)(content(Whitespace\" \
         \"))))(Tile((id \
         e444c5a7-fc5b-4141-b4cf-2488398fb07c)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3cffa3be-580e-4ba6-98dd-573a729d5baa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77c1a367-0463-41aa-a96c-be4ae2f370a4)(content(Whitespace\" \
         \"))))(Tile((id \
         13bb5fda-c05d-435d-bfb2-483c84cebad2)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a28bd39c-31b6-4bef-b97e-36a1f51200b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69a639cd-edc9-4681-93b1-26d05f5e1534)(content(Whitespace\" \
         \"))))(Tile((id \
         eb7d8d09-77e7-4436-b8a6-977990dfdaa5)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce78730c-3cb2-4777-bd58-bda62b4b83b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6abb0ab-587f-492a-971d-9341e6f6da86)(content(Whitespace\" \
         \"))))(Tile((id \
         c2a178d3-bdab-41bf-bbd5-575ecdafd336)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1645fbed-625b-4484-afc8-9edb3b50c5c5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b84d2986-ee4a-497b-ad8a-e519166f53bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ff159205-6455-48e6-bd1e-71c7becffeb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         26d3b855-2e1d-46db-af8a-03c30a96f6a6)(content(Whitespace\"\\n\"))))(Tile((id \
         480c6f71-7423-476c-9916-acbd7a7038d3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6cfff0f0-65ae-49ee-9854-2cfa7c558160)(content(Whitespace\" \
         \"))))(Tile((id \
         ceb1d0af-99af-4600-a76d-3fa8c7c9b627)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0b41057c-6d98-48cc-83fa-d2756fcef1a9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a1737166-5e1a-41ef-a2f3-b72fc5d974d9)(content(Whitespace\" \
         \"))))(Tile((id \
         5feda99a-ff50-4eaa-953a-6a8363d73bc9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9a10b30f-a17c-4ea2-b486-313677a92e6c)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b34f39aa-e73d-4ffb-875f-ce23b6ccd7ec)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         54ee44cb-50e3-4e2f-88b7-d295cad235b6)(content(Whitespace\" \
         \"))))(Tile((id \
         0a89e17c-a595-4fae-9246-6bbf01dd65fb)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b0b5a0e6-5597-4f80-adb8-f3b124022530)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         207e76b3-7296-4616-9e9c-ffefafaf4af4)(content(Whitespace\" \
         \"))))(Tile((id \
         51b2eaae-8912-44c0-b9e7-7a268daf9f92)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         689027c1-60d2-46b4-911b-ba1efa89397c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a6843f9-148a-4096-bb09-0b5f873ad4f3)(content(Whitespace\" \
         \"))))(Tile((id \
         60b79b07-b9fb-4228-b900-e2d4a103185a)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         81e6b08a-5f92-4af9-a241-e08ea58f2b9c)(content(Whitespace\" \
         \"))))(Tile((id \
         3ba9a242-f049-46dd-83a2-d3a265bb15db)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f69e2a7b-20d6-4d78-9cfb-5f1c51861271)(content(Whitespace\" \
         \"))))(Tile((id \
         841e6a8b-373c-48bc-b604-cb38fae0157a)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         794eaf7e-095d-4d8c-81f1-98ebd218ae8d)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff66dfc8-dae6-49c1-ae5c-741ab24f7dd8)(content(Whitespace\"\\n\"))))(Tile((id \
         7e915544-9536-4789-99c5-a4ed138c564d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5101af4a-54b1-4960-a3a9-32cff720a42e)(content(Whitespace\" \
         \"))))(Tile((id \
         f5feb967-b21f-4ce8-9bbf-399a09cf8cca)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         01c307e0-5941-42b0-9828-f775c93f1a67)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4a77c4dd-2dbc-4f54-b9e7-2a0a64d584a4)(content(Whitespace\" \
         \"))))(Tile((id \
         6a459f40-0559-4ef9-b70a-81bd0f2b30a0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8f3af958-4090-443d-9cf1-1c98445d8671)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         963e4726-8874-41a2-9de0-9aba267ecbbc)(content(Whitespace\" \
         \"))))(Tile((id \
         dd347870-6d7e-446d-8803-0ed8af315de5)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7e06b7e1-5a90-480a-bc74-06421fe00e75)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9e8c2d5d-0dbb-479a-87c3-f290b639a218)(content(Whitespace\" \
         \"))))(Tile((id \
         f0020f34-1c8c-41d6-aa54-8689f4cc9287)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db9062e4-ca30-4358-bf3e-891ff5fdf554)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32e7d2c0-4b3d-4578-a6fa-b688a0263012)(content(Whitespace\"\\n\"))))(Tile((id \
         3d1dc73c-7f40-457c-9d08-895aed5bdd86)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00794821-54a2-49a7-8621-0a29259cc19f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3fa7426-f8dd-4fca-8a6b-d138cc0375b1)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a201d6c2-a7e0-4386-b6b4-f2f65254776f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0fe2c75-cb85-456a-a0f9-d6f79e88d10b)(content(Whitespace\" \
         \"))))(Tile((id b7284125-3dfe-4714-ba51-f9cdb8878013)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c32e9842-89c0-4ce0-aa07-e7f8d6c5b85f)(content(Whitespace\" \
         \"))))(Tile((id \
         34e2f11d-430d-44f4-b183-684195ba47b9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2c50cd67-9cd6-41b7-a740-84317226225c)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         36e37b03-973d-49f4-baf2-0c0a5612a0b4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1c1511d2-a5fe-4d78-ab2a-0b94c490710c)(content(Whitespace\" \
         \"))))(Tile((id \
         69721aec-226e-4360-a978-38d4c57d2c50)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ea1ee041-42d5-4b29-9e00-34e2da1e74d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6c4d9a5d-6509-4eed-8ac5-d08b797b9970)(content(Whitespace\"\\n\"))))(Tile((id \
         bfa57f76-aec1-4ae2-b181-56d83035bf6e)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         788ee27f-fb9f-4fca-86f0-ecaf28a089eb)(content(Whitespace\" \
         \"))))(Tile((id \
         b77e5b2f-e8ac-4a07-b80c-74eca270243a)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d9cb5157-7ed9-42a7-96d1-75da95c88ab2)(content(Whitespace\" \
         \"))))(Tile((id \
         fe22e265-e81d-4d25-b94e-e012f24f4db9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5af9f2c-d78f-4e79-b520-734b383ba4e2)(content(Whitespace\" \
         \"))))(Tile((id \
         b32dc599-256e-4214-b22c-4b5bd6802ba0)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         489bed42-2db8-4c0e-a3d6-4b2034bf8147)(content(Whitespace\"\\n\")))))((Secondary((id \
         af0f9a91-8ba2-439d-8196-735bbcf786bb)(content(Whitespace\" \
         \"))))(Tile((id \
         f67baacb-084e-4db7-8eac-e06eaa299e53)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58c87fed-9c56-4a07-8a30-c4020e867b65)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87f5e0cb-32bf-47a3-974e-4184ec899dd4)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b033cb7-b7a0-45a9-ae81-7a819e265166)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b5060e0-c1fe-4957-be66-7077bb514abe)(content(Whitespace\" \
         \"))))(Tile((id 315c5473-802b-4de2-a95d-bb48b23e47b7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1613ba56-7523-4912-976d-70637a76edf2)(content(Whitespace\" \
         \"))))(Tile((id \
         b0f1392b-2c7e-479a-831a-21cfa671da8f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8adee4b7-e37c-4b05-aff3-9e197909672d)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         beed548e-35f0-4504-a8ab-aaca481f80f9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fea100e6-5a94-4b2a-b632-6a158fe2d0b9)(content(Whitespace\" \
         \"))))(Tile((id \
         c663e4b8-a0ec-4cc1-a000-d9e6f912d41d)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         64b15364-ed1a-4ae5-98e4-4d63520c8de5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         85682af0-8489-43af-bfe1-8375ff2baa7c)(content(Whitespace\" \
         \"))))(Tile((id a692e547-195a-442e-97b0-441b745ccaf9)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         92022a47-7a8a-4520-9e38-7b1be8d26a98)(content(Whitespace\" \
         \"))))(Tile((id \
         6164d18c-3684-4b13-96b4-64976fb4ffb8)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f2e49a40-2703-45c3-b17b-d7654192934c)(content(Whitespace\" \
         \"))))(Tile((id \
         cca4efc8-e0dd-4ac7-ad37-a657dbab0e36)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         772b8fc5-2260-4a37-a325-b2b7222989c7)(content(Whitespace\" \
         \"))))(Tile((id \
         6231086d-5aa6-4411-b26a-f278f2dfc87b)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8ff41e05-25a9-4f0c-aa29-6b32b63d0c63)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5fd2823-f45a-4e60-99ee-89a31244407d)(content(Whitespace\" \
         \"))))(Tile((id \
         edf73200-5271-44ce-891a-89dea948d531)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23ec2101-e529-4ce0-9122-a47c30db380a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e04192fd-8c7f-47cb-b04c-3edd11f60262)(content(Whitespace\" \
         \"))))(Tile((id \
         c8a49077-96ee-42ba-af29-5c247faf87a4)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         34022419-5a57-42e8-b4fa-2a68a529f35c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         670ee0c5-425e-4667-8a65-902f23f847fd)(content(Whitespace\" \
         \"))))(Tile((id \
         1d8b1d32-ef29-4936-875a-e818a2240093)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         836bbdd9-f866-4e40-8080-0c5ed5feffca)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c99f563d-95ce-48ee-8f39-ef1532e6a750)(content(Whitespace\"\\n\"))))(Secondary((id \
         59465b2b-8124-4604-8634-e93d643c551a)(content(Whitespace\"\\n\"))))(Tile((id \
         87639f53-f2f2-4f65-86cd-8bc97fed43bd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c058fdb-9fd3-4f49-a7c5-3ca34e054910)(content(Whitespace\" \
         \"))))(Tile((id \
         51eeb18f-cfb0-49dd-8754-219467089491)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c723c583-2d7a-4500-8835-a0973478aff9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5301095b-31e5-435b-85be-2925cdc3daa4)(content(Whitespace\" \
         \"))))(Tile((id \
         9b4e1e61-4ac5-4fd2-8dc3-39a1c49ecf1f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         48e92206-60ad-4a86-bc2c-04b8e04a6e1e)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4abe1cff-11e7-45b0-9b30-88c540e20dc7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b08961c1-c38a-4258-859f-6594cee84532)(content(Whitespace\" \
         \"))))(Tile((id \
         1a8283d3-cfc9-4dc4-bec9-6246c885ed3c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         54530017-d647-49b3-9ac4-67bd6aa32853)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e2685ab2-a177-4066-a611-143d8679a347)(content(Whitespace\" \
         \"))))(Tile((id \
         8c86498d-0b67-4f98-9b48-081d19caf4b7)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         20d6517f-9641-4e75-b988-99b368733419)(content(Whitespace\" \
         \"))))(Tile((id \
         5b1c2945-799b-47b9-8a26-19eed718337f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c17b4de4-31b4-40f9-8c2d-fd88e1e50ee2)(content(Whitespace\" \
         \"))))(Tile((id \
         1cda8cb7-2605-47ab-804b-2fc933fc7abb)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6984ff8e-43e7-44af-b6c0-fcc054c6ee4f)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d24ec71-c1e8-4c56-8491-00b76a297d5f)(content(Whitespace\"\\n\"))))(Tile((id \
         358ecfe0-d37b-47aa-8b57-553d5de7cb27)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dfcf6480-476d-46a2-95d5-fae9fedd7bb5)(content(Whitespace\" \
         \"))))(Tile((id \
         d829942f-f7e2-4515-8f19-728428b95277)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f7997e0c-8b6b-4267-9a96-dd6c9e4e794c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d547a8d5-08a7-4997-b91d-6ae7fbd98c63)(content(Whitespace\" \
         \"))))(Tile((id \
         d62565fe-34af-4058-ac6c-e7ed929ef49d)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c88adc37-ccbe-4320-be03-6209c5dde0f7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         652821ee-f80b-4d4d-9c36-36cdb0f461c8)(content(Whitespace\" \
         \"))))(Tile((id \
         3602be30-82af-4177-a157-c97c023aad50)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         99ad4ea0-e3bb-49a3-a0c8-4138f32098ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f4ceb8a9-51af-4645-8559-9f9cb1c22b92)(content(Whitespace\"\\n\"))))(Tile((id \
         310e5444-c2ae-4b4b-91bc-ffea37884c6f)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1097943-86ff-46a3-80af-0a637835a516)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df9936c7-851e-49c4-9a98-4fc13c65f5f1)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cd1a8f3-8e23-4de3-9181-02c9c49feb07)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         932fa4a4-6f05-47d9-9d76-a3cad15fa604)(content(Whitespace\" \
         \"))))(Tile((id 8ec5b6fe-dd05-4921-9b88-bcb1046bf97d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a366f344-7969-4ff0-9e1f-eeb35d70d499)(content(Whitespace\" \
         \"))))(Tile((id \
         f668227f-d2c0-4a9c-8b71-7f1768c3f89f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6e94e9a4-cd28-4e1d-aa75-8783321ea6ba)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a4dc464b-ccad-494a-8634-8ad280ffb493)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6edf2a57-df1d-438d-aaa6-5bbc269b49b9)(content(Whitespace\" \
         \"))))(Tile((id \
         32b15597-92e7-41e0-86ff-2fb2c4395e52)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         8755e373-8d43-4846-997a-6cf63c975355)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7de9d74c-cb52-4730-a63a-3a3ac18de4e1)(content(Whitespace\"\\n\"))))(Tile((id \
         0cab8048-aa26-4ea8-b8bf-8909415e6223)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         567f3088-b76f-4c25-8262-cb8360944fa6)(content(Whitespace\" \
         \"))))(Tile((id \
         3eb5fa89-315b-4b2b-be12-3e7c0eb97a21)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b67f3fdd-cb68-482a-82b1-ecbed184fb6a)(content(Whitespace\" \
         \"))))(Tile((id \
         0eeb2d39-1d5d-4d22-98c4-d95ad2beec2e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19c7c6d9-6a50-4d74-89bd-643d4a08eead)(content(Whitespace\" \
         \"))))(Tile((id \
         99167506-a8af-48e8-b9a7-4ff96c38def6)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b278f8c5-4e88-4747-a50a-8d542004a4ab)(content(Whitespace\"\\n\")))))((Secondary((id \
         3e93b5f2-50f7-4aeb-abc2-982ea2e01f0a)(content(Whitespace\" \
         \"))))(Tile((id \
         3c30de46-ff3e-4aaa-a114-5e8ce2dac996)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93c46098-ade2-4b48-8099-0f0dcb048dea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1e189d3a-faa4-4746-8c72-851c622704e1)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c41b82f-7e4b-4c30-8802-3e6e344ec280)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74b29d8e-ce9d-4643-83f5-ec3ba52f9a63)(content(Whitespace\" \
         \"))))(Tile((id 293ea962-fa8e-44d7-8d9d-994f9dc3dfad)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8eb7b566-e454-451d-bc44-9407ceb9d8a5)(content(Whitespace\" \
         \"))))(Tile((id \
         641bc165-f3c8-47f6-b0ed-edca1cf20e16)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b5b622f5-8d73-4cf2-991d-9c7c52159565)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e1fecb2-f7ee-439c-a63a-0dc4895b4172)(content(Whitespace\" \
         \"))))(Tile((id \
         89d8c874-9f89-4567-9b7d-7f83fbf49279)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         03f8d089-3e97-4dd7-a09f-dfb7ddaa68b6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a6d6542d-90a4-49fc-926f-6868ae4e25e3)(content(Whitespace\" \
         \"))))(Tile((id \
         e342f174-1302-4554-b84f-9d4257d4b10a)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9c03f7f1-f343-49ae-85fd-c11d330e3974)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8ec206cb-571b-45e9-89f3-ebd6fb56a158)(content(Whitespace\"\\n\"))))(Secondary((id \
         20dfc83f-e648-4205-8faf-2c7e966915f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         3179bad2-f51a-4105-984f-eddaf0a28959)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         df457a85-9ae8-47a2-a331-b74f7965d5a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebe4cf99-5576-4dcb-a6d4-e191b6d4490a)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         e842cf4a-7ae7-4141-a0ed-38a78aaccf0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c23dca4-18d2-4e56-a2ec-e92270cd86fe)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         1427ec9b-454b-4c20-a010-93ea586b26d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d7bb637-21f0-4dbb-8b85-4b2725031d2a)(content(Whitespace\"\\n\"))))(Tile((id \
         6d0ce7c0-a5e4-42dc-8ac6-bb351277fb3a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         26b651b2-5e7f-4655-bdaf-34d2f1239391)(content(Whitespace\" \
         \"))))(Tile((id \
         161c89e1-94f8-4016-b480-f0ce109773b2)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f644f79d-69a2-40d2-9f16-2d7ceb3fd97c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e0e7bf65-6a5b-4378-a687-ba820a359016)(content(Whitespace\" \
         \"))))(Tile((id \
         521e6b41-985f-405e-a14d-507ed3954d0e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         65c79dfd-a959-4816-9da2-63da0d2c784a)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a1c20e11-4379-4987-9f02-959ee653b4cb)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a18b7f42-fbe1-4c96-b189-4f011a66f1e5)(content(Whitespace\" \
         \"))))(Tile((id \
         cc0ad5c1-e32b-4c8a-996f-340922f9be89)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         88a99a4a-6fd0-47e2-a704-0209c9cf7a4b)(content(Whitespace\" \
         \"))))(Tile((id \
         71579d5e-7274-4887-ab24-0dfb007be076)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a9f89ec-abe9-43c4-965d-84e03e09f994)(content(Whitespace\" \
         \"))))(Tile((id \
         26db6e81-ee13-466f-9cca-518157c23294)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8f91f3af-fd0a-41e4-85cf-30a099265721)(content(Whitespace\" \
         \")))))((Secondary((id \
         c1507400-864b-498d-9bbc-c5443ece8703)(content(Whitespace\"\\n\"))))(Tile((id \
         e94e79c5-f9d3-4dac-88ee-10f3394c6f70)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dda69e04-c3ee-49ee-9d28-698aa04d104a)(content(Whitespace\" \
         \"))))(Tile((id \
         5c5adbc3-fa51-46bd-be20-76e534278ffb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         06531078-b588-4ac9-9a82-72ef3a47ea09)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4ec8f58d-5685-4d86-a460-60242f1eeea1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1dd795a6-24df-4374-8cd1-15d03589c749)(content(Whitespace\" \
         \"))))(Tile((id \
         a9e366b7-d310-4dd0-a790-b49f3c7ed38d)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2d243eea-a591-4dbe-90d9-31be998fe561)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f090bd29-a019-46ae-93ad-f8b130262ba9)(content(Whitespace\"\\n\"))))(Tile((id \
         726d5a9b-303b-407b-9f42-56acf5e28032)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5df7d360-4787-4441-b992-e6da28deeb7d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c5882f63-5711-44c1-95aa-9e74d39bd755)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1993a6e1-e2fb-49c3-99c5-dcf2097afd56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8ec540f-7576-4296-a7b0-1cea0af81f6f)(content(Whitespace\" \
         \"))))(Tile((id 3f1496a6-2815-4393-905d-b6ddf24e32b6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         87dc4fb1-598d-4b32-9302-57eade7d0349)(content(Whitespace\" \
         \"))))(Tile((id \
         a719c1ce-e722-4ae8-827c-851d2fba4283)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4547041f-acb8-4c18-af08-ae0f33d6921e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a346cb6-1825-4f50-bd73-502248c60b17)(content(Whitespace\" \
         \"))))(Tile((id \
         59e556cf-f2bd-4000-a8a9-8419f9927048)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9517ba3c-2b49-4db2-88ad-4879019511c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1297d380-b040-4d72-8bad-b1aac43114b8)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9462407b-cfeb-44ef-b67b-90ecb1b8928d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6240167f-d04a-4028-a7b0-d682420ea5b9)(content(Whitespace\" \
         \"))))(Tile((id e3cdd38e-ffab-45c1-8bab-d06ed230ea50)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         043d8498-35c1-4c89-a84e-b5da479cb775)(content(Whitespace\" \
         \"))))(Tile((id \
         53ba26c0-24bd-4686-ab00-ad517fdc63df)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         424e4cb4-2196-43d1-84b1-4d610f0611ca)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         437a589e-cbca-40c0-a521-58b5a90347f9)(content(Whitespace\" \
         \"))))(Tile((id \
         9326f71f-d051-4604-a0d1-2deb606b052b)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         48df0c9d-75b0-4ed0-b857-85ef87f9b53e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d86dbb4-cb77-459c-92e4-667f54b288bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         44675297-df5d-432d-bb99-4f3b7e01407f)(content(Whitespace\"\\n\"))))(Tile((id \
         e84c7d03-937c-4694-b14c-be8754eb0deb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48d9fbfb-d585-43e3-a992-268161b2afc0)(content(Whitespace\" \
         \"))))(Tile((id \
         65f98c81-1692-4e3f-89a1-505156c16f82)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7edee56f-fef7-4013-ae6c-5d2bec5f42d0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2df151f1-30d5-4852-9c19-2a3e5d6bdf0f)(content(Whitespace\" \
         \"))))(Tile((id \
         e4fb5f9a-7ae0-4269-b384-d85c67ef3c35)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c79420e0-1f0b-4024-b46c-6c6d4b620408)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e99b5818-15fe-437b-b544-173bf06edbb0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2285672e-a154-41c8-8c74-1e41ed147217)(content(Whitespace\" \
         \"))))(Tile((id \
         0530ac76-13a0-4262-a3e3-0b05400bd9a8)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         26860b73-90c9-427f-91aa-fba866388eb1)(content(Whitespace\" \
         \"))))(Tile((id \
         652c0c48-310d-4df5-8da6-214c0c3580bf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         769a3c5a-5472-4bfa-ade4-ea1d7344e8fc)(content(Whitespace\" \
         \"))))(Tile((id \
         8086be5b-16e3-4772-80f0-de389416c10c)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e48fc645-1d90-4ab8-8529-0c110b7128f1)(content(Whitespace\" \
         \"))))(Tile((id \
         6183f988-8169-4970-a638-92aad116ce2e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5e67948f-79ef-485d-bf87-fa613992c79e)(content(Whitespace\" \
         \"))))(Tile((id \
         78fc7173-9faa-4ce7-ac5a-448c54b7dbdc)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f7f1d452-d548-4bec-8ad8-db0589f9d5b3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7d70dc16-3989-4ff2-a025-20e54558a06d)(content(Whitespace\"\\n\"))))(Tile((id \
         f9ca8f9c-9305-407e-8689-6a11e9d7d0c3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         376f433c-1b8e-4d32-ab59-978453f38021)(content(Whitespace\" \
         \"))))(Tile((id \
         13392e3c-ea24-474f-9c02-09db0a465301)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         781308bb-d2d6-4691-8c65-231cece1d009)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c2ef8a20-5237-4205-ab70-0dfae03bd5c9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b9832b98-5c34-4ff0-a730-c7a54cd28257)(content(Whitespace\" \
         \"))))(Tile((id \
         9cf0a8d4-6bc9-4ad6-8d42-68cf56ecd484)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f3f47ee9-c885-41ea-a8f4-6fb7b8e95154)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9353b52-abb2-415c-9bb2-2af2d011e4b3)(content(Whitespace\" \
         \"))))(Tile((id \
         1c4b3bab-c4d6-45d7-b048-6371f45c3123)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         359d6e85-b122-4e96-975c-821bdb58bbb3)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5682d0d3-13ef-4cf4-9954-4424c2b98ce4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e1c772e1-e303-481c-9a68-597b6acad75d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         201ad728-46c8-4ecc-a9b9-4ae09400e060)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c5b8d172-8a13-481a-a912-c67ce599b892)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b8c26a7f-1f03-4966-883e-b1e0ed0a01ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31f147f7-748e-4bb8-8a8e-0daa9ee2e485)(content(Whitespace\" \
         \"))))(Tile((id \
         a6e4bc5c-8b70-4781-be36-c8125670e346)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1711bb5-3a36-46ad-9c40-91f1c67863ed)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2877b5fc-8cae-40e5-840e-10b804645950)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2695f974-8fb9-49c7-8be4-d874687bc1ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79f5789b-fc57-470f-bafd-fc0ea04b19ff)(content(Whitespace\" \
         \"))))(Tile((id \
         0014c8d1-04cc-4e85-8b1a-ef48f5c34ce5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a68bc620-b6a8-425f-984f-9cfb7275c6ef)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0e4d7bd5-a24b-4477-817a-84c8903a35d6)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a3060149-c0a6-4a3e-afff-306da0a4569b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e10dd9cd-9244-459a-a83d-40116039471c)(content(Whitespace\"\\n\"))))(Secondary((id \
         535321d8-3ed9-4bff-9dd5-048e79627795)(content(Whitespace\"\\n\"))))(Tile((id \
         bc9f5ffa-cdda-4bd4-b7eb-84b5e7a9bb8d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a9dd8125-dcb1-4a10-b563-0c3f4ae72d85)(content(Whitespace\" \
         \"))))(Tile((id \
         fa5a6275-c5f6-4553-9a42-5a6849adf66b)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         effd640b-2f26-472c-b10a-75b80e8f7904)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         56042d26-00fe-432d-93f1-f1f62754eb1b)(content(Whitespace\" \
         \"))))(Tile((id \
         cbaba6ef-20fa-405a-9e9b-7ab704a6dda8)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0f5b3cde-db59-434f-8f31-6897a814db3d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d73dfd02-2cd7-425d-8b0a-3552c7196c24)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         83b5ad11-8283-4bf4-8c28-7da5ec306f90)(content(Whitespace\" \
         \"))))(Tile((id \
         0aa36fd1-d49b-4a57-ba15-a155ae2af741)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ce266f3d-47c2-45e1-90b3-07184a490528)(content(Whitespace\" \
         \"))))(Tile((id \
         f6d065d8-ad1e-4a49-b29a-a84a064b75b8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         85e326d2-9ed5-42b1-ba51-d49b71e0e242)(content(Whitespace\" \
         \"))))(Tile((id \
         e80c5bd6-5446-475e-8f7d-10fa061dc7d7)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         75f8bb3f-e64e-43a7-9e49-4f18754f31c8)(content(Whitespace\" \
         \")))))((Secondary((id \
         e105e496-9da8-4fd7-a4c1-4a29a10ab179)(content(Whitespace\"\\n\"))))(Tile((id \
         5673465d-3474-430b-8e37-6f114bb115c9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1dc1ccc7-1377-47bc-a833-315c972c1f65)(content(Whitespace\" \
         \"))))(Tile((id \
         b6564eaf-ba5c-440c-8038-b47c949123b1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9ed53378-e170-4782-89e5-766130c31103)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e0ac8b5d-2ed0-43e3-8d3d-ff08cb404a86)(content(Whitespace\" \
         \"))))(Tile((id \
         75edf3e0-2f97-470a-af4a-02244a49d2e2)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         26e8d1df-52b8-4008-ac8a-ad5b7d51905b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b6d30450-07ce-4772-9c1d-2519b213d107)(content(Whitespace\"\\n\"))))(Tile((id \
         07050732-78f4-476f-accf-30b9e025af3b)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e3abecf1-d69e-4e11-891d-278e7e0e8592)(content(Whitespace\" \
         \"))))(Tile((id \
         05721663-eebb-475a-a97c-f1af41b43bc6)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b186ba06-089a-4f0b-b77c-57789780d17f)(content(Whitespace\"\\n\"))))(Tile((id \
         6809decc-6852-4f4a-b220-0e1600ce2d6c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2f08a97a-991a-48c7-8bc4-824bc43c6ce2)(content(Whitespace\" \
         \"))))(Tile((id \
         278eafb9-3398-4afe-a2bc-901c12d9eb34)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cc1562cc-a926-45de-aeec-9bcefb67ac38)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         4f41fc37-227e-4100-b650-929e140a1ff5)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         51c86ee6-88ab-4fb2-a90e-117e64d41524)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d75ba5b3-df41-4d23-bda2-6640641f875b)(content(Whitespace\"\\n\"))))(Tile((id \
         50084f6d-cde4-41ee-a734-632feb1a175d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         58402bd7-5865-4f6d-b9cb-7e547d47566c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         658521d3-707a-46dd-ac00-63b96ef28aa3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         bf5cf255-3438-443d-bffa-0052d7d647ae)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f145c61c-68eb-4eeb-8c94-ca64d9eb9fa2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e88046f5-abe3-49da-a567-98c3e83367ec)(content(Whitespace\" \
         \"))))(Tile((id \
         7f8799ef-6f55-4a04-828a-61e59804538d)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91e3c904-a167-4d36-825d-37e09ffdfef8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7c21e20c-c773-4de4-b4d8-4a454eaece54)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb532423-5255-4aa8-bc48-afbf270bd2cc)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         878c4bf9-ea64-47a1-bb8c-e39a5fdd0469)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ec48418-29f5-473f-a14d-02a4c31f953a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b501182-ca27-4160-9185-cd2ed15b2f51)(content(Whitespace\" \
         \"))))(Tile((id \
         cb2ad33b-8af4-4b1d-859b-4800eda950e7)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         50e2d055-2592-4d7e-9d91-6c05513aad08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0bfd8cb2-1dd1-4036-9d4b-90b02001edbc)(content(Whitespace\" \
         \"))))(Tile((id \
         af522d99-86ce-4c91-b799-84bde7c017e2)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e7a5c79-0d13-459c-b352-0effec17f5eb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ef02a0b9-7266-43a4-a311-dd161e331937)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         73cb6230-06a9-4ee9-9f34-d346ee8f7a7d)(content(Whitespace\"\\n\"))))(Tile((id \
         d8e7824c-d650-4de5-9c2c-8ace261b2c94)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         05cec094-8711-465b-870f-4ee71f453667)(content(Whitespace\" \
         \"))))(Tile((id \
         e55fb1f4-70ce-42c0-94ca-561d5de7de1c)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         04530f1a-5433-44b7-b5de-5eecc9f85144)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         ae4c22f0-31b7-4ee4-a54c-2c583b338c1c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         452e6b3a-6186-49c3-a397-0ed0ea205e64)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c397f80f-fa79-42cd-8ad6-e3e6994149b8)(content(Whitespace\" \
         \"))))(Tile((id \
         e4e19081-f3b0-4095-8e5d-844eb637ac9a)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         48571e72-a55b-43b5-94ed-2b2655b3885a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c4bd037d-ceaf-4255-881b-1a7b375a1a8f)(content(Whitespace\"\\n\"))))(Tile((id \
         6b25005f-a756-4e8f-94b3-0439ff9cdf55)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9261ab6a-0769-4518-bcd2-cadbb2f6af38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         75c20b04-d564-43e5-a0a7-006eb3769045)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5cb3db06-2392-4ffd-b7a4-959abd6da714)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e392cc9-dd4f-4dd3-b4c2-bf4bbc797671)(content(Whitespace\" \
         \"))))(Tile((id 5a923afd-ea58-47a1-b17f-b20d82120897)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         885db4c9-d840-4dad-a3ef-12556332defb)(content(Whitespace\" \
         \"))))(Tile((id \
         89a6ce8b-2a50-476a-8d65-1eb6d0b0aaad)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f11fd0ad-8f88-4e10-85f3-20d32d6a7aba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         87fc4dc5-0dd2-483d-abd2-8358bc4c3466)(content(Whitespace\" \
         \"))))(Tile((id \
         158e4f9c-8250-4932-85d2-569ccd2f79d6)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73bcde72-e2a3-45af-a94b-104033b389d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5465767f-6741-4485-b115-72908ad26ec6)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7b6ebb6-d62e-4b54-921f-f563d0ba4679)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c612762-dd8c-4964-8f50-e0e8fb15f9d8)(content(Whitespace\" \
         \"))))(Tile((id \
         bf82fbaa-42bb-4866-af8b-9a7d186d5465)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c99264b4-a5ae-4751-b460-68ad3bea40cb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b3c6b1e-4328-47e8-9106-de956e5c0f13)(content(Whitespace\" \
         \"))))(Tile((id \
         33d76e8f-80bf-4e33-acb7-4248997602a9)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3422ad6-01a1-4a58-96b6-17769ee35353)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1210b0a9-d052-405e-9ee5-5186dd1ec9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         64bcacf9-1f73-477d-9d2f-f581ce6b534f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c8031eb-907f-4648-a92d-e8e6da0ddc5e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fd52130c-5025-4c3c-bb0e-7a4235ee7392)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         22674dbe-c08f-4314-a6f2-1d5426549ad4)(content(Whitespace\"\\n\"))))(Tile((id \
         8cea98ea-e815-461b-9cd8-acbfdedb410d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1757468c-2e7a-4025-ac83-2a281127560b)(content(Whitespace\" \
         \"))))(Tile((id \
         4a523259-3f9b-4281-92b2-c6b942ae8adb)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d000111e-3527-4091-a732-0276a7911772)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         9fba622a-dfa0-49f4-876e-8e94070e96e0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         582615f6-db08-46a0-be2c-1067c0b2214c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cbcca69c-b136-4af4-abec-bbd7149febc1)(content(Whitespace\" \
         \"))))(Tile((id \
         caad1d8a-ec7b-4069-a75f-965e20bbc8f6)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2b31dffe-757e-443e-aced-6eb29791d4f8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e58b65da-89f8-4335-b87f-8d8dbaf3c803)(content(Whitespace\"\\n\"))))(Tile((id \
         c9b3f786-4b6b-40a3-b349-3be0fab5e487)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dfec058e-0067-469c-be36-15ba3fc5ec5e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4796f19f-b714-4044-8f05-614a90ca78bc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8450079d-68bb-4233-ba43-8945fbc95fd8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dde074cc-138d-4fbf-b446-fb32b3b948e6)(content(Whitespace\" \
         \"))))(Tile((id 158b283a-3783-4d73-95de-b85b0fd59a6d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a0e30397-071c-4287-8057-d9e1dcd4070b)(content(Whitespace\" \
         \"))))(Tile((id \
         85449508-506e-472e-9c30-1d281f024821)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1fd58bb0-e017-486e-84bf-90556126118f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5a897e7f-b2b5-4729-9980-116a62ed26e6)(content(Whitespace\" \
         \"))))(Tile((id \
         ab67aae9-4105-4bfa-8fd5-9056a27d07c4)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a69b8fc-f04e-493b-88f1-b9672e0004bf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         89139a38-7911-4cca-a878-4183ff722b86)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34090c54-8bc0-461b-b8b7-eac0a35669d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b654235-ff8d-40ed-8674-7c5862a627c1)(content(Whitespace\" \
         \"))))(Tile((id \
         2e8b1368-5fd7-4cf4-858b-da94862f51cb)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         230cba55-8b03-47f9-9ce6-a2b95a438708)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69264e7e-6ace-48ed-b2cb-3687976dbc06)(content(Whitespace\" \
         \"))))(Tile((id \
         2114861f-1933-4715-b074-613738e38543)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04f6d48e-0bb5-40e0-8a4b-24df45a7f09c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e7b9e913-50d7-43f1-9170-16df9c2357a0)(content(Whitespace\" \
         \"))))(Tile((id \
         ad1742a7-5ff8-4c8d-9f8c-d92a5e846b82)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         9bf97081-c893-4c1e-9e39-66b7b80559bb)(content(Whitespace\"\\n\"))))(Tile((id \
         d734164a-0b87-4d3c-acdb-f1ee46b3ceec)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         af33541e-c564-47c5-96f2-b4a393fbeea3)(content(Whitespace\" \
         \"))))(Tile((id \
         b15ef289-9f49-4ee7-9b10-8b3a9bd8ad72)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5923baa8-ac99-4c62-9350-ab2ef8d833a4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3d40d409-47d5-4e84-be2e-b12e2b21085e)(content(Whitespace\"\\n\"))))(Tile((id \
         556718d0-ef8c-423f-89fd-5148ee1e4bba)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         356bcedd-ce1a-4aa8-930d-f180ef9f486c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b2b5191-34a2-4f18-be41-dd0f82969afc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00c22ab0-868f-4a8d-aef4-0a11fdbf6dd5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70d54202-1746-4521-b7cf-32b1bf76fbd2)(content(Whitespace\" \
         \"))))(Tile((id b2a4a647-e4bb-447d-be29-9e459a02dc89)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5a189437-2d3f-4e32-9f6e-078ebb435144)(content(Whitespace\" \
         \"))))(Tile((id \
         84887122-d6dc-4185-b914-118afbe40146)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6c5d8e4e-6469-45b9-9b64-db2a327abd4a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9512a03f-a278-4435-888d-232f97b05656)(content(Whitespace\" \
         \"))))(Tile((id \
         6cc9cf86-f66c-4e26-bf77-b13e7a1be82c)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fdfb74b0-bbce-40cf-bbf7-52001fe01663)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         04204574-6fb7-419f-a5e7-f26d9f4b4ecc)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1288c55a-c632-4b11-802a-867a0c2d5cc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7b4b7ac-efc4-4998-bc0a-bf5ad4b30d26)(content(Whitespace\" \
         \"))))(Tile((id \
         d2cf6ee4-9ae2-4392-9c56-09f71dabdae5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c7a52fed-1e02-4cc7-89c4-cda3de42445f)(content(Whitespace\"\\n\"))))(Tile((id \
         e4703edb-350d-4eb1-97a4-8a88ba8b55a4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3c07b12b-1cbb-4322-abf1-0ca603be55a9)(content(Whitespace\" \
         \"))))(Tile((id \
         b4b9ab7d-654b-4641-8a6c-6040665e5429)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2c30de62-7ef9-4904-b155-87e65545d360)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         96e3548e-8826-4f46-ae05-45606fd5bc82)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         43b45eda-1be2-43d3-abbf-2d5a46aed24d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f5878b1d-3b7b-4d4c-a1b0-b6f4c1d1f49d)(content(Whitespace\"\\n\"))))(Tile((id \
         c099d20e-9ea1-4ddf-b970-49bccd63d590)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37a6393b-7cbe-4ff4-a766-69cfdba51c05)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0cdb31ff-bd3a-49ea-b583-f30dd7d319b1)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33d5fb97-a029-4531-a30a-08c2b0a41759)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c1cd6aa-03c0-4f91-af68-afb94cce40f3)(content(Whitespace\" \
         \"))))(Tile((id a6b9d5f7-6639-4a04-bd98-eee8f276f685)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9bceef53-c6bc-40db-9bea-59589f780575)(content(Whitespace\" \
         \"))))(Tile((id \
         af94464a-45f0-4c47-bf50-c4d744f4bbfc)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bd5713cc-d90c-4bfc-bdd7-ccda4ee0b278)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a8fac156-4357-4278-a86e-51ca072f98cc)(content(Whitespace\" \
         \"))))(Tile((id \
         12bb34da-b7f9-402e-906e-37b32c1d0a25)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cda0c31-23be-4a0b-8178-43c602ee69b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de375cad-c62f-4844-bdbc-ebd851b826ab)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ccf4129-ebf5-49d6-8026-28420fdeedb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0d8b27c-8f1d-4664-b5a9-24cda620ff5f)(content(Whitespace\" \
         \"))))(Tile((id \
         203812f1-765e-4f0d-b875-be8bb694fcb4)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da4961f1-5cc2-4dda-9fd6-9714dbd6c4e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         116953d8-4e53-4f04-bd11-bdd02517dc02)(content(Whitespace\" \
         \"))))(Tile((id \
         09ee8965-845a-4e41-9b15-6950ef20d1f8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0f7cb45-27b6-4a09-bd25-ed2928300aee)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         69d15332-a1de-4985-acbf-66fa478ebdc4)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         2029a2b4-0f89-439f-97ca-c8626f3c9173)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fe61dc1-61db-494d-851b-c2e25eef7ccf)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         fa8c9efe-4200-4ade-86c1-5e7f45029daf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03670f38-9c99-4429-8629-45d8b447894a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7a5b6aad-7526-4f3a-94a2-4d971666ce62)(content(Whitespace\"\\n\"))))(Secondary((id \
         57fee970-d451-4708-a9fb-860d835dab9b)(content(Whitespace\"\\n\"))))(Tile((id \
         4dacda7f-77a6-47c2-b004-5620421bce08)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         374802b6-b7bc-4e90-827e-0d87c3a15235)(content(Whitespace\" \
         \"))))(Tile((id \
         c502d3e8-3b3f-4642-a91b-6fe6e1abca32)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         16ba17fa-0d1b-4694-b65f-f9a29d7cbe87)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         62f66251-1ea3-40b6-a83d-300442784e8f)(content(Whitespace\" \
         \"))))(Tile((id \
         00b54f24-4499-4781-9096-631b097bbdd6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ed93ac2e-bd54-425b-a2c3-f83f6b06a3f9)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         446400d6-9ca8-4a50-b616-71389dbb0903)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba954159-c12c-495b-8848-d9ca1f1cff0f)(content(Whitespace\" \
         \"))))(Tile((id 51fdd7b8-baf4-4130-a82e-4c426b2ea989)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         e1850443-bd6e-4993-b88f-f210d88b864b)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         46163e6a-4d0e-4b94-bcbe-194aa2522719)(content(Whitespace\" \
         \"))))(Tile((id \
         3c13da48-c8a4-41de-a702-bf0bd695d70f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         004c5179-f370-4ad5-add8-2acb05d31a4d)(content(Whitespace\" \
         \"))))(Tile((id \
         7978e083-4a30-47b1-9104-3fdb2517ef4a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6a943ebe-1732-4466-b815-1e9c632c0557)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe09c288-c86c-4c6a-8877-9bfa509cf752)(content(Whitespace\"\\n\"))))(Tile((id \
         654a52ab-91c8-42b5-a156-35a3ea572ab8)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b80e4770-20f0-4784-bc4a-0724f4e7080e)(content(Whitespace\" \
         \"))))(Tile((id \
         050bf81e-5ed7-40a9-ae8f-562e4a669e45)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5b888272-240c-4e63-bbb4-1893c2111912)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e5dff776-dd1f-4288-8a21-96877e318cd9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba678251-43ec-4489-82ae-7877697a6009)(content(Whitespace\" \
         \"))))(Tile((id \
         b9f3b2c9-8933-425e-ad28-579eee41658c)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         953a893b-e3ff-4b54-ac8e-11fd5bf102e6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3b2e1fce-c465-442a-906f-f45da99e068e)(content(Whitespace\" \
         \"))))(Tile((id \
         8030a9a4-1843-41a1-92eb-5fdc0a22dffd)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7d4ca7b1-7cae-4e33-bcf3-ce7f305e105c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6750815b-1563-4606-8984-07ca1af3f391)(content(Whitespace\" \
         \"))))(Tile((id da3e043d-a072-4979-9f69-d0c5e9cd4edc)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         afa0342d-2622-49e7-8d97-3c49424ee1f0)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         617bc337-00d4-44e5-bb1d-9a303d0f2aec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         282ed5dc-6e9e-4114-956a-477bed097f17)(content(Whitespace\"\\n\"))))(Tile((id \
         44682c09-374e-4242-8af7-301356555583)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b92bba3f-0417-4add-824b-18fa4cdde49d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         05c5a119-bbd6-4daa-a31c-725ddf9694ac)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         88e25475-fd0b-4668-960d-32c1d5cc429d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b74b150-cb01-4488-973b-ab76133927bf)(content(Whitespace\" \
         \"))))(Tile((id \
         d57ef07c-95be-4d06-b81d-d54aa1b75702)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e1e7bda-a306-4f1f-868e-aad6809ae814)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6a2b70e-ce69-4bc6-a3b6-155565c595b5)(content(Whitespace\" \
         \"))))(Tile((id \
         16472f7e-86a4-4fbe-9cb4-609a21ef910a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a31322da-f819-4a8c-8da5-c28821eb4584)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f0b6aba9-fa88-416b-adfa-faf95adb08b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d5f1a79-4c21-46a9-b8da-d76fbdca8cfd)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f1a6427-a430-49b0-a1c9-8fce9e6d61f8)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         d2f9ae57-e8ef-43e8-966e-81d470b04353)(content(Whitespace\"\\n\"))))(Tile((id \
         8fa553ec-0c30-4a53-af8b-6e18acaba16c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ea24c0b-6f23-4bfc-ba54-c0da2c1fbab7)(content(Whitespace\"\\n\"))))(Tile((id \
         8b8ce851-0f46-494b-94f4-5c116dc2e8e4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         41cdff1f-3382-49ed-ae65-a7910c72c4cc)(content(Whitespace\" \
         \"))))(Tile((id \
         91931532-2892-4248-a0fb-ffedf3acb5a0)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         84cd30d9-90c3-4d9d-8b86-b7a14ddda06c)(content(Whitespace\" \
         \")))))((Secondary((id \
         1dd5ab85-002a-43da-9746-8897293e2ff8)(content(Whitespace\" \
         \"))))(Tile((id \
         80befd8a-3c15-4a52-907a-eb4eb37a2c7c)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec9059ad-fad5-49c4-80db-1f57085d670b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dbeea3c0-d218-4605-a280-da0bc40122e8)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fa6c276-5e4f-45ec-a60a-f5d0f78fe83d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1133665-1018-4276-b78c-2c49e0348196)(content(Whitespace\" \
         \"))))(Tile((id \
         78f5e4ff-532e-45bc-9ce7-f48b70a4ff8c)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ee48597-914c-4f66-a2c8-72c1f6535747)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b1a6c18d-7ed7-4ab7-a330-f672a5e10971)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         56a95c66-98e4-456c-853a-f87537f54f32)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4dc1628b-db3c-41e9-8862-858b3ce86416)(content(Whitespace\"\\n\"))))(Tile((id \
         1eae34fd-1d92-4e1a-a9de-0ef6737c0eca)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         12312455-52ba-446a-9d59-b77c7c63b664)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         07fda52f-9441-4012-8f8e-e3b63c746884)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6753fcc6-cb62-45ec-9424-9b2f9863abb4)(content(Whitespace\" \
         \"))))(Tile((id \
         39989d00-c855-40e8-9b5c-4957b6b1296c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5f486e4-ba63-4eec-8883-a18b4925b466)(content(Whitespace\" \
         \"))))(Tile((id ac958d1b-dc1e-408b-a8b7-99d8758f0e0e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2811e23-daf9-435f-9427-5337361f3cba)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a5a9614e-3117-46be-8692-b1f7dbd2d5ac)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19c78b9d-347b-4534-9960-c35360e3f41d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         607882fa-50e7-4dab-bff4-45564fae9d98)(content(Whitespace\" \
         \"))))(Tile((id \
         84e75a4b-1aec-4bdb-aeee-f00243cbb7e5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4aeee3d1-8d42-4b09-80d4-a2d33793f299)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2ff60ff-b4d0-4566-b943-ed53b73be1df)(content(Whitespace\" \
         \"))))(Tile((id \
         86b4c2df-ed16-457e-9403-1bac4e4e219c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         66cc356b-4030-4b2f-b15e-4831135f8b37)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14f6f2aa-aaf2-47d5-b9bd-8493c331f0ba)(content(Whitespace\" \
         \"))))(Tile((id ed9381f1-ca76-432b-8598-f29eb3816268)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dfee956d-e50e-494a-b834-6916f5117f0b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90e9b166-8956-4f14-8b43-af7b3a93869e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bab21abc-1f90-4b29-8a4b-f1b5cf2c1fb0)(content(Whitespace\" \
         \"))))(Tile((id \
         cc014ad1-66f2-45b6-a6a0-32f19fd98e59)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf7ced96-fad3-4b1e-a36e-b0ff2d9b41a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fff7d509-788e-4c23-bcc6-ecfee00bdeaa)(content(Whitespace\" \
         \"))))(Tile((id \
         ce85b733-cda9-4dc8-8642-fa6c4f18be89)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b463251c-f7f4-4415-913e-7e842198c21b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e83747cc-e094-4a61-8f79-a8d986a8244f)(content(Whitespace\" \
         \"))))(Tile((id 92ea5963-cf0b-4710-a1b2-fc23ddcf7364)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc21c29c-e9e1-4aa7-bdaf-3a27f9f3a893)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bd61753-5c41-4e30-9573-914ff8716650)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87f4e547-bfc6-41f3-a18c-26115e007972)(content(Whitespace\" \
         \"))))(Tile((id \
         41fec4fc-64ff-4f0c-ade0-fee4b384ad2d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d800babf-f941-44fb-87be-608ea77d6167)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a79b43e-29dc-4916-b31d-770ace6165cb)(content(Whitespace\" \
         \"))))(Tile((id \
         73bfde9f-eb07-43e4-a623-b50d7e233e4e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         61dfa554-1145-4c37-a8ae-95c17f4fcd72)(content(Whitespace\"\\n\")))))))))(Tile((id \
         32310560-9d14-40d9-b636-40f3fd4f7bfd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c16e6788-539d-4267-b90c-b7408729e1ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         b27016ae-e0be-4696-8424-597b7f2a8cf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         45ec6ecd-e816-4ae4-a977-ede0089bad3b)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         381af2fb-c7a1-4eef-aa38-1c7beabf27f0)(content(Whitespace\"\\n\"))))(Tile((id \
         c3f32c38-38e3-4fe0-ae12-63abc9cba16e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         799d08af-81aa-4ed8-95e6-978d738a6d98)(content(Whitespace\"\\n\"))))(Tile((id \
         a624245b-fc47-4af5-ae0a-da8b0ab45d3a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ec028fb9-ed6e-46f3-831c-2964099855f6)(content(Whitespace\" \
         \"))))(Tile((id \
         8e7aea6e-3239-47c9-894e-5ca21ab9004f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b52917ad-323d-4a5a-bb2b-37e126f47dce)(content(Whitespace\" \
         \")))))((Secondary((id \
         4e58ec31-88d2-4d16-ac23-3e5a7b532ca5)(content(Whitespace\" \
         \"))))(Tile((id \
         1e205266-3df0-4582-8d5f-c19900ada1e7)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         047d81f9-ba43-415b-8aa3-6ca2ee1411fe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b11cd8a-116c-4c9a-91d1-7f7f8d34b4e9)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb309b1c-2e26-4b73-97d8-7b9c488c00b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d88a8ca8-d900-450a-8a58-ae7aee13b8e7)(content(Whitespace\" \
         \"))))(Tile((id \
         7f48a26f-f987-47d0-82cb-8890d587992a)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f098751-d327-4811-bc85-58d3b2c6d007)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f9fd649c-2a26-44c5-b1d6-345ae280548b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1cdcacac-bb1a-49f0-83b4-af97c352ac73)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f2237364-6405-4bc3-8ffa-d50f912ed0e9)(content(Whitespace\"\\n\"))))(Tile((id \
         321db7c0-b090-4891-9a9b-af8f1beae227)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad492840-54d7-4c57-a5c4-6388b50137d7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         dd294913-5074-4290-8c30-08db0a411a1e)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         473f598a-f4ad-4f34-99d1-c73d3ba91973)(content(Whitespace\" \
         \"))))(Tile((id \
         f383c0a7-a9e1-4d01-b40a-254d46bd9530)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fb8580e-3130-46fd-aa51-1baf7320cc96)(content(Whitespace\" \
         \"))))(Tile((id bde42723-d229-4e2d-9738-d86bf428e142)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b103b71c-2762-471a-b838-c8e7c87d2cad)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d7757b55-b883-43a4-acb7-f6895276e9a1)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         655b5e65-7b20-4ae0-be6c-52e7ac1aeb41)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45afe268-80d6-4be4-a770-6e05743e9f9a)(content(Whitespace\" \
         \"))))(Tile((id \
         3d534c94-8450-4454-89fb-b31576cd2abb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7e94556-aeee-4dd7-ab20-d37ff49c9ef3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f9318b4-00eb-4659-aa1f-51b659c6c1eb)(content(Whitespace\" \
         \"))))(Tile((id \
         65fe5c6f-6909-413a-acea-92c52646e9b5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d8f8886a-0906-4d44-a501-487481c6c03b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e014290f-a2ea-4ca0-bae4-25fa91b6a3d3)(content(Whitespace\" \
         \"))))(Tile((id 1d682907-f48d-4bc1-9b40-5fc0f6780de1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f838b06-adf9-4227-b58a-d56d45e0b3c5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c32a2f2a-ee6c-455a-a912-05cf8b015f56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4c9cff7-b7ec-4c05-88f7-c36d3945afce)(content(Whitespace\" \
         \"))))(Tile((id \
         b64c56c5-b99b-4cc0-a890-c539450429d7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         576b67cc-c67c-453f-afa9-634a5f2ff90a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa0593d7-1415-47c2-a6a4-a9c16fef2bf1)(content(Whitespace\" \
         \"))))(Tile((id \
         fbd535d6-c7b7-4b36-b0c8-861decdf8d74)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4b51fe6f-9477-4a88-9b48-902b2e71fadd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a0f7af1-5f83-49ae-82f7-7a3e27a95753)(content(Whitespace\" \
         \"))))(Tile((id c12545ef-d591-4f43-8e6b-67c01227cfd9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0eb29f3f-568b-442d-8ae2-aee8f31cb8ee)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d912b897-bfb2-4751-902d-7e7677a2180d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         834738bd-ee99-4f65-bebb-c7132aa5e17c)(content(Whitespace\" \
         \"))))(Tile((id \
         85c47ca1-6b93-43fd-8d8e-aa4ecdbf5c55)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a0ae80b6-6072-46a7-a8a7-40a078cbaeee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62bed133-85b8-4b1c-9bfa-b58e380cbe8d)(content(Whitespace\" \
         \"))))(Tile((id \
         6be6a652-e79b-4045-8956-d97f6ea67585)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3c672617-d071-4803-8a6f-2e02911cd688)(content(Whitespace\"\\n\")))))))))(Tile((id \
         85e272d4-c401-479e-86ec-3fafb636e9a0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4556a5fe-8da1-4445-9001-ae02dc9757ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc4c97d6-744e-4b6a-a2f2-fd9d6509750c)(content(Whitespace\"\\n\"))))(Tile((id \
         eb14d908-a72e-4f12-882b-39283b58aa2c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d475be26-70e4-4640-8816-934cbdc8fecc)(content(Whitespace\"\\n\"))))(Tile((id \
         339c888c-e23f-4e07-8fe0-b4bc93f4ed78)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f56b7e0-3caa-4227-a0a0-f78e4d5a7740)(content(Whitespace\" \
         \"))))(Tile((id \
         fb4da340-1f53-496a-bb27-506e864f5931)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         39a5b5ba-3ac5-4deb-b944-fedca52c89a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         153981eb-99a8-4930-b5d1-ba60011947c5)(content(Whitespace\" \
         \"))))(Tile((id \
         2cd2028c-58f9-4984-aafb-6db24a17b07d)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da27e9be-79aa-4f8b-8a95-38ce57370b33)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c343c967-ef1a-4f19-95fc-edec0317880e)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30bec832-4a7e-45ca-ab6a-f0b0d7d0e78c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04030637-2e84-472a-91be-e139078e375d)(content(Whitespace\" \
         \"))))(Tile((id \
         93222fed-5795-4afb-b5da-6dfc4eb0280b)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ffed241-40a3-4fdf-98b2-d5453c779c1c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41a2a0e0-2b7a-4955-9956-4711ad3d3915)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3ba8c61d-353b-4d59-8d0a-f05f6c19a6d2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e8f5ff4-b13c-474c-ada4-44753b5e5ec0)(content(Whitespace\"\\n\"))))(Tile((id \
         03bc83aa-7e57-4a98-9092-fc1b7ff7dc9e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2089d2d8-eee3-4d69-880a-2fc14c450dee)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d31f3b31-5ac4-4b92-9145-6222f86d1987)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea2de81a-6723-4a8a-8ef1-1c7e73fe997b)(content(Whitespace\" \
         \"))))(Tile((id \
         4c0d9f02-c754-448a-8620-03a4f94c67f2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b672b2ac-3178-4e2e-9226-88a986e23dad)(content(Whitespace\" \
         \"))))(Tile((id 4952d6cd-b83b-4fac-948f-f606cf1d8aaa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c89dcfc-56cb-4984-94f5-3411d73cb211)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7c2c4682-89c2-4a98-9b8c-a39407e59b7e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0868bbf6-bc49-425e-b32d-1b80871a30d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25878060-63e1-4fe1-a50f-a85ebf4c0a7a)(content(Whitespace\" \
         \"))))(Tile((id \
         5b7ac6b3-1d42-447d-b6a8-69d8208baa28)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e5b32af-f8d8-41aa-b50a-f7916d4924e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2c9a7e0-ef98-4d80-8538-92b1ba54a94a)(content(Whitespace\" \
         \"))))(Tile((id \
         fe9ec97a-378f-4851-8f50-f4e2e4bcd5b5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         49572da9-f5b8-4451-9bed-ad7c41e32985)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b29520d-a288-4fc6-bbf8-0a2bb040e955)(content(Whitespace\" \
         \"))))(Tile((id bdf69b71-0898-4406-ab77-36ec67b324cc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ca97e0ec-443c-4899-ad83-4ec4cf974129)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         10aa0f74-01d4-4061-a9bf-751d24d9e05a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e0d370f-7588-48ba-b2a0-27569246208a)(content(Whitespace\" \
         \"))))(Tile((id \
         7e645c4a-0672-4fe7-967f-f0085bd50219)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fdbba4f-f90d-4ff1-809e-a483c4fea092)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b9e1c89-8a28-44d7-a0eb-22bb98e59f6c)(content(Whitespace\" \
         \"))))(Tile((id \
         1413bd69-4c7e-4adc-99d1-01c473445a0a)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1f6e4724-e1cf-43b8-b2ca-324edf366905)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac305e16-108a-4d54-8883-e55441dbfcb6)(content(Whitespace\" \
         \"))))(Tile((id 57c86026-9839-46ff-8b54-6d242f4679f7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         665ed21d-d0a8-4d52-a3f3-cb76f19e88c6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d72e0bf9-50a1-49d4-8a30-bf2320b8f129)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e7ce38a-3cc4-4fb6-9c2f-407e01bca386)(content(Whitespace\" \
         \"))))(Tile((id \
         fb4daeeb-ebd0-44e3-9603-96b68019249d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6928534e-5b17-4f94-a58d-9310d84c871f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba35f07b-43d6-4c59-a881-76cb5ce48cbc)(content(Whitespace\" \
         \"))))(Tile((id \
         b5f03449-1fcb-4298-9781-5fb9246f4fde)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5a7a6d5c-c420-4e34-8d5b-a258143944ab)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c9c97337-7cc4-4b16-89cd-70415527f41b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72203c1c-c657-4e57-a715-190c0fc7f068)(content(Whitespace\"\\n\"))))(Secondary((id \
         507a6dac-9ca6-4689-8598-1573aeee4645)(content(Whitespace\"\\n\"))))(Tile((id \
         f7404e2a-2de7-4bd1-95c3-b88d79345337)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8caedff5-d16a-405c-aa63-824b9bd306ca)(content(Whitespace\"\\n\"))))(Tile((id \
         73e535a7-04f6-48c8-81e7-9a7e87c8bcd9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         66dd1a2c-5301-4367-af2b-0d1daf61bb30)(content(Whitespace\" \
         \"))))(Tile((id \
         1eae2ebc-411c-42e9-aecd-7592285c6cc3)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83290a50-1c55-43ac-8780-6ca7c78643a5)(content(Whitespace\" \
         \")))))((Secondary((id \
         b595802f-205a-4707-9922-f68143d3d467)(content(Whitespace\" \
         \"))))(Tile((id \
         626747e9-2666-4893-b404-010bfcedf357)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e76d1c2e-eb88-4785-9432-ee40e19e4e28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ae279e9d-8718-421e-9394-1b642301adab)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47530a76-17b5-4777-8f9a-6dd6f067a68a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2865c60-726d-4b3c-9426-4d64a4bbd9a5)(content(Whitespace\" \
         \"))))(Tile((id a9bda4b2-4381-4ac3-a264-8c4c8797de35)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df87e5fa-9c52-4efa-bc02-98f80d259d3b)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b917c0bd-6b7a-4621-a9c3-f5e36e9629a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         034e3639-a6b2-4f84-a9d8-7bc95ed666de)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d2a444eb-65be-4cee-a90f-d67c2a837af0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2e4a34b-4e15-48a0-9313-ecf32809890b)(content(Whitespace\" \
         \"))))(Tile((id \
         ca4078cb-c4b4-4cc7-9f79-3c82f8121244)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a43a01de-c31b-4c43-b979-349e7ad9b872)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ccb97804-0b8c-4059-95cf-b565c63f6596)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         f8a1e40c-45a3-46ee-94db-f6785b428b0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a8d18b1-2588-40e1-960f-3f392d21ba3b)(content(Whitespace\"\\n\"))))(Tile((id \
         1210027a-897f-431a-931b-ac93ca2eb9ca)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30c7a099-47c8-4850-a6e9-626db734fb0a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4d950bf5-1c95-47c7-af7c-f2f68ca22fd8)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c5e228d-968a-440f-b1fe-1ab10a4accf2)(content(Whitespace\" \
         \"))))(Tile((id \
         30342962-d610-4922-b116-bc90fb71381d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b403e232-4ec7-4ab5-a957-d05b5a16f85d)(content(Whitespace\" \
         \"))))(Tile((id 58e71b9b-4346-45a8-a524-86fbcf47ba5c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         85f077a7-5487-4513-915e-d76b441b4708)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5604822f-3b8c-4dd5-8f9c-60bdaf871ba2)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18b141fe-5f58-4573-9879-c13cc426e217)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         904cc97d-7ca3-4fea-a95e-24497f3bae58)(content(Whitespace\" \
         \"))))(Tile((id \
         d419ca02-f8cb-4cce-9619-b7bec34dcbaa)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8dbe8968-aac9-4c40-b917-ff5a1aae69ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6650be40-f8dc-4399-a198-8b48e70709a3)(content(Whitespace\" \
         \"))))(Tile((id \
         0ae58453-3cab-4a02-a626-a5ea9f807270)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6aa3ad1f-da18-4e07-a96a-ae15708678c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6868487e-efb5-4cb0-91b4-a95a2709568d)(content(Whitespace\" \
         \"))))(Tile((id 2fbd8150-746b-46e6-8d4c-56612749db90)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8a139caf-3c3f-4f7c-9444-2e2ed071bb0e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b7d02f4-0fc6-4f83-8099-bc944e22d732)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21bc0fa9-955d-4697-9dd3-1869530636a1)(content(Whitespace\" \
         \"))))(Tile((id \
         0fd8f149-3b19-4015-8b55-819c5ecf2163)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         779a1b91-f136-47d7-b151-966dcccbf16e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         989b20d7-757c-4018-b0b9-52e67526815c)(content(Whitespace\" \
         \"))))(Tile((id \
         9b8411ce-73d7-4799-8152-d550ff624ac7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9a2fac8d-00da-4d90-b888-0319129fb820)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ecbdb6d-e9a4-48eb-8be2-64f08ba79ddf)(content(Whitespace\" \
         \"))))(Tile((id ec5bf0a7-26a2-4b5a-97fd-aa878e15b6d4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         98eeafa2-3ef6-49ff-822f-4c77e8da3fa1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54e6b380-8c58-451a-89f7-03d2326b8337)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39033a11-28f9-4ed8-9cbe-b86b8a27dde0)(content(Whitespace\" \
         \"))))(Tile((id \
         3814130a-82dd-4ae6-a1e2-c6f15058c2d0)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de76190a-038c-4cde-b82e-3b7b15c859e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         148ff147-cc5d-4178-8c5c-2dfeb712666b)(content(Whitespace\" \
         \"))))(Tile((id \
         186cba8e-3123-499b-9b2c-e346b790baaf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1fdd4a46-0528-402d-baff-f9cdfb6af1ed)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bcb06399-2d21-4337-9b8a-569217d04752)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         645888de-a654-4899-8c4f-6d830eb9cc97)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf7d6d0d-d062-4f40-92a0-0e070cfb2219)(content(Whitespace\"\\n\"))))(Tile((id \
         0cd4ee32-32c7-45fa-bffb-ae4e00c0b2fd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         efeef56b-b289-4245-95d0-f51c81c505c2)(content(Whitespace\"\\n\"))))(Tile((id \
         ccc4fcbb-1df4-484a-811b-ea0a27581264)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f67b2c45-ca8f-4be7-9d03-8ee6bc716f25)(content(Whitespace\" \
         \"))))(Tile((id \
         9c4296ac-0c28-4f09-a84e-a5c4eae1ebd4)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0536446c-51dd-4fa2-a530-644fbb669cd3)(content(Whitespace\" \
         \")))))((Secondary((id \
         127e977c-cb1e-40ad-8078-f9c2b2295671)(content(Whitespace\" \
         \"))))(Tile((id \
         25e63c21-5da9-4ef7-9f62-d5f7265400e6)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc9c007e-63cf-47cd-8fb3-3f644af00ef6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d0e2de13-4a16-4d51-a72d-bdabc0bb8d84)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ce2fa71-f5a7-4185-9cff-33e8e6e4d115)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e09ece45-ca83-4d7a-9edf-342cc764e362)(content(Whitespace\" \
         \"))))(Tile((id 6712ffb0-d284-49b9-bf52-3fdf778baa37)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03990667-0ad1-4191-8d53-bbef76905283)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a4fa99d-6d35-4551-93a4-525b5375626d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8fba9e74-d6a0-474d-aff2-1849dbe7292f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         044f2f6f-8975-4781-bb94-3004826b884b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7919f2c-7adc-4c57-b1dc-32e97d1ffad7)(content(Whitespace\" \
         \"))))(Tile((id \
         1e98cd7e-7e43-4ac3-92e0-032d7d302206)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fab27b29-113a-441f-bf79-9a6e41bd28d6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4bd337de-d317-482b-aed1-43cd7a53b5b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         6c47b283-0693-44fd-85ad-5420f8bc9ca7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b2950dd-23a7-423a-9cc0-d0ca803235e9)(content(Whitespace\"\\n\"))))(Tile((id \
         21897564-050e-4897-870f-e3346e343795)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f51059e1-d33d-46ea-a551-ae8806fd0f89)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e14db0b4-65d2-44b9-88be-4464ee6aff5d)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e76cd89-dfe7-442a-83ac-1647f5bb2ba8)(content(Whitespace\" \
         \"))))(Tile((id \
         fbd3c8dc-7f0f-4f08-acde-f0c830d3eb60)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         098b14c9-da05-4a08-b16b-37a5f5a615c4)(content(Whitespace\" \
         \"))))(Tile((id bdbadcca-e606-424d-a426-234be20d5260)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df86289f-e81d-42cb-b420-7babccf68bd2)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a2d8404-eafd-4881-b09d-e35672022bef)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e87210b-a8e0-4d13-a5d1-7023092c56f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3467668e-d620-4ad0-8b7c-0945499b3178)(content(Whitespace\" \
         \"))))(Tile((id \
         38ad9777-00c7-40e0-9b9c-e62d563e3053)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91014c7f-4fb9-4b74-b69a-66eff44d1658)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b602fab-8e5b-4d80-a385-a5bb4a2b9096)(content(Whitespace\" \
         \"))))(Tile((id \
         50a0476f-a4f3-434e-904e-402d25625c81)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6a1e888d-1bf5-47a4-8f91-1a587328c7f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f718c69b-dea2-4933-9a89-2cba77ee1f21)(content(Whitespace\" \
         \"))))(Tile((id 8a932c65-d7d6-4d83-a200-cb222064d707)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2af01fd3-0ad0-4cfb-a887-a5bcd8086469)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ee4284a-aea7-4df1-b440-7e8bc4eaee0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6222dcc4-ffb1-4bab-9b85-e169361e8b6e)(content(Whitespace\" \
         \"))))(Tile((id \
         ba13fd6c-c0a1-4d6b-8f83-0b0df6be4426)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41711b4b-29a8-400f-a8fa-8961c38f7395)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6aa29f6-b530-477c-aee4-7963b043a0f4)(content(Whitespace\" \
         \"))))(Tile((id \
         4fd82b5c-650d-4003-91fa-8c3d6c12aa22)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         18c8728d-5ddd-4453-8f02-3ee66e16ed38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ebe6a952-c083-42a0-aa74-0a39a67bd2a3)(content(Whitespace\" \
         \"))))(Tile((id ea2d2b30-cb5b-4d5a-bfe6-e1508024be20)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         484224f5-f251-4f7a-9803-4397268d6a29)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32bfcaa2-5611-4dac-b29f-b704914a5d7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61db0da6-37ee-4dd3-8396-4e45fa5f692d)(content(Whitespace\" \
         \"))))(Tile((id \
         f9e06d88-7460-4658-a922-b05a2a15ff3e)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34f7af7d-e855-4ae1-b839-cab5ed8b74f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4563330-e337-4a58-ab87-bded603a62fe)(content(Whitespace\" \
         \"))))(Tile((id \
         816836ab-162d-49b6-b67c-91028fcbfaa7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         196dae86-1b3d-4ad6-bc95-7149d109a518)(content(Whitespace\"\\n\"))))(Tile((id \
         573f5ef9-efff-4564-9a85-1762eb569de9)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         208929b3-627d-4459-9740-5bc745fa3f8a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e789cf0-eaf6-41ae-9d27-7847a357a9df)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07430cdb-1cf2-4ec4-bc7c-8cbeb7a4344f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0c00717e-9198-4bab-83ae-b71718bdf0f0)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7754db1-90ea-43ed-935a-0c3d72792c4c)(content(Whitespace\" \
         \"))))(Tile((id \
         ac835d23-e6df-4a2f-a164-b6d21bf4b9d3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19af941c-d9aa-42ff-9b92-8f64385912d3)(content(Whitespace\" \
         \"))))(Tile((id \
         7b2e7768-1589-4a8d-96ff-c67bdf7d8074)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d112855f-b735-4669-af25-21890af6e920)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b7dd4a60-801c-4689-b82d-623989d5c827)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# GROVE PLOTTER EXTENSION TASK                   #\n\
         #                                                 #\n\
         # The grove plotter app lets you plant seeds on   #\n\
         # a grid. It already supports planting rows.      #\n\
         #                                                 #\n\
         # YOUR TASK: Add a PlantCol action that fills     #\n\
         # an entire column with the current seed.         #\n\
         #                                                 #\n\
         # You need to:                                    #\n\
         #   1. Add PlantCol(Col) to the Action type       #\n\
         #   2. Add a setCol helper function               #\n\
         #   3. Handle PlantCol in the update function     #\n\
         #                                                 #\n\
         # Look at how PlantRow is implemented for         #\n\
         # guidance - PlantCol is similar but vertical.    #\n\
         #                                                 #\n\
         # Tip: Use auto-probe to see how the grove        #\n\
         # changes after each action.                      #\n\n\
         type Plant = String in\n\
         type Grove = [[Plant]] in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         type Model = (\n\
         grove = Grove,\n\
         currentSeed = Plant,\n\
         seedInventory = [Plant]\n\
         ) in\n\n\
         type Action =\n\
         + SelectSeed(Int)\n\
         + PlantSeed(Row, Col)\n\
         + Uproot(Row, Col)\n\
         + ClearGrove\n\
         + PlantRow(Row)\n\
         # TODO: Add PlantCol(Col) here #\n\
         in\n\n\
         let init: Model = (\n\
         grove = [\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"]\n\
         ],\n\
         currentSeed = \"\240\159\140\177\",\n\
         seedInventory = [\"\240\159\140\177\", \"\240\159\140\191\", \
         \"\240\159\141\132\", \"\226\152\152\239\184\143\", \
         \"\240\159\140\184\"]\n\
         ) in\n\n\
         let setCell: (Grove, Row, Col, Plant) -> Grove =\n\
         fun grove, row, col, plant ->\n\
         mapi(grove, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) -> if j == col then plant else c)\n\
         else r)\n\
         in\n\n\
         let setRow: (Grove, Row, Plant) -> Grove =\n\
         fun grove, targetRow, plant ->\n\
         mapi(grove, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun _ -> plant)\n\
         else row)\n\
         in\n\n\
         # TODO: Add setCol helper here #\n\
         # Hint: You need to modify each row, changing #\n\
         # only the cell at the target column.         #\n\n\
         let setAll: (Grove, Plant) -> Grove =\n\
         fun (grove, plant) ->\n\
         map(grove, fun row -> map(row, fun _ -> plant))\n\
         in\n\n\
         let updateGrove: (Model, Grove -> Grove) -> Model =\n\
         fun (m, f) -> (f(m.grove), m.currentSeed, m.seedInventory)\n\
         in\n\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | SelectSeed(idx) =>\n\
         (m.grove, nth(m.seedInventory, idx), m.seedInventory)\n\
         | PlantSeed(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, m.currentSeed))\n\
         | Uproot(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, \"\"))\n\
         | ClearGrove =>\n\
         updateGrove(m, fun g -> setAll(g, \"\"))\n\
         | PlantRow(row) =>\n\
         updateGrove(m, fun g -> setRow(g, row, m.currentSeed))\n\
         # TODO: Add PlantCol case here #\n\
         end\n\
         in\n\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (init: Model, actions: [Action]) ->\n\
         fold_left(actions, update, init)\n\
         in\n\n\
         # Existing tests #\n\
         test\n\
         let m = update(init, PlantRow(1)) in\n\
         m.grove == [[\"\", \"\", \"\"], [\"\240\159\140\177\", \
         \"\240\159\140\177\", \"\240\159\140\177\"], [\"\", \"\", \"\"]]\n\
         end;\n\n\
         # New tests for PlantCol #\n\
         test\n\
         let m = update(init, PlantCol(0)) in\n\
         m.grove == [[\"\240\159\140\177\", \"\", \"\"], \
         [\"\240\159\140\177\", \"\", \"\"], [\"\240\159\140\177\", \"\", \
         \"\"]]\n\
         end;\n\n\
         test\n\
         let m = update(init, PlantCol(2)) in\n\
         m.grove == [[\"\", \"\", \"\240\159\140\177\"], [\"\", \"\", \
         \"\240\159\140\177\"], [\"\", \"\", \"\240\159\140\177\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [PlantRow(0), PlantCol(1)]) in\n\
         m.grove == [[\"\240\159\140\177\", \"\240\159\140\177\", \
         \"\240\159\140\177\"], [\"\", \"\240\159\140\177\", \"\"], [\"\", \
         \"\240\159\140\177\", \"\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [SelectSeed(2), PlantCol(1)]) in\n\
         m.grove == [[\"\", \"\240\159\141\132\", \"\"], [\"\", \
         \"\240\159\141\132\", \"\"], [\"\", \"\240\159\141\132\", \"\"]]\n\
         && m.currentSeed == \"\240\159\141\132\"\n\
         end\n";
      refractors = "()";
    } )
