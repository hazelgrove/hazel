let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / last-element / last-element-sketch",
    {
      segment =
        "((Secondary((id \
         f3048a67-3383-424b-ba4e-edd8e81293b8)(content(Comment\"# LAST ELEMENT \
         TASK                            #\"))))(Secondary((id \
         faa8d28a-57a6-4299-9f44-f3df0280162f)(content(Whitespace\"\\n\"))))(Secondary((id \
         01df7051-74a0-4056-9199-31189b19189e)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         9cceb873-d994-4722-a1ff-32973e0492d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a00e543a-7364-4829-ab1f-4748e75a92f4)(content(Comment\"# Implement \
         last: get the last element of a    #\"))))(Secondary((id \
         f9c39d4b-da08-41b5-b98c-0faaf6d4d9f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         16b3b64e-ab40-4746-9cf6-2674b653e784)(content(Comment\"# list, or \
         return a default if empty.          #\"))))(Secondary((id \
         5edb2ee4-0453-416e-aa9e-d0c9539519d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9052e0c0-e5e6-4418-bcfa-dff644528b9c)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         700d4cf9-06de-4d17-97cd-3406c030e42b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e67a4488-17fc-4f81-896f-828354ddf1e5)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         5f3971aa-0ea4-461b-8104-a29607d15632)(content(Whitespace\"\\n\"))))(Secondary((id \
         7683b933-7bd6-4ffa-8458-8354580d3a6c)(content(Comment\"#   last([1, \
         2, 3], 0) == 3                    #\"))))(Secondary((id \
         16c6e374-1744-44f4-b44c-4d1cf47ce2ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         b89858f0-2568-400d-8290-64411db848ee)(content(Comment\"#   last([42], \
         0) == 42                        #\"))))(Secondary((id \
         0e95595e-e0cf-45c9-9d08-7b84a32125ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         1da7d4a7-36a8-4497-89d8-32afaf4bbdd3)(content(Comment\"#   last([], \
         99) == 99                         #\"))))(Secondary((id \
         91f7f979-8c40-41c5-8aa7-3643acebae4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         00d632f9-0dc8-49a4-8437-752d864a7cfd)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         265bd619-f0d6-434c-b7a1-bc21440cae61)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c42e6a7-894a-4e63-b181-29823231b6cc)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         1277eefc-e985-4e07-b7ba-783e58925c06)(content(Whitespace\"\\n\"))))(Secondary((id \
         63f97104-38e6-494a-9ef1-3e017b181f14)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         b0ec9b3a-2c82-4450-9b5c-0d60aa12af80)(content(Whitespace\"\\n\"))))(Secondary((id \
         15dc92f1-cdcd-4f1c-b1a4-7c8c6e539176)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         3f2f72ac-682f-4689-8095-8a6732eba5e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         fac806a7-7e73-46d6-8470-06a28933bda5)(content(Comment\"#   \
         fold_right(list, fn, init) -> result       #\"))))(Secondary((id \
         a4d2cd12-ba2f-4530-a5e9-212efd1962d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0710fc6c-c863-4954-89cb-eed7e6736fd4)(content(Comment\"#     fn takes \
         (element, accumulator)          #\"))))(Secondary((id \
         31551f4d-9366-4a77-9016-2509d7874a93)(content(Whitespace\"\\n\"))))(Secondary((id \
         128a4484-4a62-410f-ac8f-a8e029d6ae93)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         ad3fb443-1880-4f02-8ce7-48d14967effd)(content(Whitespace\"\\n\"))))(Secondary((id \
         84f48629-9fab-4b91-aa65-f9029925f3fe)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         902d8bd5-a996-45a6-b20f-75d76e7a52c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         acbc3fa9-de79-4501-9c68-8eaecb6b2626)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         83a99baa-a4ce-4645-ac81-ba3e31e52be0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1dbc3078-f4e9-449c-bad8-e56a8f6f26a0)(content(Comment\"# Tip: Think \
         about what the fold should        #\"))))(Secondary((id \
         e446fd5f-1224-4a05-9e8e-baf48d7de9e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f149ab9-3690-463b-bb42-ad664e59f8eb)(content(Comment\"# \
         \\\"remember\\\" as it processes each element.     \
         #\"))))(Secondary((id \
         b3df97c8-59df-493a-8cdc-c4919b57bc98)(content(Whitespace\"\\n\"))))(Secondary((id \
         85a1505e-d6c0-4e40-8081-a57c03d92065)(content(Whitespace\"\\n\"))))(Tile((id \
         5fc9709f-5d31-4ca9-bdb8-93867088dab9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da6bfb6b-78fb-445e-9b0d-8ff3b03dbea5)(content(Whitespace\" \
         \"))))(Tile((id \
         3a2e71f4-ef6d-41b8-81f6-29d8e091b31a)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aab86269-ba6a-48ad-a31f-953b7648a2d3)(content(Whitespace\" \
         \")))))((Secondary((id \
         78ff2350-a7b5-4d31-9716-4ccd46240701)(content(Whitespace\" \
         \"))))(Tile((id 71b0dbeb-0671-4b65-a883-f6f96cacdf8c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a5ec5bb4-bd07-4a7e-b344-23dc3da8ba8c)(content(Whitespace\" \
         \"))))(Tile((id \
         9aa3dfa7-4fa1-4684-aa59-b2fc40130388)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         1bf0730c-9731-4f55-81bf-31e73d7bf208)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e1a69163-dc8c-4316-824c-cfe7fc1d4a51)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e452b6af-9ca4-49f9-8faf-eb9831ef9058)(content(Whitespace\" \
         \"))))(Tile((id \
         6d8bdf3e-d092-442a-872d-499b0d0bf2d7)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         8bb46f72-4163-47ae-bdb8-5775dc32af1a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2fc2963c-315b-4779-9ff1-85774be736a8)(content(Whitespace\"\\n\"))))(Tile((id \
         8fd3ec8b-7679-4768-b3d0-cc9e504266f7)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         04fb7579-2452-400e-83cf-c28bc122b676)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         33687090-90b7-4222-be4d-c7867b8b6d8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c4388ee-feb2-4952-8395-95c6fe314738)(content(Whitespace\"\\n\"))))(Tile((id \
         87781799-0f70-4701-90fa-fd734a884e8b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2de69d01-7419-401c-a89f-0a61de61ca2b)(content(Whitespace\"\\n\"))))(Tile((id \
         a3609e26-41c2-4a1b-9f8d-2a7678d3cf98)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9805967-ce67-47c4-b04c-7e4b7c0b2d1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a688ca93-9984-4294-b393-c2618aca16ec)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         34b9a9e1-3f99-4e36-9fb4-ca19309e8502)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c6e457a-d014-49cf-8e85-4671e8a65697)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d833f833-c1ab-41fe-bb7f-42e1e8b873ff)(content(Whitespace\" \
         \"))))(Tile((id \
         9238be0a-df6d-41fe-85cd-b7b56e9aab94)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e4be380-d585-48da-840b-e52376576cf2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5c95285-c024-4333-9f2d-5f978b0f9c68)(content(Whitespace\" \
         \"))))(Tile((id \
         4b0d4ac2-c0b7-4c42-a222-e077e0ec7f5a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a750ce79-ec20-44d2-a4ed-633c505857a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f71d97c-09fd-4472-adbb-a91e96e6b2ed)(content(Whitespace\" \
         \"))))(Tile((id \
         939f261b-7864-46c2-94ab-c1035207769b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7189f68f-cd9a-4a11-8c83-d6897891198a)(content(Whitespace\"\\n\"))))(Tile((id \
         877dd550-0897-47ed-9362-4ec8f37aa5c1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8be49d63-61f3-4bf0-92f9-8a75735869fa)(content(Whitespace\" \
         \"))))(Tile((id \
         f2784887-1104-4e6e-9015-2c8373f342cd)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         70d16d6a-2909-4184-88a1-09234766c8db)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7c99b422-9dca-4f53-a548-665bcdab6ed9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fc7a6be-e6d6-40c0-b73c-79464c5c0bec)(content(Whitespace\"\\n\"))))(Secondary((id \
         99f2d31b-8fb1-4968-910c-6cf27be554b2)(content(Whitespace\"\\n\"))))(Tile((id \
         ec111018-6a38-401a-a474-b754c59a7ab1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8fe22fd0-1cdb-4123-ae95-e869991fec47)(content(Whitespace\"\\n\"))))(Tile((id \
         3522b01e-e4ed-4754-b567-d9d2c7e85908)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd55cd54-2e30-4c8a-8474-921855387c84)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f60bafa0-5524-4648-9ef8-c31d396e540e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d7cd2408-3fac-4050-b70c-c479fc3efbea)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         719d859e-52a8-491c-bf1b-9d98104e9c83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b191983-ae38-49cf-89f1-ed854c5d97d1)(content(Whitespace\" \
         \"))))(Tile((id \
         946aff7b-70fa-4395-bf36-c201fd4f1e08)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7eb9387c-c201-491c-aab1-ea1b3951fa14)(content(Whitespace\"\\n\"))))(Tile((id \
         a2a58264-e828-4bef-afc3-421aaeda1c17)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe147256-7ec9-42bc-ac40-9d9d607456cf)(content(Whitespace\" \
         \"))))(Tile((id \
         6a8e298d-8858-4a49-8def-57ce865c9111)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         59a1cc08-824b-48e3-a28b-1440f124e665)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4df4529b-4a62-4b04-b5da-71aa88f73cc6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44a9de66-a54d-4266-a982-d381427504df)(content(Whitespace\"\\n\"))))(Secondary((id \
         764ec775-0d36-4bd3-80ce-d62b40fbef32)(content(Whitespace\"\\n\"))))(Tile((id \
         734a1cd0-11d2-48e2-af26-354a8e643037)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         820bee47-5b72-4fa4-aafb-5512bf7afc2e)(content(Whitespace\"\\n\"))))(Tile((id \
         d1dd8c25-97ca-48a1-957e-696cc47215f3)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1ab556c-e975-44f9-a1ad-8d5ad99679a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         625bcd46-e797-4dfc-a542-514780ca19a1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         56feb4f8-9c6c-4416-9535-8e8ccc2d8565)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         df5bc7ec-33eb-46b9-b31e-faa7a2038c2c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2592f12a-7301-4c91-916b-6127580c4581)(content(Whitespace\" \
         \"))))(Tile((id \
         6a203733-1bf5-46ca-8f58-e03ad4c5535b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eee81077-497b-4acc-a3f6-a44c9d54c391)(content(Whitespace\"\\n\"))))(Tile((id \
         7ea494ba-c71a-4cb7-9758-55b457098d97)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d1ca7ca-436d-4901-ac88-0323f8a9855a)(content(Whitespace\" \
         \"))))(Tile((id \
         adfc6a3e-f125-4f05-ab5c-05e1f741e914)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26619a55-d281-4aab-bcab-83a27a42eefb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a7ec0607-f40a-4bf3-9fc8-e452b8f7237e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2393f554-2838-4675-9bf9-cbfad52e8f0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7303b510-a717-4c4d-a6ec-dd7569e39234)(content(Whitespace\"\\n\"))))(Tile((id \
         4d9c086b-5c92-4451-8b29-75615c394ec2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6a31723f-b9da-42a2-8e96-8c151a022d80)(content(Whitespace\"\\n\"))))(Tile((id \
         03c4481b-d502-45f1-b635-044e5aba1cf9)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0fe0f2f-df67-4d8e-816f-f37c6e615dd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b0d5e22a-10d1-42ce-ae7e-a20ec698e9e3)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2131b59f-3a58-4942-ad92-bfd2b88bce56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e39d91c3-4396-40fa-887d-46da055d3dd4)(content(Whitespace\" \
         \"))))(Tile((id \
         09947e07-4e98-49c6-8fcf-8cbcbcd36a53)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         03d8cd79-09e7-479e-bbb1-d63eac93abbf)(content(Whitespace\"\\n\"))))(Tile((id \
         b193847a-1f3e-4040-8553-07db083c55e0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d08d63a-65c9-46c3-9930-b3ef6ac9a803)(content(Whitespace\" \
         \"))))(Tile((id \
         931002c6-181e-4b23-a556-f0cf114f45b0)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78d0d177-3f0b-4243-8289-d58970e663e6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         796dc7c0-14ca-410c-b444-891dfc2c57f9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81aa9a1f-b258-4873-9eca-30d0e2538a2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         306559f0-3f5b-4f84-b86e-ea37a50571f5)(content(Whitespace\"\\n\"))))(Tile((id \
         8a43e928-9f25-442d-ac85-04e070c13066)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7f404508-9aae-44ed-8dbc-313e8c6187bf)(content(Whitespace\"\\n\"))))(Tile((id \
         ab1d060b-f991-4bcd-87c0-8bef834f14bd)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c9fa39c-7d2c-4173-8085-84b62a1cd4ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3b21220-a4f2-4b3e-9a5d-f846cf45802b)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbc5b6ef-9f0a-4dea-9df7-55df5b4c6ccf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c36f529-b6a8-47e6-afd2-d128c92ad836)(content(Whitespace\" \
         \"))))(Tile((id \
         2964a3ca-8382-4a14-a773-57d030a52421)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         169e9d44-094c-48f6-83d9-031596de508e)(content(Whitespace\"\\n\"))))(Tile((id \
         916b2743-c9c1-4d09-919f-0ceb0499c24c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dfd38fe-2936-4641-bacd-21792a7fcc99)(content(Whitespace\" \
         \"))))(Tile((id \
         f36638de-4d0d-4328-bc26-ad275ff29753)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c779af16-e078-480c-adf4-78aa14a1e107)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1708ba65-8ab4-4e50-8531-d11ee84504ba)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# LAST ELEMENT TASK                            #\n\
         #                                              #\n\
         # Implement last: get the last element of a    #\n\
         # list, or return a default if empty.          #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   last([1, 2, 3], 0) == 3                    #\n\
         #   last([42], 0) == 42                        #\n\
         #   last([], 99) == 99                         #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   fold_left(list, fn, init) -> result        #\n\
         #     fn takes (accumulator, element)          #\n\
         #   fold_right(list, fn, init) -> result       #\n\
         #     fn takes (element, accumulator)          #\n\
         #   rev(list) -> list                          #\n\
         #   length(list) -> Int                        #\n\
         #                                              #\n\
         # Tip: Think about what the fold should        #\n\
         # \"remember\" as it processes each element.     #\n\n\
         let last = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         last([1, 2, 3], 0)\n\
         == 3\n\
         end;\n\n\
         test\n\
         last([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         last([1], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         last([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         last([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
