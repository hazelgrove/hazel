let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study / gameoflife / gameoflife",
    {
      segment =
        "((Secondary((id \
         b44beac9-4de3-49d8-afa8-3967e7dc1ee9)(content(Comment\"# CONWAY'S \
         GAME OF LIFE #\"))))(Secondary((id \
         769d7410-e8fa-4475-8c5e-e41500bcfe03)(content(Whitespace\"\\n\"))))(Secondary((id \
         97ca243e-3753-4179-8c00-b03fa173e79f)(content(Comment\"# Cellular \
         automaton with birth/death rules #\"))))(Secondary((id \
         fdf7edf3-3b39-467e-a205-e7e20bc2b448)(content(Whitespace\"\\n\"))))(Secondary((id \
         61cac539-6237-40ab-9a7b-9910df408517)(content(Whitespace\"\\n\"))))(Tile((id \
         5b7c3541-50ae-483f-abd0-8f6010c00c86)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         27c6874a-1ca9-4ef0-af00-0b1ec0384aa4)(content(Whitespace\" \
         \"))))(Tile((id \
         8ad0409c-b435-4f5e-bf98-1c14ad95fccc)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6b6b6b11-972c-488e-9f6e-b4e9d2f742ff)(content(Whitespace\" \
         \")))))((Secondary((id \
         e81dd2e0-d8d9-4f47-ab5a-29a1d6750606)(content(Whitespace\" \
         \"))))(Tile((id \
         d8d7a39d-afb5-4117-9b24-13114ba80678)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e55cb605-0019-45b7-bce9-818d5df3e895)(content(Whitespace\" \
         \"))))(Tile((id \
         719d4fd0-4f77-4fc8-8dbc-e58c242385c2)(label(Dead))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ee350402-33bd-454f-942c-dd5ceb852801)(content(Whitespace\" \
         \"))))(Tile((id \
         ba3d29c5-974b-49dc-976c-18c68f4ad34f)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         feb914ee-05a2-405e-808a-2f575e69beaa)(content(Whitespace\" \
         \"))))(Tile((id \
         798dfe07-d023-4fab-83cc-68bbbde5df86)(label(Alive))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e92d1e73-522b-4499-8c5a-7c0971cff9ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6607c2ba-370d-4e95-87d5-87b581f5900e)(content(Whitespace\"\\n\"))))(Secondary((id \
         905a9882-5d3c-4dea-955e-db28fd68b750)(content(Whitespace\"\\n\"))))(Secondary((id \
         f55a74b6-8f3c-44d4-b3c4-010b74a8118b)(content(Comment\"# Grid is a \
         flat list with width/height metadata #\"))))(Secondary((id \
         f49c2afd-33d3-43ea-b563-343d84a0f99b)(content(Whitespace\"\\n\"))))(Tile((id \
         1d059ec9-35a4-4de9-82a2-9b8c819d4f5e)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ecfc398a-febb-4554-9f0a-eb423bf14bda)(content(Whitespace\" \
         \"))))(Tile((id \
         6cba784c-7fef-4ddd-89c0-396c18a6aa39)(label(Grid))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5e36cbba-ab1f-4304-9c70-cf622a3ef937)(content(Whitespace\" \
         \")))))((Secondary((id \
         0d26531b-274c-4715-b8ae-33e6a1a67355)(content(Whitespace\" \
         \"))))(Tile((id \
         4d87f62c-11d5-4086-b9ac-835af41546d4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         1347eb87-ae13-4052-93d3-39fc42986bf7)(content(Whitespace\"\\n\"))))(Tile((id \
         47aced58-253f-4e2c-be93-ecbda8f4ebd3)(label(cells))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         192eb309-c58c-43df-976a-087f94600d7c)(content(Whitespace\" \
         \"))))(Tile((id \
         d589ecde-355f-4a0b-a726-944c9d22a993)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dbaed3ab-b7ff-4016-93c8-481c1dc0e3c7)(content(Whitespace\" \
         \"))))(Tile((id 9efe23d2-f0ce-4972-ad06-2c9577fe805e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b18ef307-be86-4081-9418-b89aab31918e)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         b7724be2-1a79-42dd-bf8d-9b82b19639c3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6a641465-8f50-43ad-ba55-76a56dcb6f0e)(content(Whitespace\"\\n\"))))(Tile((id \
         fbc72830-b4a2-4888-979d-d79a65a22e7b)(label(width))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d9a6700c-005f-4077-aac0-4039c9a0bce0)(content(Whitespace\" \
         \"))))(Tile((id \
         0adecb87-fde5-423c-bb70-4feb6a0e0fa8)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1745f94a-fa07-4e0d-9cbd-84b4e256cff3)(content(Whitespace\" \
         \"))))(Tile((id \
         04559e88-a91a-4548-a7f8-c9bef73cb17b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         900c0e8e-2fec-4e0c-a1fb-4e3fb62a36f3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ad6cf55a-9158-42d7-b2a2-2350932de356)(content(Whitespace\"\\n\"))))(Tile((id \
         1270aa13-cf86-445b-9c1f-eca7a4e4caa5)(label(height))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         51875fb2-e181-483a-b886-4adbcb99e689)(content(Whitespace\" \
         \"))))(Tile((id \
         c94cac96-dfc9-4408-a94d-f774e7f7b406)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e9505b4b-34b6-43d1-912a-faca09ea0cac)(content(Whitespace\" \
         \"))))(Tile((id \
         918842c8-bb3e-4f4b-9070-98b2a4e44669)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         eba557b4-6ae8-42e4-bfda-69df000f2a14)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e1783475-8f39-49f0-b601-1df6b4b71ddd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8eb6d344-830e-4e8b-9763-f8de3a9b949f)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a015f55-c711-41e5-bf4b-e490ca6bd152)(content(Whitespace\"\\n\"))))(Secondary((id \
         27200221-1974-43bb-9663-8883ba01bbe1)(content(Comment\"# Create empty \
         grid #\"))))(Secondary((id \
         5592d6a7-9739-4e74-bed8-4dbb5cbc53c9)(content(Whitespace\"\\n\"))))(Tile((id \
         35583bae-bc68-4490-9ddc-30db09d99909)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d76d683c-59a9-4e26-8e2a-ed88eb3f3210)(content(Whitespace\" \
         \"))))(Tile((id \
         d765eb0f-9138-4fa1-8461-e34c38616bb1)(label(makeGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c24b85b3-8767-4900-bf26-24faec49d85a)(content(Whitespace\" \
         \"))))(Tile((id \
         da60a284-ec37-4878-9df7-4aa2b50edb04)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b157cff9-ab9b-47b8-acd6-4743f7dbee94)(content(Whitespace\" \
         \"))))(Tile((id \
         33c8e245-26ad-4d18-8a9d-4beaa29570e2)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ff4c995e-c678-4ac1-a65f-1b410cb37786)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f480210c-0a9f-4e67-b983-71c118587b4a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         19e27a99-cf5b-46f6-ab2f-b26f8f8b33e6)(content(Whitespace\" \
         \"))))(Tile((id \
         79a126e2-940a-4607-b073-bffb83e9747a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3e5a6687-489f-442d-a861-528823289170)(content(Whitespace\" \
         \"))))(Tile((id \
         458340ec-01cd-402d-a601-d3262256afab)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         59b9901f-12eb-495a-af9d-7bbe6dac48db)(content(Whitespace\" \
         \"))))(Tile((id \
         3260bb65-9571-420a-a1ec-13193c384246)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bba48d24-93f9-46e2-91cd-c6ab27a24fa6)(content(Whitespace\" \
         \")))))((Secondary((id \
         06e14375-51f0-4449-b4fe-0a8941c3de71)(content(Whitespace\"\\n\"))))(Tile((id \
         87be8da7-4aa1-4e88-b734-9e0b4bff46df)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1e670adf-fc0c-4f87-855e-a6eb8964238c)(content(Whitespace\" \
         \"))))(Tile((id \
         7de54df0-d9cd-4764-bf48-b1df04ff4bf0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a67b7215-87a2-4242-be4c-7cb6daf5fdcb)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         56ef4ce2-4308-419f-bb95-df29a82e5765)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f7abd766-67e1-4759-9e81-af14038801df)(content(Whitespace\" \
         \"))))(Tile((id \
         d04dd551-2644-4c70-a791-244267a36507)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         76871008-1655-433d-b550-5faeac753c0a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cace8792-8563-4904-892b-cd0845b0071d)(content(Whitespace\"\\n\"))))(Tile((id \
         5779acee-6c84-4f2c-a268-8536b1ebfeab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5b100e43-7382-45f7-aa48-a15aa6b96271)(content(Whitespace\"\\n\"))))(Tile((id \
         83586f94-39af-4ca4-b0e8-a14723843d94)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7aa6063a-8f47-47e3-a638-ad88a877d265)(content(Whitespace\" \
         \"))))(Tile((id \
         068d12f3-f84c-4805-9635-f944df73d1e9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec42a8f6-2803-4473-93e4-188daa9ee771)(content(Whitespace\" \
         \"))))(Tile((id \
         06937fd2-dc93-47eb-a489-7fb00f82ee11)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8aca49cf-080b-422b-be63-b81ad31504e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7bc9e5c-439b-47bd-a19d-52738c2c54f1)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd2a9980-3413-4b26-aac4-89aa58bc1008)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b439ed3b-739c-4ede-bebd-ea1bc32842af)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28050b4b-55bf-42ec-9c5c-5c2929af3a1d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         207f12f8-cf4d-47db-91c8-2693de35178c)(content(Whitespace\" \
         \"))))(Tile((id \
         114b1b87-8616-4aab-beff-5dd3ebecf670)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         84dcf6dd-20d3-43c5-9a67-e53e7dde108b)(content(Whitespace\" \
         \"))))(Tile((id \
         4c47dd98-8561-41fa-98ac-29e12062532d)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3351a543-3824-4f11-9f15-e36de87f146b)(content(Whitespace\" \
         \"))))(Tile((id \
         121a2978-836b-40d4-9464-427ecfd43db1)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b862de6-2947-4442-9c5c-d81c8c97ced9)(content(Whitespace\" \
         \"))))(Tile((id \
         4923cfe9-c98f-4e80-bfd3-07545dc20ee0)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54d15071-8298-4fb8-87f0-ec6339cd799c)(content(Whitespace\" \
         \"))))(Tile((id \
         e68a2b59-9f6e-4e3e-9e14-abc05b5b289e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2e65d80b-d838-4ff3-b10d-926dcb63f24e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acf24855-9857-465e-9673-446c687e3d5a)(content(Whitespace\" \
         \"))))(Tile((id c8832ebb-b9e5-4f60-87e9-9a2ee1203501)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6a5b009e-3d20-4f6d-bb2d-45f8466f6b43)(content(Whitespace\" \
         \"))))(Tile((id \
         1df92a21-41e6-4d0c-8bac-a967c1c80cab)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0e71a0eb-50e6-4194-a5d1-5611391f7570)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b90e7cec-53db-4fce-99a9-4352e8a3282d)(content(Whitespace\" \
         \"))))(Tile((id \
         2c9d2475-b661-4d74-915b-cdc810ec46df)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         975ba176-c43e-40e4-910d-f5e4c1f888d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bfa4ca7-4ea6-489c-aceb-0bd1571dc628)(content(Whitespace\"\\n\"))))(Tile((id \
         35dc1d47-81e1-495d-9b77-a77819347677)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22dd6147-b044-42f3-bf9e-33fb64a38ae1)(content(Whitespace\" \
         \"))))(Tile((id \
         1807a99e-cd4a-43ea-afdd-eb8b865fd6f3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         507c62ec-3466-45ae-b29a-289f64f75eb0)(content(Whitespace\" \
         \"))))(Tile((id \
         3938fae0-2cf6-4bf3-8c36-ae279fcd66ac)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79a54ad1-61ba-45e4-a9ec-5ac87c791459)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e1cf0c0-f554-438a-9e1b-b39f86d0d8f7)(content(Whitespace\"\\n\"))))(Tile((id \
         f4392d7b-d903-4b5f-926c-283843139669)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         852ac853-74ff-4c28-9681-66cbba72e1a6)(content(Whitespace\" \
         \"))))(Tile((id \
         13fb0e79-cd0f-4d41-8089-3a3154ba4eab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6827d81b-c70c-44be-a956-0945232d1523)(content(Whitespace\" \
         \"))))(Tile((id \
         cd144314-efb9-4688-903e-2e35291b65a1)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         338337a8-ece5-4e53-922b-fcdfe5e6483f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2b0060a6-d9c2-434a-828b-523be5f9dc71)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b7a45938-cce1-449d-930b-2c74e685dfc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9d56875-b801-495f-8847-1ee3bdc2ef1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0b4aa1f-70ad-45b1-abb6-17fcdf714a48)(content(Comment\"# Convert (x, \
         y) to index #\"))))(Secondary((id \
         2ba69b55-9254-4573-abb1-500f6fa49d14)(content(Whitespace\"\\n\"))))(Tile((id \
         0fef526a-3680-479b-8160-cb972049a227)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eb432895-91e2-46a8-9d36-e10e87dfc923)(content(Whitespace\" \
         \"))))(Tile((id \
         0cbbba03-8e7d-4113-89c7-f7d4523b18fc)(label(toIndex))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         57938f22-bdc1-4a6f-9645-d186435efa31)(content(Whitespace\" \
         \"))))(Tile((id \
         3493ec44-1f38-4b96-89da-e5234a39f0be)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a56777cf-8ae4-4487-bdc5-74f1b8d12d29)(content(Whitespace\" \
         \"))))(Tile((id \
         05ce55dd-2067-427e-ba9f-4be553113cc9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8f47e077-3967-4a92-8821-29069fd2ec7d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4c1ac408-4188-48fb-8bc9-508c1b34017e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e962207d-831a-47e0-8031-63430389fa1b)(content(Whitespace\" \
         \"))))(Tile((id \
         8edd1a28-e342-4351-b513-58c0785bb1c9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e1ff7923-5ff6-4b60-bc16-b5289b5dfab1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         07102656-0f63-413a-ba7a-85039de7f169)(content(Whitespace\" \
         \"))))(Tile((id \
         a194e2ab-3064-4815-9842-3117c5166df6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         285c0347-bdf2-4130-b820-071cd097640d)(content(Whitespace\" \
         \"))))(Tile((id \
         c782f5fb-3a6f-42bb-9480-a72315e68d1f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4282037e-0c06-4c50-8ad1-35a5c39859da)(content(Whitespace\" \
         \"))))(Tile((id \
         dac2b21d-eed8-42a7-8fa4-517a78b1a831)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         de2f0eeb-1568-4268-8866-cb9613b24a75)(content(Whitespace\" \
         \")))))((Secondary((id \
         05ede5b2-407d-45ea-aee4-a844a7253c54)(content(Whitespace\"\\n\"))))(Tile((id \
         4de0cce8-03c4-4600-9373-1fa7979194d4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4a90bd0d-ca9b-4513-a455-b4a163f3b038)(content(Whitespace\" \
         \"))))(Tile((id \
         024b006f-6deb-480b-82c2-04aeb7d9ce6b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         694541a3-70c8-45e7-90f7-6cf2724cdcee)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         06c4a244-0e24-4c19-a214-20ed68173242)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         aaa749e9-5d25-42d4-b0be-09aa2278edeb)(content(Whitespace\" \
         \"))))(Tile((id \
         bbf81b5a-76c4-4ada-bfee-a1af104e8d45)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fb68cae9-4ba5-4704-b35d-b9501b53f2d0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a501614d-6349-4f7e-86e3-ebd61c552a21)(content(Whitespace\" \
         \"))))(Tile((id \
         e9d2eeb1-1f39-4995-87e3-95393d14132b)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d26b272a-3bcd-466c-b938-3eb7d4754a24)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         27ea9479-2102-4ea4-b2a5-d08cee87a8d3)(content(Whitespace\"\\n\"))))(Tile((id \
         a26ca0e8-845d-4a2f-b843-782d2e3d3876)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9669adef-707d-4748-815a-67170268c8d2)(content(Whitespace\" \
         \"))))(Tile((id \
         6953a6c6-01b1-4367-a471-1faa04a416f1)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a101ac2-3d4a-420f-8b84-d404e92a7c43)(content(Whitespace\" \
         \"))))(Tile((id \
         2ee31828-db83-4b4a-9c4f-204acbb4d198)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13a35bb4-4496-40eb-b329-0d583d6e5f2c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4ceca8be-de35-4856-abf0-ec0b02b45148)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa726fde-cc76-4c9c-8441-f6f68d5f7526)(content(Whitespace\" \
         \"))))(Tile((id \
         47fa6fbb-00d7-4389-905c-e4c2aad5defd)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0112c33-dbe5-4c17-a681-7c40d5005b83)(content(Whitespace\" \
         \"))))(Tile((id \
         f0fd80ed-c93c-4291-808c-fe22317df2e4)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77e7de28-d3b4-43c1-88f3-2e08830d2345)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8f1f19a0-1435-40ea-a24e-ecd4f89d2df8)(content(Whitespace\"\\n\"))))(Secondary((id \
         69e666cc-531a-4eae-9124-3090435cb60a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9ee43cc-8a79-4328-8212-3c06e9136202)(content(Comment\"# Check if \
         coords are in bounds #\"))))(Secondary((id \
         d5cec502-b02c-49be-8fc4-d66bbdab19a7)(content(Whitespace\"\\n\"))))(Tile((id \
         e81a1d1b-6b9f-4ab5-8d02-1e048987b4aa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be1b66c3-03cc-4028-944c-657c50173407)(content(Whitespace\" \
         \"))))(Tile((id \
         25492f8e-7393-44fb-984e-a5c7724e320e)(label(inBounds))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25e3e3e0-cdae-4c53-b5bc-e9ddf6e488c2)(content(Whitespace\" \
         \"))))(Tile((id \
         207f2e86-cb10-4c77-88d4-ac8f01cec051)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         466b743a-8803-4f5d-a1dc-01f43ffa26f0)(content(Whitespace\" \
         \"))))(Tile((id \
         9a2d3702-2a83-4926-a164-04e0a138382e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         4f866d02-7d38-4356-b8b5-c7e14b526f31)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f912a528-cb0d-499c-9eaf-f260e86022f5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         241562e6-a60d-4db3-bf7c-104925aac8bc)(content(Whitespace\" \
         \"))))(Tile((id \
         7d7925cc-ca45-4335-af1c-2b603a05c3c1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f63f936f-7e91-4fe2-a064-310bf6e10515)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e416601c-7bb8-40b7-8a3f-128fe4bbd251)(content(Whitespace\" \
         \"))))(Tile((id \
         935f906f-b4ed-4cb2-bf65-939a0962ada9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9367abb1-645a-4cde-a83c-da176e1706e2)(content(Whitespace\" \
         \"))))(Tile((id \
         a1d98355-e198-4ddf-a067-ac65b3f28910)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         00fd8ad8-e263-420d-9e49-5904285148d2)(content(Whitespace\" \
         \"))))(Tile((id \
         0e8e2849-8798-41f8-b24f-4fd1bd988e70)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f93e932f-1b5c-4e24-9832-0b66555fd60a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6e9ef6b2-4526-4d42-a9eb-ccec759ca0f7)(content(Whitespace\"\\n\"))))(Tile((id \
         74a470ba-5e8b-426a-9732-d496417abce9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a3caed74-577e-46d5-a290-bc13202c3bdd)(content(Whitespace\" \
         \"))))(Tile((id \
         5424b63b-d500-47f0-8a9d-86e2e2d47511)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6dd4f0f8-f55a-408f-b342-fa0bb31a79ff)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         087a1cf9-c23b-45a2-b4d1-19825f818d2a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         103db396-b2f0-4ae1-a2eb-80d38fd3fac7)(content(Whitespace\" \
         \"))))(Tile((id \
         7a0e8086-e256-48ae-8583-1aadb00f25f4)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dd8d5613-1e45-4ea5-b67b-a27f8f6a9cb3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         16bd582a-ce46-42b8-babf-ff2cc235d666)(content(Whitespace\" \
         \"))))(Tile((id \
         152dd5a5-9c15-44a1-85bb-0d832e0e6b1c)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         09e29893-89de-4fd9-aa9c-40358a55d94a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34272fd9-0c89-4c48-abf6-19dffb40286f)(content(Whitespace\"\\n\"))))(Tile((id \
         ce9bd12d-374d-4250-bd66-bb6b10895c6b)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         772565e3-e3c2-4861-a3a0-a31ac7207ecd)(content(Whitespace\" \
         \"))))(Tile((id \
         0e514067-b1dc-4195-9247-38fd5c714716)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dac22fab-a06e-4fc9-949c-d6fe79e2b7ee)(content(Whitespace\" \
         \"))))(Tile((id \
         890c14f9-6d70-4789-b655-b13f51986c0e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac96ce2f-d897-4005-94a8-9a0cf4be2daf)(content(Whitespace\" \
         \"))))(Tile((id \
         6adaf700-224c-4bba-8d49-6a64379d8ccd)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad876cfb-787d-4349-8454-f2e47b545ab1)(content(Whitespace\" \
         \"))))(Tile((id \
         1906738e-ca96-4a3c-9f90-8c16b5428656)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63f04a51-dc07-4c1a-8064-4a476c8f7dcc)(content(Whitespace\" \
         \"))))(Tile((id \
         c99111cb-9382-4b51-a768-d7083cd428bd)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f557f85-ca07-43e8-9768-637ddfc0d3eb)(content(Whitespace\" \
         \"))))(Tile((id \
         50a2a3ac-cdfa-4f8c-b0b8-dc95be984aba)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8707260f-ec9f-4ff2-b8be-884445f6a91a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         49683229-6dda-4465-ae77-e0916268b1f0)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf20f772-1a8c-4c4e-be24-bdc8c1477742)(content(Whitespace\" \
         \"))))(Tile((id \
         3b462a8f-1876-48f8-b7a2-39d50a2572ea)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f11c689-fbac-4c0d-a80f-136c152f7c2d)(content(Whitespace\" \
         \"))))(Tile((id \
         81de968c-2353-4750-9fbb-b669b4172d1f)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         30ed8297-3af3-4db5-8be3-b45b690a2c62)(content(Whitespace\" \
         \"))))(Tile((id \
         8cbc4eef-acbe-440a-8b2c-ce88816ca440)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         858deafd-c8e8-4be1-a33b-74ff81df9927)(content(Whitespace\" \
         \"))))(Tile((id \
         d4e49e00-b27b-4206-af7d-f98f0c54db7d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         20892fb0-afd0-41f3-8edf-7a2a886bfec5)(content(Whitespace\" \
         \"))))(Tile((id \
         9a4d4f8b-7b94-482e-8995-8f73a79d03f6)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         518d94f8-71c2-4a15-abbc-8afa642411a7)(content(Whitespace\" \
         \"))))(Tile((id \
         7126f287-dd97-4836-a2e9-e5e337ab59d1)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b17cdd30-85c8-4aba-9af0-375210703690)(content(Whitespace\" \
         \"))))(Tile((id \
         f007c6b8-5b01-420f-886e-1a94d47bfa61)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3184da41-2175-480c-aa18-51b616843d36)(content(Whitespace\" \
         \"))))(Tile((id \
         6e93baf3-bc9a-477c-9e97-25180d57f145)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         424790c9-5e07-468a-aa78-47d0a5aa840c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         24643713-3728-480e-a8b1-6e89006b166d)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a23b264e-9ea8-448c-a0d7-91859aaf4316)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         249d4e29-18ac-4524-ba17-417c77817509)(content(Whitespace\"\\n\"))))(Secondary((id \
         54f4de42-fb03-4f7d-8c70-c654c9c14e67)(content(Whitespace\"\\n\"))))(Secondary((id \
         86ad9439-d5f1-4e05-8f81-b8520d2818b3)(content(Comment\"# Get cell at \
         (x, y), returns Dead if out of bounds #\"))))(Secondary((id \
         e6a7030d-44fc-4073-807b-1ebb39039af7)(content(Whitespace\"\\n\"))))(Tile((id \
         2cbeb53a-bc81-4eb1-adbe-669182549c50)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8b318426-b56d-4f8d-ba25-a90fd86eb60d)(content(Whitespace\" \
         \"))))(Tile((id \
         7cd3bafc-26b0-4c16-80bf-0691973e0d8e)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e3964dd3-3efb-4e2e-b6c2-ebeda8436806)(content(Whitespace\" \
         \"))))(Tile((id \
         1991e97a-b7ef-479f-9089-7412a69fc24f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         333b3f08-5b3d-40de-a506-122de80c3f83)(content(Whitespace\" \
         \"))))(Tile((id \
         7d3f726e-ddc0-4d5a-b84a-e3b21304d961)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         50ecab58-1e09-4aa3-82cf-7a423b826720)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         284e2d11-3432-4eee-bf50-812636c40e03)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         92075cf3-86bc-4967-9adf-b993088a47fb)(content(Whitespace\" \
         \"))))(Tile((id \
         6c1cbfcb-77d6-4c95-ad6e-fd87ddb44322)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         41590709-a304-41aa-a5a8-44ae1ca62dc5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ccdc3bd0-908b-4edc-a3a5-62cb4c4cc824)(content(Whitespace\" \
         \"))))(Tile((id \
         ce94e525-a226-4b44-8976-9a763340fd43)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         cc33b2fe-202c-45d8-b54f-31ccdd24870f)(content(Whitespace\" \
         \"))))(Tile((id \
         2f90aee4-cedd-44be-8314-c9038302b11b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3270cc41-863b-42cc-a341-d6231696f3e7)(content(Whitespace\" \
         \"))))(Tile((id \
         b53463cb-cbfc-4f5b-82f7-e49f524b51e2)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bf27c4d0-198f-41cf-9de8-0c1e0e236d51)(content(Whitespace\" \
         \")))))((Secondary((id \
         b20ee5a7-3a30-4827-8bb2-136975cf7da3)(content(Whitespace\"\\n\"))))(Tile((id \
         f521d526-01c6-440d-8803-8649b1b2ecde)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2b39e538-d8fa-45b2-8f9e-31f144468414)(content(Whitespace\" \
         \"))))(Tile((id \
         52cdadb5-2985-495e-880f-e137e820f299)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c1513e47-0f4e-495d-b9e7-c5574fe4c9e7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         effeeef8-92a4-4a6b-90f3-3a5e5388a996)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3c99e6bf-5da9-46e1-8c67-7a04c4bf0857)(content(Whitespace\" \
         \"))))(Tile((id \
         5cfc5c82-c1f0-4307-a01b-a334bdf2fd84)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4ce7091f-5c3d-4e17-b937-b2c930aceece)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         593a1df6-0c37-4cb4-a555-a892033962ea)(content(Whitespace\" \
         \"))))(Tile((id \
         757ffe99-fbfc-43b8-ac76-f672de19b5b1)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         103e2742-ae61-4f75-b52b-666cb191f5f0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ed442a8a-c76c-4619-8fe9-4e4b6ec7dc62)(content(Whitespace\"\\n\"))))(Tile((id \
         8fa21470-3d86-42ea-9b61-ddfcee2a632f)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         736ed817-ec35-4527-af65-11a754d47fe9)(content(Whitespace\" \
         \"))))(Tile((id \
         ada0bd99-5ddd-4e5f-b19b-3b34f0b36b01)(label(inBounds))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66a788e3-28c0-417e-bc39-b08397f952b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6769155-f4c4-4075-81cc-4994c63d5c75)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         149f8b36-808f-4a9b-9923-2ae41b938c05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1231db4-cdf1-41e8-8328-d6f1b18020c3)(content(Whitespace\" \
         \"))))(Tile((id \
         c84b104a-d20c-4101-aaa8-fe6bc70cd0ee)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfa6979d-5213-4a04-a369-ee56c7324647)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa989f17-88ff-4e8d-8e3d-76a0735764f1)(content(Whitespace\" \
         \"))))(Tile((id \
         ae819567-3e3d-4ee3-88f3-6b17bb8eb9cc)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1702deae-3a25-4ed7-9e28-027e05fc1dea)(content(Whitespace\"\\n\")))))((Secondary((id \
         219c24ad-d952-446b-a99c-b890362dc18d)(content(Whitespace\" \
         \"))))(Tile((id \
         f41bc461-52a4-46e4-ac5c-909021b8115a)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c931ec8-031c-439c-a133-02ce203db43a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a226f3f-2287-4ec1-b05f-6ea15e5c78f3)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a2a776e-0256-4391-a58b-d6fb678f599f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         96e32661-809b-4f9a-8a5e-152cbdb6b899)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eec4b724-3427-4cf5-9b88-790a83c04dac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74190d54-aea5-4f27-8d54-50c739c4d554)(content(Whitespace\" \
         \"))))(Tile((id \
         9a327595-1d37-4fc4-8bf8-24cac409d46a)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52b1a8f0-d210-429c-ab68-d7592532ecd9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8f2e91e7-844b-48b6-8752-8e5e06fe120b)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67ac8140-2861-4704-9ef3-c3fb24f4706b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb433e2c-96ed-4af2-82de-ee90968711a5)(content(Whitespace\" \
         \"))))(Tile((id \
         a079b3ff-4ff4-48f1-b002-a669bfd5a900)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e30bcdea-2fc1-437c-bb2b-92cafe05b319)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ab01fd6-7462-4eb7-972e-ed103aabcac4)(content(Whitespace\" \
         \"))))(Tile((id \
         7e73690e-5de2-4c5d-8180-4cff13f40f11)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         13a78fa1-52a0-4ff2-ba35-012ba0faeaa0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         952d8bc0-4cbc-4c4d-b478-151a62ae0dcc)(content(Whitespace\" \
         \"))))(Tile((id \
         b33df422-497d-4daa-8fb8-cd0bda14bf27)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3c5d8e2-0751-4a4f-b0e3-09f5025fd635)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f972aec1-672a-4be6-bad2-5900d6b41d22)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd5aa067-07cd-41c9-b407-66c0739f34a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         556bea46-7ebe-4711-82e5-9f53a698d012)(content(Comment\"# Set cell at \
         (x, y) #\"))))(Secondary((id \
         c6a44b47-e196-4c61-b3ab-decfbdc0fb24)(content(Whitespace\"\\n\"))))(Tile((id \
         780d563f-f45a-467a-aa02-2f3ccdb7bfa4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3194db91-9c8f-458b-a9bd-ee608f1e6099)(content(Whitespace\" \
         \"))))(Tile((id \
         2ad1e215-6014-431b-b186-9827436af1e7)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c454be28-3527-4a88-9ff5-a913fb83b094)(content(Whitespace\" \
         \"))))(Tile((id \
         b0cffb18-1e57-4f0b-a7d5-6cfd7a15503c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         373f07ba-aadb-4467-b7ba-a1f240922de5)(content(Whitespace\" \
         \"))))(Tile((id \
         998dab18-679a-4ac9-b843-b37c3633eccd)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         cabe6ca6-4a83-4e32-9c5c-04e11b431def)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e5d5406d-ba96-4d60-82ce-3d8635e3a57a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         92442202-9012-4e5e-8419-9983ca39e5d8)(content(Whitespace\" \
         \"))))(Tile((id \
         51766542-b926-4d26-a985-29f1166f1020)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a30713a3-4c9b-487b-b660-265eef903525)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         701c6059-da80-4bf5-8bd1-c7312703c53a)(content(Whitespace\" \
         \"))))(Tile((id \
         9631823e-4f46-4a48-87e1-4726fc9190b6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8fde2e7e-de76-4c36-b59e-f9c528ac16d0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d794d68a-096a-4349-b8e5-4d84a732b955)(content(Whitespace\" \
         \"))))(Tile((id \
         8a1dd786-a6b5-419b-8564-c8f61c602b85)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6f426a77-d7d4-48fe-a20b-e9446bec946d)(content(Whitespace\" \
         \"))))(Tile((id \
         3c67d6e8-338c-4db9-ae03-50d6ccdea8cf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0362089a-58dd-473a-b2c9-6a419e7b16e2)(content(Whitespace\" \
         \"))))(Tile((id \
         0f547f6d-23d3-47d4-b7b6-091cffe0cd00)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ee9ab644-2bd1-45bd-968a-989b5223bc12)(content(Whitespace\" \
         \")))))((Secondary((id \
         07157594-1a14-4684-92dd-dc2199d57267)(content(Whitespace\"\\n\"))))(Tile((id \
         50f1bbd8-379d-42fe-b1ca-0d99dee3dd5b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3fedc90e-144e-4d82-ad5a-0a349cf69b47)(content(Whitespace\" \
         \"))))(Tile((id \
         e7b90ce3-6b08-4445-ad43-c22c58860b80)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bd0ead44-0625-4e57-988d-63c87f3e0b51)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         536969a5-df44-46c4-b79a-c9e3ab066eb5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ee0e0480-04e2-46ac-b6cb-c01550a6db28)(content(Whitespace\" \
         \"))))(Tile((id \
         025e66fb-8dbf-4f0f-867a-bd973b10ccb9)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         19567a1c-efb7-4e6e-9ba1-cbc30520c884)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         aab1fe61-4e73-4a67-b268-e00e8a443e29)(content(Whitespace\" \
         \"))))(Tile((id \
         37c6ef2a-0d4c-46eb-9b71-3593efe06251)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         44edc626-95b5-40f5-b4cf-5867afcb1c09)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1edee5a5-9a77-4249-94ae-f602603a5ce8)(content(Whitespace\" \
         \"))))(Tile((id \
         89c4e862-90ea-439b-9dc6-54476f68e51c)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c87d75f4-204c-40e1-abd1-9f575503e10d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f78e0fc8-9e76-4f70-9c40-709e719b1b08)(content(Whitespace\"\\n\"))))(Tile((id \
         59e09604-2b47-429d-8251-0cf59af9dcc2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         302e7e58-a2af-4a1a-9a5f-52e0434e1a16)(content(Whitespace\" \
         \"))))(Tile((id \
         ba65e2a8-6c8b-4c28-b824-a8367997e60d)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6204585d-a5c9-4753-aa58-53e115724d15)(content(Whitespace\" \
         \")))))((Secondary((id \
         e8e94be1-cc3c-4634-a20d-00f357d741a9)(content(Whitespace\" \
         \"))))(Tile((id \
         597f5628-5139-4c00-bfac-66665d4b363e)(label(toIndex))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99e22436-01c7-40eb-9412-e6bf239ff2b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ee30194-0230-4d31-93b7-71ddd24cf584)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         afdfc78f-7fa0-4be0-9b9a-0a7da0f21ec8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19ff8bf7-09e3-4260-b6ec-e0afd989ddcf)(content(Whitespace\" \
         \"))))(Tile((id \
         b739757f-d13b-4efe-bc75-997c00f67f52)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ff1ecbd-7ec1-4f8c-a69c-05233d8472ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d4289cd-d2aa-4898-8955-9131654a7b2f)(content(Whitespace\" \
         \"))))(Tile((id \
         117eac9e-fffe-4978-823a-3b5645d4ba56)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b5ae3937-4c0f-493e-a1e4-244ebdbbc0f6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         217d5d79-2afc-4b8f-8659-f52201e9c3a8)(content(Whitespace\"\\n\"))))(Tile((id \
         8dcc7199-5598-4f6d-a8ab-51bbb007701c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c83cde45-f1a3-41fb-a00e-2975ebfab4ab)(content(Whitespace\"\\n\"))))(Tile((id \
         468ba315-0c67-45fc-917c-d76ad63f5107)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         359ef7ac-eeff-4ce4-ac6d-c5da893c1211)(content(Whitespace\" \
         \"))))(Tile((id \
         0376eeb3-bb31-4214-9a0a-dc2e355422fe)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e8673fb-c0af-4b20-bf90-5554a5303beb)(content(Whitespace\" \
         \"))))(Tile((id \
         0d0d9d0c-2ae4-458f-986a-f4dfc83655f5)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b96b5489-3374-4e47-9dd7-df8643bb38b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         57c75d57-f5e5-4c20-bcc5-ab726238d2ae)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adce5750-d432-4078-9165-929c19594007)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0db83a51-cf5c-40da-bdfc-458d0aec8f3d)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcfec769-74bc-42a6-a71a-aabaa0d9b385)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34dbaad1-7248-4060-98de-80befaff7925)(content(Whitespace\" \
         \"))))(Tile((id 02f13239-de40-4848-95e9-edc1905615b5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dda7ba11-c73f-4449-bea1-8c252e3a265f)(content(Whitespace\" \
         \"))))(Tile((id \
         3e07d33f-762d-4210-ad38-7d1af4a37b53)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3d53d8e3-d2f9-4ca7-b7de-cc2f0428d37f)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         43b6d504-70b5-4cbf-a30e-1c83c5a9fa2e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         952caad7-f3a2-4b12-9cd8-db441cfe64ac)(content(Whitespace\" \
         \"))))(Tile((id \
         2b1495da-0280-43de-9e33-e7fc5c7ea0d3)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b19fe446-4fe8-44e0-a154-2ee85283d42f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cebc84be-df62-46f8-8bd1-5ccc66544933)(content(Whitespace\" \
         \"))))(Tile((id aad69678-a978-4861-a3d7-b92139de674d)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         febdc21d-18dd-4e08-b98d-35b4a693ad75)(content(Whitespace\" \
         \"))))(Tile((id \
         de3c9352-106a-431b-9699-6579f723ea0d)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5f2f8d5b-7d97-49a0-8a32-0110bace1235)(content(Whitespace\" \
         \"))))(Tile((id \
         3c576545-243b-4777-826e-e3ff6bfcbcb7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e1a56c9-e9cb-4ebb-b5ec-c0055bbd8343)(content(Whitespace\" \
         \"))))(Tile((id \
         a4c8ed0c-b3cd-432f-8e8e-77f6192b1879)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aea486a5-429d-4ff3-b889-c485663a8a8b)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe91350a-ef68-460b-8a71-7bee1ca21268)(content(Whitespace\" \
         \"))))(Tile((id \
         97efe83f-6e7c-4a0d-88d3-e28caf248217)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         662cf027-31a4-438b-93cf-c066c7981129)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bf5b9308-d619-400c-ae3f-acad7c68a0d3)(content(Whitespace\" \
         \"))))(Tile((id \
         ddf9d9b2-582e-4c6f-813f-c429df8748d0)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b345b1ba-0446-4631-a2a6-6b80ede94220)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         965b2d59-927c-4150-9c7b-0b12011f593b)(content(Whitespace\"\\n\"))))(Tile((id \
         dce72cf7-c6ae-4f53-8a4b-3eb627c366fa)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2a6427cf-ea45-41f5-82c9-8c0bb817a946)(content(Whitespace\" \
         \"))))(Tile((id \
         57f50986-0136-43e1-a05a-cbeeae9f1495)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ebc4fcc8-961e-470b-b6b0-1253d9b6d703)(content(Whitespace\" \
         \"))))(Tile((id \
         d9bd5058-803d-4e94-8c67-1f18bbe56f44)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6dc4279-4dda-46dd-b07e-563b0c3cc3aa)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f11f7602-72ca-4ef4-abbb-a45fe5c096d9)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e79e5fed-06e8-41e4-a580-96d6ba65f580)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d154e8b7-e2b7-4f9d-a0a6-554a3c530fa8)(content(Whitespace\"\\n\"))))(Tile((id \
         f2e27830-e8a0-46b9-b329-4a1793f7a48f)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d88d6a0f-28e2-4b3a-9705-19b581df53df)(content(Whitespace\" \
         \"))))(Tile((id \
         c7a12cb7-6514-48bd-80e3-77de7da151a3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4709e987-bf5e-4050-a691-95fc544d481b)(content(Whitespace\" \
         \"))))(Tile((id \
         800883ef-70bb-4e91-9fbc-006ff3bf8718)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a88cc70e-093c-4314-9d24-20177c6c6d08)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7798e1bf-4ca9-4ef8-a415-694e89cb94f1)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4efe6ab5-fe6d-422c-b88b-3f849762349f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2313f796-8dc1-4901-8298-e0447c64f3fe)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         35382d73-8ad0-47ba-b35f-a0bced0ea216)(content(Whitespace\"\\n\"))))(Secondary((id \
         7272ba8a-2f1f-495e-af43-f2bc55675c02)(content(Whitespace\"\\n\"))))(Secondary((id \
         08d39a33-91e8-49d1-8fa6-c8782606b6de)(content(Comment\"# Count alive \
         neighbors for cell at (x, y) #\"))))(Secondary((id \
         6e032078-8eb0-4b7e-b75a-d1d4cb54ce54)(content(Whitespace\"\\n\"))))(Tile((id \
         64e479b2-aec3-4503-8043-5c09a90b495c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a4d8c3dc-e120-4c4e-bc1f-d8fab6dab0a7)(content(Whitespace\" \
         \"))))(Tile((id \
         e36d471a-4c52-44fc-90f4-6e2a40613835)(label(countNeighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aab75f63-922c-4baf-a416-25117082ab3f)(content(Whitespace\" \
         \"))))(Tile((id \
         fd0eddc8-bbdb-4317-b80a-8f4536d1c9bc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9a7064fc-826a-4765-985e-f2bd2168376a)(content(Whitespace\" \
         \"))))(Tile((id \
         4e3d671d-2ca3-47d1-a49c-1da8f4712cd7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         dc6a511a-4612-4feb-a4fd-2367b2b95bff)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         770aee10-e439-4492-89e4-8abef6a8eab4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         12754f48-2482-4a66-baae-91a262143208)(content(Whitespace\" \
         \"))))(Tile((id \
         a8eb53ff-0eac-4ed8-92ae-159ca9dffe62)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         094743ed-f644-431e-9eab-f70135c7108d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f75d24e8-3f68-4f49-9331-a027fd7d63c1)(content(Whitespace\" \
         \"))))(Tile((id \
         3ede258a-9c11-4e0b-b5c8-bff1d961617f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         70b2412e-f5b6-4e36-9ec5-c6e0237de7eb)(content(Whitespace\" \
         \"))))(Tile((id \
         9186e15f-e49d-419d-9332-f08c3b3c4d08)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0d4f4962-3cfe-481f-b88c-27d53f0eeab3)(content(Whitespace\" \
         \"))))(Tile((id \
         12415026-df5d-4c29-bd6f-63f5c0fbb802)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         19337573-88ba-4346-a9d7-b5ee36dc2fac)(content(Whitespace\" \
         \")))))((Secondary((id \
         09662dc4-3836-42a2-92ce-2edf6a630f76)(content(Whitespace\"\\n\"))))(Tile((id \
         c5bba1dc-3539-4bca-b38b-c97761a3fb09)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f4fc30da-a574-40be-a0c3-74c716f0e7a2)(content(Whitespace\" \
         \"))))(Tile((id \
         5e5141d3-1b8b-4933-9c3c-d2ac3369b702)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         0c2568e1-f2b9-4e52-b60b-b0903c07ac2a)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         49792d2d-b0c1-475d-ae52-4768d976c53d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7181bb89-2755-4a3e-b156-3075a0f45501)(content(Whitespace\" \
         \"))))(Tile((id \
         d08a9818-6dd3-4c4c-98c3-f76386ff5891)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3a018c74-9270-4506-97e8-4950a95ba4f8)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c1e72f91-91a4-4b51-af89-4f6a350c2e06)(content(Whitespace\" \
         \"))))(Tile((id \
         0e1bbd66-1e58-415b-9535-c310f5fd7f3b)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4477e36a-5e57-41d0-853b-4b994d16b58f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0a8bed21-3f90-4925-8a9d-b2f989d7414d)(content(Whitespace\"\\n\"))))(Tile((id \
         806aa365-b961-40b2-954e-8095b10f9350)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         150f8212-5577-40ea-99b9-9d245d74ebe4)(content(Whitespace\" \
         \"))))(Tile((id \
         f25c3d0d-d47a-4886-b0d4-8e8e0c39b678)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e6e96d23-aad1-49ef-a5b0-7f7f6de84eab)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca50e0ed-06b8-462d-b7c8-ac12e21d3bda)(content(Whitespace\" \
         \"))))(Tile((id b00e1863-5a7c-4a6c-8a2c-931ddeb8a8fb)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9a9d936c-5127-47ca-ba89-35125446922b)(content(Whitespace\"\\n\"))))(Tile((id \
         74f14c67-bd50-4cd3-9b48-1cff67a01155)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b7005f9-d352-4195-855c-8d1de6e4bde3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91fbdb7f-4a9f-4426-b554-bfd507c8e340)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9be5c8d2-2818-4e1c-9461-b9d9a870a192)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a639cfd-6bb3-484b-83f6-ca1aedab100d)(content(Whitespace\" \
         \"))))(Tile((id \
         e786e039-c644-4c5b-8654-a5c782f6639a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6446f274-7131-4d49-afce-70f4e95d5c73)(content(Whitespace\" \
         \"))))(Tile((id \
         03176546-ad93-4f6c-b739-cbf3d5134ca7)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         657cdbe5-ae0d-4be8-a0e3-078950eab8c7)(content(Whitespace\" \
         \"))))(Tile((id \
         d445cd3d-0487-4a2d-b407-74386511d679)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a9f94ad-8ac6-4c94-8a83-ff0a0606d4e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0011646e-3821-4230-b011-8fdba1d20795)(content(Whitespace\" \
         \"))))(Tile((id \
         054a14e7-59cf-4e67-8067-df701b56767b)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2ccccd5-3f77-4b86-b42b-022f194ff304)(content(Whitespace\" \
         \"))))(Tile((id \
         eac6180e-2c6d-4878-9d11-cb2b3dbf8130)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a271cf49-bd13-4ba3-a27a-065d437efb29)(content(Whitespace\" \
         \"))))(Tile((id \
         6c279970-effb-4f06-97b6-d8a0456c2974)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b284c2b8-db67-4143-8034-57c8590ee198)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85bf126d-c02c-4929-8832-008aba811597)(content(Whitespace\"\\n\"))))(Tile((id \
         36d685dd-0178-4cb1-ae8d-6e6c7d9b34b6)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         062042ff-3713-42d3-abad-4cf04edcc4c7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b710b1e7-4f92-4509-b5c7-b86098bbe3c0)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed73b071-993c-4d5c-a502-ef4cc89480df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         913b1b77-1b8b-49a9-883f-beec7a985504)(content(Whitespace\" \
         \"))))(Tile((id \
         69280ad6-ddfa-46bb-bd6f-d41bd3888cef)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0170dc12-ae4c-4c4a-9472-16092b848151)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5297fa4a-6bdf-4970-998b-dcba1530599b)(content(Whitespace\" \
         \"))))(Secondary((id \
         26f9d5fb-60c1-4760-8dc6-1fc42c877d79)(content(Whitespace\" \
         \"))))(Secondary((id \
         76d9e636-dc3c-47c4-b6b4-54dda8aa49d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         f6c8c717-f97e-48e8-b0ef-7b710fbf9104)(content(Whitespace\" \
         \"))))(Secondary((id \
         fdaf42a1-bada-4109-82cb-cea2faa0170c)(content(Whitespace\" \
         \"))))(Tile((id \
         5b8a5d47-b5c4-4e87-bd14-66fc71af7dff)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8af6bf1f-9e96-46ef-abed-dbe11ac2b1e4)(content(Whitespace\" \
         \"))))(Tile((id \
         7de9c4a6-db19-4b38-a9de-b17aa361db83)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a41f84b9-ebc9-428b-b46c-ed69d4f4c01c)(content(Whitespace\" \
         \"))))(Tile((id \
         4151525b-d693-4948-8f69-fe33db46422e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c62c6e5f-add8-4c3a-b7d1-6b25de967df5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         447df32f-5668-4a5a-a7f3-a683b33a01b7)(content(Whitespace\"\\n\"))))(Tile((id \
         7760b329-ed94-4eb0-9c3f-222a5988440f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a13ec580-a879-4da0-86b0-cfd1516732c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df1a86f7-ed7d-498c-a21e-ae55b2532caf)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb543915-c5ec-48b2-a55a-41dfefa1198a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8adafee6-846b-4eb8-bb61-243d6eb22213)(content(Whitespace\" \
         \"))))(Tile((id \
         769a3c7e-1afb-4ab8-bbc4-84d9cc0eca92)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         94e76c40-037a-4e96-af25-80440c0a13bf)(content(Whitespace\" \
         \"))))(Tile((id \
         d02b9d21-cde4-4a85-a8af-adf6a3d44271)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85265fd7-1a8e-48ab-81bd-1be2152b6eb6)(content(Whitespace\" \
         \"))))(Tile((id \
         5554cfec-ba8d-4b72-871b-d8846a869a48)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         651943d4-663d-4a76-892e-766ec91103e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a87726dd-99e1-49eb-8aa2-d5cadbdf7dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         bb11df93-79e1-4b4c-a2e7-58557ab0031b)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc03a070-acde-42ba-bd78-ebafd8fd1579)(content(Whitespace\" \
         \"))))(Tile((id \
         7312ab84-935f-486a-a4f6-d41c7f346796)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce7f8195-470e-423a-87a5-11528d5d458d)(content(Whitespace\" \
         \"))))(Tile((id \
         4a7e6811-5e8f-4804-a3a0-516d7e99b88e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f055469e-d114-4519-b0f6-64eba3527486)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4a81cd6-45ac-4ed7-82bf-5f8189b82150)(content(Whitespace\"\\n\"))))(Tile((id \
         a14e682c-4a2c-43fd-aea1-217fdbd6be6d)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70ba52e3-1568-4476-abe1-2808cddad92d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42e666c5-91a0-40f5-882d-7283f3486e15)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d53f2ee-0d8d-4b4c-aa4b-c549fccf17a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c09faaf-63c0-487a-91b9-142420e437b0)(content(Whitespace\" \
         \"))))(Tile((id \
         5209abec-989f-483b-9c54-e900e8cb0602)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e6128a6-b6d5-407a-a089-1d07327591e6)(content(Whitespace\" \
         \"))))(Tile((id \
         dc34a35f-4d2e-4e25-b255-ccaa6949c951)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         458f7dd4-3d3c-4a4d-9bc3-b6226bfc2984)(content(Whitespace\" \
         \"))))(Tile((id \
         0369e495-a6dd-44da-8b9e-7da2a0ab0700)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0aade709-5269-488d-a9e9-f6e71dec8a3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d1a10ea-74ff-472b-800b-fac8723ce6d1)(content(Whitespace\" \
         \"))))(Tile((id \
         6ff3f226-5c6c-4279-a610-555fde9b30a3)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5bd5b201-c2c0-408f-bbee-562bfed690eb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         346a514d-2404-47cc-88af-dd5e46cfd3ea)(content(Whitespace\"\\n\"))))(Tile((id \
         424e728f-dd33-4dc2-946d-7113613fa622)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f1da74d-b38c-4110-b875-011a357681c5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ec92bf2f-c411-4b7d-b306-d99c2d55bbfd)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b50fa9d6-1b12-4c6e-bfde-373593ad14a9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e99704ad-6bdd-49d2-a6c7-a17d08590973)(content(Whitespace\" \
         \"))))(Tile((id \
         0dc36141-0e01-4948-86cb-23c688fc6276)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3b30643-0275-4f21-9105-0e9331635b7a)(content(Whitespace\" \
         \"))))(Tile((id \
         17fc087f-5c53-46d5-bd76-d8a73941b32b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f53b3100-6d72-445d-bc8d-9c853cc2a458)(content(Whitespace\" \
         \"))))(Tile((id \
         0db199ac-52c7-418b-ad75-4b014c54243d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77e5507f-d82e-40ac-a00d-8b2fb4acd86b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25d377e3-9146-4e9d-a8bb-e4b9d36bf251)(content(Whitespace\" \
         \"))))(Tile((id \
         ad86b298-7cb7-4a20-90ac-ebd0d4968893)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0c6201cc-eb2f-4ed9-80ff-6e9353cba8f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         272e1a52-c2df-4b07-9652-13d3f8a0a556)(content(Whitespace\"\\n\"))))(Tile((id \
         35e48994-6318-4d1f-b8d4-0c54ecb12681)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         621044a6-75f7-44f7-8a17-b74e9b78c798)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ea44a4b-a5f6-4b66-873e-9cabae65de51)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07fc5618-c231-4676-9bc9-b4da4575a9d8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         afc8674d-34a2-4974-bcb3-34d881ff060d)(content(Whitespace\" \
         \"))))(Tile((id \
         ac35c540-c96c-4f3f-bb4b-776814197760)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22a7492c-9856-4a82-892c-ac4c86790d3f)(content(Whitespace\" \
         \"))))(Tile((id \
         b8737c58-1ed9-42ac-ab6e-83f9b4611eff)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4774bffc-89e4-4e7f-a5af-0fe4248e43ca)(content(Whitespace\" \
         \"))))(Tile((id \
         b68c6d70-5824-41ec-a866-188f042568e3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20ffad96-a8fc-491b-9308-bb2e1f9d223e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0005ef6-0afe-4bce-ab0d-e8d047262318)(content(Whitespace\" \
         \"))))(Tile((id \
         5ecf7706-e86e-4603-b9ee-ca298314d6cb)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44ed94ca-0a16-4117-9bab-25201955e0b2)(content(Whitespace\" \
         \"))))(Tile((id \
         fc632ec8-20cb-4676-87c6-369df93e5062)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74b7470c-4d40-42f9-abc7-6fe9192a84de)(content(Whitespace\" \
         \"))))(Tile((id \
         7aa079ac-61c7-446c-b394-fd9c4c2e32b4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         22cc53d1-63c0-4e9e-b1f4-9a8d222fecd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f680df3-1b3f-4286-9aec-794bfb5becda)(content(Whitespace\"\\n\"))))(Tile((id \
         9bd60b94-823a-4e2a-8e1b-228012301ff9)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c8596e9-ec24-4a06-abf6-a0c51537bbb2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2c6e0c5-cdd1-4276-8803-94a112851ccb)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         498087ca-7b12-4c7b-b0d9-9621a0fc4c97)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a41c215-0999-436d-a792-5ec581240d46)(content(Whitespace\" \
         \"))))(Tile((id \
         64ee0774-16f0-486f-b203-867255ed654f)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59f02be8-ae67-4f9b-af27-4d0c66636033)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20a28e56-93d1-47f2-82d7-62fde7efc1d6)(content(Whitespace\" \
         \"))))(Secondary((id \
         c6c29a0b-0e0e-4972-8508-6051ae831821)(content(Whitespace\" \
         \"))))(Secondary((id \
         02b0955e-b556-4ebe-9923-3e00ec4a7889)(content(Whitespace\" \
         \"))))(Secondary((id \
         101cc50c-f5ec-454d-92cd-b01ade68b04a)(content(Whitespace\" \
         \"))))(Secondary((id \
         708f8085-8973-478f-8dce-6fcd013161a2)(content(Whitespace\" \
         \"))))(Tile((id \
         635f4825-6a48-437b-9d9b-3802ad33e704)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23f7b36c-38dc-4293-b09d-39d7553b588e)(content(Whitespace\" \
         \"))))(Tile((id \
         ec6f3fef-6b29-4bb2-be3f-209ef73f261f)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24e2bd04-9603-4b62-b295-4e538fc082fa)(content(Whitespace\" \
         \"))))(Tile((id \
         ef807af1-a88e-426d-9272-aaf8cc0685d0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8a4d0e29-3c5e-4837-997d-70609b76ee5b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67af0133-e466-4ad3-8a5a-3daf94e0e7d4)(content(Whitespace\"\\n\"))))(Tile((id \
         0896666d-e128-4c48-954b-85f0b37628b5)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9229be24-81f7-45f6-b818-ed3bd0308fea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d4e632f-de48-4d8b-98d4-e49c31b0a9bb)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c0e8495-b297-4644-a942-6f348bde0228)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06c6e672-1093-4bc0-8d02-d584412a3fe5)(content(Whitespace\" \
         \"))))(Tile((id \
         ac32bfcb-b9ae-4abf-9da4-7ee1ffbe1bca)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ad6f13c-53d2-490a-8b80-414f3c203a71)(content(Whitespace\" \
         \"))))(Tile((id \
         d4cd46fa-6a9c-4dde-b4ce-c574fb5f79ef)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38230762-ac43-4de1-8b0c-ca0e50af9165)(content(Whitespace\" \
         \"))))(Tile((id \
         b177a161-fccf-4cbd-8da8-8ca43766e0b0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ef89cb2-fc87-4caa-8986-4c6103713be8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5aa7a6a1-fe88-4112-8e2d-b1d94fd47e38)(content(Whitespace\" \
         \"))))(Tile((id \
         8760bb6e-2866-460e-9c1c-e3d5d97a4b78)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         076b890d-2b8f-40e6-b36b-194f350d16b5)(content(Whitespace\" \
         \"))))(Tile((id \
         c181d771-a807-424e-be42-b4ce5103aa60)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1fb2b7b-b4a8-4a76-8042-ab8686301df6)(content(Whitespace\" \
         \"))))(Tile((id \
         7df94eac-c8d5-47ae-9c28-3e236713d472)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f47bb82d-4f2b-4fd9-be31-67a5128ea46c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0df4bd9b-ddc3-4d44-8dca-9be970cdf05b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e608349b-888a-40fc-a592-ab966a0a7a69)(content(Whitespace\"\\n\"))))(Tile((id \
         35429eb7-e2f8-4a40-8f10-c1620add3218)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3b8dfea-e997-4ef7-91d9-228d876a2a09)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3c3fdbc-b421-4a72-bea3-894695faf444)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f410e0db-db8a-453e-aa1d-c06e8b991baf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a6611f2-fabf-44e2-b7a3-9b840d568308)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a13388f1-5121-4c7c-a0ea-66c93861c0f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90aba9f7-56be-4004-8357-91220f66ca07)(content(Whitespace\" \
         \"))))(Tile((id e61ff798-3630-49d3-9962-19fc8f2503a8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9bc9a168-a6ff-4314-9277-c5d9317e8e74)(content(Whitespace\" \
         \"))))(Tile((id \
         76f179bb-8336-48fe-a0b7-845335e7d5f1)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1d6bc417-fb6a-4333-ac92-5bbd36dae430)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f54c43d-26e8-48ac-99fc-98c18a9831cf)(content(Whitespace\" \
         \"))))(Tile((id \
         e7e6f859-b7cf-43a9-87a9-a881c8e3f993)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5fba21b3-77f1-4e8f-b7c4-1396de25f86e)(content(Whitespace\" \
         \"))))(Tile((id \
         fd4cf617-e45b-4eeb-894d-26cb28ff1c68)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81495358-801a-4899-ba08-edf5d9bac7b8)(content(Whitespace\" \
         \"))))(Tile((id \
         7c1e9905-83df-4d63-a6b4-18aa38d2d262)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c345fd7a-2ebb-4060-bd81-60c712bf4ba7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e4377b39-11ec-4449-b1eb-7e2d0bf5ad3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         078b545b-98ac-4369-be04-7d3a29bac93c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e50797e2-c6ea-4987-be43-743de7f831bc)(content(Comment\"# Apply Game \
         of Life rules to a single cell #\"))))(Secondary((id \
         bf6619f5-ed3f-4664-841e-4e8d7fc1de0b)(content(Whitespace\"\\n\"))))(Tile((id \
         276a0e58-4f9c-4900-b589-cf74f474dbb9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2293b8bd-89c8-47ab-b52f-42ccab88e6c4)(content(Whitespace\" \
         \"))))(Tile((id \
         d0dc8d26-b5ff-4904-8a36-da31fc224b8b)(label(nextCellState))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         60d9a425-ae92-453a-9b4d-987a4bbc5012)(content(Whitespace\" \
         \"))))(Tile((id \
         768ba6b5-1288-4a3c-b105-f027e1ac2550)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5afe64eb-8d7f-4f8e-aa8a-14db8c882bfa)(content(Whitespace\" \
         \"))))(Tile((id \
         2ba0e7d5-c8a8-442d-865b-c8e729eb43c7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         cad586a8-a98b-482e-83da-e4264d846bde)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5bc63a3a-e8ee-463e-ba5f-17af2db60355)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         59f0b878-ac1b-4a48-b725-49bd4fecb373)(content(Whitespace\" \
         \"))))(Tile((id \
         bbc689b7-9cc6-408f-b7b0-3a72ef338368)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ca6c569c-d188-4c27-8e2d-4c4b68a0d974)(content(Whitespace\" \
         \"))))(Tile((id \
         f1e3093b-4ba8-4897-b5d1-854a3591839e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9c5972c6-7ffe-401d-a630-44dc4f68d8cb)(content(Whitespace\" \
         \"))))(Tile((id \
         e3b8a437-e691-462c-9c5e-ca1c4f6f1d27)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         95877d4f-2ce0-4b2f-a10d-04c46841e3e3)(content(Whitespace\" \
         \")))))((Secondary((id \
         efdf827e-67b2-43c5-83ef-f50b2b4e040f)(content(Whitespace\"\\n\"))))(Tile((id \
         45af2090-5c47-4496-b43f-4ca33e86787e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         459ee903-8dc7-41e9-a3d7-e940a88ef342)(content(Whitespace\" \
         \"))))(Tile((id \
         1fc9af4e-4c7a-4ec8-9cbe-4f2ac053f132)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bc3fd36f-a026-4881-ab0a-05269ac347bc)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a85f8e24-fbb0-4a98-a317-24169ee3fb6c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c43a3698-dc6c-421e-a0a3-7b46ec6c9922)(content(Whitespace\" \
         \"))))(Tile((id \
         b18d5cf6-c252-4ce3-9cf6-384ab3657d4c)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         99debf56-708b-44bc-9bb2-6a9f4ee905fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1733ecb5-a169-45f5-bf50-e138df2a5c84)(content(Whitespace\"\\n\"))))(Tile((id \
         1315ecf1-2b18-43fd-8740-f8d4dcb69b54)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2d68c075-f720-41bd-a90f-319838d12acc)(content(Whitespace\" \
         \"))))(Tile((id \
         2a291b96-4b68-4d5e-b214-d2f5a23e7fb6)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc0b697c-a9ac-4773-90a8-7b531729b180)(content(Whitespace\"\\n\"))))(Tile((id \
         7ee599ee-8bcc-4a6a-9b5c-15b7f7f8b87a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         919ea613-8ef2-43ca-ae78-451253cb1d3b)(content(Whitespace\" \
         \"))))(Tile((id \
         a0dc55ad-9315-4c1a-b67f-4a5ed6ee677f)(label(Alive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6bc8d42c-93f9-457e-a61e-3113f2c7e4cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         17da138e-35a4-4490-b5b1-59fe0fc05cc2)(content(Whitespace\"\\n\"))))(Tile((id \
         b6968f5e-5b1a-49bc-9276-9d55f35562e5)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eef02ea4-2865-4d2f-af5a-4ae04ee01ca9)(content(Whitespace\" \
         \"))))(Tile((id \
         1bceecac-7e44-4ce6-9347-7f0c0f2d400e)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7accdb72-e5a1-443c-a425-18f65e16d9f6)(content(Whitespace\" \
         \"))))(Tile((id \
         8107a5d4-fb3b-4597-91dd-7c0b0cd1f8ee)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4129f493-1e29-4cd4-8513-3e69eff4ee61)(content(Whitespace\" \
         \"))))(Tile((id \
         d0103405-cdea-44f9-bf9d-fcbe0b057a2e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95500518-6328-426b-8eee-5609b23b3499)(content(Whitespace\" \
         \"))))(Tile((id \
         7fa41164-db68-4821-9abc-b7f98f2d40af)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a37e684-fa8b-44e8-8617-ae1009319d91)(content(Whitespace\" \
         \"))))(Tile((id \
         302ebf75-cb68-4ed7-8b30-c67f053a0cbc)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9ef6f1a3-5a08-410e-b9c7-f7c896f45c22)(content(Whitespace\" \
         \"))))(Tile((id \
         c65ad057-c00d-4a3f-8458-7f2af72035de)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69eb0292-26d9-400d-addf-df5dac61146b)(content(Whitespace\" \
         \"))))(Tile((id \
         9d4853c1-5575-4f6f-8b4d-dbcba0458756)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82012c21-a7e1-4c36-9331-60210f217652)(content(Whitespace\"\\n\")))))((Secondary((id \
         e3215eaf-8229-4a9e-99e8-3b881e537a00)(content(Whitespace\" \
         \"))))(Tile((id \
         1c14cf1a-c967-46de-b7d9-3e07f510c03e)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         856106e2-dd40-4e3e-a6da-0eb60daee77f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         83f42ccd-0907-44a4-a9de-622f017c435c)(content(Whitespace\" \
         \"))))(Tile((id \
         16046d15-3331-403b-b50a-47528d95d60e)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d468851f-c119-4210-8760-fb29daea6c94)(content(Whitespace\"\\n\"))))(Tile((id \
         15acf62d-6568-4a64-b8c7-07346e36daab)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c3317056-af80-4054-b130-dfd396dc3522)(content(Whitespace\" \
         \"))))(Tile((id \
         ca4527a8-37bc-4998-b32a-7dd994b1f1db)(label(Dead))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af0882f9-288a-4f68-8e6f-5bc8c11f98d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         61e96922-6a0d-4f44-9b67-926d2523593f)(content(Whitespace\"\\n\"))))(Tile((id \
         c1e5b881-ddda-4826-91e8-12f32524e13e)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         de407ec0-dab1-45a5-ab88-8050903f2714)(content(Whitespace\" \
         \"))))(Tile((id \
         46f906f3-f659-4434-8804-26eae49c0a8e)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1bb9d386-52ef-440c-b009-fcbb874efa81)(content(Whitespace\" \
         \"))))(Tile((id \
         9fa60db0-cd89-4b01-9afc-955422f3e4e6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         573a7642-e71c-4d99-a3ba-37cb738fe195)(content(Whitespace\" \
         \"))))(Tile((id \
         7ecc1d99-02b8-4ad1-bae1-ad1ccad42436)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5675ac5d-932b-4667-8e92-7097732c808e)(content(Whitespace\"\\n\")))))((Secondary((id \
         f7e09260-938e-40e9-b29c-89ea1e8e89bf)(content(Whitespace\" \
         \"))))(Tile((id \
         6d1f7895-0385-4a22-9da9-fa2eb040fcaf)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91b0ff20-d084-4ccd-929a-be8f613d0212)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d25be2d8-c01a-4f16-9945-5c686194740a)(content(Whitespace\" \
         \"))))(Tile((id \
         f23ef010-76e9-48f1-85f5-f1dd9eed013e)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c96582a3-6d4d-4ee7-a692-b6ad9ae6e613)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3af4b97d-7d18-4aee-9e17-f297b4e01397)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         075daf53-d4e0-4806-9533-ec6adeba7246)(content(Whitespace\"\\n\"))))(Secondary((id \
         58c39275-ffa6-4780-8997-306b81ec3b26)(content(Whitespace\"\\n\"))))(Secondary((id \
         036d324f-b85b-4e89-9047-cf84c2ce429f)(content(Comment\"# Step the \
         entire grid (simultaneous update) #\"))))(Secondary((id \
         b4dfd5b4-472c-4339-9791-89d625d71563)(content(Whitespace\"\\n\"))))(Tile((id \
         6ecdabd0-d1a6-4da1-87c3-ed86019c42ac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d0131b6b-2ccd-4361-b4f6-33d8a59c290a)(content(Whitespace\" \
         \"))))(Tile((id \
         56ecdda6-2dea-41c5-81f9-69131c28c2cb)(label(step))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fec48d8d-d6e7-4d0a-9436-4158982af54c)(content(Whitespace\" \
         \"))))(Tile((id \
         0a1a7c55-8350-4740-bc03-4cb88da81ed5)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         917936d1-d6bd-42f9-8d35-389a887c6729)(content(Whitespace\" \
         \"))))(Tile((id \
         499e5d9c-8606-457b-8e39-5b2b6017e65d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8e460460-9757-4544-889b-86fa7083eefc)(content(Whitespace\" \
         \"))))(Tile((id \
         53e6bc2f-4281-4551-b537-b9b77760e5f6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e24d1a98-1d5c-4637-aef7-ae99ca4bda26)(content(Whitespace\" \
         \"))))(Tile((id \
         7c7f3191-2757-4f63-bff5-003de221dba2)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         49577a65-ae5c-4015-98a4-b50e9348f3a1)(content(Whitespace\" \
         \")))))((Secondary((id \
         279494e9-2b34-400e-af80-86bb05968a66)(content(Whitespace\"\\n\"))))(Tile((id \
         a5a958bf-5522-449d-96f9-ab009bfd26fc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         27742e4c-bf80-4fb1-ab94-7d1114ecc35f)(content(Whitespace\" \
         \"))))(Tile((id \
         bc91da10-1c78-43e7-b70a-6d8d9a9e4b5e)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0d610153-35c3-4be6-9f1f-c0faf8bf4e91)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         820cd759-3f78-4746-a70f-5d2ce23b3953)(content(Whitespace\"\\n\"))))(Tile((id \
         f0a09638-9e02-401c-a23e-ee3fa2174a41)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2d90f8d8-1617-4687-99c9-8f8b14636024)(content(Whitespace\" \
         \"))))(Tile((id \
         23a68fa3-f7de-4417-bd3b-82230cd01751)(label(newCells))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         87ca9290-4293-4242-9771-82294e37df08)(content(Whitespace\" \
         \")))))((Secondary((id \
         c236f79c-4f06-4a36-8044-6cc36c31739b)(content(Whitespace\" \
         \"))))(Tile((id \
         558d21c2-d8fd-44d6-9546-02b76c76bb32)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ea2fa42-1114-4e36-a9b7-ea9b60eab3e8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ec3f9d8-095e-43bd-9d1a-33dbe9eaf0b9)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         446edd1c-d2af-4d31-9e40-70d2e409cfec)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0ee9c2d7-c87e-4a4e-9960-6fc78f173fe0)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99cc7bb7-0fc2-4951-84e2-f50c8e176e07)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a3f78eeb-6a80-476f-a516-fe92e7403483)(content(Whitespace\" \
         \"))))(Tile((id c6dcfcd7-bc59-481b-b641-3df850d165fc)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7fcd7fb5-84a0-465a-87ab-cb4d10455907)(content(Whitespace\" \
         \"))))(Tile((id \
         bd5c63f9-3015-465a-9823-0c22529026ee)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9bb4a952-832b-44e2-b04d-b40d5f90861e)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4f1cbc13-af0b-4407-8e5f-1cc9e84c5c41)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1227987f-ecb1-4951-8fa1-0839e9fb0b2d)(content(Whitespace\" \
         \"))))(Tile((id \
         80dbea1a-c9e7-4c93-b72c-3dd4f728c182)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         801eeb52-4e77-49a3-b562-e8ff04d23963)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a838dcf7-69c6-45de-9e8d-778af223bcb6)(content(Whitespace\"\\n\"))))(Tile((id \
         710fd468-656e-42d3-a2a9-a117c3d6ee25)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07741565-2377-4038-805d-b7736265590b)(content(Whitespace\" \
         \"))))(Tile((id \
         053bc535-03e6-4966-9495-f681d3b875a3)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a2613fff-4299-4a8f-828e-9c2132117f07)(content(Whitespace\" \
         \")))))((Secondary((id \
         f8c08a75-4892-4807-baec-096fdba70f04)(content(Whitespace\" \
         \"))))(Tile((id \
         359a302c-fc94-4c8a-a6a5-e41575366f56)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3583a468-2749-4a47-9efa-a52a56a4e1ab)(content(Whitespace\" \
         \"))))(Tile((id \
         263cfd7a-ce2e-4944-91dd-598aa0034ef6)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         820aee8a-8da3-4bfc-844e-7b3dfa2e8cc0)(content(Whitespace\" \
         \"))))(Tile((id \
         a68e2680-e7f4-4010-a5b9-69779098cf53)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e9f7895c-617d-48bc-b54e-fa2617035fe4)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47ced800-c64b-4ffd-a4a0-1f10502f2b85)(content(Whitespace\" \
         \"))))(Tile((id \
         9d99a984-fd72-448f-8c02-a30b86383e03)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2957638a-ef68-4375-9d15-5146b904eaab)(content(Whitespace\" \
         \"))))(Tile((id \
         db16b7d9-dcc2-473f-bb22-af02ae07655f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9eb32f58-6ee0-4a39-ae5b-dd992e52bea4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a8a657d7-2488-42c7-b147-3e0fb5728698)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4ed941e4-42bf-4924-a0cb-776d634ae01c)(content(Whitespace\" \
         \"))))(Tile((id \
         fdd83d1a-c634-4841-b408-e4577ef3d4ec)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         509b7f1f-5014-4660-a814-ae80793daea1)(content(Whitespace\" \
         \"))))(Tile((id \
         d57fe523-53a5-463f-882f-39d5b8239466)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         123317d4-7f9a-46cf-a5d4-52770ed5aa4c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6ca975a1-cc26-48fb-9808-63c9c99b2ae2)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d56f23b-3d45-476c-ba5b-fb7817395c5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         39e6f8b0-6a46-47f4-b1d0-efc2d20a4225)(content(Whitespace\"\\n\"))))(Tile((id \
         fa02a8fe-6ee9-478b-b6b1-47c9f050fa96)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6972955b-e117-4ebb-bd41-3d3fe09b4607)(content(Whitespace\" \
         \"))))(Tile((id \
         42ac290c-0bb4-46ee-a1c4-2fc4c35aaa23)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2750aa0d-d8be-4469-b768-60340b5b3ad8)(content(Whitespace\" \
         \")))))((Secondary((id \
         fd7a75d5-88c8-42cd-8ae6-e561aad9b01c)(content(Whitespace\" \
         \"))))(Tile((id \
         dd915f63-e650-4dad-89ff-8addc7481e4c)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e46bd809-0168-42e3-ab48-63be82e5bcb1)(content(Whitespace\" \
         \"))))(Tile((id \
         c8cf048b-a369-423a-9a6b-34882af971e3)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97aa3471-63ea-48ab-a100-0f12a9182a61)(content(Whitespace\" \
         \"))))(Tile((id \
         021bf511-a395-4f32-8e8e-faf193f1533c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         546a8bcb-86aa-4792-8c6e-0ec58e665fbd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0b49e289-eeba-4631-91b9-a8a6699e8fe0)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f5ee9fd5-dad1-4aea-975f-57c6eee7d0c3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4fbc13cb-93f0-47f3-904f-6c40e95b67cf)(content(Whitespace\"\\n\"))))(Tile((id \
         ea985bf7-9e13-42c8-94a6-91ce8840e678)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         82d491b1-1905-4f7a-be47-34b75c01b1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         da98ea28-348c-42d4-bd20-a90bb6319032)(label(current))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         09f8f748-affc-4682-8bf4-27410f78d773)(content(Whitespace\" \
         \")))))((Secondary((id \
         aaad054b-1ff8-4be5-9825-b51237199e9f)(content(Whitespace\" \
         \"))))(Tile((id \
         2cb4124f-71a9-4ebc-b8bf-d3c1bc581432)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9724d462-7bc7-4109-aaed-a6e7a0db585a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4090d8c8-2cde-4538-ab99-c8ea35ed2f0a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6fb03d8-ca2e-45bb-9c89-8350c0025c0a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6639e802-0ec0-4b1c-9074-4bfbe0cdcf9f)(content(Whitespace\" \
         \"))))(Tile((id \
         0b05df12-cfe0-474c-9b32-e0d33ca00309)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccabf08d-ee46-4955-9bd7-b658d4c5d4d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4288aa33-de9e-4316-bd09-230d588c5aa1)(content(Whitespace\" \
         \"))))(Tile((id \
         25ef21a1-1ce2-45ae-84d1-69404c0d4e97)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e452c480-cfca-4856-8b33-ff3b632ce516)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         51baf3f3-fdde-47fe-ade1-cb1e9b6ff909)(content(Whitespace\"\\n\"))))(Tile((id \
         312e6959-855b-4e2b-98ec-333d8eeb93a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07f0180f-3a93-4f96-8206-ca381f701aa2)(content(Whitespace\" \
         \"))))(Tile((id \
         77bc976c-120b-4ddd-81a4-f787c40b350b)(label(neighbors))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         13bf5b8d-7491-45b4-96cb-6b31acc6f258)(content(Whitespace\" \
         \")))))((Secondary((id \
         f0ac521f-1956-404a-a197-b48b90c7aa51)(content(Whitespace\" \
         \"))))(Tile((id \
         10a432bd-5cac-471c-8f09-35bdbf2aaf79)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b4b4bbb6-b16b-40aa-b22a-f37f713d3f7a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c51ca6f-d3d1-452c-9129-078dff1fe5f2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         001fe149-a4a2-40bb-b871-ccdd33683d12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fac5138f-99f3-4d20-bb11-2dc03222d2ee)(content(Whitespace\" \
         \"))))(Tile((id \
         b072f0b1-a845-4b57-b408-b48fa50ec71a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fe2d8cc-3f5f-4b43-af33-1e989aa4892b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         314f7670-e383-49cc-b31e-d36b1e7ddd3b)(content(Whitespace\" \
         \"))))(Tile((id \
         c59b3919-e53e-48b8-8be6-8b289b553075)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8bdf6522-65e4-4331-9006-b34c03cf1e2d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         316af4b4-ce50-4fc6-8b9c-de21518a734a)(content(Whitespace\"\\n\"))))(Tile((id \
         0916fe1a-203a-4d95-98b4-9c4af155d011)(label(nextCellState))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2194832e-7d1b-459e-b296-4dc3e02468ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5dedc92-15e8-4fae-8af4-343a0c9520ec)(label(current))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a50c9c1-528f-42f8-ab0f-8fb6f03b1e8a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91cb7661-a0a5-4ac2-9b09-86afda559fa3)(content(Whitespace\" \
         \"))))(Tile((id \
         c2061282-dc0e-4974-8cca-052b3d67a729)(label(neighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4ca285a4-24d7-45f1-988f-68f1503260a3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ff7e0767-be80-478f-8ca7-e3bd8e24028e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f4b1f917-d227-4c83-abfd-51e0a0c91413)(content(Whitespace\"\\n\"))))(Tile((id \
         30e78340-8810-4794-8c62-4aae234447d2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3720b81f-c320-4bb5-9df3-ea838448a2f6)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3edd9343-7314-4e5a-a4cf-61ad08ada67b)(content(Whitespace\" \
         \"))))(Tile((id \
         56c66363-7348-42e1-b9e9-c4e6295bf6d6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         823948ce-1494-47c6-aae0-2d3da3bcbff4)(content(Whitespace\" \
         \"))))(Tile((id \
         46cab7e9-6014-4fcd-a40f-63380b6cad55)(label(newCells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         abf9f4f8-8034-49b8-ac7e-bdb3eb6ba743)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         886a7621-dd9c-4dc7-b666-7c2b14f1efd2)(content(Whitespace\" \
         \"))))(Tile((id \
         f0af46dc-e1db-4717-a204-f8fbc51a65a7)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5757553a-26c3-40a7-9027-f522fb745387)(content(Whitespace\" \
         \"))))(Tile((id \
         253d8a43-bfea-4ab8-a716-bbd294058eed)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16907804-35f6-4e12-9f84-e87ed04bbd88)(content(Whitespace\" \
         \"))))(Tile((id \
         3d086a2c-acac-4522-a5c1-3ee3bc973f70)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         055eb882-cb94-49c2-9ae5-c6047149dd9b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f01f6ee5-41b5-418c-abeb-5a68270e4724)(label(width))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3adb70b7-fe53-4d29-8a40-14561337814a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64428899-7d12-4eaf-9200-3ff806403938)(content(Whitespace\" \
         \"))))(Tile((id \
         5cfaef8b-3932-4f0f-9483-25bac8e1eb13)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2d549179-5225-4bcd-a204-920c5bef32fb)(content(Whitespace\" \
         \"))))(Tile((id \
         ecc4d719-3951-4d29-80bb-ccc3619e2835)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a05e2edf-b3ac-42f3-886b-2e5618999232)(content(Whitespace\" \
         \"))))(Tile((id \
         e4f41f53-75f0-478d-94e3-925055707550)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19c0bbb1-7e3d-4689-af4c-abd12a466a2f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0686a29d-a96e-4f6a-a8e5-09cc2d946e40)(label(height))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         67a8e78f-3bb5-4fac-a57d-3353559697c1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5e14b24d-9562-4a3b-a06e-32c24487cc88)(content(Whitespace\"\\n\"))))(Secondary((id \
         315c1f7f-d43e-4ae4-99ba-7c56191c4bc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         46627579-907c-465a-b08b-0b489b74b447)(content(Comment\"# Run n steps \
         #\"))))(Secondary((id \
         d20da68b-fdc1-4046-908a-c5be3691419d)(content(Whitespace\"\\n\"))))(Tile((id \
         625ef52a-d16d-46b6-a08f-01ff09b545b2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c43ffe3b-1061-448c-a378-76e89ead149e)(content(Whitespace\" \
         \"))))(Tile((id \
         1cea13a9-2429-4403-b949-93c50d1b3d31)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         115144c6-1b31-48a8-b589-474ca6119d4a)(content(Whitespace\" \
         \"))))(Tile((id \
         c4efe7e9-9bc4-498f-8933-1701ff984917)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         10d52792-b468-4976-b529-58936f944200)(content(Whitespace\" \
         \"))))(Tile((id \
         58d4c823-c8e7-41b4-871e-af79c7c6a850)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         449c53ec-d7cd-4644-8cd8-97e4ec5db67d)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ab4c3d9a-454d-4d73-9209-4ebedd0dd690)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         882be1d6-d0f1-4f37-a1c6-e43b1e7c322d)(content(Whitespace\" \
         \"))))(Tile((id \
         c42f2e72-917f-4831-980f-b20b86723ec7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6d562053-c17a-42c3-aaed-5dddded31165)(content(Whitespace\" \
         \"))))(Tile((id \
         831a7c11-6395-4020-8c1a-b4c4911a73f4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e47dc19f-5d73-499f-8ed5-ce3c517d1c25)(content(Whitespace\" \
         \"))))(Tile((id \
         880d6a3b-9ab1-43b7-8450-bbcc9070f761)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ce65be3b-4b11-4804-b9d8-4dcc7cb4d031)(content(Whitespace\" \
         \")))))((Secondary((id \
         02343f9b-7c20-4ece-bde3-903eee2c7529)(content(Whitespace\"\\n\"))))(Tile((id \
         7adcec9b-3e89-41ca-bf8e-7ef45b8ede85)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         97ac39d8-5e68-4a56-bbc6-dcc6103b1bfa)(content(Whitespace\" \
         \"))))(Tile((id \
         786c8df9-142b-4177-98a2-05bdb85e742e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b806c8c1-43e6-47ba-975c-ed4df0ad1ed9)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         96ffc24b-19c7-4740-ba5f-ac4c7f4da16d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0f2f87ad-4729-4efd-886a-b272c931d898)(content(Whitespace\" \
         \"))))(Tile((id \
         073e81ea-d1c1-4b57-9a6f-9d581d58a7bb)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         489ea14c-e9f5-4c1a-bb7d-9b19b52d59ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91adaf3f-e80d-4502-b4f8-eb6aa83c8ee2)(content(Whitespace\"\\n\"))))(Tile((id \
         91e1d1f8-a111-4c57-9656-e38ba28cf3e9)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         43c6a4d6-0cae-42bf-bce8-8e2b8de67d68)(content(Whitespace\" \
         \"))))(Tile((id \
         7482e2ec-838e-4416-ace8-5c651d84816b)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4194d9ee-b9b2-4d72-8106-203607c3e41a)(content(Whitespace\" \
         \"))))(Tile((id \
         cbe3e4f9-eea9-499d-8c11-259b993d18f2)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a08b050f-1e09-4f92-9a0e-40cfd3254261)(content(Whitespace\" \
         \"))))(Tile((id \
         50cd75b8-7d50-4c2e-a363-89a0a4f15df2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d0e7957-b09e-4c81-b26c-b38e0ae414e5)(content(Whitespace\" \
         \")))))((Secondary((id \
         0b081bf8-f8cb-44de-86ac-292343a8846f)(content(Whitespace\" \
         \"))))(Tile((id \
         d1c338b9-54bf-455a-ad61-a1acf95cf2bb)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78c351f0-7433-4d08-ae06-e6d3458f0e4f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         653936b2-283d-4f3e-aad5-2fce32f07a17)(content(Whitespace\" \
         \"))))(Tile((id \
         04e6ef21-740a-4a67-98b5-f5d6801cac38)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f309ccb5-0d80-4940-8ea6-c99a8d88d704)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         680e1efa-4612-47dd-87b8-4e93fa93ee75)(label(range))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3cc6d32-db21-4883-8297-4025083a764f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3772236-22c8-4eb3-99e7-bbe00a972300)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22ec8153-9aa2-45e0-8d9e-a2ef2c00d688)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25bc0aa0-44ec-4519-9af7-b1435a42a349)(content(Whitespace\" \
         \"))))(Tile((id \
         547d1933-0c80-414a-bec0-bcd263feda30)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8997cf82-8852-4c62-8870-1fac2efd1ecf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24a7411b-0ebc-44a1-a7c2-828258e71128)(content(Whitespace\" \
         \"))))(Tile((id f3628131-dd69-4328-a196-ecbfa66b40ed)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a46b941c-3e66-467f-a4e0-0dae79eee9b7)(content(Whitespace\" \
         \"))))(Tile((id \
         0d187a7f-7a66-4318-be70-42e0b79269e6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d3754cfd-fe04-4e5f-a7c6-c927acb8f571)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         58ce21af-d4d0-4955-8cd6-ef50ef8625b6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ae015aeb-5a1b-45ef-8534-ea837b4e02da)(content(Whitespace\" \
         \"))))(Tile((id \
         2e26af86-676c-4689-b167-edc6d46f7366)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d4a30e02-ca3d-4fd0-aa58-5450f2cca402)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8f77a488-342c-481d-bd5c-a64a15715f7a)(content(Whitespace\" \
         \"))))(Tile((id \
         4e626a29-5bbf-4194-819c-1a66e48db653)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a51a5be-e473-47c5-93ca-a668a5b4331e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df986dd7-c3e7-4076-8f84-f5c1c8e57462)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1946686b-65a8-4270-b881-c73375b4cd83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e1d55c5-d8bf-4fb9-8b82-f63f04e100bb)(content(Whitespace\" \
         \"))))(Tile((id \
         3d5f7975-f8ba-4f5c-bb8b-41a2af4bac2e)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5e84ab2-9e6f-4d73-af9d-b3e588a01774)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         efa5d15b-5c93-4ddc-8494-4563dbea32fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab12ef07-6e4a-4bc1-82da-5582bed71128)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d4c0cf9-07a6-4006-8be1-05e6650a1d24)(content(Comment\"# Helper: set \
         multiple cells alive #\"))))(Secondary((id \
         dc8e166c-73a1-47a6-a62a-43b5b1071031)(content(Whitespace\"\\n\"))))(Tile((id \
         ef8106ae-c916-4a4a-ae92-81d8a9251ccb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e4f3e5cc-a85a-472e-bbe0-477303230fd6)(content(Whitespace\" \
         \"))))(Tile((id \
         86d117ac-51cc-4181-95d2-20f90cdd5f3c)(label(setAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         be57b972-786c-4688-9e78-4a88654319ad)(content(Whitespace\" \
         \"))))(Tile((id \
         79268b32-ad1c-4cf4-afef-97b979443ec9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4e545d0e-0aab-4914-aa20-a4e9bf11d51c)(content(Whitespace\" \
         \"))))(Tile((id \
         80279839-3b91-4893-ac88-02817cc95430)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         cbb6a407-2eb8-467d-a248-f6a1a7640cfb)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0e347382-8d96-4c6e-b706-1f34e1d9eece)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         90ddbb83-f7ff-4b4d-8250-1058750e7964)(content(Whitespace\" \
         \"))))(Tile((id 41de5594-2d4b-4f90-b4b0-0ec45e87af4c)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b1d40320-da47-4b78-8cdf-60a6d0336be3)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9a26e842-86ed-4821-9631-a33674df0b10)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e7876c87-f5a8-41e3-becb-88c5cdea9d71)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ce2004b-b402-4ee2-a44f-8b293c359446)(content(Whitespace\" \
         \"))))(Tile((id \
         3afb89c0-23b7-4a2e-9886-da90dbd848c9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         0606426b-3b31-47d7-b93f-591615a51cc5)(content(Whitespace\" \
         \"))))(Tile((id \
         64aeeead-e403-4314-b1b7-c743afd2d56a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7f9d8065-d05c-44b2-8dbe-b861a1a66bf3)(content(Whitespace\" \
         \"))))(Tile((id \
         621aea36-c323-4008-81fb-c61f1f2aa4cd)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         91f1a8fc-cb2c-45f1-9168-ec76e93fbad9)(content(Whitespace\" \
         \")))))((Secondary((id \
         cc5285df-5fbd-4b9c-b632-b1cbd8a0fb60)(content(Whitespace\"\\n\"))))(Tile((id \
         66ba2843-e7e8-4204-a1e4-9b51a74ebb87)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         afa7f03c-5597-4e04-8a6d-52d57eba6797)(content(Whitespace\" \
         \"))))(Tile((id \
         83b84a6c-cd90-44dc-8ff5-e1934578cb5b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a199b0ea-4b30-4131-80bc-abe33aeb77bc)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         12e27395-57c8-4278-bcef-b2e947bde685)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         118ba801-5890-4457-ace6-65752837e11f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a44829f-9a12-4ea3-a8ef-a148172f0efd)(label(coords))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         152b1d06-8cb7-41c9-9c02-902a6caf13ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91c6c77b-f751-4ce9-991f-85bffd7d21d8)(content(Whitespace\"\\n\"))))(Tile((id \
         e0a007bc-bed1-4680-8859-bbcf598d1081)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5543f9a8-e204-42c0-b67c-dc89aeb96b86)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6913959-4327-4ff9-a67a-2703042cb9ac)(label(coords))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f0b6957-99e5-4fc2-8bbc-da4f0d044f0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7debb87e-76a7-471b-8718-715d816af245)(content(Whitespace\" \
         \"))))(Tile((id aacd8c9d-2a07-4154-8474-e241d59b9282)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c25c6e9a-43a5-459b-8da2-1e62611ef7db)(content(Whitespace\" \
         \"))))(Tile((id \
         67edd0b5-e99e-43ff-8e57-2448cc82d24e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         20cf1e26-72b5-4a63-b400-fa42e5d189d1)(label(grid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d803e302-4815-4b45-bc20-213bdbb35751)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b05340b7-432b-4c7a-add2-04468e4fe625)(content(Whitespace\" \
         \"))))(Tile((id \
         44db47a7-1a9c-43f8-a9c3-a5a83503dd16)(label(xy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         683fa1b0-823d-4214-bb49-53027f4922f5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         163297f2-004d-4fd9-98c0-f8a9968dfc88)(content(Whitespace\"\\n\"))))(Tile((id \
         3ff9b72e-8b41-4cb5-9070-31456d431519)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08bdd9d2-04e0-4549-8983-7dbe423f4933)(content(Whitespace\" \
         \"))))(Tile((id \
         4a21e65a-4f49-4715-91e0-d469ae7ba0f4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6fc0151d-dc23-42b6-9edc-020ac844949f)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d8d6e1ac-6623-4072-994c-6ef26610b669)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c99c8e45-4051-4a72-bf40-00439bd3c081)(content(Whitespace\" \
         \"))))(Tile((id \
         3513147f-5e86-4324-b197-ff309077e01b)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3b3487d6-2ecf-4ff2-b849-3621716628d3)(content(Whitespace\" \
         \")))))((Secondary((id \
         28d25539-e463-4cad-89b0-712f1cb78fde)(content(Whitespace\" \
         \"))))(Tile((id \
         90cb5eba-8697-4d4e-a07e-c5388036a15e)(label(xy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         93f63c07-fc62-4ef8-9eae-dfe4efc39277)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         436e502a-2bb0-452b-a72c-d14a714c8443)(content(Whitespace\"\\n\"))))(Tile((id \
         950eb064-daf9-4c7b-96e6-683e476f925e)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb07fb7b-3e81-4b11-b875-8bddf44d6ffc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ba770d71-277f-4b7a-9430-bc8725f6c035)(label(grid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4152d544-0749-4023-885c-ff69bfd21949)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4c9960e-7c8d-4a91-81c5-6da141807c70)(content(Whitespace\" \
         \"))))(Tile((id \
         8e30489e-9131-4be4-8f3a-a83a844ee410)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f5226c9-a311-4310-bd6b-0fd55835fd0e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78a679c0-a8a2-4054-8628-69869d94f984)(content(Whitespace\" \
         \"))))(Tile((id \
         8a5180a1-d328-4e9e-aa83-9867e583728a)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1371fa32-3682-4111-bc63-0238a4d12cdd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fa6d088-89b5-472c-8cef-e5b6e7eebf68)(content(Whitespace\" \
         \"))))(Tile((id \
         4836f262-c6ab-408f-aaf6-2c52750919c7)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9e75379c-d88c-48a5-a9de-3fc68bd4ede6)(content(Whitespace\"\\n\"))))(Tile((id \
         ce8b4597-6513-4d6f-b2fc-6544200f13f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ae8a0ec-6514-4ce6-82ca-e53c91b37611)(content(Whitespace\" \
         \"))))(Tile((id \
         9c58c5e9-d77a-406b-94d6-50b18f215fe9)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         90249606-27bb-4c3d-91c1-29e57acd0b94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f01296e8-3b9a-4371-a73b-5c96682c7a24)(content(Whitespace\"\\n\"))))(Secondary((id \
         24fbf452-6dfc-4ff6-a6e4-7984b2a5fca8)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf2c32aa-dad1-489b-94a5-904d1be50fc5)(content(Comment\"# Count total \
         alive cells #\"))))(Secondary((id \
         353a2806-411e-45c2-86f0-5e7b36ddc920)(content(Whitespace\"\\n\"))))(Tile((id \
         d2cf0d74-5f9f-4ba6-af2e-32a3aa3e8202)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05364554-c279-46bd-a97d-15162523f804)(content(Whitespace\" \
         \"))))(Tile((id \
         2ae3ca4d-9209-42d9-8068-51bca5a2ae20)(label(countAlive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8ebffa85-a6e1-499a-bd00-903746ecb470)(content(Whitespace\" \
         \"))))(Tile((id \
         d6f0c2ae-d032-4cdc-b30b-c2205c0862f8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         835c12f6-4c06-41fd-a09f-ac9f15cd84d7)(content(Whitespace\" \
         \"))))(Tile((id \
         257b37a4-34bf-4b76-be54-02f74bb39f37)(label(Grid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e9050912-6e1a-42b7-a895-397f5032c79b)(content(Whitespace\" \
         \"))))(Tile((id \
         01df752a-fc67-4bfa-8ed6-54dbadc82711)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9ca82222-303e-4078-a5f3-c0767ceaa896)(content(Whitespace\" \
         \"))))(Tile((id \
         40f68ded-f482-456d-9ccc-39301c2ea7aa)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         71fc036e-7791-4e64-b82a-841a16f874c6)(content(Whitespace\" \
         \")))))((Secondary((id \
         7ab08c8c-25ff-4375-aaf9-d7066e147d98)(content(Whitespace\"\\n\"))))(Tile((id \
         a0f76d5b-9805-4d13-9c5a-99863666d508)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         484e1949-6f02-45a8-9d8b-4c7e3c4950e2)(content(Whitespace\" \
         \"))))(Tile((id \
         8aee3ab7-6685-4bbc-8302-993c3df26839)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00d03a25-c08b-4cc3-8bc9-f8ffd039f2b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2dbdd35c-5ee9-438b-b752-12eb02db04d2)(content(Whitespace\"\\n\"))))(Tile((id \
         aade6a56-762b-46bb-9948-fa5f351bf6ce)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19cea246-3720-40f3-9e5b-a4b36ff506ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f71a7777-0633-461f-a78e-1a68a20e589b)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1358878c-59b8-413f-a5a1-787508b8722c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5050e877-186f-4a07-89a6-713b0ac08867)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f6016fe-fcb2-4f5e-aeb7-b987ae33bdad)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         44674065-b758-4b21-93e2-c9772203f37a)(label(cells))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e0bda69-5ac2-4daa-b338-e4116363439e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         540b0476-f3a6-4bd6-898e-811203a1aeeb)(content(Whitespace\" \
         \"))))(Tile((id e8980a12-7289-4edf-8d82-827b4746586f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         40fb4c71-2fbb-44c7-bc96-2b7434275bde)(content(Whitespace\" \
         \"))))(Tile((id \
         4a2c8822-f2cf-4cf3-845f-d03afef2aadd)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a1c6cb11-a8f0-4be8-b7bd-0ea882fd1bc2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bc91679a-94fe-4f80-8741-0fd5fe756db7)(content(Whitespace\" \
         \"))))(Tile((id \
         cf84b48b-7f62-4e34-898c-2bab21eb9183)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36d66a83-33d9-409a-95e4-ee9c202eecc5)(content(Whitespace\" \
         \"))))(Tile((id \
         32ed6f5e-fecf-4acd-b9e8-474ceda107b4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c8add0f-6e2d-4857-9393-839a655e6c25)(content(Whitespace\" \
         \"))))(Tile((id \
         3b737303-98e4-4d7b-9acb-1ab221d3e6b9)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fc5a3add-a29c-4c3d-adf1-108f8c5b3d74)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c8019054-a2ec-4bb6-9dab-da7bcd950c99)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fd0a42b-3a96-4f9d-8bfb-30369164e4e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         d85d769c-f5fd-4162-bb58-5a624c3b7b23)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         ce66d201-85f6-4270-95f3-6e88643e1ea1)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa8971d7-bd6e-41f7-bc45-228eec9befa8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6cacc46-f688-40bb-a71b-66e8eae68b73)(content(Comment\"# Basic grid \
         operations #\"))))(Secondary((id \
         39cfc952-4d90-4df6-b35c-d8ef31827315)(content(Whitespace\"\\n\"))))(Tile((id \
         9712d0aa-492d-4215-8a2c-e8bf7ec1cc90)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29433d4b-1060-41b2-9454-6876c555f5bc)(content(Whitespace\" \
         \"))))(Tile((id \
         17b384c1-eff6-4c00-9554-32e1aebc2f59)(label(\"\\\"empty grid has all \
         dead cells\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         059bd292-90f6-4de2-bea7-9b5db5e19382)(content(Whitespace\"\\n\")))))((Secondary((id \
         4b51d7cf-c10f-4793-b1ff-ee43cea2992d)(content(Whitespace\"\\n\"))))(Tile((id \
         03f0f804-c8d4-48eb-b6b8-a9dd95b675e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc9e5124-20f3-412f-bae6-c261013d9d2a)(content(Whitespace\" \
         \"))))(Tile((id \
         9c1f99c2-681d-4b61-8241-cb2f37e3f454)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dfcd363c-18e4-4eea-b4c5-d579fbc3972a)(content(Whitespace\" \
         \")))))((Secondary((id \
         dc5ab34a-0af0-4482-8b7c-fcf8154aced6)(content(Whitespace\" \
         \"))))(Tile((id \
         bbff6a86-7270-4fe7-94ae-8f9101f42b5c)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf2d3930-7c69-45d7-867a-cbc1a9655458)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         217be9d7-98ad-4b6c-995c-eb5794679509)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66bff0ed-988a-44ef-ac5b-8877c3566865)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60736cf5-7796-4515-86d4-53182287df2f)(content(Whitespace\" \
         \"))))(Tile((id \
         a05d08ec-a083-4fa3-8ebe-31dd5e48128f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb6c1bc8-f4ec-47f0-a043-0a18ab2d7ae9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1bfb32d0-0c85-452b-bc9f-e673a98f209f)(content(Whitespace\"\\n\"))))(Tile((id \
         5c589e34-88ae-4139-b21f-1f75bdc70a5a)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39beda7c-f13a-412e-8d8a-4abdd8dc9781)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         65454d7a-64c3-4150-a7bb-8cdca28fd62c)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac181de9-2298-4dd3-92ef-54f7a5815de8)(content(Whitespace\" \
         \"))))(Tile((id \
         9c55ac8d-186f-4d35-9b8d-a7f0a55e108a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca88327c-e516-43d1-ae3c-1ae5accf9d9d)(content(Whitespace\" \
         \"))))(Tile((id \
         15bbec44-8ffd-4707-a489-c39002264968)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e169864-1d68-4676-902e-cc13e97b89c1)(content(Whitespace\"\\n\")))))))))(Tile((id \
         87bcd06e-a284-4b1a-bf2a-335d16dd062a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         186d664e-6864-4661-89ea-83d8f24f52a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd4abdfa-e289-4245-befa-d3b81729e58c)(content(Whitespace\"\\n\"))))(Tile((id \
         30bf4b5b-878d-4a2a-9beb-0695ea915ab3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a64ed2ae-166c-44dd-8daa-d8e12d114998)(content(Whitespace\" \
         \"))))(Tile((id 2968cb45-6b17-4d83-a48a-4eab557a9d9b)(label(\"\\\"can \
         set and get cell\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52c27683-92bd-4cb3-9681-82270c533bf9)(content(Whitespace\"\\n\")))))((Secondary((id \
         a9988210-201e-41f2-bea6-36028bca266a)(content(Whitespace\"\\n\"))))(Tile((id \
         25333390-e572-413b-be4e-cdc1d485b3ca)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d780213b-cc22-4c3f-979e-b07616a2f8d4)(content(Whitespace\" \
         \"))))(Tile((id \
         354709e8-a0ef-419a-af99-af78888a09f7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a49686ff-de71-4446-82e1-f14f66560ffc)(content(Whitespace\" \
         \")))))((Secondary((id \
         659dc3b5-0012-4a18-a56f-baa884081cf0)(content(Whitespace\" \
         \"))))(Tile((id \
         89522b13-3658-4a7f-9177-d0653a5ffa1b)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6451c06e-3cbc-4bc2-89fa-1c86b7ae4695)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7465017e-4e04-4228-aac9-55ae7e1571ed)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cce9534e-938c-4f34-bbef-7f74302ca25f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         09e19879-6ea6-47fb-a5b9-8b870b8acc15)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81072669-0746-445e-9cdf-baf7ad9610c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb59b096-0c9d-4872-9e5b-65e7f5305b3a)(content(Whitespace\" \
         \"))))(Tile((id \
         7c49cc20-638e-4818-9ad0-42727a8d31d9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         63fdd31a-0654-4470-9b2d-657d1bcf9392)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42899bbe-4679-416e-ab67-6301ae16a9a5)(content(Whitespace\" \
         \"))))(Tile((id \
         4ad97a99-c52b-49d3-aef3-90227077aa4c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55f647e0-3b68-477a-ac61-2fc9a33e9d60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6110829c-32b4-45de-8031-7f9fb2579dc5)(content(Whitespace\" \
         \"))))(Tile((id \
         c2bf3013-9873-48a8-a15a-993c5e8368e9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d050ee54-2726-4299-ab33-87b25f945e0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e44f1176-fd2a-4fe1-b085-6540febef0f5)(content(Whitespace\" \
         \"))))(Tile((id \
         4e163f5b-8b66-42c4-9ecd-03e37e0628bc)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b60d808a-b4be-4772-9d98-b8a98b05688e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2579a2f-67f4-4035-8f58-d43ef48ce484)(content(Whitespace\"\\n\"))))(Tile((id \
         3833ed6c-0c19-4085-a1a9-57e9728c917a)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5de98ec-0eeb-4c61-ab88-f269583af722)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1dcea0ce-f04e-46e5-b8ac-994ce985f379)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9888fbf0-8b68-4844-a98e-dc1f76240828)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c532941-59c9-491b-850b-4d025250b5e2)(content(Whitespace\" \
         \"))))(Tile((id \
         81dedd1b-e030-4895-98a3-cc99939030d3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41b8d7f6-f57b-4261-8e28-4ac333a531ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ba75016-5652-4f1a-8a95-dc70670d65f0)(content(Whitespace\" \
         \"))))(Tile((id \
         a49522e3-a0bd-4118-8f46-85e3793f4e03)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f0c0d743-5ad3-4b54-b045-2d340d98f591)(content(Whitespace\" \
         \"))))(Tile((id \
         071b748c-bfcd-48bc-9299-d3bcc6edae9d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6287255c-ae0d-491e-99cd-322a6930fbcd)(content(Whitespace\" \
         \"))))(Tile((id \
         af105d7a-7d32-4350-a239-3d122590ac9e)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8f733da-95e2-4c84-b82b-97890c290fbd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         db6b0941-145d-4462-8f60-585ebe2167b6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2ba9d8b-f056-48fa-ab52-b5e8ec6a42a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2ee65e5-4e2a-4853-a90f-83c2488848d3)(content(Whitespace\"\\n\"))))(Tile((id \
         6817eb54-f3ec-44e2-a705-d66775e5159f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         77e9d3db-ffa4-4433-bb2d-2897818577b5)(content(Whitespace\" \
         \"))))(Tile((id ed744e0e-2770-4597-ab5c-29b09d5f2e6e)(label(\"\\\"out \
         of bounds returns Dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3230ca1a-4070-488b-b116-5ea57912705d)(content(Whitespace\"\\n\")))))((Secondary((id \
         01e64bd3-23c2-4600-9726-815adb21f4dc)(content(Whitespace\"\\n\"))))(Tile((id \
         78978ac0-c795-48a2-b021-88e2ebc0d6a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         56f84827-4ae6-4bc0-841b-91fa13e2487c)(content(Whitespace\" \
         \"))))(Tile((id \
         1439eabe-2cfb-467b-82af-01f26bc82201)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef7a82b3-0ade-4598-b8ae-175739011fa3)(content(Whitespace\" \
         \")))))((Secondary((id \
         557338af-d647-4c6f-9fdf-e67690d04ccc)(content(Whitespace\" \
         \"))))(Tile((id \
         c09138d7-77ee-4891-a173-b844c37e6c8e)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28e79bed-315c-40ed-b1e8-d7b3b8ab1b9a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2d1acd6-6df1-4ba3-8f2f-cec3cbeca0d5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e480a910-ef6f-4a7f-857a-8a6164d793c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24a3d17b-c109-4f38-8aba-24840ab2c69a)(content(Whitespace\" \
         \"))))(Tile((id \
         6fca9810-4d18-4c32-93a4-d28e5d02e6c4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eaf1fa74-d640-4d87-9824-e4b0266bc349)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1a19bd4d-6600-46d7-8407-efb5d5edd3c5)(content(Whitespace\"\\n\"))))(Tile((id \
         61aaf2eb-69f7-4e47-8e69-56962c7f5096)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b7a8761-a24c-48e4-a1af-d52996a09172)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ce3eaec-c11e-4b98-8104-a6533666032a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c57067b4-556b-409e-8a70-f0cddb050a20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dff119c-a205-458d-9e76-3ccfe341da95)(content(Whitespace\" \
         \"))))(Tile((id \
         54721cf6-5106-439d-abe5-1ad7c178e55e)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70dfb75a-ea5e-4d5e-8e35-d0e00020377e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53010ff1-67be-481d-9e90-24edda26e705)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9b12efc-f358-49c4-989d-ded1d76cec20)(content(Whitespace\" \
         \"))))(Tile((id \
         4cfe92a4-87f3-4ae1-9bf7-a4e270b6a34b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d901895f-7dd2-46bc-977e-a7d8da38aecd)(content(Whitespace\" \
         \"))))(Tile((id \
         f1c9ec2a-a8a5-46a8-a7c2-7c6bdacf34ae)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5df7e900-1f28-4e52-bebd-09cf68ae6e6a)(content(Whitespace\" \
         \"))))(Tile((id \
         8c90f1e2-a7b1-463f-bf56-8fdad0827702)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         27021910-6f28-4ce2-82df-ade79646f016)(content(Whitespace\" \
         \"))))(Tile((id \
         8c09aa61-15cf-4783-a350-453b79dbfc56)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d413d8cd-650f-44e6-9dae-34b5795734ac)(content(Whitespace\" \
         \"))))(Tile((id \
         a3d89f47-a8a3-4cd1-b09d-6096544dd635)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         579fee98-8522-40ae-9c93-565d26487e9e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         273afcdb-9771-4506-b578-500294f91217)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d3f02ed-8704-4a72-9964-4128ba74e046)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4ab8e9e-5765-4e70-8ad3-53abd2f6b881)(content(Whitespace\" \
         \"))))(Tile((id \
         0a98b765-37a4-4a38-8c9a-ce7481eb3b2d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         116aa82e-2ec8-4598-9ffd-04cfef20ba03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3af2af28-e997-4c7e-ab96-fab145ad05a2)(content(Whitespace\" \
         \"))))(Tile((id \
         318a7098-1184-4058-a919-d07403bf932d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5483acbd-5764-4606-bbd2-78697f8ca643)(content(Whitespace\" \
         \"))))(Tile((id \
         0e43eae2-5684-48be-8421-914d18620fb5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f83aa6a-b809-4fe6-a0cd-96548613f5b7)(content(Whitespace\" \
         \"))))(Tile((id \
         385f4926-d1e5-4fda-91dd-afd8853d46b3)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2dccd15c-d76d-4601-9c40-f4ae922efdfe)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6ccdecdc-b91c-48a0-b4f1-73bb8e7a9469)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         607c2ab1-0df2-4a48-951d-7eb876745b74)(content(Whitespace\"\\n\"))))(Secondary((id \
         61c6825b-ed7b-498f-ad63-1bf912732889)(content(Whitespace\"\\n\"))))(Secondary((id \
         25c9f70f-e84f-42c5-92e5-541fb44276ed)(content(Comment\"# Neighbor \
         counting #\"))))(Secondary((id \
         20a8c83f-b54c-451d-9d92-e0a32564d5cb)(content(Whitespace\"\\n\"))))(Tile((id \
         0552591e-9e52-431e-ae54-d09c9f160479)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08b05dcf-6e76-403a-bdcb-d6f3d8216ebb)(content(Whitespace\" \
         \"))))(Tile((id \
         1342ab2e-6cca-44d5-8006-1d83277bf11f)(label(\"\\\"isolated cell has 0 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0742de97-ee7a-4567-8747-a71682bbf146)(content(Whitespace\"\\n\")))))((Secondary((id \
         0ce841f9-d89c-4e33-9442-a0088a9b5051)(content(Whitespace\"\\n\"))))(Tile((id \
         759cedc2-59ae-4d18-9888-cdf8e884d368)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4990eb65-e39b-4516-bb92-3f4108244019)(content(Whitespace\" \
         \"))))(Tile((id \
         d88c97ff-d742-4489-bba0-3c545bc82f05)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a8e0d3ba-13db-4060-8143-b428d4511c60)(content(Whitespace\" \
         \")))))((Secondary((id \
         096b0f96-266c-4a24-9420-429c9af82234)(content(Whitespace\" \
         \"))))(Tile((id \
         cc8680f2-5019-42a0-9085-e27d4c61a911)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17a224d7-80f7-4db9-b2a9-1fe564d862b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4d72b0b-7a47-4365-9ecf-5e83b0ccc240)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21b5d68a-b18e-4f73-8836-2cd56cd37409)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f4b0c9be-6998-4ed7-94a9-9231b25ee943)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e010a338-3c22-4740-8fa3-7a01bf81ad7b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acdaa947-7195-4aee-8146-486227824e3c)(content(Whitespace\" \
         \"))))(Tile((id \
         70ad963d-2495-46ca-978e-ba4f5800cf8a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d96f7e98-3abe-4efe-be09-68b07bb81cfd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         797c2d05-f452-412a-9578-5c97058f0e80)(content(Whitespace\" \
         \"))))(Tile((id e88255f6-a360-4036-b528-83498f358750)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c229b79b-0fd7-494b-ba9c-a4791d78d4c7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a33ab42e-b186-4de7-a291-ab8602462222)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         639a7b81-5283-45c9-af36-840d9042e41e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd402374-e087-4da4-a857-3d23accfe560)(content(Whitespace\" \
         \"))))(Tile((id \
         4ce45013-7c7d-4a89-bde2-d2e8d59bf618)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         d8a24b25-8b21-4f03-9446-13f1b3e69719)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a0f10455-07c6-4ac5-951e-c93dc556c703)(content(Whitespace\"\\n\"))))(Tile((id \
         b6506b08-bf94-4df7-8a30-9afe849281fd)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ce1d706-56e1-41b8-9292-8a92fa608b37)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         57d3f8e0-003a-4805-890a-93bc6faec5cc)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee3d5fcd-80ce-4cfd-8947-626b77b81b34)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51cd4c2e-ee5b-423f-b9a8-8862040c6393)(content(Whitespace\" \
         \"))))(Tile((id \
         be9e85b7-5cff-4d70-9ed2-d79489443b70)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e23c0373-d4ed-457a-9c66-2d56b8360e4c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57582ac8-f191-460a-ab8c-21cf5572e3d6)(content(Whitespace\" \
         \"))))(Tile((id \
         b39d5693-d162-4bd2-a4bf-92a863da05fc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         265524a3-fa1e-4fa1-aabd-a4fcd7f0a8a8)(content(Whitespace\" \
         \"))))(Tile((id \
         0c2d6ab6-170e-4b87-9548-63040e0e5274)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54c7ebac-b9c1-4951-a689-25acf7592d74)(content(Whitespace\" \
         \"))))(Tile((id \
         8ff82d90-bae6-47a0-81ef-321d96925429)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73dd0fd9-c256-417e-8505-1df9295dbc90)(content(Whitespace\"\\n\")))))))))(Tile((id \
         44c35eda-3550-4bf8-b951-58c1767eeb99)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d88d7aa8-7e99-4624-b69a-742cd7b7b655)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7d4a656-6401-4ad4-8bf3-012821426c17)(content(Whitespace\"\\n\"))))(Tile((id \
         11a8047c-adda-42c7-88ec-addf91c53052)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c57cdb85-52a3-4abb-96cd-3117aa09b020)(content(Whitespace\" \
         \"))))(Tile((id \
         1937c963-5768-4c21-8669-1d8083bdc6fb)(label(\"\\\"cell with one \
         neighbor counts correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e177832c-c195-4440-b135-ae1f3635155c)(content(Whitespace\"\\n\")))))((Secondary((id \
         fcdbdc8a-9ec9-4370-80c2-cd7b94844cfa)(content(Whitespace\"\\n\"))))(Tile((id \
         b2e229d9-4ac9-4bcf-b98d-e078bca28f84)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eab5c516-75e3-4130-ae55-63d37dc0011e)(content(Whitespace\" \
         \"))))(Tile((id \
         42bb844a-b619-4b0c-a5fb-c5383b655a3b)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1b59567b-1b6e-45ca-a26c-e1779c360271)(content(Whitespace\" \
         \")))))((Secondary((id \
         980ae0da-7ccc-4544-abd0-6c5d3743c0fa)(content(Whitespace\" \
         \"))))(Tile((id \
         75142fd9-d0e5-4356-930b-64ff81acd6f0)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f01d32c9-fcd6-4522-b23c-6e420ab52aad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3d4c3bba-001e-481e-b0ce-9f42ef5e3777)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf974c4c-92b3-48dd-8920-9bdaa37caf85)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da22743f-6671-468f-b301-ee378f21d994)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47ebbe82-0319-4e8d-ba43-1a43fe076d09)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce00453c-9ad4-4c2a-be88-3fb9f8594cc8)(content(Whitespace\" \
         \"))))(Tile((id \
         2723b0ae-ee06-41f1-9938-2d975de7ef12)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2cae7d9d-b342-4334-b363-f0d15f5c0ba7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         755e0e60-ef2b-482f-a82b-313ac417454c)(content(Whitespace\" \
         \"))))(Tile((id 3798bd4d-1e15-475d-992e-4b00fca31247)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ff5f2d39-1ff0-4504-8a6f-59d8498f29d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c286d6be-f577-46fa-9ac7-87068ef7022a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c954d924-59a2-4685-b1bf-bc588d611194)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b80e4f6-9115-4261-8083-4b66c3a09f74)(content(Whitespace\" \
         \"))))(Tile((id \
         c50bb6ac-88ec-478c-876e-7522896a6ab6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         93484a6a-2b17-442a-ac13-8c40b4f18cac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         221e037c-f4b0-4729-9851-bf9a5eca0d48)(content(Whitespace\" \
         \"))))(Tile((id \
         c1efcdc2-f28b-41b5-a7b0-f977c762371d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3bc8d7ae-8af2-46f6-952d-2f2a11091310)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33bf68ba-381d-44be-ae96-efb04c0dea56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5de5f75f-45fc-4344-bcbe-f0f5d999d108)(content(Whitespace\" \
         \"))))(Tile((id \
         f22e2ad3-60a9-4d30-a8a0-3a03b4520ee5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         132b50e0-d750-470c-95c1-5829327d79d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         49f7bacb-5af6-4cd7-880c-b61725eb52d6)(content(Whitespace\"\\n\"))))(Tile((id \
         b505968c-cbcb-4dcd-a112-055ef36d309e)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e699dc1-1492-4e24-ab26-8d0817e81581)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ca8fd2ba-8387-41c6-9cf1-def2209d4ba7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61f9e68e-a6e7-4db8-8b5c-f9ee8d276d3d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50c5b987-4721-4622-82f5-23e052d10764)(content(Whitespace\" \
         \"))))(Tile((id \
         744f5072-bbf1-436e-afc8-b83e72c15464)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd710f96-684f-4108-9496-769dd7428bec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b2d8b1e-e871-4e46-a152-bbd6017cdc8a)(content(Whitespace\" \
         \"))))(Tile((id \
         ae688fc7-e8c9-4772-90bc-f651aa1b061a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         948f4ecb-98f9-4554-8e6c-2f9065093759)(content(Whitespace\" \
         \"))))(Tile((id \
         c5eba284-1f05-40d9-bdf3-acaf3f8339d1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5da4d53d-3e75-4adb-b318-45a6c0966be8)(content(Whitespace\" \
         \"))))(Tile((id \
         1e867359-4a9d-4b12-b7ae-3babafef5584)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ec402de8-97a3-4484-9ab6-6472b48c801c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8e694e73-233d-4576-851d-cff64145cb02)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b040cfbe-2c53-4834-8881-7964f8538366)(content(Whitespace\"\\n\"))))(Secondary((id \
         007034a1-4baa-437a-954f-210348702f1b)(content(Whitespace\"\\n\"))))(Tile((id \
         f35d061d-0451-4f08-ae1c-cdd1902eba33)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2bfb108d-d40a-403c-b5cc-56ca092442e5)(content(Whitespace\" \
         \"))))(Tile((id \
         843aa06f-73e9-4a65-a181-7a4efdada8e5)(label(\"\\\"corner cell counts \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f092e0a-0e1d-4ce5-a0f3-669d5ac8980f)(content(Whitespace\"\\n\")))))((Secondary((id \
         f95d81c4-3f4b-449d-8c04-d73d1945b7e3)(content(Whitespace\"\\n\"))))(Tile((id \
         92daf70d-d02d-41c2-8983-efbdda81b986)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29158c16-5c65-4bf8-8e0a-637edc63b7ee)(content(Whitespace\" \
         \"))))(Tile((id \
         6142e05b-b717-49a6-9bed-df3335f70d53)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d5d0022d-51bd-4efc-97d2-3ea2f60e0663)(content(Whitespace\" \
         \")))))((Secondary((id \
         8886f2d2-95d0-41ef-8719-89636a07dd0a)(content(Whitespace\" \
         \"))))(Tile((id \
         a0a41ff5-f2c6-4b6f-bfab-dfe734eded63)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df3be3d9-2212-49b0-a9a3-fefc34d54d1e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ce4c1d2-83b9-4f85-896d-062f1d2327f8)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1563f2b-3e2b-4f77-9f83-58eaafc83945)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         048bf2b9-82d0-4911-bf81-96f9033bb966)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2965920-5a81-47e6-b251-0af39f63ade8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5529e713-9836-47e4-97ff-5e1c08d91c01)(content(Whitespace\" \
         \"))))(Tile((id \
         b7709ea1-6ff6-448e-be40-2427cde9875d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         23cb7a63-ad8b-4648-bfe4-110bdd8412f5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec87428a-7f9e-4d65-ba73-ccc655bb7e81)(content(Whitespace\" \
         \"))))(Tile((id c142fefd-58c7-4721-9ff6-2b97b6898083)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e91416fd-8dc3-4fcf-a24a-506b90cb8367)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3b433557-9460-44cb-b097-33570ce1d9b2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f9171113-984a-4278-a3a8-dead7fb6bdd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6244a546-bb4f-4268-8557-a33e781b52a1)(content(Whitespace\" \
         \"))))(Tile((id \
         de4906e9-2982-40c4-8898-240a2999c431)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7bb08dd4-b643-4ef5-907d-bc693632a25f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef34f07f-e226-4a0e-929d-bc4929b674c2)(content(Whitespace\" \
         \"))))(Tile((id \
         16a90473-cba1-4681-b548-cd9fa9984e97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7fa0c1de-8f04-484e-9f39-ae88b0318571)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         784baea5-6275-4995-96ca-048851084e53)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e003834-db2c-401c-b51d-6ff137cf5f8b)(content(Whitespace\" \
         \"))))(Tile((id \
         50331414-04ab-458e-8b51-d4fc7e9a97c0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a77d89ad-549f-4df1-8dd8-eafad9a423a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16ab6852-2199-4e3e-bba0-391de85ac2fc)(content(Whitespace\" \
         \"))))(Tile((id \
         b32d4dc7-1a5c-4fcc-8e26-029034dfa97b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         60ca3019-47e3-4795-bd63-d6581153a751)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6e95d1d-29e9-4086-8e47-dfdbf8b5faa7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e95f49a6-0e2c-4db4-9ac0-51e98809151b)(content(Whitespace\" \
         \"))))(Tile((id \
         b06f456a-73e4-4f23-b69e-c40d237b8000)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         8a06c76e-334e-4f9c-8d63-4e1a8b262f46)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c5b6b20d-1f55-4db5-99a8-2e2e5c9d5942)(content(Whitespace\"\\n\"))))(Tile((id \
         aa927ad4-90a9-4599-a7e3-d93712d46a95)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         904ed128-6893-42f4-bc6d-d76861d9efef)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         11c8f852-9437-4fed-9fa5-775417d4873d)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbcf5f09-e893-4b08-b63d-1ac1aea127b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08503148-7a88-454a-87e0-38c6def585a7)(content(Whitespace\" \
         \"))))(Tile((id \
         443e64db-36de-48d8-8f74-6f2f8dc14d9c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1321d01a-6faa-4e53-aee9-9ba179e76ee9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7b6f6c1-64f9-45b9-8133-d4c98e8a4ef0)(content(Whitespace\" \
         \"))))(Tile((id \
         6c800c4c-4472-464e-a9b5-633599886f23)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2bdd9c8d-a472-4772-a865-40241a01617e)(content(Whitespace\" \
         \"))))(Tile((id \
         85b85c99-3193-43ae-a922-d09d5a8d6926)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e4cd4d3-8c4e-4c26-b711-669f26e28894)(content(Whitespace\" \
         \"))))(Tile((id \
         53699b03-66c2-40f9-9f64-5cd5f3dff732)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6cfd3003-9ee6-451d-b6bc-a87cfe514067)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6d97edb7-eb2e-4f6d-9355-a0305819edf6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4bab7fb-51a6-4980-8d9e-6d9771bf5285)(content(Whitespace\"\\n\"))))(Secondary((id \
         2505775b-5fb9-4322-9b90-e0fd8c802486)(content(Whitespace\"\\n\"))))(Tile((id \
         58edf2f4-ed2d-4f25-8ab2-477437a7c8ed)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f096501b-58ee-4a58-96ae-6cec3c4088c8)(content(Whitespace\" \
         \"))))(Tile((id \
         e780d038-a81e-45bf-b629-ff295ec2167c)(label(\"\\\"cell with 8 \
         neighbors\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         55c6132a-a488-4dda-9c86-53794a69bbcd)(content(Whitespace\"\\n\")))))((Secondary((id \
         046d3992-f944-4317-9044-5db0e236280c)(content(Whitespace\"\\n\"))))(Tile((id \
         de3f6f73-6893-4757-ba3a-f4f5c05183fa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93fa06b3-891e-419b-829e-0fd9c40f079a)(content(Whitespace\" \
         \"))))(Tile((id \
         a7cc1087-259b-4761-9933-e13c02588cdd)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         daeb10b2-c7ee-490c-b1a4-b5728bbe6d30)(content(Whitespace\" \
         \")))))((Secondary((id \
         d12aa5af-1a4b-43ee-9b27-0bddf0a50297)(content(Whitespace\" \
         \"))))(Tile((id \
         86c1ffd0-88ea-4b9e-a9d2-9ea8d6871630)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35727980-d7d7-424c-a6bb-286f8877690c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         49f22aa8-0d58-4525-838b-8770069a6684)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c1f666a-0ed1-46bc-b313-752b22b2880e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         40c54f7a-3a06-4305-9a48-024ac949b860)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b8d9588-55cf-4050-8a92-bf7b34060844)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c08ba22-b4ca-4dd9-b001-9c17e252460c)(content(Whitespace\" \
         \"))))(Tile((id \
         90601dea-6781-44f2-bb68-a27277988c3b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ea7648b1-8c21-4f30-a4ca-64dd91d672bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffd26ed7-973b-4a0a-9b45-f3f33fc5607b)(content(Whitespace\" \
         \"))))(Tile((id 50c35750-1c65-4abc-bb28-cca3f94907c8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         890d9cc5-1e0c-46e0-b35b-794dd6a0d3ff)(content(Whitespace\"\\n\"))))(Tile((id \
         db65180e-db0a-4112-b922-3add0eee4965)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff04b6ae-1672-4181-b29a-88ac057201a2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6a4f376-292d-4c69-9acb-afadb4389480)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dab2a68b-7e19-4ac3-a3d1-91c07f082047)(content(Whitespace\" \
         \"))))(Tile((id \
         f4424910-e60c-47f7-b8df-9967baa39261)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         572ca6a7-df30-4028-b32d-ad8188eee5e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa9c9f49-1d2a-4a2e-a2cc-2f094ceaad15)(content(Whitespace\" \
         \"))))(Tile((id \
         b2d137bb-9cd0-4fc4-9dad-157afa692cb9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8d681f59-c811-438d-b98a-f9dfbf0d65ab)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e5302dd-a5a3-416e-ae8c-3da566ad69d8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         742f0afe-f914-4be4-9fbf-0e0337a608cb)(content(Whitespace\" \
         \"))))(Tile((id \
         4777d242-72cb-48d0-8b34-b217b3d97467)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         45fa5fe8-8858-4f21-a233-94d8b3d02e20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4963539-cbb6-4ff2-aefb-f269bffd4df0)(content(Whitespace\" \
         \"))))(Tile((id \
         3fc5e689-cd7b-4e80-9237-d61a20db82c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e124a20d-a835-45f1-8139-03ddc4572e08)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8fa9f14-9bfa-4557-9aa5-1486597b1b1d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0864e4c-ceb8-4e1d-aca9-ca679e8871f5)(content(Whitespace\" \
         \"))))(Tile((id \
         b9affdc7-3cdb-4deb-903a-a9099ac13897)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0797885f-b086-4a54-8bb4-5a84d38b5a55)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7b6ddb8-82c3-459a-990e-36cea843943a)(content(Whitespace\"\\n\"))))(Tile((id \
         c60425a5-16d4-45b1-ad9d-b353247371bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d76728fd-d3e7-4071-abd7-c0ae0dd8a652)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7db7779-6963-42b1-874f-a80ad674c3c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8721d3b-44e9-468d-a6b9-5009ba65ff5b)(content(Whitespace\" \
         \"))))(Tile((id \
         293f7eb0-ce5d-4020-beca-2b6849db72f4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d6b53116-9ae0-4db9-9f43-ce182a540273)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28026d52-95be-47a0-ad37-93bf377dcc4c)(content(Whitespace\" \
         \"))))(Secondary((id \
         36a25e9f-69fe-4932-bb54-2712213b89b1)(content(Whitespace\" \
         \"))))(Secondary((id \
         d015968a-e461-443b-be88-5a3b7efaa211)(content(Whitespace\" \
         \"))))(Secondary((id \
         2823fddf-fc7c-4092-8eb1-5b0a3855d788)(content(Whitespace\" \
         \"))))(Secondary((id \
         41eb7779-cd44-4658-84a4-78eb27c21e9f)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce602ec9-70e9-4314-8a71-e988cee335d4)(content(Whitespace\" \
         \"))))(Secondary((id \
         ae5b88e7-2d52-4fff-af45-043a3bd0a412)(content(Whitespace\" \
         \"))))(Secondary((id \
         9bb54ea6-2a9d-45b4-b948-bf9f35cd6921)(content(Whitespace\" \
         \"))))(Secondary((id \
         b41f0096-c4d5-45ba-a492-e033985f96ef)(content(Whitespace\" \
         \"))))(Tile((id \
         f43e97b3-0175-433d-b6d5-8975f5b58684)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         63577479-39cd-45b8-977d-b40a8c04546f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06bbac0d-63a9-4a68-b663-34aca29937f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         effd6599-faa9-4942-9b61-e9b36c1077bb)(content(Whitespace\" \
         \"))))(Tile((id \
         2b0a18ec-7381-4e5d-9b71-882e2ad17475)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         94ebedf3-7b99-4f0c-b515-2985d5ba993a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5b598fc-a903-4859-ac52-749e9e86a872)(content(Whitespace\"\\n\"))))(Tile((id \
         1e232213-3d69-423b-9766-6d8fd473dd14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3f2b2b6a-b06c-467a-acf9-3e3258a886b8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e8cdf3b-720b-4219-a094-362eb4452eb2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01baacbe-0f4e-40ef-9db9-a02003f29a99)(content(Whitespace\" \
         \"))))(Tile((id \
         dee38655-58ba-4652-a284-64906e2cb998)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ac251711-e72a-4c95-8836-a0495c6f8b6b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cba6c49-b5c7-4f6e-843f-c1947a94af4b)(content(Whitespace\" \
         \"))))(Tile((id \
         5e571604-7162-4184-aa08-9d373f3fb053)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0951a4c8-d62a-45f9-8ab8-a15dbe4a16b6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef022bb9-c59d-44e5-a17a-a3fa8d470371)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1106683d-8915-494f-a544-e17370ed91d0)(content(Whitespace\" \
         \"))))(Tile((id \
         57e5b147-dc84-4941-a262-a66c461255a0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cc3d14cb-aee3-46bd-b9ed-30c020599792)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea79497d-5c1f-48ea-90a5-8597753a0fa4)(content(Whitespace\" \
         \"))))(Tile((id \
         5d9eeb8e-d5a0-4f8a-8e51-bf2e4c41d87c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         42547501-9c5c-4d9f-9545-4506e05a3eb7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         955947ac-0270-4720-bfac-02b4cdf158a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b90171b7-cd5d-4826-ad0c-40d23d17284c)(content(Whitespace\" \
         \"))))(Tile((id \
         99148f9a-f379-493d-a2b0-b40604cb6ea4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         30d98563-5261-4d72-9dc2-0bd57c3b97af)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         fee65b5b-1fee-4af3-a2a0-7456614bc077)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         77cb9510-d087-4c80-9eb0-26c4e794db86)(content(Whitespace\"\\n\"))))(Tile((id \
         35b8e6da-176b-4896-888a-ddbbafd5dbd7)(label(countNeighbors))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8aa4e0a-f722-45e3-99d4-2984037494c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c488407-9ecb-4da9-9dda-dd14b3faa3de)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0e64249-68e5-45fe-98f7-3bb64ea085a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e34e80b1-4974-4f2e-bac7-2019b90b8c2c)(content(Whitespace\" \
         \"))))(Tile((id \
         42ba0112-9d1f-4f23-9e60-7da64af8cb11)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f42ba0e-6a3c-4ad9-bfb1-fcb8e20934c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         926927e6-9dce-4144-95f0-e7ca2bf0630c)(content(Whitespace\" \
         \"))))(Tile((id \
         f3d78223-7f8b-435c-975d-ed92668b34aa)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         444c8ffe-a4d8-4296-a119-b1a8ec1d9e6b)(content(Whitespace\" \
         \"))))(Tile((id \
         9c77c50d-8c31-4b36-bc52-955ddc8e1c8c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         480fffc8-f435-4b45-a469-c74638e48d7d)(content(Whitespace\" \
         \"))))(Tile((id \
         a35c7d40-040d-4b67-a0c3-2632bb45a871)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2367529c-6afe-425d-9da3-933481589e82)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e8a3defb-079b-44fb-af12-3cbebc42da24)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b95556b-0a84-43f6-9758-9fc4e5840bf0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aef4756-8e86-47bc-a220-e911895d7964)(content(Whitespace\"\\n\"))))(Secondary((id \
         74f84676-a5a2-4b8b-a4ed-0166a69a9485)(content(Comment\"# Cell state \
         rules #\"))))(Secondary((id \
         8bb46eda-e942-4e85-bd06-5005038ab9e4)(content(Whitespace\"\\n\"))))(Tile((id \
         4f0ca770-81f9-4519-8206-37790f1ccfc1)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be455623-9283-4371-8a7a-0f6674da401f)(content(Whitespace\" \
         \"))))(Tile((id \
         36a0674b-bf8c-4504-9e51-ac6b1b1e94c3)(label(\"\\\"alive cell with 2 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0439e99-dfa2-46c3-b024-e7473d8492da)(content(Whitespace\"\\n\")))))((Secondary((id \
         631f853b-4c90-400a-8f3f-9a0a066ecce6)(content(Whitespace\"\\n\"))))(Tile((id \
         d8846e0d-ffe2-474e-9ae0-b2b0dc6f5923)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dd972f45-367d-4dae-9ad6-39586ad5833a)(content(Whitespace\" \
         \"))))(Tile((id \
         b0ec689d-e55d-4b9a-83a1-08b15d442da4)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         51b58985-e122-459d-952c-981111a1b85e)(content(Whitespace\" \
         \")))))((Secondary((id \
         59e36f0d-8bb0-4780-b17b-80767d51a01b)(content(Whitespace\" \
         \"))))(Tile((id \
         8432aedc-e89b-4367-ad1b-10ba54984175)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c64aae65-375a-4a70-81b1-04b1cdb5f70a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87e87473-db92-41f3-bf13-44b0bdd6aff6)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11de3686-d5d5-4853-b7b5-3aa1eb9c2d4c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8226735b-d7de-4652-b31d-4674c8f2cc65)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99650eae-9112-4866-9373-c946ca97ceb3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb8d8203-83ba-42df-a003-025fd6d29159)(content(Whitespace\" \
         \"))))(Tile((id \
         a8cc8de8-3049-406b-a10b-7d0c69e49c30)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         db8f6c3a-4831-4c17-a2ea-deb00bfdd9d8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bec946d1-475b-4db8-89ec-f96de5d14426)(content(Whitespace\" \
         \"))))(Tile((id cd71ea3e-1ebb-4e20-88e4-25b15b38c72d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4aa75ec0-d206-47bf-87cd-5f308c866554)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c495b03-5255-4d1f-9715-631416b064a8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         149d0fa2-561d-44c3-acdf-df55b6934e35)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8229d78-92c0-4f51-aa24-046c419d19f4)(content(Whitespace\" \
         \"))))(Tile((id \
         d1695a6a-acda-4e0c-bd2a-628a3c850517)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b8afa477-caa4-451f-a726-151dc290f24c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c357c3c3-a79a-4d3c-a4fc-49c3034550df)(content(Whitespace\" \
         \"))))(Tile((id \
         0509b829-6418-4d65-ac35-8b0a254de1ce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c76a10c6-6297-480d-b853-579d5d1eeb91)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc1227c8-be5b-42a4-8e62-1c354bd07fed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a94e724b-fde4-441c-8f87-24c818b175ab)(content(Whitespace\" \
         \"))))(Tile((id \
         19ed841a-9a40-4fd7-8d9e-6c585e1bb096)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5e38199d-aad7-4670-8214-b18c804489bd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d97f93a-3d2f-4688-a851-7974798c326d)(content(Whitespace\" \
         \"))))(Tile((id \
         997246ea-6f88-4c94-a4ef-06ff05008f5a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         461b4085-b597-413e-b838-27e5e4735794)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbcaaa16-5d2b-4a7e-b2c6-4473f98a9a0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d65e68d-0dac-443f-b960-9f69d0c7883e)(content(Whitespace\" \
         \"))))(Tile((id \
         f2f576d3-b91a-4c1a-957b-c84f59333623)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         f86a92af-c75e-46ee-be25-f91d30a27fb3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f1e5ec80-e902-48d8-9193-867c367988b6)(content(Whitespace\"\\n\"))))(Tile((id \
         a8b2d3a7-0f3c-4e28-835c-ac33fd7d31c6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         412d0460-53c7-41eb-9ccc-0466f9b6b427)(content(Whitespace\" \
         \"))))(Tile((id \
         75a7cacc-afd8-49bc-b994-07475805a5fd)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c8a23016-3c8b-4619-8c5d-a88233fcbf8c)(content(Whitespace\" \
         \")))))((Secondary((id \
         aace9752-5624-4ca1-85af-a33988cc25b4)(content(Whitespace\" \
         \"))))(Tile((id \
         f27ec97a-37f4-4d71-ab06-cf877f2ae01d)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6146e0f1-fdb1-44ff-ba76-257021e9a14c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef5750d2-7232-45bc-bbcd-275d11c54ba3)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1ba395af-60e2-4064-8cd7-9b9b59b15a9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         157610d0-5434-49fe-a3b2-43262dd272c6)(content(Whitespace\"\\n\"))))(Tile((id \
         393d0f0a-ff09-40a8-8eb9-1d6bb9f717bc)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         540597dd-ce7b-4add-8002-f6cbf1d054b7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         82115654-2505-4100-a133-6f096ea4ef03)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0aa0f0c6-6471-4417-b1bc-139b1c323382)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f02bc46c-53fb-4ab8-8781-e131bac06bda)(content(Whitespace\" \
         \"))))(Tile((id \
         fe35c27d-2c5f-4304-bfa1-7457a182fb54)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         750e2346-9183-4fd4-b148-b2627809f4ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e363e34-a19a-4762-8e79-50b0ee30e0ff)(content(Whitespace\" \
         \"))))(Tile((id \
         8c9316f5-314a-4bfb-be9c-c4fb3daa6357)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2e75afd3-326b-4c9f-9de2-fc6b2e7b9a11)(content(Whitespace\" \
         \"))))(Tile((id \
         0963a82a-2879-427e-a998-6299ede94963)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d928562a-7b32-438d-80ee-0269d53375c3)(content(Whitespace\" \
         \"))))(Tile((id \
         66be9f2e-9425-4c15-9357-9f0c0c027db0)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e459b7f-502b-4357-a527-a7bc67ede5df)(content(Whitespace\"\\n\")))))))))(Tile((id \
         08db16ed-1e65-4fe0-bb31-36e709835303)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18162191-6c4d-4726-a5bd-f17be972a2e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f735a730-ad2f-42a2-bc6c-ea27d2c99420)(content(Whitespace\"\\n\"))))(Tile((id \
         4ff12167-d5cb-4031-b815-71a207b3d844)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ab051afd-a0c8-4ddc-86fc-ea41e01a6796)(content(Whitespace\" \
         \"))))(Tile((id \
         2db883d1-57eb-4b8f-be68-baf1a703bb86)(label(\"\\\"alive cell with 3 \
         neighbors survives\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf14e86e-7b19-4b1d-ba46-3fb6baabcea2)(content(Whitespace\"\\n\")))))((Secondary((id \
         0d6cdb3f-8318-4c0e-8b73-6ef66920f124)(content(Whitespace\"\\n\"))))(Tile((id \
         62b76a10-aa20-46d3-9043-2ff347f21ca5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a1732a69-f927-473b-882a-52b5d44ac71f)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b3595d-191a-4e41-bbc6-6c6d6aeaf7c5)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         077d80d1-80fe-46c5-9c83-e5c435c53a23)(content(Whitespace\" \
         \")))))((Secondary((id \
         3069d862-5ef4-4006-8f0a-f731e58f9497)(content(Whitespace\" \
         \"))))(Tile((id \
         cbe1498b-4b31-4d11-92fd-3056dc3a44b9)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25cfc31b-ec4a-48f0-afd6-7bb1d7644db1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7a84746-56b8-41a4-9a83-91fc2ef7d71b)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0bcfa19-11a2-4040-ab66-8faa5038a54d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1f4122e2-9f42-47e4-8314-e806f13b9ff5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe3aeaef-ea53-48c7-a0a5-7283ac4cc473)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d40dbaae-1fab-420a-8cbc-9f3bb2cd5738)(content(Whitespace\" \
         \"))))(Tile((id \
         0b278c53-09b1-49f8-8f3a-12e02fc48007)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e95a4b3c-7947-431d-8f6d-49a19c27e234)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81e6f6fa-2f7c-47b0-a37e-1412e9b82de7)(content(Whitespace\" \
         \"))))(Tile((id d28be53e-7469-47a2-b7c3-a7704666c753)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         26bb016d-373e-408e-8133-cf517fe09566)(content(Whitespace\"\\n\"))))(Tile((id \
         85a290e2-9c63-4a44-8d32-8c929883efd9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4448ca27-9fa2-4aba-b43e-a6e654ddebd4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c9f8b06-466e-4e58-b1ac-82ae3ba5a327)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2e0cf49-9251-4acd-9fd1-f5319aace634)(content(Whitespace\" \
         \"))))(Tile((id \
         f1eb5f26-9e0b-4808-96e1-682ff67abd05)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         45d7d5c8-9de1-4c34-91dd-69d3932e1710)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f9429d5-0979-4f1e-ab53-ee12e6da2842)(content(Whitespace\"\\n\"))))(Tile((id \
         dd0a8db4-bac0-4b19-ab4c-5ba5a1561d46)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         18f95705-6b60-44ad-ae17-b1f382e1a462)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         88d450e1-61a4-4e7b-aaf5-a414e39cb821)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2498c124-055a-4ea0-bacc-d51bd03e70a0)(content(Whitespace\" \
         \"))))(Tile((id \
         33fee869-8586-4685-b83c-75356d8b88c5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f93118ff-b68c-4d65-b35d-b60e9c705d1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4c55e91-c26d-46f9-9904-dc4def837036)(content(Whitespace\" \
         \"))))(Tile((id \
         77983e4d-46da-477b-8c04-6fa721e3c923)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         41792aa6-be88-4bb5-ad8f-42d554665fb1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         746710f0-e3b7-41bb-9b24-eeb97635ba2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e55030b-9a79-4238-8015-d021b662a271)(content(Whitespace\" \
         \"))))(Tile((id \
         9978d9ba-4ae5-418b-9364-9baf5b0f5d51)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         45804b21-d965-4ec4-9745-95217b8eed54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24b54e6f-1d9d-4b12-9367-c62561361260)(content(Whitespace\" \
         \"))))(Tile((id \
         ba9a58eb-3411-4978-98e7-d4759f58abd0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ef60572c-9f9b-4842-a732-763ecfc96fec)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9f848f0-474c-42e1-816e-c13017104cda)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fa9b9bd-d7fb-4be0-8cb5-455a5ca0650f)(content(Whitespace\" \
         \"))))(Tile((id \
         b2f6bd47-72e7-4cd2-abfe-12f66b669810)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f472b376-afb4-4675-95d6-be9b39ed36fb)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         b9f50ffc-2f40-4978-83a6-0fa291c7f4ad)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0a5ebd72-003b-4723-b9cc-b060e3b4e2b1)(content(Whitespace\"\\n\"))))(Tile((id \
         d1626ae9-e812-4285-b55a-bbcc9ffde9cb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         39cd452a-1892-4bc5-a1da-00dbd66a5f4b)(content(Whitespace\" \
         \"))))(Tile((id \
         53d2ebff-9a24-4eac-ad28-d64761df0492)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4949b9e5-516f-4f20-9628-4d296c079a5f)(content(Whitespace\" \
         \")))))((Secondary((id \
         e062506e-b6d9-4d57-aea7-862b8cfc9d47)(content(Whitespace\" \
         \"))))(Tile((id \
         44e999a8-50ab-4d7a-9ee6-a9da2749fb4d)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74f3bb57-12b7-4dee-8616-73a98e68f20f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ca5be30-eae5-4b5c-8390-8f1f98009be7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19cb5f93-4e3c-4edf-ae35-c46a4740c42c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a0d7d17c-d9b1-4489-8a2f-2c7df8ad972c)(content(Whitespace\"\\n\"))))(Tile((id \
         0a2e09e8-4c7d-4455-8084-6d885a449b6a)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ad281ff-e565-4129-a516-94480b03943d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         557f71da-dd5d-4c8d-a46a-0de4ac6764e3)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1361daf7-4544-4e65-bbe4-2743cfdec042)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cec7703e-cf63-4b3a-b4f8-81b144399e48)(content(Whitespace\" \
         \"))))(Tile((id \
         71e56be8-88eb-4f5d-9fe8-54880aa1c5d2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc7f817d-dc95-47a0-a9e6-ee952a0eb007)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         226b8d48-29f4-4309-82bf-0282f4000156)(content(Whitespace\" \
         \"))))(Tile((id \
         f9005910-6689-40d0-8d2b-700be7b9b40e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         609f10fc-b7b9-426b-ab7a-a879ba42f240)(content(Whitespace\" \
         \"))))(Tile((id \
         c4bad957-6936-484a-b5de-52e849ce1013)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cafaf706-4adf-4f4d-a3e9-9ae279ff6c71)(content(Whitespace\" \
         \"))))(Tile((id \
         5870e721-8629-454d-b878-2d75918ff2f9)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3b1fd26e-9b18-45e3-a59b-bd252a9adcf9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1f469267-eb0d-4a83-aab3-793af30cb65c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         923bec1a-64cc-46f7-8e66-ee88ccbfba0c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6184c5ea-277f-4e36-87f0-f5126e96734d)(content(Whitespace\"\\n\"))))(Tile((id \
         63f526eb-2d73-49e9-93ea-31995d9dbd3a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         40c5ba35-152a-46ad-a92e-113ee30fd3f0)(content(Whitespace\" \
         \"))))(Tile((id \
         3ad2bf73-0fd9-47a6-b6f3-2a203ded4152)(label(\"\\\"alive cell with 1 \
         neighbor dies (underpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e48c50b7-e7f3-4aa4-9360-af2cca14dea6)(content(Whitespace\"\\n\")))))((Secondary((id \
         2c459c55-1d53-4563-b64d-a65d8ac35b79)(content(Whitespace\"\\n\"))))(Tile((id \
         b0d1cb27-8982-4412-adf5-7e82cb4808dc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc4c6f11-851c-438b-803a-4dc2a15c281a)(content(Whitespace\" \
         \"))))(Tile((id \
         15cbff4c-1e75-4e80-901b-0344bf138d1c)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fa34b5d3-5c57-4be5-874d-0b56117c7cd6)(content(Whitespace\" \
         \")))))((Secondary((id \
         5d485fcf-8b79-4fd1-8f98-5fbdb94296e9)(content(Whitespace\" \
         \"))))(Tile((id \
         c4f1f3e7-3c66-4524-a61b-30ba9b2d4982)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a587372d-6172-4c0f-a483-77f535d2f700)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8065edb7-3da4-4eda-8738-8d9b6a43b63b)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3354a1b3-ad5c-40aa-8228-5a8df9019e0f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c6aaf7d-c57e-4804-b096-0f702026b973)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85d3ac9c-49b8-4f9a-8902-3db32f5b6b64)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b5f2411-ff68-4232-a860-0be8f657cc23)(content(Whitespace\" \
         \"))))(Tile((id \
         bd9079cd-f2d8-42d7-825b-851516da8764)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5a6d50ae-1c46-42e8-b6b0-ba6b28415d6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         167967ab-6dcb-459f-9e58-44128e25cbb0)(content(Whitespace\" \
         \"))))(Tile((id bc62f48e-eaf1-4575-bea2-0b76a9fbf2a8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4ac5021-8d30-4e2d-8824-b582b843802e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         26cd02be-477c-40da-a569-3ed1629d5155)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76542f54-38fc-411b-b082-ac6092356365)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de7af2b6-393c-4253-84bc-21a6355f413f)(content(Whitespace\" \
         \"))))(Tile((id \
         faa7b005-6bea-4efb-8b4a-392ce8ac565f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         10824195-a4fb-4d6a-8cd6-6ad1c18a4407)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f30fc9d2-1322-4fe2-a676-31416994e1f5)(content(Whitespace\" \
         \"))))(Tile((id \
         5896196c-fe6f-4dab-ac99-57258a58e1c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10f3d7bd-7587-4f8f-ac6f-462d404d6972)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d2196d4-e779-460e-81a8-b76e395fa45d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc003652-b676-4734-b880-a43e169cc7ad)(content(Whitespace\" \
         \"))))(Tile((id \
         06b7dce3-dcc1-429e-aea7-7a21ee6a2887)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         b01efd0f-3cec-4251-96a4-888b116655ba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9254d06d-9919-4d21-9173-3b181cb7eda5)(content(Whitespace\"\\n\"))))(Tile((id \
         a4957b4f-1c2f-4a7d-8eb8-33b809205cfe)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1df88c0f-335f-48dc-b268-19e682e3ff33)(content(Whitespace\" \
         \"))))(Tile((id \
         39030a5e-1d41-491d-a895-f8dfb9f1cd5d)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         88204f99-c306-46b6-9ef9-1689760e8b5e)(content(Whitespace\" \
         \")))))((Secondary((id \
         b03c59a4-47f4-4225-879d-47d1de0a749d)(content(Whitespace\" \
         \"))))(Tile((id \
         5af850b1-aa74-4c30-b757-92e1aefc8282)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ce0a7d9-11f6-49af-9506-cc11677e7fa3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a943ab2-941e-4d9c-b1dc-d4fff3b8396f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cafebeb5-e40b-430f-9835-b3fbe1907c04)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         64b66c8e-c12a-4022-a599-8ba82bd6f375)(content(Whitespace\"\\n\"))))(Tile((id \
         7a596671-c02f-44ea-8c1e-35a1d0bf2938)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8751edf4-bf22-41e7-b0e3-a1b7bd85f723)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d42c25b1-f118-45be-8297-87e483b5d837)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63c6ef1c-882f-4e90-b3cc-0068695ce064)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         896d4c01-8436-47ae-a4b8-d242fdd7c3e7)(content(Whitespace\" \
         \"))))(Tile((id \
         997b113d-d9f3-4736-8aeb-6a622828e70d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91dd15d2-df15-491b-8f2f-2f5aa6bf39a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dde4b735-1167-4ae1-baee-58fe212c19c6)(content(Whitespace\" \
         \"))))(Tile((id \
         02ab09b4-444d-44ee-8582-aa0ed142b3a4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b9ec39c0-37b6-4807-b57a-995f084d63b3)(content(Whitespace\" \
         \"))))(Tile((id \
         543cd8ed-7c9c-49fc-bcdc-099e18c705bc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         053d5b68-fe4c-4e8f-a2b1-14fd06e230ec)(content(Whitespace\" \
         \"))))(Tile((id \
         df61932e-c4fc-4863-8874-e128182812db)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         620b2ab1-737a-46bf-9fff-3f6d0559f3b5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         20a3940c-e2ec-4d89-9cef-a06d78963325)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d77b0a48-ef27-47e2-92f9-0ab275ff6db6)(content(Whitespace\"\\n\"))))(Secondary((id \
         05bc3040-308a-49c5-a710-e0bac4989cfd)(content(Whitespace\"\\n\"))))(Tile((id \
         a8597810-7bab-4a94-bfdb-4ec3d00db381)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         630fb87d-9c70-45c8-931f-c069bfe429e4)(content(Whitespace\" \
         \"))))(Tile((id \
         97ace2e6-b824-428a-b8e8-13df9d26b021)(label(\"\\\"alive cell with 4 \
         neighbors dies (overpopulation)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de4dc467-ec6a-44d7-8216-4040f52e7aef)(content(Whitespace\"\\n\")))))((Secondary((id \
         2902d12c-94a8-4a4e-b640-ad3523544a95)(content(Whitespace\"\\n\"))))(Tile((id \
         5bf1b73a-dc36-4fb2-9d7b-12b8d3b9b99e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         602bed16-1f58-48cb-816c-7360c6b6aecf)(content(Whitespace\" \
         \"))))(Tile((id \
         cc002c9f-14ab-418c-9e22-daa9dd5cf76f)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         69b90ce7-97c2-4cf4-8d53-9edc47cfacd1)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ebd921d-abf8-46ab-96af-51c2d4a766f4)(content(Whitespace\" \
         \"))))(Tile((id \
         b2e19fc3-fda3-4364-ba01-b0f2e96334c1)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5678d33b-084a-46a9-8c31-72a0cda74da7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bfd60897-8d69-4250-a35e-7219a084a504)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3737967a-634e-400a-9024-23d38e2d1f91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4458340-48e8-4cca-b060-3cb8acd5ed81)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e4ae0b7-026d-4e9c-b2e8-bf3b0195893a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         007b2418-3d2a-4a17-b25b-b5e18bd6a827)(content(Whitespace\" \
         \"))))(Tile((id \
         925e45bc-47d2-41f9-93a3-d6aa234e329e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         34a6f5ec-d04b-42ea-b31c-c971011ec03a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d1916a3-6e9a-432b-8677-8b89738151c1)(content(Whitespace\" \
         \"))))(Tile((id c0a0677b-2e29-488b-b3af-41ce720b29d8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         89cf6990-651f-4f47-a4bb-2da40630b558)(content(Whitespace\"\\n\"))))(Tile((id \
         5fc543ad-a1de-4871-a766-4c91ebc43ba3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7dca75e0-a2c0-40a4-aae9-27491f223253)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28866541-b62e-4d41-aaec-0a686e4329d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b6096ab-fc51-4619-a2bc-28e6f7f1ae82)(content(Whitespace\" \
         \"))))(Tile((id \
         ae203d65-6ea3-4dce-af80-01b18c6ca386)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b8875127-04e6-45ac-9b28-3f1b05856e7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f48366b-a6c8-492c-b79c-6ab7a9948937)(content(Whitespace\"\\n\"))))(Tile((id \
         93f4cec2-c165-4da3-8011-517404b83561)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         51def498-1885-4d6f-a3ec-040648da1818)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b51c26f4-ec33-4791-b2f7-e1d0883e090a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37e6d7b8-bd6d-4e16-a0a9-f69168c5b52d)(content(Whitespace\" \
         \"))))(Tile((id \
         6445ae64-068d-4395-a3a3-aabed1309043)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e4b087dd-20e4-48c0-870e-e0a60d73f1c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31e2fbdc-b332-40e4-a547-72612f663eda)(content(Whitespace\" \
         \"))))(Tile((id \
         e532751d-c39f-4ced-83b7-dc53db34d241)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         90393167-74ab-41ce-9ed9-666946dfd92a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         285b750d-fddc-4693-90e7-2bfbd8c1b60c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4641da83-ec20-4de6-b0c1-d2329fad6f70)(content(Whitespace\" \
         \"))))(Tile((id \
         8fa5eb64-0e49-4ba2-b1f4-4fb2f71127d4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0491badf-f58a-43f1-88ce-e19a7d005bd2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         472713fd-c18f-4f6c-914d-b95441237eda)(content(Whitespace\" \
         \"))))(Tile((id \
         0942b6ce-9942-4941-a8cf-46512a009ebc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         12565605-c2fb-4257-857c-f38dfc85505a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68b330a9-0a66-412e-97a7-de8f8954985b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         15a84848-7133-474f-a690-2d9365ce3bf7)(content(Whitespace\" \
         \"))))(Tile((id \
         6686d607-bfe9-46f5-90ef-8bab0d113098)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fea8ba0a-9993-437d-a30f-a4be83ec84e4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e0650af-8d1f-44a9-9c14-29eed9d44518)(content(Whitespace\"\\n\"))))(Tile((id \
         ffc638c4-6e9b-46ab-aee5-ffcd28f575a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         253cc17a-c721-4ca6-800f-02bc03d098a8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d7e6420-d87c-4a5c-baec-111bc5db4be5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee8c6f81-6e9a-411e-9eb6-69f74e873565)(content(Whitespace\" \
         \"))))(Tile((id \
         6a1ea859-745a-46c7-9056-cdb63ebab573)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3dc5a5fc-cfc5-4770-a6c4-7d4b818a0d5a)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         0d8dcf1f-6ec9-4c8a-976c-5c0721c3faaa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         17ede2b2-7ca1-4cad-8fca-2f647e47bb68)(content(Whitespace\"\\n\"))))(Tile((id \
         3e09222c-0fcc-4256-97ca-01d61237d444)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d67970ea-7808-4b2a-8269-4b77c6081557)(content(Whitespace\" \
         \"))))(Tile((id \
         fe257fbd-015b-46cb-b9f8-addf74805c10)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0d928aed-77a3-4b18-afdc-02a40e4c89a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         53668099-d9cc-45a2-bfb4-d3e7e84f07c7)(content(Whitespace\" \
         \"))))(Tile((id \
         a690d1d6-f804-4d41-a362-382356ce89f0)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         95251e57-0325-48f7-a3e1-06c268444709)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         727a2d91-200d-49b8-b6c8-0e2f962b6493)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         31758357-db38-448c-9bba-64feb33b629f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e43fa85b-d437-483a-a170-48ca28d8157e)(content(Whitespace\"\\n\"))))(Tile((id \
         8796c358-f6c5-44ae-bb06-20c1c9fa4af6)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         954dca1e-3f3f-4f02-a13f-a4eacba40ac3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         23e92ea0-02a7-404c-9871-5295dbcb2f57)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         749b48d7-5d64-466f-88e3-fc6ebd8c4f08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7df7f8dd-6cf9-492e-947c-0e634d2a671d)(content(Whitespace\" \
         \"))))(Tile((id \
         3f2a246f-e1d0-4621-8f7e-72befd0c412a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b94ecce0-4e5d-4449-93ab-1eef63f2dc1e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f63eb80-f702-4e1f-aeb3-5ffc30eedb39)(content(Whitespace\" \
         \"))))(Tile((id \
         66346b23-1b58-4c1e-84f3-40334a7437c7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0d97a16f-c210-4c5b-9b5d-86f53336c706)(content(Whitespace\" \
         \"))))(Tile((id \
         bf1f044b-9813-4b49-8335-9b132ad1a21b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27a16c14-16f3-486f-b293-51a648d0d89c)(content(Whitespace\" \
         \"))))(Tile((id \
         26b14c08-1c38-40ef-a61c-7c18345ee7c5)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63660989-edcd-4bea-ac20-fa975297ef66)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d65b358d-22d0-47d0-8c7c-002b87a2f249)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e117920-531e-4ee9-ba33-667d26b8698a)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa52ab48-2098-4d1c-bda7-1c20a8e26ea0)(content(Whitespace\"\\n\"))))(Tile((id \
         89a1933d-9d1a-468e-8b11-4d6d86720a1a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f8f913f-e213-4d52-9698-036fbc63586a)(content(Whitespace\" \
         \"))))(Tile((id \
         7be6c435-0218-4b3a-8696-d54c5e29eb82)(label(\"\\\"dead cell with 3 \
         neighbors becomes alive (birth)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24561fe4-6d9e-49d5-895f-c8f76e645408)(content(Whitespace\"\\n\")))))((Secondary((id \
         22c05a3f-f413-49c5-997c-3b7c69a994fb)(content(Whitespace\"\\n\"))))(Tile((id \
         bafa57cc-80f1-4861-b5de-1e440c1fba28)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55a44855-98b7-4a08-a913-4d2c7caf47f9)(content(Whitespace\" \
         \"))))(Tile((id \
         aa070e9c-b624-48c3-9244-8cdd0c39c447)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dabee09c-7098-4c8b-bed6-57f5b0db0833)(content(Whitespace\" \
         \")))))((Secondary((id \
         40c0a3fa-40bc-4259-bc2f-ab08017b62e0)(content(Whitespace\" \
         \"))))(Tile((id \
         8c8b4405-0087-4cfb-83d7-95dee151d7b2)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c322d20-25c1-448a-89e5-6ff2d443dc2a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         09f04967-ff26-4600-876f-c8a2a78bb3a8)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0c36718-c5cf-4c61-95d3-8a587777158c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c98368e0-0ada-4ff9-841b-6c61abab9cc7)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1ddb083-b3f4-4f2a-859f-5c9d6e547ccf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c79cdf8-ce21-48b1-99ac-bf3dcb9b56b6)(content(Whitespace\" \
         \"))))(Tile((id \
         297b3c21-60fd-47e2-afc5-d65628c7b19b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         23ece402-b422-4dfd-a83a-e4572a867af7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e5994af-1dba-414c-a28e-b157d30129a0)(content(Whitespace\" \
         \"))))(Tile((id fb5a5668-f1bf-4efb-8410-df3542c5c823)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72ade9a5-956a-4001-bbd1-e3f7e9197aa6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b57030f6-b74a-4dc5-a30b-63d5be416d49)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2796f5cd-fce7-47aa-b38c-457e718e2e95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1955b56-0c4b-46f0-b6e9-c8fb826a0578)(content(Whitespace\" \
         \"))))(Tile((id \
         352b4f4a-49ae-4901-9a0f-c6fc95d49236)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ed6c2d40-0653-4f33-8fdf-82b71d77ea51)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a48ba341-c7cf-46aa-8f6f-32bd5452d7fc)(content(Whitespace\" \
         \"))))(Tile((id \
         15affaef-6018-472d-bec3-19126e4acab1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         da18cffd-b57f-4695-98c5-80dcb5bbe111)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         284195a1-771c-4919-a5a3-f0a2c756f512)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36277a21-0d3f-40b9-b2be-7e23833a2336)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5bf20f-b2b3-49e4-8bdb-72c6e37c3d73)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f8682fa5-bff1-460d-8899-534bb005b556)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c84a295-7d87-48b9-bdc8-ddc4f9551b3c)(content(Whitespace\" \
         \"))))(Tile((id \
         f473bcf0-5425-494d-bdff-e06f1fffb644)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5e33e390-9852-4896-abff-ee87d97de074)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49dcdca3-2f4a-4eb8-b0cd-80f65f2dfef0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3361285-3bf4-4144-80ba-5f4fdbea2224)(content(Whitespace\" \
         \"))))(Tile((id \
         08d2f3d7-8b3a-42f6-80b7-8cdc780d219c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         fe320bc2-4d54-4c2f-8222-2ff2a8c82fef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2d18cdc3-3b0c-45bf-a8b7-431461c3acb6)(content(Whitespace\"\\n\"))))(Tile((id \
         eadc191f-245c-4bce-96f1-4ae8aac39211)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         17cfd3a3-1521-4170-989f-aafda969c1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         6f58c68a-eab1-4b7c-a31b-9fde97cc3850)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bcd28ec5-cb6f-4435-872a-3d7c086d0d86)(content(Whitespace\" \
         \")))))((Secondary((id \
         ecf25bad-3e1a-4e23-b4ab-2a5af295db87)(content(Whitespace\" \
         \"))))(Tile((id \
         5369c3bc-6ef2-4c44-9dbf-7760c84c75b5)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         103a78bb-74ea-4fb0-87fe-b35a76487503)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         945bd9c2-b268-43b9-a10f-a00ab7d7b64f)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         911e614d-2df7-40a6-977f-68389b39906b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5267d9e3-6570-42ef-af93-6c87356b2565)(content(Whitespace\"\\n\"))))(Tile((id \
         255d2bb2-31d0-48a6-ab2f-b249628ea008)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30db676d-c34c-4c77-83c8-946a82cfa2fb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1913cc2e-7e5c-4ce1-99b4-555832965e61)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         377f2a29-674d-49a6-b2cf-8a0d9962fc44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f920db3-8b8b-4f69-b488-4c597b9c5208)(content(Whitespace\" \
         \"))))(Tile((id \
         d01611aa-ebc1-45d9-b019-0e480d2e39ee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0af3ed9-d5bf-4229-8b01-f71d22a1b09c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1edaa3a-3b30-4a70-af51-57c8c394736c)(content(Whitespace\" \
         \"))))(Tile((id \
         02ccc571-d8b3-4cc8-887b-0aef49fb88f6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         18de1880-0a3b-462d-8c4c-f437d1128064)(content(Whitespace\" \
         \"))))(Tile((id \
         70659b34-9904-40d6-a203-49705df29e7b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98d8803e-6f83-4bb1-bc90-5d3b8a57da92)(content(Whitespace\" \
         \"))))(Tile((id \
         0f16898e-f7e1-4070-9781-fe6a5728faf0)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a668ea78-c6eb-430b-9a02-e17509bb8a0d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         be3c4e3f-4fe5-4608-9417-01b722cc3265)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5edb8282-f3fc-47f9-a4c5-62ad0faecc18)(content(Whitespace\"\\n\"))))(Secondary((id \
         36da359c-33d1-468e-9c70-981e610be9db)(content(Whitespace\"\\n\"))))(Tile((id \
         4dc58a7f-9cf6-4627-ac05-e55dfa17bbf9)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         145856d4-6ab2-4c48-8efb-6f729e820dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         6933e64b-5736-484c-b476-5d71a4378d20)(label(\"\\\"dead cell with 2 \
         neighbors stays dead\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c894243a-7c8e-4cc5-a2d9-a223fb497b29)(content(Whitespace\"\\n\")))))((Secondary((id \
         12f1daa9-4a2b-4178-9649-9863b80d36b1)(content(Whitespace\"\\n\"))))(Tile((id \
         17f6e78b-9962-43ca-b8d3-3edc07c39272)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a8f35fcb-bacc-4b28-8c45-4dbf5b6a1b36)(content(Whitespace\" \
         \"))))(Tile((id \
         b014213d-80ad-4bee-81ba-7e3fa7de704d)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6186d76d-97c8-4c57-9938-d49e4f6e6c78)(content(Whitespace\" \
         \")))))((Secondary((id \
         2cf58279-c7e4-4821-b431-a1d75e22937b)(content(Whitespace\" \
         \"))))(Tile((id \
         e4a25d56-b972-4640-8300-988c117c2f93)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c77cb7ef-9da7-4122-b2b2-4eb9ef6de61b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce6c02e3-b171-48cb-bc6c-00fe3ad8cfcd)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce4df797-6d3c-4fca-8cba-ca440dc7c75d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1000dfbb-4275-4a05-a377-92c8712c4931)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07285cd5-2060-4302-965b-7b74ed04c786)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5788db9-e687-4e56-ab30-1a0497a4d67b)(content(Whitespace\" \
         \"))))(Tile((id \
         445d1dd0-c9bf-473d-8ed7-f31389f789b7)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5026ca60-e446-49a5-8830-7ade2f2aa6b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c96474a-f916-4016-a4e5-363e4c0f0f0a)(content(Whitespace\" \
         \"))))(Tile((id 2ed9ea53-d7ac-46dd-b840-51e444efa67c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         000f4762-baf8-45cb-a13e-f1f3cd5f0e1b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a91e6cc-18ff-490d-a576-76d32590f4be)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0d81df5-58e4-4ca5-9fa0-19b2cb7530b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1a38dd2-c702-4620-a759-eb9c14f247ae)(content(Whitespace\" \
         \"))))(Tile((id \
         57eb2937-bc64-4ca6-aa4a-69cbff6e918c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4d268197-0f6b-470f-8770-fd71a197fd60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a92ff31e-ce08-4276-b348-ba995eea5f32)(content(Whitespace\" \
         \"))))(Tile((id \
         035b2d8b-4ff8-4d0b-b070-6685905e934f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e05bc474-e749-4782-99d2-8a9c4eed536b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d511d3ed-aba6-4558-bff7-b70c10ebc353)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d61dfc43-69c7-40df-be70-821915ec5a6e)(content(Whitespace\" \
         \"))))(Tile((id \
         51aa550d-4b6e-494c-8882-ecdca83b7b6d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         e78c4060-852a-4bf3-8a6e-6a426f83d21a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3f981c31-48fa-4206-8df2-dfbe205b65e2)(content(Whitespace\"\\n\"))))(Tile((id \
         d84fb3d1-ccdd-468a-9fa4-a812dcc3da6b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7b413b0c-8d64-49ed-adad-5e7a958e835d)(content(Whitespace\" \
         \"))))(Tile((id \
         a1bb0e28-e2b5-4b05-9a91-026a46bc6ef8)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         13feaf73-7c23-42ef-8547-a7f954551acd)(content(Whitespace\" \
         \")))))((Secondary((id \
         3814aded-fa41-4742-9c61-8672873190d9)(content(Whitespace\" \
         \"))))(Tile((id \
         7c010603-0931-4aba-b97d-1e78bada18cf)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72257f40-8e9e-43e9-93a8-681a72a73a7e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d6211309-5fad-48a0-afc2-912c236d13cd)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3e7b9b38-3864-4ae9-b082-d584421bf5d1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         779cd049-d7b8-46df-bd28-045723d170f3)(content(Whitespace\"\\n\"))))(Tile((id \
         623bc76b-0d9d-4326-a427-80ec31e640df)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab3bd343-fa9b-4e5e-8beb-60afbdc7ab99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab5693db-5b3d-420d-8a87-a7c946ccb4a6)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8430f505-fcbb-4501-ac2a-4baa106ad38e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8df4ccbc-96d2-4648-a48b-04863a0276de)(content(Whitespace\" \
         \"))))(Tile((id \
         2777a0bb-6861-4ac8-8d6d-b148aebd7dd9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57cb162a-affa-41f4-93a5-5155d45f37fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         359e0c25-7ccd-47e1-b4fc-abfa5f93b522)(content(Whitespace\" \
         \"))))(Tile((id \
         a383043d-2a25-4b77-ac36-67c2d48c0544)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c3339929-030d-4c51-af44-6943080ec176)(content(Whitespace\" \
         \"))))(Tile((id \
         575a2240-3e7c-473d-bfd9-8e745bdfd913)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9b77306-38da-475c-9cc1-e8e4df9925f3)(content(Whitespace\" \
         \"))))(Tile((id \
         b583d83d-3ce1-463a-ae67-9eefa2427859)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1fb7c2da-8a61-49c5-978b-4adf91d75f64)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c4a581de-6206-4bda-9618-4d0cd90306ac)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02d240c0-5798-488b-83d5-166322c0f647)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e28865f-daf5-4a93-a645-efaa22caafe7)(content(Whitespace\"\\n\"))))(Secondary((id \
         001d9f05-2b32-4650-a60b-69f1f12b265b)(content(Comment\"# Classic \
         patterns #\"))))(Secondary((id \
         f30e4d21-374f-4112-a447-150200a1bd03)(content(Whitespace\"\\n\"))))(Secondary((id \
         eacc6257-743a-4bd5-b55a-13fc88e4d659)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ff8bc7a-28fc-42e6-9249-b1e365123747)(content(Comment\"# Blinker: \
         oscillates between horizontal and vertical #\"))))(Secondary((id \
         8f6aec93-4dd4-451c-bbaf-78647f22ece0)(content(Whitespace\"\\n\"))))(Tile((id \
         b2ee0e70-0c36-4712-a7b8-076bf934d42a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         54aa819e-2f2c-4455-a455-e8f909a13321)(content(Whitespace\" \
         \"))))(Tile((id \
         87de7e38-2635-421d-977d-cc4241c08dae)(label(\"\\\"blinker oscillates \
         (horizontal to vertical)\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16e9fef9-e3ab-4aaa-92b3-2d137370284a)(content(Whitespace\"\\n\")))))((Secondary((id \
         b296cfa4-7bcf-48ad-8b0d-f2fc3064afe8)(content(Whitespace\"\\n\"))))(Tile((id \
         5aec1f02-cedc-43ea-9b9d-4eeadc5c298b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3d4b1b83-051e-498c-95e1-bab3449fc62e)(content(Whitespace\" \
         \"))))(Tile((id \
         8f3005cc-ee78-441f-9777-ddbab22d35d7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         396c5576-c82c-4264-b006-f6bccf4c33b6)(content(Whitespace\" \
         \")))))((Secondary((id \
         257a9d22-78a1-4fd9-9885-80b84d514253)(content(Whitespace\" \
         \"))))(Tile((id \
         92114d2c-dbfc-48fc-b764-85401b6ea6aa)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae53368f-3874-4bcc-b05d-81dd516707c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d6f7c6f-cc01-45ae-9fa5-e32ef207d6a2)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8ae70c1-7e9b-4da0-9b85-de870e205141)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f1309778-8fbe-4ca9-a8a7-53563aa91520)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         901c76df-aae8-4fdb-8369-6104eba984de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b29871ee-38e4-41be-837d-3fefbc87f55d)(content(Whitespace\" \
         \"))))(Tile((id \
         1afa1e23-5778-42e1-96bb-d2242cc7ad02)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         72419a15-2fa8-438b-baac-ba589b1b3908)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d2b4dd8-6b1d-4844-a709-247bf58bbb0a)(content(Whitespace\" \
         \"))))(Tile((id f7f29376-07f3-4fb3-9582-c16fddf07978)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         654bc738-557b-49a1-805b-aa8e90cf3ef3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc601307-a142-402d-b674-7bd5382aabbd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61bc05d5-b07b-4926-b159-e392a1d15518)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dc0b6c2-85c5-4e62-a73c-0d6dc28d9e2b)(content(Whitespace\" \
         \"))))(Tile((id \
         767e5ef5-ea22-40ff-9553-fb5000a05717)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         010ecc11-82ba-4f49-8eae-b7d0a5512bfd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8941438d-3abc-4597-81aa-ab7af1db9673)(content(Whitespace\" \
         \"))))(Tile((id \
         160dd96b-6e90-416c-9da2-3ef014de1b24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6d5003b0-eddb-48bf-a501-5391df5cc80a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f63f724-4285-42a7-88e2-6f6c6d422339)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71741698-ffba-4008-87a1-d3afba617704)(content(Whitespace\" \
         \"))))(Tile((id \
         e44c2061-34d0-4487-82b9-c9f7d98ea261)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         54ace8d9-c5c1-4f8f-b53b-487067b352b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1eeac143-7ed9-4e84-9d68-9b4474b696e8)(content(Whitespace\" \
         \"))))(Tile((id \
         9ee3b03a-cec1-4450-b98d-260b9fa13bc1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6da66fce-814b-487f-9302-533c3702ef03)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69373012-25e8-4bfa-87d5-7ac3ac351a00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb0be5fa-7cd4-4769-9a41-e2a3e5261863)(content(Whitespace\" \
         \"))))(Tile((id \
         7fef75d7-57e5-4979-bd4a-c40a51a1ef75)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         9b2d2c17-f199-40e5-ac51-3f5b41dc0cd1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af7ca800-5672-4fff-bb09-1a8323074999)(content(Whitespace\"\\n\"))))(Tile((id \
         0abed7c2-8a14-4683-a763-f41ca1a920f2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e293d713-e296-440c-a84b-aafcef502cdf)(content(Whitespace\" \
         \"))))(Tile((id \
         404a6ed7-fd49-4b9e-9b50-a2a61bdaebc2)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         483bbb13-8d43-4a4b-a3b2-4aa9abe660de)(content(Whitespace\" \
         \")))))((Secondary((id \
         297a8721-25ae-4de8-a038-bafb2c03cd69)(content(Whitespace\" \
         \"))))(Tile((id \
         60d3a607-6ed8-4e92-acb1-c8fa7a12c962)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f06fa6ae-bf8d-4c7f-8d19-61cb44f2af25)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0492a93f-4bce-499d-8e0a-c7766b83e555)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         436a72aa-a994-4619-8885-8a22c081937b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d5bfe6d2-18e7-45b2-a33e-58ff9bcd2583)(content(Whitespace\"\\n\"))))(Tile((id \
         385dbaf0-4687-43b7-8531-3f453a8ee440)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         001716da-a4cc-466f-bfad-cbe44f9a0a72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aca9a642-9210-43e6-92a6-2a114d92f725)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fa8840d-7468-469c-903f-94cd2c7a162a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09e05066-4705-48cb-86c6-79727453c7fd)(content(Whitespace\" \
         \"))))(Tile((id \
         7526b809-d4a2-440d-b9c2-bf11f07afff4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87415b0f-1d45-41ba-81ac-0806b2627c9d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         847f938a-bd08-4967-9f0e-c74e69d7718f)(content(Whitespace\" \
         \"))))(Tile((id \
         e471d3b6-2b4d-4c27-bb7e-b28f5a57df5b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7e9c7109-3772-481e-becb-70dd285bb414)(content(Whitespace\" \
         \"))))(Tile((id \
         9dd12cb6-004c-4dca-b8db-d7248fe7cad5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01d250a6-3250-4cae-b457-8ecf11abcc9a)(content(Whitespace\" \
         \"))))(Tile((id \
         402cf7c9-8dda-45ba-b1cd-f8e1d355156a)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1b442283-cdb4-440c-b5c7-ba2cd5f3e776)(content(Whitespace\" \
         \"))))(Tile((id \
         8175b23f-d981-47e2-92fe-078abeaa5bad)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f21ef1b-0183-4b8a-8bd4-57cf3d682884)(content(Whitespace\"\\n\"))))(Tile((id \
         2215f131-2e0d-40e4-b147-c4796ed71d0d)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56d22c7c-39d6-4596-a10b-c341052deb8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         502a130f-085c-446b-95cd-322044190bc4)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18f3d376-deab-485b-9db0-60fc05d3e88f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75e4dc6b-0d80-4faa-8dc5-a5ef7bb03387)(content(Whitespace\" \
         \"))))(Tile((id \
         849d44cd-be5a-47a8-ba4a-1608fc609a03)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52ba4459-8c9a-4e3f-819e-6e6616652421)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f5921cc-abd3-42d2-a6d4-c9e5a2db5be1)(content(Whitespace\" \
         \"))))(Tile((id \
         c43a4e5e-2252-4d71-8afe-b9063c7b2c0c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2e6f283f-8e1d-47bf-86de-37abb6e0f554)(content(Whitespace\" \
         \"))))(Tile((id \
         11da75a8-284e-4781-acf9-10563d80a142)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d90ec9b-506f-402c-888b-7ba9a3addb9f)(content(Whitespace\" \
         \"))))(Tile((id \
         b859c572-11d7-4c36-a460-5b73eea874aa)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc04735d-96d3-45b6-814a-c45edc187565)(content(Whitespace\" \
         \"))))(Tile((id \
         eb67ee94-ebd2-4f27-ae8a-68641ac45761)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98879c3a-6a46-41f1-ac14-616e1931853f)(content(Whitespace\"\\n\"))))(Tile((id \
         67c12a9f-e65d-4326-9ccf-8393afff3850)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2c14f91-7e57-4ca9-bb62-2d246459eee0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e0af756-cc57-444c-9bc9-43dc2ae4b40b)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de2c3223-a8ba-4cdb-a43e-69727fca31d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdec315e-b8ec-4be1-947b-cd9e7e356281)(content(Whitespace\" \
         \"))))(Tile((id \
         e645297e-38dd-4017-bc34-2be88d3a54aa)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4c6e2cc-2c33-4ffc-8933-7cb146ea732b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09c77916-f05c-472e-8d89-d636d2d7376b)(content(Whitespace\" \
         \"))))(Tile((id \
         37e81f4d-ac01-45bf-91e4-504aeec9de1a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0b487216-f56d-4e17-93e1-1a8c4d569cb2)(content(Whitespace\" \
         \"))))(Tile((id \
         32aa1d71-a43b-4f82-a117-970eceb25c73)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43dc0f8c-2cce-4bb2-a6ee-27d9ddf618c9)(content(Whitespace\" \
         \"))))(Tile((id \
         0ad1ee97-6c4f-454b-b21f-10aba938509e)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6259c837-9367-44d4-8e6f-edc47d392777)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fd8deabb-85ea-4b9a-b377-8abd1f195a4c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec34ca38-cd7a-4bdc-89c2-71baad95d49b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4118c33f-2668-4a97-82a0-64bf2871d137)(content(Whitespace\"\\n\"))))(Tile((id \
         2842ed98-b5c7-454b-815d-ebf61babfadb)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2583d440-6426-478c-ba57-d2c72836ced9)(content(Whitespace\" \
         \"))))(Tile((id \
         4647d3f3-f93a-435b-846e-dd0f29c9dbcd)(label(\"\\\"blinker returns to \
         original after 2 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1640706-1de6-456a-b385-05def3c36ffa)(content(Whitespace\"\\n\")))))((Secondary((id \
         34641a21-0f7e-4bc0-af9d-e4bd17c86cb3)(content(Whitespace\"\\n\"))))(Tile((id \
         2c153795-650f-4049-b2e9-88dc01ae0211)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b5969650-e273-4807-b1ba-6d84e1e75e52)(content(Whitespace\" \
         \"))))(Tile((id \
         89285106-25d1-43ec-9752-84fa31b247c7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fc10c8c8-0f5d-478d-8202-56bb2bce9798)(content(Whitespace\" \
         \")))))((Secondary((id \
         265cdc95-d164-4663-81c0-3306554c0135)(content(Whitespace\" \
         \"))))(Tile((id \
         88c2e92e-dc53-4498-b9b2-68a2ee393068)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08a21803-c8bb-42ef-99af-88338948dca7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c3842f7-4d8d-4c63-a03c-a7d28ed79683)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ab943a1-7511-4163-81c4-8a06346ee222)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b58d114d-e589-4abd-b0de-c2a8fe23bf00)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7639cf80-a004-4341-91fb-cdf27b3b0125)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         125a6daf-1d10-423b-bd71-db82b586771c)(content(Whitespace\" \
         \"))))(Tile((id \
         e78b476c-d9c6-4b98-a70b-8e621f87d2bc)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         20e8c066-ade0-4755-95c5-934c1410ee32)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfca9e0f-f372-4082-ae54-99bca45bae09)(content(Whitespace\" \
         \"))))(Tile((id a64e3656-9153-4654-9266-efb2e22c2db4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ef23a5f-63b9-499e-b6dd-aba6a9734513)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6c9a1b40-b180-47b0-88cd-103e92b878e0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbff1d47-1fd9-4549-b2b9-5b96308c315a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08e42c90-0460-4011-8053-9f56d7ee5251)(content(Whitespace\" \
         \"))))(Tile((id \
         670f92e8-cfe4-414c-b5a1-baaa60df8c35)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1ee39f1b-012e-4ac1-accf-51e685042294)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d828d8d7-81a7-43c8-8d13-cf47cec41ada)(content(Whitespace\" \
         \"))))(Tile((id \
         fdb62244-8242-44f0-af68-a6e7a36fc75f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7618e56d-ab57-4a0a-b548-42089c41285b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5ad78dd-ab49-4c6c-a89a-181a6019cb91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         957340dc-da5c-4bd3-9419-dd55f55eda6e)(content(Whitespace\" \
         \"))))(Tile((id \
         f3157a69-36a7-4bd4-83e3-dfb996ae8c82)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b3cbd49e-8f7d-470e-9d8d-0bfbc9b375d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eaf095d9-1026-4092-9ed9-4910bafcfd3d)(content(Whitespace\" \
         \"))))(Tile((id \
         767cc701-9166-4a07-b19f-15086cff506c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         398c07f8-87bf-4f48-b3f2-9701891c4c1f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a395246-ff6c-4a79-9ca7-d92fa5a24149)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05c314fd-ab8d-4866-8b0d-9d26586da8cd)(content(Whitespace\" \
         \"))))(Tile((id \
         0cf68872-5dbf-4db3-bcfb-2fccb4667aeb)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         3c63a62b-2ae6-4a65-b0af-8ab60803ecd0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a317f72b-cda6-4edf-abb8-9a4a0b8f7449)(content(Whitespace\"\\n\"))))(Tile((id \
         b684fe2f-f870-4b97-adb9-323e0a82c700)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         28e3a870-ac7b-49fd-8ba8-0660a7d37d39)(content(Whitespace\" \
         \"))))(Tile((id \
         d0f064c5-9de4-4e2e-b354-8a50688897c0)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1679e200-ed10-459d-b0af-936a5b6b06d8)(content(Whitespace\" \
         \")))))((Secondary((id \
         1e576fb1-ce7c-473b-bea4-a79c86f62c96)(content(Whitespace\" \
         \"))))(Tile((id \
         609fc6d3-9669-418b-a72e-d0d30bde3400)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e80393e4-48bc-4b51-811b-9d5f4b308458)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e2b8edec-b888-4b6b-b628-d8590dcdff20)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b72087a-a958-4ebd-9458-36b7c8783f72)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50baee17-f080-4748-8b23-97af908a165c)(content(Whitespace\" \
         \"))))(Tile((id \
         c5d682dd-718f-4088-a9cc-57db2c1b3301)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5486f9db-bffa-4215-88cc-ebe62b448854)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3af2698a-cd83-48c7-8bcd-4db0710f7384)(content(Whitespace\"\\n\"))))(Tile((id \
         0b7520d2-cbcf-4811-8b6d-a7573443d8cf)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0c28a86-b6ac-44dc-b4d7-a8c032c992aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         512c0c8f-da15-44e5-b2aa-1f93a76da28c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4b40d57-cde2-49a4-aaa2-5e84495a6fe6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de3fd9e2-d2df-4957-9727-a2559326b87f)(content(Whitespace\" \
         \"))))(Tile((id \
         ccec4694-6526-4e3f-a61e-47127cf7e0e7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f6b6589-bb39-4412-8026-be4f3904fd63)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d03d9841-c6b7-4f96-aa1f-46877b75dfcf)(content(Whitespace\" \
         \"))))(Tile((id \
         6601bacf-8523-4b56-b6cf-e61dea3e6de4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0cf4f54b-8e16-4675-8b97-14433ed26cdb)(content(Whitespace\" \
         \"))))(Tile((id \
         edfae626-23f6-41b8-bfeb-dea1d123e112)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8964af5a-626f-4d86-b542-2ba6ebe0bf24)(content(Whitespace\" \
         \"))))(Tile((id \
         ba4c74da-7de2-4b57-be0d-025d79d80e1e)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         227548ce-ca2d-4901-bffd-6881fa833950)(content(Whitespace\" \
         \"))))(Tile((id \
         9268dae1-87e6-46da-8b88-3e9cd852a0d1)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62124acf-dc4e-4012-890e-2ee748171795)(content(Whitespace\"\\n\"))))(Tile((id \
         436b0ba8-78f3-475a-87e3-df1824a4198f)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d8bf7bf-d413-4836-9b08-bd43384ed479)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de5c0b3b-544c-4ad2-88fb-3d3a0641b2ea)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b62cab51-063e-4995-9394-f2e598365064)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1e5c48f-8a2b-419c-978e-a188ffea687c)(content(Whitespace\" \
         \"))))(Tile((id \
         f27028d8-2887-47b2-bced-befd59d5c34e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb7990a3-5157-429f-bdf4-e19a69575269)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2834ab73-c465-448d-9eb7-73c556d49acb)(content(Whitespace\" \
         \"))))(Tile((id \
         2af47915-3ad3-45da-8df3-49d6f48f51f8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb77c255-64f6-473c-9065-bc6fb1ac1326)(content(Whitespace\" \
         \"))))(Tile((id \
         d08d6da4-ac21-4839-9239-60589cf7e8cc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         626c796d-73da-46ce-9de6-ee8933c31ece)(content(Whitespace\" \
         \"))))(Tile((id \
         d0565468-8176-462f-af4b-21988be96e8c)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ca37df06-d114-4aa6-945f-6dac7a10cb5e)(content(Whitespace\" \
         \"))))(Tile((id \
         cac0f23f-3fbb-4128-93ff-dd80e18bcc82)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d620448b-aa7e-4118-adcf-b37d8d05f537)(content(Whitespace\"\\n\"))))(Tile((id \
         1745dcef-2a22-41ee-8665-20ef700180e5)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be712859-41bf-405e-83aa-e94a8a0da1f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9a2c5778-2f92-42a7-95c3-effdc5484817)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c435b87-a7c9-4835-9997-eaf18f9022cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a649c15a-29e7-46e1-9b14-2a620d49bb83)(content(Whitespace\" \
         \"))))(Tile((id \
         78271ee3-e508-44d0-84e2-0f8b472f684f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4b3d32b-68b7-4e24-be85-12abc08fe538)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9dcdd7e4-5504-49f4-9c1d-98ba25573a13)(content(Whitespace\" \
         \"))))(Tile((id \
         e7c0fa67-fcd2-4912-ae0d-c8ebc4c9bad9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         02d6e361-ed44-4731-8653-7228eb9a0e5a)(content(Whitespace\" \
         \"))))(Tile((id \
         0e9e150c-27bc-487f-828c-05e4a994e84a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63bb92fa-d881-43be-83d8-dcc26cb2acf3)(content(Whitespace\" \
         \"))))(Tile((id \
         ed989921-67e3-45f0-9f33-c336d31c0ff8)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc9797f0-0f1e-45bf-ae1c-068113ee2be9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e6120873-6cb3-4f03-ae93-6f386ba95f06)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a0b93b9-b427-491d-b66b-3dd21e20f93b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7eef124d-b606-4fc8-99ff-209929fd82b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e1c3bdee-0565-40cb-947e-b45d75b5f181)(content(Comment\"# Block: \
         stable 2x2 square #\"))))(Secondary((id \
         20b24e2c-5bd8-4b4c-a15e-972f03c5999c)(content(Whitespace\"\\n\"))))(Tile((id \
         57c942fe-20dc-4cd6-9f2b-586ee5975ec7)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34e3bbb4-6f6b-48ac-80a4-926062802733)(content(Whitespace\" \
         \"))))(Tile((id \
         a33d85c7-ee2f-4b29-aadb-a962b33a00d1)(label(\"\\\"block is stable \
         (still life)\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         83b69575-7122-4285-90d2-3e51c02cdf03)(content(Whitespace\"\\n\")))))((Secondary((id \
         79a54634-996c-448c-b8ce-ef6da1cf2308)(content(Whitespace\"\\n\"))))(Tile((id \
         db7026d5-a387-4afa-97fa-03bc30aa62b8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         992739de-7860-42fa-a7d5-0689ddb4e1cf)(content(Whitespace\" \
         \"))))(Tile((id \
         0a26f14b-97c2-43c1-aed2-82816e8cdaca)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         14e742c6-4e54-41a4-9e49-42790f4bcdf8)(content(Whitespace\" \
         \")))))((Secondary((id \
         5b9f4937-03fc-4434-886c-f5db6e3bb5fd)(content(Whitespace\" \
         \"))))(Tile((id \
         2003df3a-de5b-4441-b4bf-3022a31b9aed)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         249dfdf5-a5b1-4713-a07c-7a8565a2b6e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ba1144ca-3c6d-4ef3-b2d7-7a21c5a2d64b)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a78e152-0fca-4cdd-b500-95f7b0226de6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d8495ab-5347-4b2e-965a-02542177acd3)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f19e7efa-79f3-4ff7-b011-138d7c536aa4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0061ec39-092b-48ae-abd1-4311a773d2db)(content(Whitespace\" \
         \"))))(Tile((id \
         ae8e1c40-de30-430d-b81a-b7a09b07b47e)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         df5f1992-9836-4bdd-912a-91b61351b554)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48652ac6-9d37-4100-aa27-db1a468bec74)(content(Whitespace\" \
         \"))))(Tile((id 8f2831da-b8fb-4f3f-8558-5ecf29d8132c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea205d02-0d90-4320-9a31-f686737aa8fa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d47cc7d9-a2db-4820-9d15-122a9cf36c7a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         592c5e79-7d2d-4336-8fdd-666b2ed604b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf8ae7f8-93af-4358-9eed-597a5a719d96)(content(Whitespace\" \
         \"))))(Tile((id \
         e3114c55-fc64-4ae9-b3c3-fa5751bf6e92)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         100a5af0-6d24-4905-819a-b79504693dfc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66a9d2f9-8dca-4b3b-98f8-dcb64d7280f5)(content(Whitespace\" \
         \"))))(Tile((id \
         aae454b7-7e85-404c-9e44-b538937d1a40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c209eece-e853-4b64-82e1-e5e719fbf464)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         528d2ec0-2817-44ce-b3cb-c55e971423e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8da8702-4bd5-4d91-905b-e706acd39f0f)(content(Whitespace\" \
         \"))))(Tile((id \
         32ea1c45-b342-4cc1-af31-fc02cd8ae799)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         96fd0a9f-75db-4c58-9102-fcb1f0dadbc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bbfa8cb-79d1-420e-9ea6-2e3120b8d2ed)(content(Whitespace\" \
         \"))))(Tile((id \
         5b18a675-ba6d-4539-8fb2-bdc265934ed7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7dcf9577-54e8-461e-a66f-2e9bf3112594)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ef77261-0768-4379-8c8b-8d41104edd91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b02a15df-424f-4ca8-865c-9952ea97819c)(content(Whitespace\" \
         \"))))(Tile((id \
         e06edad9-3028-4d96-9f45-d99c4aa8a82c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f95814d7-7235-4e4e-9f0a-31602b0e3d2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7930446-b989-4739-ae7e-baaaaf5bb5ac)(content(Whitespace\" \
         \"))))(Tile((id \
         2e3a5eb2-2417-483b-ab1d-c9d249ad7fa3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a874affd-b6ad-4fe6-a17c-e522438e751d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1bba2879-7c81-4524-8375-ade3af2a6d1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         118bf784-4ae4-4623-964c-ab2af2a6b56c)(content(Whitespace\" \
         \"))))(Tile((id \
         9c21e0d2-65bf-47b2-a40b-b37bec160a4c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         29aa5aa5-f067-462b-8fe0-a77f6d24015b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c940ffa7-4fd8-499a-9f7c-974de9feb34e)(content(Whitespace\"\\n\"))))(Tile((id \
         77d8820e-feed-428f-8798-bd8dc066696a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0dfd15ef-a876-4d9d-8116-b4557ce7eb71)(content(Whitespace\" \
         \"))))(Tile((id \
         04f8b30e-c0cc-41d5-8257-b8287a2b1430)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d29e68f-4a2b-43a7-982d-9a2d09b43a69)(content(Whitespace\" \
         \")))))((Secondary((id \
         ef5909cc-31dc-454f-a245-7ab0861906b3)(content(Whitespace\" \
         \"))))(Tile((id \
         910a4045-4a79-43af-827a-a34ba9531e3f)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         710d7d41-5485-442d-90e2-a14a5587ddc8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a9499ed-c3cc-4aca-8ff2-1be5161fca53)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d6f8ff74-0b60-430b-9dac-c6797c9feb05)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7b2ac907-4e92-4ae1-8021-cdd807b302b7)(content(Whitespace\"\\n\"))))(Tile((id \
         d0eca74b-3440-422b-8595-0a3aa30c06cc)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         670c590c-1146-41bb-a0b9-9b6008d66eb2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f592cd82-eab0-4f97-866d-6e764891b13c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1c9a2bde-953c-4700-acdc-5cf08f132247)(content(Whitespace\" \
         \"))))(Tile((id \
         05408e74-bd9d-46d2-93bc-ba6a77181f1d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6430d6a4-9f5f-4ccd-aa47-c3f97cc4ac76)(content(Whitespace\" \
         \"))))(Tile((id \
         755d5550-30b6-4c8b-9bf0-fd55809bbd96)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         813ebbe2-bf39-46e6-8000-edd81f7709b3)(content(Whitespace\" \
         \"))))(Tile((id \
         8eec7eec-b046-47dc-801f-1bee361eb0b7)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e9712ac-2018-4206-9571-034a640e7a61)(content(Whitespace\"\\n\"))))(Tile((id \
         ad82f013-8f0e-4a61-b4ee-6e47d15b2fc1)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cde53646-4a6e-489d-a923-49d1f110adee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         26159707-8474-43f1-b5cf-3f531a494276)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c74f080-28b5-47b3-99fb-af98976af153)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8176606-eeb9-4192-8dd6-bf7e256964dc)(content(Whitespace\" \
         \"))))(Tile((id \
         9a73d252-5092-4988-bd6f-793ad8962cfb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a26acf2-c52a-4bd6-8979-ae940f231276)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47b57f60-7a91-4fd2-90ee-fb8ad58e5601)(content(Whitespace\" \
         \"))))(Tile((id \
         cf2fab5b-97a5-4017-97f0-d1747b0bf396)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a183a7be-bc58-4f2a-8357-40ace224f3a3)(content(Whitespace\" \
         \"))))(Tile((id \
         500a084a-fae9-4e04-a167-840f4893dde4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         582eb2bf-cc16-4c2c-9634-408bce71e403)(content(Whitespace\" \
         \"))))(Tile((id \
         07c10de1-beeb-49e3-b0d6-83a1a108db63)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08a54d8e-008f-4736-9614-ee22ff34a281)(content(Whitespace\" \
         \"))))(Tile((id \
         6a7cd61b-9102-482d-a900-964bb93e5dcb)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f839c5ff-eab6-44ce-b7c3-e376183ca837)(content(Whitespace\"\\n\"))))(Tile((id \
         f2e9e347-33a7-4202-9177-2eb23ab7a0ba)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd036593-1671-4ca4-a1a0-3536f81ef504)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d12ed341-0710-49fe-bd6c-3d8f228cf87c)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d58c81d-bea6-4101-a474-ba717f6876f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33d5aca4-e5bf-482a-927b-6020814f0ce6)(content(Whitespace\" \
         \"))))(Tile((id \
         a055c834-4ae2-4005-9e7e-e7cb67b42f89)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b6f1c72-6f13-4477-be18-7af59b83ff2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e699305-abf0-4675-a3e5-005977766e5e)(content(Whitespace\" \
         \"))))(Tile((id \
         fad8f921-aadc-4015-a05f-b3fa378db568)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c209bd3f-db08-43bb-a347-536147346a98)(content(Whitespace\" \
         \"))))(Tile((id \
         686269bc-ee39-4321-8427-692675848cce)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3db2315-7222-4fa3-b6ac-f20c68480c41)(content(Whitespace\" \
         \"))))(Tile((id \
         31fde49f-4eb5-4344-b0a9-7a1b48acbdd9)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         186435f2-5d1f-48e6-8f1c-6a495c082e12)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3005b4a3-43d7-4ce1-83f8-23dd9ebdee7c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dbe3ad9-788b-4f8b-8609-305222959786)(content(Whitespace\"\\n\"))))(Secondary((id \
         c36ce5c5-dab8-435d-a71b-8a7df50e336a)(content(Whitespace\"\\n\"))))(Tile((id \
         74a35310-2c9d-47cf-ae4b-c00848fae755)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb91f015-0fc3-42de-b711-49db65368bb2)(content(Whitespace\" \
         \"))))(Tile((id \
         3227f1f2-136e-4c8a-866b-5449d12ce0af)(label(\"\\\"block remains \
         stable after 5 steps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0720f76c-007f-4cba-ae3d-02d26c2b3891)(content(Whitespace\"\\n\")))))((Secondary((id \
         1e8d25a3-cf81-4f0b-9600-21261ade3fdd)(content(Whitespace\"\\n\"))))(Tile((id \
         a1dd785c-9726-44cc-8b9a-b2b01e0f8ada)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5497015d-aacc-4bbc-aba9-bd85e7c576cd)(content(Whitespace\" \
         \"))))(Tile((id \
         efbd3d4d-ae45-4de8-bdaa-4ccb1b870999)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9c7631b0-0c7c-4c00-8360-50125bcd2aa9)(content(Whitespace\" \
         \")))))((Secondary((id \
         ba005593-6df8-470f-a3d7-d34cab1cd4d1)(content(Whitespace\" \
         \"))))(Tile((id \
         65862ae5-2b64-413b-a9b4-21eefbdb7c97)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0610c87-90c6-4158-b8f0-e33536d8ecc6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         68d4b2f5-3e12-44f7-9b08-da4f8bee6a0e)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ca9b1d9-f1a6-47c3-b11e-bc72709d2b16)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2be4e6a-7972-4e52-8913-4b09240f231f)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fe7d178-f0a9-4f19-8937-46dcde617482)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c564d0d-6cfa-4009-9284-0530c7c62429)(content(Whitespace\" \
         \"))))(Tile((id \
         cbc63e76-3a8f-4799-8b68-5cb87a766ddc)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         22e0f1b3-854a-40b6-90dc-6a9de155a67a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2b3cb79-228b-43c0-88f2-3351f2cb3deb)(content(Whitespace\" \
         \"))))(Tile((id bab1e523-d203-47d5-8919-45ed3ad4a49c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f8748b76-2fae-48fd-9cca-7b8ca7aab0d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6d96c77c-e1e5-4045-82c5-185efc1962cb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         349886f5-216e-4368-bd7f-954be6f6f588)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c722c376-8acf-46fd-96fa-2ea013436fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         db9c38db-2ecc-4d73-9ff8-de631dac8356)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         263c3abb-b9c9-4025-82d2-c0040fae222c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59b3128f-6ad4-4b89-ab7e-236a14c4da63)(content(Whitespace\" \
         \"))))(Tile((id \
         5dcbd544-5ae3-4909-856f-e3a88ea797a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         95558ead-94b7-4b61-8ec0-107544d22020)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d0128d7-b144-4d21-bb25-3e2470395dff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d472b58e-b421-4419-a029-880ed749e9f3)(content(Whitespace\" \
         \"))))(Tile((id \
         da6059b8-e922-4beb-a854-75d28265c789)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         34e89d26-867f-4d4a-80bc-396095ebc7f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20b902c8-ed76-4db8-85d8-6109fcc13510)(content(Whitespace\" \
         \"))))(Tile((id \
         62521182-16d4-4599-aa84-bd0a15efb531)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         80a641ac-2700-4723-a8cf-4f4523b2e9a8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b89b02ad-777f-4ab8-89a2-c7dad58b58f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4e9d925-1e2f-4355-aafa-b374063db32e)(content(Whitespace\" \
         \"))))(Tile((id \
         2dfa8380-16f9-4f03-9245-08b9076e06e3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dda620b1-c1f4-4692-97f5-363d90c2b120)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d20918c-208e-40cc-a995-f8fd2eb86f94)(content(Whitespace\" \
         \"))))(Tile((id \
         4f0a7358-533c-4b0a-ac0f-66588564f979)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7c556b8e-b8d4-4617-8018-056e403819c1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b16247c-eb0f-416f-a4f1-56caadfed7a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7f25bbb-52c1-45e5-b796-885a9e8e8382)(content(Whitespace\" \
         \"))))(Tile((id \
         84c0c466-efb8-47a1-9fd5-5b66ab54012f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         84b1ffee-c941-4521-a605-f1b76759a9e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9ab318d8-f0c0-4739-830b-b41414268358)(content(Whitespace\"\\n\"))))(Tile((id \
         523e0efc-422b-4095-8787-7888fac5f7a8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fe15a648-1e6a-419d-95ac-ad49e448ea64)(content(Whitespace\" \
         \"))))(Tile((id \
         bc70e016-3489-4c72-8d6e-73049fad729f)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6114899a-fa37-4be8-b487-312d39e9a9c4)(content(Whitespace\" \
         \")))))((Secondary((id \
         9b883b7e-f227-4b6f-8328-28c41af92822)(content(Whitespace\" \
         \"))))(Tile((id \
         c9d79f8f-8aad-4874-a3d9-005a7942a733)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82761a92-3971-4bed-a631-11df24788f58)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2914d43-298c-4294-810b-55f3bc7ce311)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2381bc4-414c-485b-9941-3d651f6ad74a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea5df2bf-bfbf-4506-966f-13542fab2f45)(content(Whitespace\" \
         \"))))(Tile((id \
         f6c4582b-a661-4ca0-8753-af4cd3d439dd)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3c569cd0-f55f-4936-b624-d5aadf030b21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c31ef512-94ef-42f4-996b-768aee59f9ea)(content(Whitespace\"\\n\"))))(Tile((id \
         9a87ed22-c665-472d-b6ad-32a5858749c2)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff964020-3ad7-4de9-b278-7f474a6dc5a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b15b56c-f448-4662-9b77-0c2ed8e65edf)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         23d85d3b-ac3e-4faa-a01f-7d03637ca001)(content(Whitespace\" \
         \"))))(Tile((id \
         9ba23bed-d29e-4ac2-a5d4-718c187b4933)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fd86056-155c-4a60-84a7-f72a1c180654)(content(Whitespace\" \
         \"))))(Tile((id \
         9848ab0f-e7e3-416c-b382-a32246c02b4e)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6542c643-7fed-443b-95e8-21eabad1cf37)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8938d672-d3f0-4437-816d-f58378672051)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         496683ea-1bee-47cb-9141-df678a414ec0)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c3c2089-3f17-41d3-ac99-90db16b462b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         4abbd07c-046d-43d4-98c4-73ec95490c9e)(content(Comment\"# Single cell \
         dies #\"))))(Secondary((id \
         e9b9283e-26db-42e8-b819-59a995036964)(content(Whitespace\"\\n\"))))(Tile((id \
         cff33687-7bea-4823-9f0a-1c86a67a6ca7)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         634a7d0f-d913-4e77-b37b-6f33ee4d8bf3)(content(Whitespace\" \
         \"))))(Tile((id \
         5d3b0b38-d9d0-4c06-b5ce-3bf7270403da)(label(\"\\\"lone cell \
         dies\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         844f9b1d-fbcb-4c2d-a3af-dc96497669c6)(content(Whitespace\"\\n\")))))((Secondary((id \
         578ec6a9-fec2-4274-86f0-136c5e6bdb2c)(content(Whitespace\"\\n\"))))(Tile((id \
         fb744a4a-8e62-496f-b4a1-d4c84a81f2df)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         97d03e29-55dd-44f0-bb97-198093d0a895)(content(Whitespace\" \
         \"))))(Tile((id \
         464be5a0-a66a-44df-bf40-24169ed0bba7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e96e029b-65d3-4528-8ec6-98e6f36f950b)(content(Whitespace\" \
         \")))))((Secondary((id \
         fa6292cd-4717-4e71-b736-f32478a6e527)(content(Whitespace\" \
         \"))))(Tile((id \
         1a72460a-5dbd-46e9-8da4-736993f2267c)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98173a83-6e49-41da-92ae-eb27f7fa478e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1a770273-bd06-464e-b7a0-29b3550c226b)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b82c7dac-4392-4251-b215-92756e06fc78)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         032d9586-3a4e-4428-98d3-5220bf7c9481)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f72e78b9-59bb-4b08-99d5-2e44c2e9a1cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a5bfd2a-bfdb-473d-a1bc-290a6c917ddd)(content(Whitespace\" \
         \"))))(Tile((id \
         42299231-c3c4-454a-9b80-6eca74fe3a04)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c05f9d8e-b53a-48bc-a080-27e654cd383f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57de0003-da8b-4e0e-bf8b-a470a2eb9afb)(content(Whitespace\" \
         \"))))(Tile((id ec42dad3-c138-442f-a7fb-a56e6dade8e2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         415310f2-99f9-41ab-8bdb-147d468a81d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0cc3c3fd-bf24-4d07-8c10-02f254e6b2bd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcdebcde-caa1-4088-8d6d-c0d6c313ae1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43a37e85-337d-4fb4-835f-91fb38dfb01d)(content(Whitespace\" \
         \"))))(Tile((id \
         c65e6ed0-e6a5-43d2-b043-1027d4337dda)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         faca2830-b1e9-4f41-bf26-e13a459a46b2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         27607296-0856-460a-855c-155f4c3b632a)(content(Whitespace\"\\n\"))))(Tile((id \
         bbcbd1ce-1332-4c3c-b547-e2ea1929e9f9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1d225d8c-7397-442f-bee4-35583632b6df)(content(Whitespace\" \
         \"))))(Tile((id \
         852d7dfc-c303-4b2c-a416-810147f04b89)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6a1f7898-094e-43b7-9da9-8d7c6b8b5272)(content(Whitespace\" \
         \")))))((Secondary((id \
         1ff06821-9437-4687-b766-c4dc51eae214)(content(Whitespace\" \
         \"))))(Tile((id \
         8a2b90ae-95ae-49b8-abdc-a0e3b820671b)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6796c9a-5049-4a88-9286-aa31742847ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d7953a9-6800-4d1d-bee1-2d0711717b83)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c9ff39e7-6983-468b-b116-1f9dfff5c054)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         84af88a3-2238-4768-8c4a-9e7c1da55bea)(content(Whitespace\"\\n\"))))(Tile((id \
         d5bfba14-f5a3-4c38-b5c9-8ac24e9d9881)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d660daf2-15a5-4c3e-9279-3c7562acb16d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f002ab0c-57d8-47cc-90e6-dc94f5ffaff2)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ff262f5a-e6a0-40d9-a22e-a27e9e573153)(content(Whitespace\" \
         \"))))(Tile((id \
         c05f8ca0-3c9f-41d6-9eea-b21c548c2aee)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4420054-844f-4323-ae5a-3b8f62c80702)(content(Whitespace\" \
         \"))))(Tile((id \
         4cf79018-a320-4606-839a-0229b480d516)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c85722ab-baf3-4577-a2aa-d155aa3ca3c7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e39f7c89-0e00-42d1-a1e6-4685c2859b40)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         120c6af9-8489-4832-a571-395b59da7cd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         43210e37-5eab-436d-9126-9a2f21d2fa53)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ca00cad-4b5c-4d20-bebf-c8c5ad809819)(content(Comment\"# Two adjacent \
         cells die #\"))))(Secondary((id \
         d9129ec1-a567-4ed5-8cd6-6223671d64e5)(content(Whitespace\"\\n\"))))(Tile((id \
         a89b5c7f-f6af-4866-a14d-c0cc6d8167e8)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         988fff6f-b2b6-4772-be01-5fcda38b7def)(content(Whitespace\" \
         \"))))(Tile((id 08b8c73f-55bf-4b0c-8e4e-7345209b9b13)(label(\"\\\"two \
         adjacent cells die\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89f0b5b6-8bd3-4a69-b147-b7bcfda17d07)(content(Whitespace\"\\n\")))))((Secondary((id \
         67ce2b1b-8d93-46d0-90ac-abc783934ca9)(content(Whitespace\"\\n\"))))(Tile((id \
         59ab76c1-f376-4d3d-af10-94eb1aef9b8e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e9579338-dcf8-47bf-ad6d-bdefd30063eb)(content(Whitespace\" \
         \"))))(Tile((id \
         b1e9005d-2678-4019-84d5-afed9c98bef9)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00ded417-dcdc-4597-b7db-85ab4f6e4c27)(content(Whitespace\" \
         \")))))((Secondary((id \
         141ac058-bcc6-4cc5-9cdd-1764bef471f2)(content(Whitespace\" \
         \"))))(Tile((id \
         9aeaf2c0-6f0a-448c-ba09-c85bff975515)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13e75fb3-776d-46fd-b434-b86b8b8ae7a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3f8b18a2-a7af-477f-b84b-44988fd55dce)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3188ffc1-0d5a-4d3f-8a0a-9ba29b07f348)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e488d2fd-d4a5-43a5-80bf-2fcb9eba1b3d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a6a1782-de9a-4009-8d9a-ee19deee6a9a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e7b92a3b-4dad-4456-93cc-f3ed6d59a80d)(content(Whitespace\" \
         \"))))(Tile((id \
         6e2d3750-3cfa-425f-82a8-570281beb9ca)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e0d39b16-4b5d-4e6f-9069-3a9ba359cdf8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89e30019-c33d-4349-b63f-a74fcfa7e585)(content(Whitespace\" \
         \"))))(Tile((id eeb6e178-3f6a-4351-90a3-2aeb731f3bcc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce6a7066-c460-4f5e-bc98-5f2c1947a961)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fa86e88c-8bc1-4101-95b3-1956e4042de5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebfa0459-4f8a-4cea-b4b5-34546eefdcf5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9f7fc9e-585c-4570-a149-693088bad841)(content(Whitespace\" \
         \"))))(Tile((id \
         57a19df5-dfdc-4faf-9728-5f93d4240cbd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cdfae0ff-a5b5-4f04-8da8-3f69fac0e1f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7793548-3383-49c1-bab4-8c4639764ca1)(content(Whitespace\" \
         \"))))(Tile((id \
         0480b72b-b9bc-4e5b-9de1-ea49f3b90447)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6dcfbbac-89c7-4dde-a565-4dee09421752)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c4bfa2a-3977-4ea2-8e15-911954cc8f6f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0d4410d-0c07-48f5-aaac-6d3b51b0f00c)(content(Whitespace\" \
         \"))))(Tile((id \
         03992291-ea8c-455a-afc1-8815b0e7f562)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         d40974d5-ce7c-44e5-b14c-46c887347351)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3725355a-f88d-4e86-9904-e3b5db929b15)(content(Whitespace\"\\n\"))))(Tile((id \
         f7f0d453-2e16-4553-aa3d-9913751d9138)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         79b4d657-4d8f-4101-8c74-44c28e545ff3)(content(Whitespace\" \
         \"))))(Tile((id \
         54246e64-9c83-403b-961f-c177e53feb7c)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         945071fa-e058-4558-ba53-2168c6df5415)(content(Whitespace\" \
         \")))))((Secondary((id \
         340ca5e9-447c-4a03-a856-e1be3c957bf3)(content(Whitespace\" \
         \"))))(Tile((id \
         f20557bf-2b5f-4a0a-9ca1-b04c89b2f16f)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fbea9e4-c493-485a-b31c-d14835ea1046)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c623cf9-58b7-4468-b76c-f7f2fe4406f3)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ab2255a9-e3d7-44d9-a193-100f7dd9c625)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f06da137-fa15-4f7e-a182-95f51115bfe2)(content(Whitespace\"\\n\"))))(Tile((id \
         3704c465-d8ba-4cb3-829f-8ac7a2b2cd00)(label(countAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89880aa2-2475-46d6-bc8b-a47fb6b8f0ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20c26779-ee02-46ee-a556-c3d3fbf2ffea)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4419aa07-115a-45bd-9b05-510351eaaee5)(content(Whitespace\" \
         \"))))(Tile((id \
         5edc80d6-c254-4545-981d-3dd546f25f10)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0632caad-0b23-45a5-a920-1dea27a8a972)(content(Whitespace\" \
         \"))))(Tile((id \
         62ae63d8-695b-467e-9fe6-ca386d8aceb0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ce0b46c-47b9-4b65-9570-0a5802b19381)(content(Whitespace\"\\n\")))))))))(Tile((id \
         00fa833c-b07b-49b7-b00d-1dbf58ed56dd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84524277-fd7e-484b-84a3-18b0c79ba059)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e8c5e14-f661-482e-88b2-6eb006adfff0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ad52291-eb1f-4ea3-b0d9-cd875d433290)(content(Comment\"# Simultaneous \
         update test #\"))))(Secondary((id \
         6203f673-6915-44c6-81ad-b004ffad8d19)(content(Whitespace\"\\n\"))))(Tile((id \
         049d6414-49fe-44af-8c4d-76b0ae4b5b41)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f073ded-799d-407e-b8cb-d60c3cba706f)(content(Whitespace\" \
         \"))))(Tile((id \
         6ceabd7e-1d1a-4252-9349-0b27f9727f58)(label(\"\\\"updates are \
         simultaneous not sequential\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43181fe0-f1bc-4189-ab79-9ca44575071c)(content(Whitespace\"\\n\")))))((Secondary((id \
         5f6f7681-933d-487b-8b19-4edf28beceeb)(content(Whitespace\"\\n\"))))(Tile((id \
         762e78f9-78dc-4261-9361-e1eed8105e9a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3ccb0f6d-2b93-4d74-95d8-ac7df5e09198)(content(Whitespace\" \
         \"))))(Tile((id \
         57cecee1-267d-4191-8677-f8b59d9cc280)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         faf3ad40-3d9d-4637-8f60-2ea5ce04a87e)(content(Whitespace\" \
         \")))))((Secondary((id \
         a279fe58-3431-47b5-9148-428932f0b1f1)(content(Whitespace\" \
         \"))))(Tile((id \
         a62c81e8-b3f8-4ca3-859e-9cc31141e0b1)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70d22eb9-7e4d-4561-94af-636365f90567)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1286dec3-f265-48bd-8876-3faf0edf3093)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8702ebe-390a-4683-aa99-188817f0bbac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d020367-7720-42b1-9a35-f1715969b6b1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b797e60-1ceb-4fbb-8dde-255de7146b2e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5f75a05-22c0-4aa7-ab5e-36cdf1c30527)(content(Whitespace\" \
         \"))))(Tile((id \
         63752f8c-28f4-4c38-814f-31e6af2d0b64)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8dec6938-8f27-46b8-b47a-c6b00dc3be4a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f773a6c-3698-44fa-9891-513f07246fb5)(content(Whitespace\" \
         \"))))(Tile((id bb68ff26-c8fc-468c-8475-279dcbb9dc48)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         00a8a63d-7a55-4aed-bb21-267e36d9cc41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2ef5e6ab-d64c-48b5-977e-a70ba6fb7c85)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86d713f5-df37-4835-9b1e-fab6d1d28d77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f8d386b-d76e-4b87-b9a0-c92c51dc930d)(content(Whitespace\" \
         \"))))(Tile((id \
         59d74d85-1fff-4b18-a68f-09809b89901a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         16b42ac3-48a8-4a86-be65-384edc7ab819)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         355df3ab-7580-4da5-b74b-6b1582208d74)(content(Whitespace\" \
         \"))))(Tile((id \
         877e68e8-557d-4666-93e0-a2e890e9f041)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6938365c-7b82-4980-a7ad-03026dd13fd7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b93c1e7-de79-4d74-981b-ce717573cbb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8da05bde-8421-406b-a6a7-c763440c8b05)(content(Whitespace\" \
         \"))))(Tile((id \
         bbec70b4-0c1f-4c1d-9742-22b2415d05b1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         80258fa5-9ce9-4f18-986e-e9dada6cccb0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         313b1016-ff6d-4ca2-be3a-63f9eb91717b)(content(Whitespace\" \
         \"))))(Tile((id \
         f8625aa0-3de3-4645-a21e-7e3f6640f462)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7e7bfaa9-5b7d-4e6e-8470-9c05ad0d1860)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61ea8c01-54c5-418f-ae77-2e7e10ea4566)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fd97a4e-e7ab-4d62-8a34-61f2c37a55a5)(content(Whitespace\" \
         \"))))(Tile((id \
         d71c7fe0-4f26-4392-80b8-8d3bc603b9a9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         b0ad6770-75a7-4203-b70a-fc3f0d2b503c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43ba44c4-dad4-4033-bee9-c6d91c404a07)(content(Whitespace\"\\n\"))))(Tile((id \
         b112af5d-532f-490d-9d97-aa11d59e9520)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21dbe100-ea11-4766-95a2-addb4ce74d7d)(content(Whitespace\" \
         \"))))(Tile((id \
         c8eb351f-b464-4cb1-bc4e-1566b813863d)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bb4af8e8-66c4-410e-813b-9799e5324bcb)(content(Whitespace\" \
         \")))))((Secondary((id \
         f091f78d-9f6f-4a18-88bf-8a285ec0874e)(content(Whitespace\" \
         \"))))(Tile((id \
         86c63cd3-9e02-4105-881f-89e0b68a37b7)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bef033b6-cd54-4294-91b6-b504854ac018)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cedf02d1-50c8-4c40-a768-e47808562ba2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         54158d36-5d0b-41cc-aab4-f5ec23877244)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aabd9f9c-57d3-4290-bc95-2669664f32be)(content(Whitespace\"\\n\"))))(Tile((id \
         5c765caf-89b4-49ff-831d-c4dcc16d279e)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee57fece-a8b2-45a2-8d39-efe20cce77ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2b95079-4c60-4a6c-b4bd-64c83a948338)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e1ba690-fd7f-4772-885c-b0baf2602745)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3908d450-5ae9-4513-b8e3-f6ae86975c04)(content(Whitespace\" \
         \"))))(Tile((id \
         c52f8792-534d-4f7d-896a-335004e3e59d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69967ce0-719f-4422-8ae4-ade8e2c393a1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1023b59-4383-4d06-bb1e-5353a29c8232)(content(Whitespace\" \
         \"))))(Tile((id \
         75baff92-db18-437f-afdf-7e135326f0a0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b1ee5b53-2945-4129-a04a-511e031a411e)(content(Whitespace\" \
         \"))))(Tile((id \
         f825643d-25ab-4a16-8da3-7f4b8762a947)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c6efd28-5762-4415-9428-0cb7fcfee437)(content(Whitespace\" \
         \"))))(Tile((id \
         e6097c39-047e-4743-b4e9-6ff4aeb033d0)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8418bcb0-65b3-4a46-9c42-982679111563)(content(Whitespace\" \
         \"))))(Tile((id \
         87ac3d78-a253-4747-90ea-21a93a0d93ff)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69412969-2174-4650-bdf1-55ee8a8fc4ce)(content(Whitespace\"\\n\"))))(Tile((id \
         6869055b-7da1-4d04-b472-a870e423cc04)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f1947fe-907c-440a-838f-005af43d41b7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8487961f-221f-4a36-88ae-cc5c2cfdc229)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         504ae7fe-04dc-4363-999a-0620d1ed516a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86d3b27b-058b-465b-aa40-a9f75e7ef37e)(content(Whitespace\" \
         \"))))(Tile((id \
         49ad2761-50cd-4fab-88a8-65a6fea7b8b0)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         521a9a96-a710-4073-a568-7d7cf3ac5c0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c754fdac-7b33-4acb-9008-e5b0ad1bad05)(content(Whitespace\" \
         \"))))(Tile((id \
         d36d4cec-9cc5-4aea-bc7b-c27c3e2524b0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b3eeecc4-5fdb-4be2-ac29-66c727120c1a)(content(Whitespace\" \
         \"))))(Tile((id \
         9406f917-b685-45f1-852f-07b8c2708f23)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fa2a25b-7ee6-4708-90ab-6a3c926ce02e)(content(Whitespace\" \
         \"))))(Tile((id \
         6035ffd8-ab0d-48ed-b7c0-cccd4414fdc0)(label(Dead))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73b4946b-114d-449b-947f-948b16d3db0a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1202f1f8-9d4b-45e1-b319-3610524d935e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c8cb555-7ff9-4de1-a0db-ab11d793dc84)(content(Whitespace\"\\n\"))))(Secondary((id \
         02423bfc-1d77-4b03-84dc-d30966e4b20f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0107f885-1dae-43fa-a9ca-85ad01a72128)(content(Comment\"# Edge \
         behavior #\"))))(Secondary((id \
         5bdc7a2a-8da7-464a-b442-94fbc69dac30)(content(Whitespace\"\\n\"))))(Tile((id \
         72fea937-2a9c-4cf9-8eae-f8d585a5785e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         977f2116-e88c-4092-bf62-0c273c0733fa)(content(Whitespace\" \
         \"))))(Tile((id \
         8ea2be88-508b-4a80-bae6-fcfbc7a815d0)(label(\"\\\"edge cells count \
         neighbors correctly\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fad7680d-f25f-4c9c-ad84-84e570e1cefe)(content(Whitespace\"\\n\")))))((Secondary((id \
         f116b6c1-2162-4e17-9b46-ae797cd5dc9a)(content(Whitespace\"\\n\"))))(Tile((id \
         e014a2c4-2e49-4d65-a438-a1c3890d1fe4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3738feb3-fde3-4a47-82b1-f6b57de35eea)(content(Whitespace\" \
         \"))))(Tile((id \
         07e93bfb-d371-4e10-9313-6ee8a8e35e01)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af6395f2-4d24-4a87-a2e6-6a2e06cb5044)(content(Whitespace\" \
         \")))))((Secondary((id \
         b02f6a13-df2a-42ae-bbbd-8b351fecb6a0)(content(Whitespace\" \
         \"))))(Tile((id \
         dcfd0478-5b0a-4952-9a4a-4b145d8835c4)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85770c71-7843-45a2-9050-fb6dd4982095)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         68150cbf-a352-40bf-9fe0-96499dfaf01f)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         817c13c7-66e5-4be3-83c7-8250ca645509)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d46fb6f4-a57f-4e4c-93ba-9fe2b1ae31b0)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc375b45-3ec1-443c-958d-d7458cb48f99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ade7d7f-88f3-47cb-bd6e-9e2eabba9f4b)(content(Whitespace\" \
         \"))))(Tile((id \
         3ae7b6fa-f665-4186-b647-b642da9c3a0b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0fe6f1e3-4f24-47de-b5d0-e1d3eee66fb3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08908001-e619-4b63-a7fa-b3ecdfab40a5)(content(Whitespace\" \
         \"))))(Tile((id ba10f044-1d98-4409-ab78-dd1701d52a86)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4d416774-b134-4fab-af32-da7ed241406e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e48ec1b0-d4e4-4f16-bda6-21626edab5c6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         092cdc49-e70e-43e0-8175-19abc393fa4a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc9b7e5e-93f0-4384-90ca-f2d4dbc84ba3)(content(Whitespace\" \
         \"))))(Tile((id \
         77f760f2-f1bd-4203-bc98-9dfa09b885d9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6752ab30-01b8-432b-ac9f-4bfd0ec9f2a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         710aa043-476b-4842-80eb-22e7b8c3a17c)(content(Whitespace\" \
         \"))))(Tile((id \
         28495463-bf5f-4762-bcd2-a3fdda453e3f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e102c7dd-520d-4f53-97b4-8fc6451f746e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a06a369-76fd-40db-9567-4f7b63e0f474)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19d69814-c72f-462b-bcaf-0de4f85d42b8)(content(Whitespace\" \
         \"))))(Tile((id \
         4d2b51cb-2eae-4b0d-af35-6f758322cd86)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cfc9f31a-d507-4ced-bf72-5a67dc481a2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         056fca34-2a2e-4e73-a130-a5db3a107bfb)(content(Whitespace\" \
         \"))))(Tile((id \
         8776ca37-435f-4fbd-9655-e0bb714920c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c2b382c-2444-435d-bdab-c8544ed285f9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c50bfc29-b729-4e36-b8a9-febe030a2f60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec821a8b-393f-41cd-9d2e-88e13aeb0cab)(content(Whitespace\" \
         \"))))(Tile((id \
         661b355a-f74d-4354-b9ed-8eb6b4749e02)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         12ca7cc8-d8d7-4fa7-9883-caa48cce746e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9a5e94f5-ce09-473e-944f-e518d52cd4b6)(content(Whitespace\"\\n\"))))(Tile((id \
         2f784869-0af0-462e-b105-71d5949029cf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1eeb1fb3-398c-4eb2-927c-2f741764b16b)(content(Whitespace\" \
         \"))))(Tile((id \
         2c134aa7-5295-4e1c-88e3-98fde5fa994f)(label(g2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5773bcf3-d4bf-4830-b778-1f07f902b68a)(content(Whitespace\" \
         \")))))((Secondary((id \
         b1ea5e89-3306-4db4-95d3-5fcbc5d7776d)(content(Whitespace\" \
         \"))))(Tile((id \
         00ff95d1-f779-4fea-a2e5-a31ef2550305)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df6aa467-fa53-487e-8de0-164c17d3bd57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         16913ad3-551d-46b9-9f3f-b6b86021cdb5)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         150b23ec-367a-4917-a363-b5dc90141102)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         03dc278d-bda6-4d7b-8b7b-d5c1c597cbab)(content(Whitespace\"\\n\"))))(Tile((id \
         b087c19a-157e-4999-a43b-f3304b2dea76)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc7345d7-e6c9-4653-aa4c-5ed627a152d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b7e23583-325f-4110-9eeb-a1244ae1ab59)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dae2bfa5-8db9-44a8-b2d2-18a0b4c92999)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c72f2211-f528-4972-9164-71f24b742d56)(content(Whitespace\" \
         \"))))(Tile((id \
         76440275-c418-4e1b-b954-32b524aa056a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8562e3aa-5819-4008-b207-a449431f3af3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa6af849-871d-4c38-98f7-15cce08464c2)(content(Whitespace\" \
         \"))))(Tile((id \
         d6819297-f2f4-4cbb-b0e6-432542e65bf9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fcbeed67-d908-467b-bd71-e2fbc1ac0d57)(content(Whitespace\" \
         \"))))(Tile((id \
         82effe00-4af0-4a0f-9d65-f7207b22e008)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03476295-5681-4659-b303-bc2717f0436c)(content(Whitespace\" \
         \"))))(Tile((id \
         36403f07-07b7-4d4b-8f39-52f3360d7080)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         192c0495-bd9c-4ecc-a721-93c655740178)(content(Whitespace\" \
         \"))))(Tile((id \
         2df196d2-a48e-4f18-a3f3-213d9563121c)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c52c45ed-3d75-4b4b-ae56-afa76cb0e6cd)(content(Whitespace\"\\n\"))))(Tile((id \
         e102e00b-4f99-48a4-ae0d-b4c8af7c3873)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e570783d-a5d0-4862-8a62-3762f2182054)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         57dcd6e5-9b17-42e3-9c44-8ce20d3d8058)(label(g2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d125951-f295-4485-bf59-96722c8a7e11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9d53cda-3f8a-4fb2-bca5-68aacecee917)(content(Whitespace\" \
         \"))))(Tile((id \
         24735535-ed4c-444c-85ff-f5d1c1435869)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0455f57f-2e60-4103-949f-fcfe6016bc41)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a742a448-4f96-4d01-a8ad-fad84492c5a9)(content(Whitespace\" \
         \"))))(Tile((id \
         d15fcdf0-f093-44ff-ad74-f5d10d6449a1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         42c4a26f-fe3f-4768-ae68-51dee29db77f)(content(Whitespace\" \
         \"))))(Tile((id \
         c592c3bb-9edb-49e2-adcc-b4509d1fd120)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bac95bb-810f-4689-9a77-f9d3505d1cb0)(content(Whitespace\" \
         \"))))(Tile((id \
         1f849d7c-50cd-4aa4-bce7-6d5738bd22f3)(label(Alive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7cac0e72-f41c-459d-bd26-3be297922f0c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         19d4887b-2a1e-4877-9636-0dab027b8f4e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db611d87-86c1-4456-8610-8b598873213f)(content(Whitespace\"\\n\"))))(Secondary((id \
         64a45c65-e5e5-4cce-a781-bd42192b9952)(content(Whitespace\"\\n\"))))(Secondary((id \
         7265ec75-3131-47f6-98e0-ae1043ec54ed)(content(Comment\"# Demo: \
         Blinker evolution #\"))))(Secondary((id \
         c02bffc6-af96-4409-974d-8a3e6e02a095)(content(Whitespace\"\\n\"))))(Tile((id \
         78423676-4775-41fe-bf22-8c53928a46f0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         77d08850-6010-4e79-a68a-7e6c214ed23b)(content(Whitespace\" \
         \"))))(Tile((id \
         192acd49-4cad-494f-8b79-9a8093babfde)(label(blinker))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         58473195-8ae0-4a8e-81e7-509003dd0f17)(content(Whitespace\" \
         \")))))((Secondary((id \
         01ffe5cc-3fbb-4d45-9085-35afaef2e99e)(content(Whitespace\" \
         \"))))(Tile((id \
         5c8c8aad-d537-4a38-9ab6-f318841c71bf)(label(setAlive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cca269f5-9359-471c-98df-d307d19b8704)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         64527ee1-91f8-48ef-9ee3-7639a4967bbb)(label(makeGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5eb7baf-3f3b-4ae7-a597-7a9e1efc70a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6442d04d-3b17-4f83-a6b6-e93fa35cbaf5)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de6d55d4-01a1-45a5-9b13-6eabe7e33849)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         999fb388-5387-401b-97af-1f931ccf3fed)(content(Whitespace\" \
         \"))))(Tile((id \
         12c408fb-71d6-47b5-80c6-e1f4ec32081f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6b8b7374-be8c-407b-bc6a-1dd0f3f0a7c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         050b6eaf-ced6-4b90-9b90-c65ca104c141)(content(Whitespace\" \
         \"))))(Tile((id 37082e28-5f1e-491f-8135-4813105a2819)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9d42feb6-d7a5-43ba-8b26-16f95e9a5d5d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a5db4863-3557-46d5-a837-671194568a26)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f81a5a0-d0e9-4e79-921c-dbada52b417f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         634c8478-01fe-41cf-9e93-9c8d40526bcc)(content(Whitespace\" \
         \"))))(Tile((id \
         c69dc50b-53e8-4a23-aacf-135fcedab341)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d3abd6d5-c797-48c5-8924-62f2e20ecc95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdf56add-4b01-4da9-8d73-a217e04c2bfd)(content(Whitespace\" \
         \"))))(Tile((id \
         fc0d09da-8e47-45e6-9759-172e01ca497d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8ce4f8ff-8390-4af2-86cc-3fe2362d78ca)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c55037b-29b5-4133-a24b-1f476f10338a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b785ed3e-c4cf-467d-bf62-c1a5e6a725cd)(content(Whitespace\" \
         \"))))(Tile((id \
         e5e36c11-3fbe-48a3-88a9-62a17b99558c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e9b20e44-4b7c-41c9-9ceb-63d8c0dc6e0e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3da4c268-7919-45a2-92d7-5e8017043f2e)(content(Whitespace\" \
         \"))))(Tile((id \
         e14195af-cf0b-4e7f-9eba-bcfb653e8276)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4ad2d9e8-b1d8-44d1-9c4f-4f89e70cda2f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60607a98-8340-4902-8ab0-e84fc7ca37af)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36f55be4-7c05-40d7-a753-49bfd3e543b7)(content(Whitespace\" \
         \"))))(Tile((id \
         b2ca7a44-fd59-4ea0-a63e-404fedb92883)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         f7d44ff1-c846-4056-ba33-6ca6271656c4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ff697574-0e5e-4edd-b898-6357b6f5e5d3)(content(Whitespace\"\\n\"))))(Tile((id \
         dbf6131b-d9e6-4aee-ad8c-67869cc23a0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0f1ea01c-9c69-477b-891c-6cc3b8d3840d)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc4a6225-2e30-4ea7-8ccf-6a7b56f98add)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0350ea1f-3452-464b-be45-9bbe8528596a)(content(Whitespace\" \
         \"))))(Tile((id \
         a5ce7b85-4f39-4ab8-bc20-a2ae3b9fc488)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         214e82a3-7dc8-4674-ad74-555a066babfa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ccca87c-e8ad-4805-a165-dc5380332cf4)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dd4fcf5f-111c-4019-a874-9381f01f2b4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d8d62c6-c900-43de-84cc-5ef1c6aa4186)(content(Whitespace\" \
         \"))))(Tile((id \
         d986530c-7810-423b-af2b-68e2fa45da1a)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6b6f02f-256a-4175-b6f4-047de235c0a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cacb17bd-7744-4127-b712-1d5ed3a80c0b)(label(blinker))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c9b0632-228e-416f-87e9-ec94148d0a97)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1fc6c9f4-9367-490a-92df-860707daf8f7)(content(Whitespace\" \
         \"))))(Tile((id \
         5a6647f6-99c5-41cb-a5ff-637d45834aa4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         653b1980-63b7-457e-a8ff-c8251b789f63)(content(Whitespace\"\\n\")))))";
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
