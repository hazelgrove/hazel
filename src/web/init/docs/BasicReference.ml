let out : string * Haz3lcore.PersistentSegment.t =
  ( "Basic Reference",
    {
      refractors = Haz3lcore.Zipper.Refractor.Map.empty;
      segment =
        "((Secondary((id \
         aca843fa-ed08-4307-84d7-48ebf26e77a0)(content(Comment\"# Hazel \
         Language Quick Reference #\"))))(Secondary((id \
         5f018e7f-0929-4f23-9744-b418adde69cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         27c061bd-0ff9-4ec9-a699-7886a0871fdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         1362eecc-8c2c-4e16-9746-6d9fe78aa785)(content(Comment\"# Empty holes \
         stand for missing expressions, patterns, or types \
         #\"))))(Secondary((id \
         84a2d03d-a2c6-40b7-8424-3723c24443a7)(content(Whitespace\"\\n\"))))(Tile((id \
         718694a0-1210-443d-b121-e4c265f07062)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8c287474-6538-4449-b373-a5d09527db65)(content(Whitespace\" \
         \"))))(Tile((id \
         acb221e6-ec3c-4ac0-afcb-20fdf8456b28)(label(empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4906c3d3-26e5-4c88-83ea-88efb9712705)(content(Whitespace\" \
         \")))))((Secondary((id \
         9375f8e2-fb77-47f7-bdf5-86177103791a)(content(Whitespace\" \
         \"))))(Grout((id 6376a909-9135-4db5-9949-d4b253cda702)(shape \
         Convex)))(Secondary((id \
         38da46dd-bcc8-4e9c-8398-e78be2fdf1a4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1b2c5b9-9010-4c2c-b845-650e43fc08aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         56e09c7a-8bb0-48e5-b88c-2065dca2d31b)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b372ac3-52bb-453e-baec-851a8feda63a)(content(Comment\"# Non-empty \
         holes are the red boxes around type errors #\"))))(Secondary((id \
         10087807-6211-4de5-b8da-515afec09e3d)(content(Whitespace\"\\n\"))))(Secondary((id \
         10c2a496-5ea0-4124-930a-8c9263a4c71c)(content(Comment\"# (you can \
         still run programs with non-empty holes) #\"))))(Secondary((id \
         fc742dfd-3322-42c6-82a7-d5bbcfc62a18)(content(Whitespace\"\\n\"))))(Tile((id \
         ba45e253-241e-4ed4-aed2-7afae3f63bfd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f58125e0-afd6-49ff-9b38-9ce2e4129676)(content(Whitespace\" \
         \"))))(Tile((id \
         499b371c-1ea2-441d-a9c9-eb77a28daa96)(label(non_empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5bda1b7d-4c61-463a-b706-8afbd7cadd20)(content(Whitespace\" \
         \"))))(Tile((id \
         552ccfdc-03fc-4cf4-bf40-dfd5fe4c0d68)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         92afa7cc-eb14-4e78-8e21-a2c3b6514f50)(content(Whitespace\" \
         \"))))(Tile((id \
         90d4f9ab-3046-4107-971a-54c4666a7b78)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         28a47eed-0330-41bb-be8e-673d0ef5d813)(content(Whitespace\" \
         \")))))((Secondary((id \
         3400c823-12f2-491b-9717-6bb076821dbb)(content(Whitespace\" \
         \"))))(Tile((id \
         4706f4ba-0059-4899-8b64-59910b2d7e78)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2003229f-9e63-43ba-961e-88e826d84c7e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f8bcebc3-b1cf-40e2-b98f-38fc531ff3bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         9907c67d-671f-4ec0-9abd-18b11202a4f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d0ad236-760e-4957-8c75-822970e00613)(content(Comment\"# Booleans \
         #\"))))(Secondary((id \
         77a5fa05-fd49-4d81-b14f-f021c918e5cc)(content(Whitespace\"\\n\"))))(Tile((id \
         8acb35fd-904d-4b97-bebc-a516c314c6e3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e2917572-8336-427a-8b71-7b4f4150bb08)(content(Whitespace\" \
         \"))))(Tile((id \
         4474fb19-5acc-4162-963a-cf864352b80b)(label(bool))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         75535f63-4095-4fcc-bbc6-5603055999ff)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         62caedef-c148-4bca-8231-74ab2ba3b4c9)(content(Whitespace\" \
         \"))))(Tile((id \
         85f3f302-dc04-469d-b639-5012b2725601)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a9e3fb81-8b6f-476c-a14d-d2b249223c80)(content(Whitespace\" \
         \")))))((Secondary((id \
         58fbcc2b-3b37-4bff-9b6e-532d706cac32)(content(Whitespace\" \
         \"))))(Tile((id \
         23d97c4b-f562-439a-99e6-002e9f727f4f)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdf44d10-f2b1-43a2-9390-806e2876fefb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bc9b9faa-7687-4318-9158-3716081e6feb)(content(Whitespace\"\\n\"))))(Tile((id \
         7de1a529-bd29-4a38-a971-de5805e4deec)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9ff67f72-a52a-445d-ae81-68c609275dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         cf0e9dbb-79f9-4a1f-bfdf-6f3ebd565455)(label(operators))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f7e36fe0-9e7c-42a0-8b75-ee32c3bb41af)(content(Whitespace\" \
         \")))))((Secondary((id \
         48b5c9a0-e235-4f46-9f6a-7c52dced9078)(content(Whitespace\" \
         \"))))(Tile((id \
         3fe813d2-19af-4552-89d7-62411f489a8e)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8779c9f2-1ffe-4a83-9882-86ffd21fb05f)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2647efe2-34aa-49d5-95c4-324f4928535c)(content(Whitespace\" \
         \"))))(Tile((id \
         63e6f4b7-8bdb-416d-932c-9e0cbc946a46)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50b65723-baf1-459a-922f-ff4ecd4c56d3)(content(Whitespace\" \
         \"))))(Tile((id \
         323345d5-848c-4be9-a0f5-d628551d201c)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d588301b-d472-41bf-b05a-965594c34306)(content(Whitespace\" \
         \"))))(Tile((id \
         56963222-8b45-47e3-afa4-ffa29b763a78)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3339bb36-bbc8-4850-8c71-f458e3b142c2)(content(Whitespace\" \
         \"))))(Tile((id \
         1e5c139d-56cc-4700-9a42-34a20f8ef874)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         125f366e-8658-41d9-908e-b0b2facf1dfd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59eac5f9-6cb5-4475-a5a2-3b2bc8c521fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b23831e-a64e-4b83-9821-7fa511f53ff1)(content(Whitespace\"\\n\"))))(Tile((id \
         8be1953f-3c41-4dc3-b25f-828ee8dda279)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c468e54-c579-4a78-8c4e-e84e8f68dd0d)(content(Whitespace\" \
         \"))))(Tile((id \
         063d1b19-dcc4-4949-93c8-ab376192ea2c)(label(conditional))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d98ce1da-8fa4-4b66-a3a0-3167e607ef8c)(content(Whitespace\" \
         \")))))((Secondary((id \
         7712b318-d460-4ec5-aab1-d73c22852c0f)(content(Whitespace\" \
         \"))))(Tile((id 861cdd89-f1f7-4623-99de-64f615c4fcd3)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         27249e86-7712-4dc5-8783-6fbc421beec0)(content(Whitespace\" \
         \"))))(Tile((id \
         324f5dea-1d5d-47fe-b0c0-228eb89db8bd)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a28be543-463a-4ec7-b13d-9b158fe61007)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ae11233-d81b-47f4-a17a-367348f0b1fa)(content(Whitespace\" \
         \")))))((Secondary((id \
         fdc6a1d9-bbf6-4bf6-9d1d-50855da1c8c2)(content(Whitespace\" \
         \"))))(Tile((id \
         68a64823-ecec-4f3b-85d2-5ed06eabb662)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         276c1950-ffac-422a-ba07-317ee25845a8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e87a2e7-5d16-443e-a401-09d0593b1053)(content(Whitespace\" \
         \"))))(Tile((id \
         2d4cd910-f2dc-40ad-b4c8-7b34ddc3e499)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24351762-8a13-4bb2-8c10-58a158a971dd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         881a7b51-a00d-4d91-9eec-063c7cb53581)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6050185-8a37-4c54-8284-6bedb461af13)(content(Whitespace\"\\n\"))))(Secondary((id \
         6cefce6d-a61d-413f-80ee-764eb0c9518e)(content(Comment\"# Integers \
         #\"))))(Secondary((id \
         de738a66-0df4-42a9-8f5e-90af36dd4949)(content(Whitespace\"\\n\"))))(Tile((id \
         19cb6348-35f7-438a-9b24-78edae4bc844)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1213d21d-34f7-4ea2-89a0-93a7f345e009)(content(Whitespace\" \
         \"))))(Tile((id \
         cc5d71d5-fe1f-4336-b13f-c991bd3fe562)(label(num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fa1ccffd-4033-4ef1-95d4-72c2fbf753c6)(content(Whitespace\" \
         \")))))((Secondary((id \
         09efc9c3-2ca4-4d0a-99f0-687dc7c060da)(content(Whitespace\" \
         \"))))(Tile((id \
         2802b01b-78f5-4398-8bc3-55634708944b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08a542d3-36f6-467e-ad77-c8861903fcf1)(content(Whitespace\" \
         \"))))(Tile((id \
         f114fafb-7bca-4350-91ed-0541f9ab4e5e)(label(:))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 24))(sort Exp))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         103f4451-94a4-4b31-b35f-e5fa5cd801de)(content(Whitespace\" \
         \"))))(Tile((id \
         06081d32-4fc7-4d46-ba49-610eb72588d7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         01cdfdaa-0b81-47f5-b6e9-317e5ebef517)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         374f5616-c98f-452e-8199-e5c626421249)(content(Whitespace\" \
         \"))))(Secondary((id \
         9b44e3ea-e7e7-4bc4-ba71-06717a9da139)(content(Whitespace\"\\n\"))))(Tile((id \
         74537703-5d73-4bf4-8895-6d7822efb90a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3b960cb6-bf59-4d8e-a097-5c19452767b7)(content(Whitespace\" \
         \"))))(Tile((id \
         b783cd4b-c3ad-4394-9380-cce30685d77e)(label(arithmetic))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f6b37012-d17e-4dce-acfa-4194ab569eda)(content(Whitespace\" \
         \")))))((Secondary((id \
         7ada612b-83d8-4f82-b3db-caf1b959d78a)(content(Whitespace\" \
         \"))))(Tile((id \
         993c8cef-4831-461b-beb0-94efe5056ac8)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4776392-ca20-4ec6-a5fd-7bcddc0bf901)(label(num))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb0f1549-d190-4a66-86d7-5b10eb1dc14a)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6041b1fa-4c2b-4e85-bb96-17d9b3ee47b1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c7da8dd-0e36-403a-9377-79f10401d044)(content(Whitespace\" \
         \"))))(Tile((id \
         96e59945-e92f-4d3f-947f-67b157730c05)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c21a71f5-8e5b-4691-8da9-7fd120eb36fa)(content(Whitespace\" \
         \"))))(Tile((id \
         6d95c09c-2b36-43a4-972b-142e2227c0b5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         965b9968-0e6a-4634-a153-6c88af9f2506)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8d96b335-e62f-4a3e-89d8-475a2b613f84)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b6349b8-a899-4b9c-870b-f93394483541)(content(Whitespace\" \
         \"))))(Tile((id \
         f1925b26-ca05-475b-bb91-487ffcdedaf2)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca94063f-0fa4-4901-b789-18d7f4386f6d)(content(Whitespace\" \
         \"))))(Tile((id \
         74ef64e9-7b4d-4949-9701-eb71c853d9b7)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         251027b4-a8ff-4fe3-b2f8-d7a1677f6f15)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2b876d3e-8b4a-45ae-b246-3a766cab1bd9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         69bd3e30-fda9-4770-877b-dc12b3a2be46)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56a154bd-b845-4a82-b6ee-4325820ea688)(content(Whitespace\"\\n\"))))(Tile((id \
         f13fef77-c621-497f-85ac-a1f9936cab55)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         67542418-4b14-4b16-b64d-8b9f8da5f1f4)(content(Whitespace\" \
         \"))))(Tile((id \
         23ff45c2-f5d0-4131-87ba-a9bee086dd46)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b8a9c1b3-dcd5-490d-8e7e-26e3d6723d74)(content(Whitespace\" \
         \")))))((Secondary((id \
         56272563-b29e-4fee-ac3c-417c1b3db8dc)(content(Whitespace\"\\n\"))))(Tile((id \
         f51a4e69-0cc2-423b-9886-9eb8191a3661)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1d6e2c50-df6d-4db7-a600-97185ccdff43)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43e4ab2b-a8a2-427d-8d38-3e53d52db6dc)(content(Whitespace\" \
         \"))))(Tile((id \
         f8765931-1840-4dfd-8d2f-69ebba96b4e2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cc2a610-aa05-4693-9ea8-95b77429c6ca)(content(Whitespace\" \
         \"))))(Tile((id \
         7070c885-9b9a-4942-bb76-036fc6904dfb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c11bc73-a44c-42c5-99d6-9b75f1217fe1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1be8557d-a333-4589-9f01-7b74a866c940)(content(Whitespace\" \
         \"))))(Tile((id \
         58577755-23c3-4ac2-a530-50588197238c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c1b9940d-3ac0-4965-9244-799b6902b98f)(content(Whitespace\" \
         \"))))(Tile((id \
         d3d87acf-9f3f-4356-a843-3c1958db84a3)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdb6dacc-7483-4693-9175-ffdff8d07eaa)(content(Whitespace\" \
         \"))))(Tile((id \
         5d4f46bf-3527-4e98-8dbe-8e9561fa6642)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1dd53c45-fde2-4637-98ac-1743dbda04bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f67bbda-957f-45ab-85a6-313245ac2b0c)(content(Whitespace\" \
         \"))))(Tile((id \
         068c9da9-7842-4bbc-b475-4a707f2e729f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4cec26c6-1cf3-4b56-a08d-8468e0084ab5)(content(Whitespace\" \
         \"))))(Tile((id \
         46b6e9a4-7b03-4bdf-9932-d04c6e58b482)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93540c81-87f4-48fa-a998-2a78c9205f01)(content(Whitespace\" \
         \"))))(Tile((id \
         361d9408-de08-495b-9c55-efdf1fb25e27)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7bd2a04-324c-40e7-bd15-33389d25e4dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6de6e601-cd82-40ad-a4f8-1c73e3716870)(content(Whitespace\" \
         \"))))(Tile((id \
         ad430c30-cb50-406f-ad0d-da974dc0d30b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         27dae59c-3f16-406f-baf1-a78bc87de5d1)(content(Whitespace\" \
         \"))))(Tile((id \
         171d0103-70cb-4920-8f57-d6062aa273fb)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ced87e42-09d9-428a-a5b7-ad71b32e6770)(content(Whitespace\" \
         \"))))(Tile((id \
         1a71d411-e169-42f2-984d-e52ae04d8e41)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         001f0dad-3b26-459a-9b42-7eea8f12aa51)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1213f93c-9938-48ca-964d-7552437aad10)(content(Whitespace\" \
         \"))))(Tile((id \
         92a6fafd-9ba9-4ca5-ad4e-d3c379f57a8a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b7b39c85-5ef8-45c2-b3c8-c6d8802be3d4)(content(Whitespace\" \
         \"))))(Tile((id \
         e7b2dfc5-bdc7-4cf3-b11b-f219c7b808da)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         150f9e9c-ef62-42dc-a7cd-a580702ef992)(content(Whitespace\" \
         \"))))(Tile((id \
         502d1116-c590-45ad-a2b2-c9ad73fd1280)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         01646ae9-e8cc-49bb-9ba4-1ff014adc955)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5267ae5c-385b-420b-b991-ee7d465832fc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         00b046e7-6e9a-47db-ab19-acf229825830)(content(Whitespace\"\\n\"))))(Secondary((id \
         d78b5bd8-0f72-4e19-ac3a-8890c4645905)(content(Whitespace\"\\n\"))))(Secondary((id \
         0548a98a-5c5c-4ba9-8603-91b80ab3b161)(content(Comment\"# Integers are \
         unlimited by default #\"))))(Secondary((id \
         e89a3a2f-2e01-4d28-9363-07dbd65e3fe5)(content(Whitespace\"\\n\"))))(Tile((id \
         1f6306dd-60ba-4f25-b6be-2434a5db0176)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6dd206dc-68b5-402d-a583-bc1f1a983465)(content(Whitespace\" \
         \"))))(Tile((id \
         741e4e4b-782b-4b83-9206-9cc34e8248dd)(label(big_num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         11d83307-d210-4dd3-a833-87c628d8b4d6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         62e8284e-435b-4a14-9029-4b37748acf7c)(content(Whitespace\" \
         \"))))(Tile((id \
         2df45b72-2e89-489a-b15f-a8f0fef99844)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f9ff8681-1134-437a-9a67-1d62eece4c21)(content(Whitespace\" \
         \")))))((Secondary((id \
         70b583ba-5a1d-4fbd-bb05-50db6f867565)(content(Whitespace\" \
         \"))))(Tile((id \
         58e48409-e2fd-4a80-9370-aa041027101b)(label(10000000000000000000000000))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f43aa14-7cd3-4b5b-bd87-2dd8efb810b0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b34321d-7635-455e-b61d-525197f0ed34)(content(Whitespace\"\\n\"))))(Secondary((id \
         29a2a5c4-b6f1-4fb9-b4b2-5df58945e121)(content(Comment\"# Use SInt for \
         fixed-with system integers #\"))))(Secondary((id \
         6f9e43a7-9859-4345-b403-3941d2ba8852)(content(Whitespace\"\\n\"))))(Tile((id \
         a223976b-e8db-4a9e-a5fd-0e620c210ffc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d652e2d5-8c6a-43a3-a354-3aa4d8121371)(content(Whitespace\" \
         \"))))(Tile((id \
         80d656af-ee08-4867-a8c7-52555bbbe933)(label(bad_num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         efa530dd-bacb-418f-a8b1-a38808930d86)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0e6cea9f-e77e-48a0-a7d2-3a16aea1ea22)(content(Whitespace\" \
         \"))))(Tile((id \
         5af64a4b-21c1-4b17-a8d9-465d58e6a051)(label(SInt))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cc3623c6-7d6f-4621-a007-e8d5c00d4440)(content(Whitespace\" \
         \")))))((Secondary((id \
         714e25b7-8523-422c-b166-a77324facf8b)(content(Whitespace\" \
         \"))))(Tile((id \
         90c362c9-3296-4b6a-993e-9a0e7f552f30)(label(1000000000000000000000000))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb698b7c-ca73-480d-98c0-ff09ff6ea5a4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1a53c3b1-4fd9-43a8-8ac5-64c43c1b1f4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8fe62c70-7159-4252-8fea-251cb325fd2a)(content(Comment\"# Use Nat for \
         non-negative integers #\"))))(Secondary((id \
         2afbe831-74c5-4b18-801e-08c4abf03a5f)(content(Whitespace\"\\n\"))))(Tile((id \
         0c3644f9-2e2c-4ca9-8ed3-722a83969b88)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8c81bc12-69b3-4249-81fe-63ab9e1d9405)(content(Whitespace\" \
         \"))))(Tile((id \
         80921858-645e-49e7-b529-2a7279835277)(label(nat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6c1047d6-434a-42b9-8053-c9e17b857816)(content(Whitespace\" \
         \"))))(Tile((id \
         7577143c-b734-412c-899f-42dd6f2db039)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         04c1d447-9392-42a8-9efa-e30470ce9552)(content(Whitespace\" \
         \"))))(Tile((id \
         812ce988-e3b9-4cca-a9f6-05e51baf7780)(label(Nat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         de1f6624-affb-4d07-80a7-3815b242b5bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         7cf404f9-7887-4575-a7f1-e2f186402d8e)(content(Whitespace\" \
         \"))))(Tile((id \
         39fda0c1-f007-4174-abf0-19d047c1bbf1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1b8ad07-6b07-404b-a688-fc28a8266d2e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70a3e531-c3a3-4d7a-96ee-f5206f0f6bd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         0007ab2c-95a6-4a17-8611-7997f1e08204)(content(Whitespace\"\\n\"))))(Secondary((id \
         67c16547-e4e7-4f49-b7df-e2f0db463cf0)(content(Comment\"# Floating \
         Point Numbers #\"))))(Secondary((id \
         de01c6c9-a54c-4e7c-b8cd-f104aac55c62)(content(Whitespace\"\\n\"))))(Tile((id \
         aec078b0-234c-46c3-8431-bd7763e435e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1876b6b6-792e-465a-9113-1642af9b28fb)(content(Whitespace\" \
         \"))))(Tile((id \
         c7b51452-fcc4-4a29-94d7-490ce92e6839)(label(float))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         719b277e-345a-4c11-980a-e83fdbbca62d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         25d58de2-d864-4599-ac51-4ffcae614c1c)(content(Whitespace\" \
         \"))))(Tile((id \
         4a2172a6-8e30-43aa-9239-0f201c6e6c7b)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         792e263a-1959-4c6b-b8eb-8ea2fa935cca)(content(Whitespace\" \
         \")))))((Secondary((id \
         5df746c0-eb6a-4ddf-bed9-7dc1d753660f)(content(Whitespace\" \
         \"))))(Tile((id \
         60e24e10-b100-4f5c-aa60-4524c4abb7a6)(label(0.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24ceae9c-1527-45d7-a6fe-2cbdc3daeeb5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         71bb9a8d-024f-4626-bcdd-25a9de1fdb17)(content(Whitespace\"\\n\"))))(Tile((id \
         b04fa488-8b8b-4e44-85db-12e506d0797c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4c95be1f-e14f-40e0-b0f7-f98da2451b40)(content(Whitespace\" \
         \"))))(Tile((id \
         b1f822d8-b4a6-40f1-b7a8-c586159a0c9d)(label(arithmetic))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d7b0f1a-190d-429c-9704-ec820de694d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         5033846d-341e-482d-bfbd-fd8d19914bcd)(content(Whitespace\" \
         \"))))(Tile((id \
         78cd9489-9875-4504-a03f-6c5a1565af62)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5f3f8a1f-2614-4649-b655-6807c7dca194)(content(Whitespace\" \
         \"))))(Tile((id \
         c29a32bb-4436-4a07-b99e-4a17240b08ad)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ab52290-9789-4f29-839b-c944f6e65b14)(content(Whitespace\" \
         \"))))(Tile((id \
         d74fba7f-6bbe-4731-a222-88283e4d158e)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2be82e47-3269-404b-9115-8286614ee957)(content(Whitespace\" \
         \"))))(Tile((id \
         bfc5c9ac-ba90-4ce4-b95b-e2533230c1b7)(label(+.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d00457b-e0f9-46c2-a225-c630119a7982)(content(Whitespace\" \
         \"))))(Tile((id \
         82f4ba5f-b817-4934-a98c-b2a4dda3582f)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         203e092c-80ae-4893-8bc7-90f7076eed56)(content(Whitespace\" \
         \"))))(Tile((id \
         d1aa7a30-1ef3-4652-917b-c91e1aae0c64)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05c30a77-270a-4d0c-8cb1-443f8d12c670)(content(Whitespace\" \
         \"))))(Tile((id \
         57ae8fff-9d7a-469b-b514-cf060c2be688)(label(3.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74d1e114-034f-4cfa-9c26-11f5112d9b34)(content(Whitespace\" \
         \"))))(Tile((id \
         9b797cf2-3736-4ca8-bd13-48bbe1f86289)(label(-.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96e8989a-b2c2-4855-bd18-f0fdd4558634)(content(Whitespace\" \
         \"))))(Tile((id \
         4bccb7e7-14a5-4bcf-980f-f64c32ea0495)(label(4.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0c6c139-ac02-4753-9fed-e5b02af2b6b2)(content(Whitespace\" \
         \"))))(Tile((id \
         d9019ea9-196f-4ccb-97f4-130d80d3692e)(label(**.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db1b4841-8b03-4480-9ebf-668af11b3e39)(content(Whitespace\" \
         \"))))(Tile((id \
         7c14cbd5-b78e-481d-b98c-dfe7cb4718ac)(label(5.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1b8b94fc-3066-47f1-aba1-84d7f8a4ac1f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         09fdd540-f89a-437d-bfb8-d2b3d66ce6fe)(content(Whitespace\"\\n\"))))(Tile((id \
         b4971879-3bd1-42af-ba0e-b9a723281cde)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b0685986-b754-4022-8ab0-ebc28fef5cb7)(content(Whitespace\" \
         \"))))(Tile((id \
         a4e75869-ce67-431e-b718-4efd60ae2ecc)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4f437136-1297-4abc-bc1a-e1a7deafa463)(content(Whitespace\" \
         \")))))((Secondary((id \
         31a6de2f-c181-4ffc-b763-fa098c5f62a9)(content(Whitespace\"\\n\"))))(Tile((id \
         589c3899-679c-48a2-8d4a-b2b6d248ffcf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e8125518-8cf2-45a1-aa30-266ccbc9da17)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa3fe556-cd4b-4eff-8205-ee8664c3c872)(content(Whitespace\" \
         \"))))(Tile((id \
         59fce1ae-9cc2-4443-bed5-6fb38337d870)(label(==.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d3263b1-4161-4795-b5f3-2567965a1746)(content(Whitespace\" \
         \"))))(Tile((id \
         b7cf3f79-df70-411a-83e6-c5fd1458bb43)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0600d7e-8540-49df-831f-8913413c16eb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6aff25c-7e5a-4509-a993-27cdcf9e95b5)(content(Whitespace\" \
         \"))))(Tile((id \
         fec46664-69bf-4443-9c3e-a9b54aa0ba0e)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc1d7805-73f4-42f2-b040-262ff67fad29)(content(Whitespace\" \
         \"))))(Tile((id \
         fe419764-88d2-496e-89cc-3853b27cc914)(label(<.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d0701b1-9c0c-4c1a-9a3f-454948948169)(content(Whitespace\" \
         \"))))(Tile((id \
         f5c69701-969a-402b-9d30-dae88e3271cf)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71f26902-f8ae-41b8-80f4-ae0158802e6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1312ece0-0c04-4a26-af9d-09a38a331c7d)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8994d2-1f28-404c-a4bd-a65a1064d01d)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c38b3c94-3cca-4f31-9609-b406960d659c)(content(Whitespace\" \
         \"))))(Tile((id \
         74e411ea-2192-41f1-a491-52542aa916ce)(label(<=.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c387620-4088-416d-aa52-eb71aaad9c74)(content(Whitespace\" \
         \"))))(Tile((id \
         83777594-7daa-4956-84e4-49d249316e93)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a83a21a0-60d5-468d-85a4-1c72b433a691)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         549fd2ee-1478-4a61-a02f-61a90e07bee7)(content(Whitespace\" \
         \"))))(Tile((id \
         cb235f9a-a1a6-43f6-8a49-e47137c053bc)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e4fda28-20eb-43e8-b25a-7670dfdd73cc)(content(Whitespace\" \
         \"))))(Tile((id \
         4f771291-52a1-45aa-9c7e-6ca8b7b75f5e)(label(>.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cb9d4a3-fcf6-4b3b-86c2-6869676adf98)(content(Whitespace\" \
         \"))))(Tile((id \
         7a7d9e59-0fac-4894-9597-a00fcb94f946)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d24547f2-4025-430b-8f8d-7bb45a3dee3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1045c303-60f2-4cd0-b12b-625d93174b8e)(content(Whitespace\" \
         \"))))(Tile((id \
         2d24f84b-a057-4135-948b-e156f33a4e63)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61264722-9900-4ed3-b807-1a438436e515)(content(Whitespace\" \
         \"))))(Tile((id \
         b1006509-106e-40d8-a5fe-1a8beec40a00)(label(>=.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74e3c412-f9f7-4022-a09a-791aec271320)(content(Whitespace\" \
         \"))))(Tile((id \
         1de8081b-397f-4ef6-bb2c-635b90184ece)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b0f4bcca-b2d5-4358-8aa3-2f37a51f466a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         10dc8281-82a4-47e9-a522-e99455adb43f)(content(Whitespace\"\\n\"))))(Secondary((id \
         559fac50-02fc-4f7e-8eb0-d89e9d7ae21d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8833b960-1a9e-467a-a5c8-a75eac134525)(content(Comment\"# \\\"use\\\" \
         lets you set the default number format #\"))))(Secondary((id \
         e7fa1e55-40eb-4d29-99d1-c2dc5b3fa34d)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4425310-5a1f-45a9-ba1f-9e18dbd3e0e0)(content(Comment\"# for literals \
         and operators. #\"))))(Secondary((id \
         d8d095cd-2884-4e26-9848-9f93c687952b)(content(Whitespace\"\\n\"))))(Tile((id \
         b64ba05f-f0ee-459e-8cef-36dfe199666b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dee7dc22-52cf-4efc-8f1f-221b54d3fbf0)(content(Whitespace\" \
         \"))))(Tile((id \
         106176d8-f596-4a81-8869-5f7e9ff65b78)(label(natural))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5a8ccd55-7887-43f0-9e71-9ea5b5d59e74)(content(Whitespace\" \
         \")))))((Secondary((id \
         28eeb3d0-edd9-4838-89fb-9237f647238a)(content(Whitespace\" \
         \"))))(Secondary((id \
         a20a46e3-c40c-4674-a63c-b4d705f04d3f)(content(Whitespace\"\\n\"))))(Tile((id \
         96d39792-5c19-4a22-b343-842faefb8e45)(label(use in))(mold((out \
         Exp)(in_(Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2fd5e936-e43d-4205-881e-232d0088110a)(content(Whitespace\" \
         \"))))(Tile((id \
         a56eb526-9e79-4b33-ba57-95daf5b0ce35)(label(Nat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bef92908-a50d-4933-9f4e-5c13b3615c11)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15bd185d-c614-41c9-862f-39cf2710bb77)(content(Whitespace\" \
         \"))))(Secondary((id \
         c3f07ba5-cf89-4470-b33f-8b1e62eedc45)(content(Whitespace\"\\n\"))))(Tile((id \
         4b93e4be-d820-4524-8fcb-cd5de5cd9267)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad8050ee-7649-4e21-ac10-aa49f9ba995a)(content(Whitespace\" \
         \"))))(Tile((id \
         ba9c6017-4307-4271-8777-bdda24b91d4b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ee88478-6789-4638-b3f7-788bb6d0bba0)(content(Whitespace\" \
         \"))))(Tile((id \
         78653ba3-8408-478c-bc70-8885f17da0fb)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e7116b2e-d155-475d-9093-85c3384b0bac)(content(Whitespace\" \
         \"))))(Tile((id \
         7222022a-ec8e-42ed-abd5-70eaa0dd2d4f)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d980a40d-ab81-439f-975d-aa2980544bc8)(content(Whitespace\" \
         \"))))(Tile((id \
         8b70b69a-f8c7-4f04-9469-a54a41cfe35f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd710703-2675-43cb-8786-bc3745390b0e)(content(Whitespace\" \
         \"))))(Secondary((id \
         636ed15a-f94f-42c4-a8a4-216af8c78129)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2ffbb706-f8da-497b-a7cc-28d01593b60c)(content(Whitespace\"\\n\"))))(Secondary((id \
         50bbb21d-1038-49a4-bc79-14a4989aef28)(content(Whitespace\"\\n\"))))(Secondary((id \
         c67e3e18-7afc-43ac-9190-28b6733d70b7)(content(Comment\"# Strings \
         #\"))))(Secondary((id \
         d975b70c-fba1-420b-bfb1-ecd1e74615fc)(content(Whitespace\"\\n\"))))(Tile((id \
         fb5ca361-b6d3-41e9-a5a4-4bf82c1e6d8b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         629575cb-558f-4908-bf58-7f0c6587ddac)(content(Whitespace\" \
         \"))))(Tile((id \
         3c529270-92aa-4d7d-8348-3aeaef6ec375)(label(string))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5098568c-abf2-4168-9d80-37192729bed5)(content(Whitespace\" \
         \")))))((Secondary((id \
         b2081912-d8af-475c-9617-120cf5e752e5)(content(Whitespace\" \
         \"))))(Tile((id \
         daf0863c-9b29-4260-a790-79b3cd6eeb82)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0822a86a-4d26-486e-aa22-bb67c0e294b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c9020f2d-9f98-49d1-a70c-70eed6ec07bb)(content(Whitespace\" \
         \"))))(Secondary((id \
         c3739230-cf15-421b-ba3b-4e4466ee3c13)(content(Whitespace\"\\n\"))))(Tile((id \
         bc81cef6-848c-4da5-aedd-2be0c06d4880)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         56fd53ad-49a9-430d-83f3-17f70da1ff8a)(content(Whitespace\" \
         \"))))(Tile((id \
         8609fc58-2be1-441e-ba7a-b9d8668e5c8a)(label(concatenation))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         29e553f8-88c3-40fb-a037-3fb7a99531d4)(content(Whitespace\" \
         \"))))(Secondary((id \
         b2ee2841-aa4e-4f37-92d2-a8bfb9ca9e0e)(content(Whitespace\" \
         \")))))((Secondary((id \
         905c4a7e-fb2a-4692-b3f2-12a1d7b48fcf)(content(Whitespace\" \
         \"))))(Tile((id \
         55068302-a239-45d5-9cb3-a54951d03b43)(label(string))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3da3eff7-c6df-422f-9329-9a5122f9e830)(content(Whitespace\" \
         \"))))(Tile((id \
         d4d5f6ea-37a9-432d-8408-0bddf82e790f)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da10ec26-1375-46c4-b62c-955cd8475fcf)(content(Whitespace\" \
         \"))))(Tile((id d13ad37c-1709-4127-9457-28450e0cbb2e)(label(\"\\\" \
         Goodbye.\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e120652-a39c-403f-9b0c-1f6a6f904054)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         57384a7c-6a65-43b2-9f10-58185db39bfc)(content(Whitespace\"\\n\"))))(Tile((id \
         85974943-4b38-4a70-a9c2-dac5687147d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e323d05-382d-4f0d-8154-ed64c448a748)(content(Whitespace\" \
         \"))))(Tile((id \
         701b7b6e-d211-48f5-ad9f-234152f9d85f)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fcff3350-3501-463e-823d-95b8e05edef0)(content(Whitespace\" \
         \")))))((Secondary((id \
         2586b92f-5420-4740-b7ea-f1425baf8f61)(content(Whitespace\" \
         \"))))(Tile((id \
         742744a1-cd52-4e75-9e37-6c6308ea00a1)(label(string))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e152e50d-4dbe-445d-a0d2-909ece4dfee5)(content(Whitespace\" \
         \"))))(Tile((id \
         a257b714-5ec1-4d19-bfb8-77cc4d2df5e7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         404033ef-fe6d-4c6b-bf75-f99e9b0234f5)(content(Whitespace\" \
         \"))))(Tile((id \
         f4b64707-5220-4cfe-9e30-ebb4451d2fed)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ce4bf6b7-28dd-4911-8612-8c9eeac25ab3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         698f6131-fd0a-420e-abf5-84b3dd1ff7eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         1215e31f-e846-4bd9-85fb-565eb4b124c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0e9ebd0-1120-4bea-919a-a67fd4785f3c)(content(Comment\"# Tuples \
         (Destructured with let expressions) #\"))))(Secondary((id \
         a6fd7d20-e50e-448c-8278-a35d32a20093)(content(Whitespace\"\\n\"))))(Tile((id \
         d7680c25-53fb-4228-b122-bbf45d27b2b3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4277693e-e437-4284-9edb-6b5be83fbb01)(content(Whitespace\" \
         \"))))(Tile((id \
         0132f3e9-f471-4990-9f16-7d9dc5d8cb8b)(label(tuple))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3ac644d4-fc11-4108-a3fd-6db0a5a4153a)(content(Whitespace\" \
         \"))))(Tile((id \
         a74badfd-3e6d-40bf-8b7a-d2dac873a229)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         63984fb9-af21-465f-bd0c-e9c8208e7fe0)(content(Whitespace\" \
         \"))))(Tile((id \
         c9ebc716-c29a-46b1-b508-02a8cee8af73)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e6e12090-4cac-4697-a46a-443958b95e36)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c531f7fa-50a8-4890-8b09-7d06ff14d9cc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a2db2f51-37d0-492e-a0d9-b186751699a0)(content(Whitespace\" \
         \"))))(Tile((id \
         61b8fa4b-491b-4523-9c7d-e14db07a126a)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8e78c816-d369-401d-95b9-997cd6438a8c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e27d3431-7b4e-472f-abc0-aafc1c1993f7)(content(Whitespace\" \
         \"))))(Tile((id \
         9106e870-f4b0-4d28-a1e1-2b9906a91800)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3193f88b-5930-4a49-bc9f-e9a13431e6ba)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         15cbf0a3-6adb-445c-9803-83392aedae0a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         674b9b90-0f5c-47c5-a506-6b624702aa4b)(content(Whitespace\" \
         \"))))(Tile((id \
         7fa0132c-4365-478a-8f51-4f9dbdf17866)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         422626e5-b516-40b5-8311-9df7dc4cf24e)(content(Whitespace\" \
         \")))))((Secondary((id \
         7b3bd5b6-f9d0-43eb-9dcb-1b5bd484c5cf)(content(Whitespace\"\\n\"))))(Tile((id \
         dbb77f82-7f9d-46ad-8786-1dda9bf21ebb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4cd2753c-1d08-4f41-a0a1-2bd9e84d6181)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22b12233-53d9-4b3b-9380-bd875f8d8f03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f0dae7f-07d8-4473-92c1-abd16cb77e79)(content(Whitespace\" \
         \"))))(Tile((id \
         10bad358-77fe-49e1-88fe-34dafb717da4)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9f60e00-efea-4f0c-95b5-b68f71ff319d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13465bbd-8cb1-4832-af40-803b3b1640d9)(content(Whitespace\" \
         \"))))(Tile((id \
         ee6f13de-8920-43fb-966c-01721dcd6c4d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         17e91699-374c-4e3b-9949-afda5ac2cd8a)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ceca3db-c16a-4810-aee0-2ce8d502b752)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         260b864a-a45f-45f5-b202-faee7ed04342)(content(Whitespace\" \
         \"))))(Tile((id \
         24fb25fe-c597-4fa7-badc-ac2d2e063aa5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cb7e793a-1263-4287-a9ff-c4ad877294e8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc0fa2f6-e758-4c70-bb3f-8f53097432b6)(content(Whitespace\"\\n\"))))(Tile((id \
         b60d1a9e-01df-4e7e-87a9-56b8f61aa0cf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a7f3d612-f45e-45a1-83d6-827bf2295864)(content(Whitespace\" \
         \"))))(Tile((id \
         bed905c4-34bf-430f-8d8f-dbb3376a04ba)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         22d8d25b-54ea-4fa6-9e33-76f06ce935d6)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ff5a3009-579e-40fc-8044-7a106acf0ddd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         99778a2d-025f-4835-991a-3e2f19777485)(content(Whitespace\" \
         \"))))(Tile((id \
         31b45a9e-2aad-4a67-9121-35de5ef74bbe)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         588f2306-e70d-4b5a-9844-0686d0fec80d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fa3fa84d-5b0b-48b7-9c83-00ab952dc74b)(content(Whitespace\" \
         \"))))(Tile((id \
         245e2742-4e23-4cb9-9dab-96079bccf316)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         05aebfc4-cad1-4c81-b882-c3286d47170e)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         01fe9cb3-8c4d-4c37-be6d-7962362e2795)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         24192489-7b30-499c-9ad4-30d2d7757e15)(content(Whitespace\" \
         \"))))(Tile((id \
         70067dfe-9095-43ea-9060-597de9808e39)(label(d))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
         d417ff4d-98e2-4738-ab7c-4731b29649a1)(content(Whitespace\" \
         \")))))((Secondary((id \
         132e50da-e502-49b7-af84-8b6af7dbcec5)(content(Whitespace\" \
         \"))))(Tile((id \
         6155cef6-2159-4c48-82dd-c9de71fc3d81)(label(tuple))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f8cf3d7-1980-4cd4-ab6a-727bdc801d30)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c05c8c76-5cfb-4157-b6fc-16fde5597edc)(content(Whitespace\"\\n\"))))(Secondary((id \
         607e7dcb-85dd-4b44-99de-3ee7e45c7927)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d152058-31f1-4d68-9090-98150969c64a)(content(Comment\"# Functions \
         (Take a single argument which can be a tuple) #\"))))(Secondary((id \
         44e10792-e622-4d82-8388-f13f46fc7281)(content(Whitespace\"\\n\"))))(Tile((id \
         9d3c00f5-01ea-4d4a-a105-fbf8aa7e3926)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa79ea88-762e-41da-a347-9d2d792af952)(content(Whitespace\" \
         \"))))(Tile((id \
         ea038f07-4e77-4198-8a96-86fdfedc57fa)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         be2eaa65-26c1-41a8-aef3-2fdf13cb754e)(content(Whitespace\" \
         \"))))(Tile((id \
         26b0b3b8-f522-46cc-8e82-856460899e61)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         286968b6-8289-42df-9f84-e99da5e188b8)(content(Whitespace\" \
         \"))))(Tile((id \
         033426fb-14d5-4223-8458-c28f75d9a0c9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0b6f0a8e-9eb3-4ed2-a20a-6a916f3346ce)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ec282f64-b8e8-4027-abad-95b38fe7ad49)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         54465854-8d2d-4b60-9a50-7bd83424e0a1)(content(Whitespace\" \
         \"))))(Tile((id \
         b3dc99dc-46d9-437c-8bef-e4791eac224e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1da0d6e3-456c-4c4d-8091-f2f9c99ee89a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e739c2b5-8249-4de0-a54b-ec9becef7313)(content(Whitespace\" \
         \"))))(Tile((id \
         8fb424e8-60d7-4a80-8ba2-3eb67c4b9f8c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f34078dd-464e-4343-9222-66e302baa1d7)(content(Whitespace\" \
         \"))))(Tile((id \
         35236331-71ff-4f33-a8c6-c3247cbd2b6c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         01a89e00-c9b5-4c5e-a522-6a21c5300248)(content(Whitespace\" \
         \"))))(Tile((id \
         d2f6a8c5-4d62-41fc-9357-14ce804a2192)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0bcfa643-c873-4e33-8f30-f94f220cca0a)(content(Whitespace\" \
         \")))))((Secondary((id \
         a1f14075-caaa-4c5b-8f4d-bbf95b16887b)(content(Whitespace\"\\n\"))))(Tile((id \
         8b7950eb-c4dd-4ea8-9305-195fa5ed8f84)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ef4d581d-4043-4017-a3cc-40c28da4b6c9)(content(Whitespace\" \
         \"))))(Tile((id \
         ae33266b-eb30-4c84-8df2-2ad54a8f88e4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9fac6379-f9ae-48b1-962c-2f55bc6220e7)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         839f1de3-acff-4e5a-a185-a255967d17b9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cd05da75-276b-4b99-b0d5-50ba7735e99d)(content(Whitespace\" \
         \"))))(Tile((id \
         0d809e30-94a8-475e-b936-610c93ff2b31)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         975bb248-6506-4c07-94fd-cad176ef8a6a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         12457099-ea65-415e-8d31-b95797c7d0fb)(content(Whitespace\" \
         \"))))(Tile((id \
         4641d870-a986-463a-bc0f-8337f5078fda)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         79a64c8e-cf68-4b50-acb9-f1d0b9336c80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7b5af36a-8c9c-4c1c-a9b9-f9c6d466f798)(content(Whitespace\" \
         \"))))(Tile((id \
         a010bfbd-0d4f-458f-95ca-eaf6399a3e79)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d850f308-c0ff-40e5-a84a-2dd2161dec14)(content(Whitespace\" \
         \"))))(Tile((id \
         35eb9a4c-4752-48a9-a627-9f953e840f59)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a8777a7-0c7a-4b0b-94e3-5d30ca729fa3)(content(Whitespace\" \
         \"))))(Tile((id \
         87e1dee3-b513-43b1-bdc3-5544b7a27d33)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45547b33-a47a-42e1-a745-ff61c614bad6)(content(Whitespace\" \
         \"))))(Tile((id \
         10fe11b4-5dc6-47be-9fe8-0e85ce0aa808)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0700e74-1fa6-4319-abeb-dfe8bb8ddcb7)(content(Whitespace\" \
         \"))))(Tile((id \
         5cb6889c-4a9a-4eba-8cc7-0357e523760e)(label(b))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cea7c936-6982-49ee-923c-db131b3d968f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2b9c7d92-de54-4927-b205-f117a1dd35ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         46f94e60-2ee1-47d5-881d-20834a9433d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5979e4c1-75cc-4c20-9d9f-2de303f64975)(content(Comment\"# Recursive \
         Functions (Arrow type annotation required) #\"))))(Secondary((id \
         9bf699fa-a2ad-4d96-b70f-3b02c672d9ce)(content(Whitespace\"\\n\"))))(Tile((id \
         f6200c50-756f-4935-ad96-a51887cdf767)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         78f2d59a-e180-4eda-aed4-ca74d73b5de8)(content(Whitespace\" \
         \"))))(Tile((id \
         1b56078e-d7d6-48b0-afcc-d6684b9a8bf2)(label(double_recursively))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1ffd4d1c-85e8-4dc9-b3e4-2df5662d5f29)(content(Whitespace\" \
         \"))))(Tile((id \
         5dde2585-ed51-4994-9937-0844546c2d9c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d27330bc-d3f1-4f7c-88c1-a2f8929ad62a)(content(Whitespace\" \
         \"))))(Tile((id \
         5b013f72-ad3e-4eb0-b4bb-7d2020acf4e4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a3c07e35-08f7-4d5e-81f7-fd5e86c58b83)(content(Whitespace\" \
         \"))))(Tile((id \
         1f9851bf-3bc6-44dd-bb57-aeb532df45db)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e3c6f446-d2aa-4426-aab1-6ede1265da71)(content(Whitespace\" \
         \"))))(Tile((id \
         90ed98c9-9dac-4784-bab7-3c2941328049)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         92fed24e-ccb9-416b-aff7-0c8b3a74d555)(content(Whitespace\" \
         \")))))((Secondary((id \
         1e8ce905-f6d8-457d-bdb9-081e56f21ce9)(content(Whitespace\"\\n\"))))(Tile((id \
         d26942bb-b453-4749-88c6-4ed163fef065)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         02c7dfab-661c-4315-9369-0c343ef9d100)(content(Whitespace\" \
         \"))))(Tile((id \
         ce988a97-4d98-4ddf-aadd-37b9b703ba14)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6b5134ba-f638-41dd-b280-1e8a4fd6f86e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6af15293-57b7-4848-b6e9-26b807cf885f)(content(Whitespace\"\\n\"))))(Tile((id \
         ad4a8f26-9ef4-40cd-bd46-e6fc7f89707c)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1ad40388-0931-4c93-ada8-fff26058ed05)(content(Whitespace\" \
         \"))))(Tile((id \
         a5d2a1b9-6612-4dab-8123-a405b47efcf3)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         325046bf-7830-4a7f-953b-c944c8e818d7)(content(Whitespace\" \
         \"))))(Tile((id \
         e8bb30fa-ae1b-46ea-ba8a-4e5f6deae428)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         777a3fd9-26dc-4c27-8c0b-7dbe688ef27f)(content(Whitespace\" \
         \"))))(Tile((id \
         329fc154-b391-4bc9-b4b3-1c252ac49da9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96d98d26-5291-45b6-bf9a-e0448ebd2a8e)(content(Whitespace\"\\n\")))))((Secondary((id \
         891484aa-d44b-4c6a-a57c-d0634c46441a)(content(Whitespace\" \
         \"))))(Tile((id \
         316630d0-05ac-4266-991e-3d020f20a6b0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         09c3092b-1092-4275-9aeb-6281bf28d0b5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0ba324d4-8732-40b9-932e-6256df956d9c)(content(Whitespace\" \
         \"))))(Tile((id \
         10026090-11dc-4dc7-a228-00e7c42ea284)(label(double_recursively))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8d325fa-c522-43c5-b652-b4fef7f9de92)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18b38b81-abe6-4efe-aa32-fed3d984c575)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         49c5a832-4cf1-433a-af89-a47400f511f2)(content(Whitespace\" \
         \"))))(Tile((id \
         8e297b8d-b666-439a-b6d9-20869f1ab99f)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a8ff029-2438-43ca-93c6-58c75c197aca)(content(Whitespace\" \
         \"))))(Tile((id \
         7cc3b5cb-c0af-4e72-bcb4-41fccda3c8da)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8e76dd1f-491f-4997-a916-1ab614de5cd6)(content(Whitespace\" \
         \"))))(Tile((id \
         3158c89f-6c78-4be0-bee6-67f64dc5560f)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9a5f500-f200-4f42-aae4-b39e8416f4d4)(content(Whitespace\" \
         \"))))(Tile((id \
         1e42a814-6b6b-48b6-8014-a6dade8f64ab)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd5498b8-e293-4ce9-856e-bb234b2d51ec)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aeedfaf8-9797-44ec-acf9-58f9bd414c56)(content(Whitespace\"\\n\"))))(Secondary((id \
         b05ef2c2-7466-4d65-8d5d-e29815ae6c16)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d8ff95f-fd25-4b73-8a26-9e9b6ff59e99)(content(Comment\"# Mutual \
         Recursion (bind tuples of functions) #\"))))(Secondary((id \
         8541e084-58bf-4482-9caf-7f95c3094c07)(content(Whitespace\"\\n\"))))(Tile((id \
         d6abfcf3-26ea-48aa-9e54-2822a8c71270)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6aa35f3a-3018-41d8-a827-4355573d1489)(content(Whitespace\" \
         \"))))(Tile((id \
         5b140e52-06e1-4176-b0f3-4883d6df9fbe)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         ceeeb5db-99ba-4b16-bd06-e3443866d619)(label(even))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7bb8f41e-6bf7-476a-8804-d1c0a29c37b8)(content(Whitespace\" \
         \"))))(Tile((id \
         093d0eba-6839-4ccc-a790-c19941d38c4f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         53baaf8b-67e8-4d75-855c-7959a8fe31d3)(content(Whitespace\" \
         \"))))(Tile((id \
         3ed21534-ab12-46f9-bc1a-5f5a61c71f8e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         09a592f2-2a39-435c-b146-b804785d79db)(content(Whitespace\" \
         \"))))(Tile((id \
         8aedef3b-d284-442e-8b80-ed4191ef7d3d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         07602f3d-ca2c-4b8b-b353-69fb207bacab)(content(Whitespace\" \
         \"))))(Tile((id \
         58ba2981-e5b2-4190-9983-1b030df37888)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7083e4b6-bd7b-4838-907e-f109e4ef9d7e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8112f3d7-bfdb-4ada-af35-ef59df3e70ad)(content(Whitespace\" \
         \"))))(Tile((id \
         f650f93f-79ea-4a39-b1c8-5c0d9b93b996)(label(odd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5ace9d33-af03-41d4-b8e9-4f89fe24184e)(content(Whitespace\" \
         \"))))(Tile((id \
         6e8f3c45-609e-4daa-a474-f93e8f6f08e3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8cd9891f-982d-4d33-84f3-18c0f46e4a0f)(content(Whitespace\" \
         \"))))(Tile((id \
         87c06c75-12e8-4c72-899d-b8c36df1cc74)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f769ed32-219a-48d4-bbe1-9ae3fe61d72c)(content(Whitespace\" \
         \"))))(Tile((id \
         fe16f158-4fba-4ccc-90c6-dcae0ef90c06)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d18a7a6c-7ae6-436b-bb27-36f20a950624)(content(Whitespace\" \
         \"))))(Tile((id \
         ba1cbf22-e6ee-4815-bda7-e05b3eca24f4)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d036e3ca-14fd-4fa8-b87d-39219d73c9d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         3945d64e-ef17-49a4-9238-346c8993ca49)(content(Whitespace\" \
         \"))))(Tile((id \
         4e9b3fde-fd77-4f8a-8ddf-92457d7942e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d712405c-d5d9-4480-80b6-a0e3e41dddbb)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f9ac17-f2fa-4eb9-a548-5b7cd82f63be)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         800e25d6-c2d0-4ac8-a984-a16eca3c3a76)(content(Whitespace\" \
         \"))))(Tile((id \
         1faf370a-75dc-48cf-8dba-a7cb48184a6f)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9ed5aab4-019f-4e6b-b803-feb88af6df58)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         286bca3c-276f-4ebf-9957-9e29b9f1e309)(content(Whitespace\" \
         \"))))(Tile((id a44a3c8e-5456-402b-a568-2809a41aebdf)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         4e6994f0-9ecb-46a5-903f-afafd98c4db3)(content(Whitespace\" \
         \"))))(Tile((id \
         24dd8761-630a-4673-bc48-45871c294ffb)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8c1c2d6-e16d-47f9-a365-d88cc13516ef)(content(Whitespace\" \
         \"))))(Tile((id \
         b8f1cead-2b02-4e3d-9c2c-693c63c0577a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c170cfd1-9910-42ad-9a4b-04afebb22287)(content(Whitespace\" \
         \"))))(Tile((id \
         635046af-a187-494a-becd-49a798226125)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef62306a-48dc-4496-aa93-fd8de52fa20b)(content(Whitespace\" \
         \")))))((Secondary((id \
         2593058d-8025-44cf-909d-e96b802b8468)(content(Whitespace\" \
         \"))))(Tile((id \
         5983fb96-dd10-45fe-8f6c-d1d3d4c3f33e)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc6a9575-af45-4505-95b3-651324abeff0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cda0bf0b-3424-46b9-bd5a-c221cf3d8eaa)(content(Whitespace\" \
         \"))))(Tile((id \
         7ad344a5-5622-4305-8c7d-a0b731f0a397)(label(odd))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cd4fe32-753e-4e48-b880-ace0cae0cc11)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         431744ff-d65a-4c02-8ede-574424652f52)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bbf36e45-f16e-4a33-a9dc-ec292ca3d515)(content(Whitespace\" \
         \"))))(Tile((id \
         d6f52cdf-9ed5-48ee-b53f-8e35bed2a349)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70b14704-4fca-4b00-9edc-135c4a1d471b)(content(Whitespace\" \
         \"))))(Tile((id \
         44e97bd1-703f-4d2e-96c7-0128c3f7c3c0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         46ffdc23-0b29-4487-ba37-c97c199c9bfa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96e70af9-bb9f-4612-a2b4-7ab27b7d4c7c)(content(Whitespace\"\\n\"))))(Tile((id \
         888defc7-ef62-40f8-aa96-9d1004392508)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9ac459cc-fef1-4a1f-9096-9690358b0827)(content(Whitespace\" \
         \"))))(Tile((id \
         22da0947-dfcb-436b-bc5d-8d8f27603d2e)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5b7d3b05-e3fb-49fa-a049-0fb88ee7ad21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9eb4b411-f83f-4ba3-a817-d9946dda18c2)(content(Whitespace\" \
         \"))))(Tile((id 5c9c6f87-baec-48e8-a82e-bff508f5d2c7)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         574318a1-e909-4405-8ff2-daec5a9d91d5)(content(Whitespace\" \
         \"))))(Tile((id \
         444f21d2-388e-45f4-84eb-25554dc797f6)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97f80b2e-8e75-4498-b472-95d07c502f8f)(content(Whitespace\" \
         \"))))(Tile((id \
         c57674c3-481c-4584-a8e2-3ca67fbe9dae)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60dbcf0f-a73b-442b-a8e5-01bf611fdbb7)(content(Whitespace\" \
         \"))))(Tile((id \
         e0a11e74-7c34-4f58-9183-db40a040f21f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee567279-2a11-4f3e-8c77-0a59ef9e33c7)(content(Whitespace\" \
         \")))))((Secondary((id \
         7c686f43-9a52-4aa6-9eda-0ff476302c94)(content(Whitespace\" \
         \"))))(Tile((id \
         520819da-ef09-4c02-83fd-3139c6925359)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15629605-a69f-470e-bb71-8a3f1f339cb3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         393640f3-fa07-4f05-aa9e-1de63838700a)(content(Whitespace\" \
         \"))))(Tile((id \
         74dbcbed-f97c-4e62-86e9-857c91bd9504)(label(even))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0166b48d-6a5c-40b9-a73f-0b86e5888160)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c69297f7-a27a-43e2-8449-a167b5d93126)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         636ad9ec-5029-4b87-aadb-21a4371802c3)(content(Whitespace\" \
         \"))))(Tile((id \
         130d72bf-7f6f-4d42-b829-5390682bbe6f)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30e3e0ae-af86-443b-a1e1-629c2c578ee0)(content(Whitespace\" \
         \"))))(Tile((id \
         55be2eea-3dd3-4fd2-ac8a-cc05b7969023)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         05bed412-ce7f-49b1-a1b4-3f1ec5d03778)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         87ef9874-ca63-4191-bf92-4c2e701c04a5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         290a99f8-ebd9-4974-a17e-b412e91e095a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4436ed0-fb22-4ec3-ba88-ac5b21d4cf00)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fa15230-677c-41da-9342-76a5cc1e9305)(content(Comment\"# Lists \
         #\"))))(Secondary((id \
         d29f517b-82a8-4239-a060-d247a5d4337b)(content(Whitespace\"\\n\"))))(Tile((id \
         2cfe6ff1-e119-47b5-82db-ff0f105053f8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da795995-a29a-4f72-96a3-f2f75c2f0f39)(content(Whitespace\" \
         \"))))(Tile((id \
         56ba338f-a5fb-4e65-ad54-c49775f1bb1a)(label(empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         edef03e3-5c60-4134-9f94-d27d9ebca980)(content(Whitespace\" \
         \"))))(Tile((id \
         a1975631-8bdc-44ff-b670-7c81021540d0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b442e400-8d35-4798-a086-cdb34a02030b)(content(Whitespace\" \
         \"))))(Tile((id b5861c6c-80cf-40be-91b8-d46e10e86720)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8d131e3c-4d06-4da2-8385-2d0b88d643bc)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e1c3c859-9b5c-4f1a-a7cc-c17674a94c1c)(content(Whitespace\" \
         \")))))((Secondary((id \
         df025eff-0d92-49e9-9c03-102cf4d5b34b)(content(Whitespace\" \
         \"))))(Tile((id \
         a782ba0b-bead-48b7-b313-8df93602f16a)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3449aeab-eeb8-4586-b06c-600d8abbde33)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3e8891ff-5966-497f-b563-4efb14bed3f6)(content(Whitespace\"\\n\"))))(Tile((id \
         1a41eb19-fb40-4eb8-9976-fb719e5237fd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         36f4a676-3a5b-45f3-a2b5-020f56e2fc8f)(content(Whitespace\" \
         \"))))(Tile((id \
         5274a313-a0c0-4d65-8219-eeef3d7367d0)(label(non_empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         93693839-2bcc-4cf6-9abd-07d7ead328dd)(content(Whitespace\" \
         \"))))(Tile((id \
         aefa4d81-8fb8-4abb-a002-d6574fc9f23a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c574319b-47be-4e09-a783-be9f6a017a0b)(content(Whitespace\" \
         \"))))(Tile((id 129be9ad-11a5-4327-98ea-cb0c47871ce5)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         efeadf98-61f8-4aef-a043-ff97c3df0338)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         00dd0fe8-4513-4506-981e-589a1d9522ad)(content(Whitespace\" \
         \")))))((Secondary((id \
         703f42d0-0c99-41a6-b9bd-8f9270811bf7)(content(Whitespace\" \
         \"))))(Tile((id \
         a8ff6840-b20c-4123-a3b8-4d44735b9496)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ccb8374-12fe-449b-bedf-2c4fb93bac8c)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3005a50d-2d19-4e54-b6bb-25464748c57a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2fa4350-5fd8-4554-8697-808126fda66e)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         28f5d94f-5b83-4aa1-89e3-3eb87cdf596b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         355efd62-733e-4333-8c26-fa1d00a517ff)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f427292f-22c1-4fee-8ff6-30e91fa40663)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f91b8ad8-2145-4f37-b6be-36b11547005f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b75c3cb1-07a1-41e3-a2e0-f76096611123)(content(Whitespace\"\\n\"))))(Tile((id \
         5b462811-4586-4b0c-aceb-9a2e28f3c22a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9797c54f-b982-456f-86e9-af197a2471a3)(content(Whitespace\" \
         \"))))(Tile((id \
         1d94bdd1-338f-4c96-ac37-8b168dfc551b)(label(list_literals))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1b61a6f5-8a2f-4da2-b37a-c9566ffaebe6)(content(Whitespace\" \
         \"))))(Tile((id \
         fc299700-3220-45b4-8871-0fd2b75b408e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         89cfdd43-afe4-43c8-a2f8-e130331eb213)(content(Whitespace\" \
         \"))))(Tile((id 898f57af-ac51-441d-b2af-3ab13568bbe5)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a0ec58ae-c737-4a2c-81c9-8644bdc4fd0b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8d479c6b-9024-4ff2-8719-23ff9c3fed30)(content(Whitespace\" \
         \")))))((Secondary((id \
         d53b07f7-007c-4c6d-b34e-643a5d2fc5a4)(content(Whitespace\" \
         \"))))(Tile((id 62db601a-016b-4b2f-a7f2-856618d5bec8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         852e8903-0e59-49e4-b03d-3488860458d2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9490f8ae-f4d9-465f-a9a6-5a4153492615)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bf5200c-5b90-4097-8720-96567e5fc5a0)(content(Whitespace\" \
         \"))))(Tile((id \
         32bd093e-f48d-4e9c-85ae-958b9999f995)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5bc12b28-827f-41a2-ba4b-2cc1cf6b8aa1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         afc4d785-cc3f-4cde-b7a6-fd01c59d1672)(content(Whitespace\" \
         \"))))(Tile((id \
         5fd1135c-77cc-4d14-af1c-886ca315992e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e5965592-4c70-4737-80c5-b11a2149b510)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9055dd32-101d-4d71-8633-50706b2ec2e4)(content(Whitespace\"\\n\"))))(Tile((id \
         81598378-ad19-4a2f-91cf-0e2901197f14)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         95224600-c913-49bb-b5da-16c67e9aea26)(content(Whitespace\" \
         \"))))(Tile((id \
         72918fac-dbee-488d-aa32-4ecd7f4c1a9d)(label(length))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd576208-872b-4ba0-b06f-9a8421f57ff1)(content(Whitespace\" \
         \"))))(Tile((id \
         d57b7969-4df4-42a3-a63d-d8f8937b6b49)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5bd7e776-ab89-4d44-8a2a-e65251a41b0a)(content(Whitespace\" \
         \"))))(Tile((id ca3346b5-dc84-4a10-81c1-03d74845df49)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9979377c-451f-41a1-b210-6a50cfb46215)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         95ae7bc8-8426-4101-bb1f-1495d0238162)(content(Whitespace\" \
         \"))))(Tile((id \
         26f5d87b-230d-4eda-89e2-1821e4d1f8b1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         526e5f9d-3489-4ace-89cc-ec2137b70fd1)(content(Whitespace\" \
         \"))))(Tile((id \
         3618aee2-ee0b-48d7-839e-091ead3e10a4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e5704a9d-715e-4c2b-b2c0-290553134ee1)(content(Whitespace\" \
         \")))))((Secondary((id \
         48f0e724-da6a-4006-b629-b666266a1011)(content(Whitespace\"\\n\"))))(Tile((id \
         6004c051-45e2-47e7-a1a2-d6ef61f2a73a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4c3af32f-e53b-4382-a268-93e455efe79f)(content(Whitespace\" \
         \"))))(Tile((id \
         6c0a94e3-87fe-42ed-acb2-54c6080a19fe)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73b1310f-ca62-47f7-8c48-fc062c4034c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fe945ef6-894b-4b5b-9c79-ab182c92a850)(content(Whitespace\"\\n\"))))(Tile((id \
         82f68230-0e66-485b-bed2-62a3639ea04d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fd2b72de-1e0a-4ec9-aae0-f4baafc828c5)(content(Whitespace\" \
         \"))))(Tile((id \
         7c2fddb7-594b-4164-8682-056ac9eb01c6)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eabdf047-2dea-4ce5-a241-f21f765999f7)(content(Whitespace\"\\n\"))))(Tile((id \
         c9837160-e3f9-4d53-84a2-f56fecceac3d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d64a4907-775d-40ea-8035-03e86968874a)(content(Whitespace\" \
         \"))))(Tile((id \
         ec610f3d-dbe3-45ee-98c4-13cd4e746d51)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         27e5f314-ecb1-466e-8981-e88db1d6e381)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f835e533-8b69-40bb-8b61-4a7ce173f2b6)(content(Whitespace\" \
         \"))))(Tile((id \
         f7cf7d00-f5a0-4a11-8eab-f40a165729a5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         12a7f5bc-ece5-4576-804e-f78093487241)(content(Whitespace\"\\n\"))))(Tile((id \
         41a75894-9d86-4b38-af9b-dc3c912fac1b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         239a800a-be31-4bd2-bb03-f92284643933)(content(Whitespace\" \
         \"))))(Tile((id \
         227706a9-b643-4ae1-b8f4-8ff161d0a921)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1d7c0b1d-d496-4e5c-858f-06b07dcc8b32)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         8926df36-270f-473e-bed8-e71491ca712d)(label(tl))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a2c18154-dee4-4c4e-95a0-c3a97fa2eda0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a69ab8e-0039-4749-b6de-afc8595b2ee8)(content(Whitespace\" \
         \"))))(Tile((id \
         68d4c662-f760-402f-b65c-2bbfc1a0d202)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07c3138c-3899-422c-8e78-d26bfcb24a7b)(content(Whitespace\" \
         \"))))(Tile((id \
         adeb8b71-4008-47c8-8fb8-00339030d41f)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a1afaad-bf7b-4e08-a16f-5ffe24d9ff5b)(content(Whitespace\" \
         \"))))(Tile((id \
         45a9e3a1-4e89-4783-a73c-6ad4dc467eb4)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c70378f-34d0-4895-9ef2-69a3d062e018)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         68629bb0-bf5d-4eae-aa83-1435febed124)(label(tl))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2c0bd7e3-6b12-4a48-aa70-a31fc1618300)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a0e3c5cb-ba55-4ea4-b2b9-098926eb190b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d38d972-0557-4888-8cde-0bc9cadce2f0)(content(Whitespace\"\\n\"))))(Tile((id \
         b48addbe-cc8c-4c49-ad8a-91101a8ee61b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d4f944b-62af-4f65-96fe-851a644a7034)(content(Whitespace\" \
         \"))))(Tile((id \
         cd7cea2e-a8b2-4c01-ab97-5794c96a9329)(label(has_at_least_two_elements))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         611ceab7-bb72-42bf-a7c4-6ceeb7f74ff8)(content(Whitespace\" \
         \"))))(Tile((id \
         07d0876a-308a-42c3-8511-9fe1ef337a88)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f8387161-c1ed-42ab-a191-6676a5a94b6c)(content(Whitespace\" \
         \"))))(Tile((id 376e0441-61e6-4266-9aee-e726e713f5b2)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         db9ef4ed-2fae-465e-bb56-882d488c140b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         326e99fa-3ec4-4c09-a35f-9414428fcc4e)(content(Whitespace\" \
         \"))))(Tile((id \
         df35925b-230f-4c35-ad28-d5c1ad2adca9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e19c6bab-d745-401c-9ca9-ae04afb4318e)(content(Whitespace\" \
         \"))))(Tile((id \
         bfc893d4-05b3-401b-a3e8-5d97473fd7dd)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8c23c11c-3a9e-4be5-bfad-2ce834aa7c20)(content(Whitespace\" \
         \")))))((Secondary((id \
         216a39be-449b-4ee1-8014-4de10ba398a1)(content(Whitespace\"\\n\"))))(Tile((id \
         24c153ae-18b7-4f99-8c12-38982ff0343b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         915542c2-797e-4483-b29d-37be17a92b56)(content(Whitespace\" \
         \"))))(Tile((id \
         02d0ac58-da21-4f7f-832b-4e6a639ee911)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c1648abb-f146-438f-bc2c-d62efa928350)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cdc769ef-39cf-4f9f-b657-86c06e6de585)(content(Whitespace\"\\n\"))))(Tile((id \
         47913b4a-0816-46f5-b6d8-87bc28853f64)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6e55722a-75f5-4600-974b-dd644f565ccc)(content(Whitespace\" \
         \"))))(Tile((id \
         22940400-64ae-4b10-be50-234d28ef6880)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd5a075c-4de3-4f82-8d6b-8ee7f020a6ad)(content(Whitespace\"\\n\"))))(Tile((id \
         633b16f8-0466-4723-a251-bdeff4fd35be)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         77cb298f-8afc-424c-bdf3-35674d4fce3f)(content(Whitespace\" \
         \"))))(Tile((id \
         dd1132c6-9aa8-4960-b99f-24c6e969a57f)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b66e276f-baa7-4a65-8131-d34672b60ad3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a9981520-09d4-4f6f-b642-4e6bbb471e04)(content(Whitespace\" \
         \"))))(Tile((id \
         ebaf4925-4ede-42e0-ae3e-3266f3eb3833)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57f5bf45-0e38-4358-9510-4ac6a66e9592)(content(Whitespace\"\\n\"))))(Tile((id \
         988f89b7-488a-4f73-bd18-04012293f617)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         61a2c6a8-0df9-47bf-a0f5-f7cf4f756c34)(content(Whitespace\" \
         \"))))(Tile((id \
         e1bbcb8e-59a9-49e0-9a82-b33a477577dd)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1e9817e9-72e4-4ab3-a0e5-914adbd74b37)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         99c44c48-7b2c-495f-af96-44944cd865c4)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         860e80e8-d6c8-4337-8256-74d185ade28a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca51db1b-4f2e-46b8-8414-1c08ff297178)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf0533f-185a-4be5-81d6-00d9590810d3)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1d404a7-b7e6-4716-bbd3-6002b3130c52)(content(Whitespace\"\\n\"))))(Tile((id \
         fe96da51-a645-4571-8670-7b8601a13b6c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         417d6fc3-0405-4605-af0c-f364d6b22db7)(content(Whitespace\" \
         \"))))(Tile((id \
         103e3c9a-abfd-4878-af36-a930cdb52cec)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ebe1ae7f-aa78-411d-8574-3e21d9509b16)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         0cb6c520-9f18-4c8b-a291-350da4ae3b43)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         73574484-a356-42b6-a9f7-ae8343582e19)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         d973e2c5-c6cd-4cdf-afa0-82aeadda496b)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5338f1aa-795c-412f-b2ae-5a886660f06d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5faf062e-639b-42db-8779-bcc23b948629)(content(Whitespace\" \
         \"))))(Tile((id \
         3948c61f-052c-4a70-b2c1-676682af6b43)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a166273-aa7a-4cc1-b138-42b772440385)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         70af3315-d458-41e0-9051-6ed6c09f1af0)(content(Whitespace\" \
         \"))))(Secondary((id \
         838c7177-5f5f-4e80-877f-a04258460685)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         83d373b0-e7a9-44cb-bff4-3fbe579dc197)(content(Whitespace\"\\n\"))))(Secondary((id \
         43f3bc38-ff09-439a-9159-264caa93f250)(content(Whitespace\"\\n\"))))(Secondary((id \
         40caace0-b279-4038-91f2-96e173e95a31)(content(Comment\"# Algebraic \
         Data Types #\"))))(Secondary((id \
         873a035b-1484-453b-abaf-3c10aca6216d)(content(Whitespace\"\\n\"))))(Tile((id \
         800f93dc-6ff9-4d70-b4f7-815fbea63d30)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         89e9ce50-04a6-4498-a6b7-1940f1940b84)(content(Whitespace\" \
         \"))))(Tile((id \
         c105791d-f576-443d-8109-4be0c1b3490c)(label(Exp))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3bbe05ec-8117-4bf7-ae1e-32ee26828c5a)(content(Whitespace\" \
         \")))))((Secondary((id \
         ec04b0e1-c1ff-4309-8b9e-6698958fb2dd)(content(Whitespace\"\\n\"))))(Tile((id \
         0400b7ad-41df-4397-b98f-b4ed2862145b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         89de8c44-f938-46d6-92b2-c24afcd8df32)(content(Whitespace\" \
         \"))))(Tile((id \
         bd8b3c28-2efc-45e7-b09c-89be8f29ba74)(label(Var))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7907608e-dece-4601-93ea-4aa9f9d1c12f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3318361a-012c-4a0a-96d1-5ea494b1e5b0)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ff0ebdde-4f09-46d4-9991-7d6a4d6d6c7f)(content(Whitespace\"\\n\"))))(Tile((id \
         c9c792b5-eb03-4ddd-a8fe-8426b12f0ba1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         498c9a45-99a6-4bcb-aff9-6c1737951478)(content(Whitespace\" \
         \"))))(Tile((id \
         e046bf23-7acc-4b6e-a630-2985db6d1792)(label(Lam))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         831d85ae-8b29-47c1-abf7-bb55f240e71b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8b45ae06-f6e0-4da9-bb60-ef8c5ad4df15)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         209e047d-8beb-4dd1-bbfc-3cf49c5a85ac)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1a4b1e22-7cef-474e-80ca-b33761ce3b07)(content(Whitespace\" \
         \"))))(Tile((id \
         2770689c-bd18-45a7-a648-d5f848ae262a)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         138ce8bd-c28c-414f-b2eb-f7ce37db76dc)(content(Whitespace\"\\n\"))))(Tile((id \
         6a8f4ccc-2589-4f76-ad60-1522a1db6a91)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f2f57595-5b8a-40e3-bd28-e3d5d449cb93)(content(Whitespace\" \
         \"))))(Tile((id \
         13c83cc5-34ec-44fa-bcb5-d9c5916422b8)(label(Ap))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1aa2ac4c-ddf8-413c-9d62-02797cce348c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         24075276-a5e8-413b-8753-8280d21c87c3)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         38be7ea3-f719-4e5a-8c01-6de99bbd9e2c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d339794c-a063-4676-965b-eda06a1bc785)(content(Whitespace\" \
         \"))))(Tile((id \
         89f39054-8d04-4d69-a7e6-bc2d927bf3d9)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         aa56b453-9d4e-4560-b0af-1b41b018c5b6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b850f90-4e8a-42a6-96ab-9ef16dd4efc8)(content(Whitespace\"\\n\"))))(Tile((id \
         2a973634-6c47-40ae-ac8b-3c88bd28a769)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b831315f-d149-4d3f-abaf-915cd5b9c446)(content(Whitespace\" \
         \"))))(Tile((id \
         73e70cab-1a3c-44ad-b658-1cc1c09a9d7f)(label(exp_equal))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8b71c9ef-c842-4319-92f6-7841373e27f3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         482650fe-cc79-4244-8d17-0ab7f5032885)(content(Whitespace\" \
         \"))))(Tile((id \
         5a3e8322-1ec7-4527-8d17-d2f806c39bee)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         47b6faaa-9d57-4022-b970-c3e2ca8e28e8)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1c377df2-aa76-458d-9eb9-e70ffd8e4696)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dd81557e-f55a-4f70-b87d-4f9c8128dc86)(content(Whitespace\" \
         \"))))(Tile((id \
         c2114842-2e99-4ff5-9844-cb9230bc281d)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         56592ebb-e782-418a-a45c-9f2d58e2f980)(content(Whitespace\" \
         \"))))(Tile((id \
         83997eee-23fe-4a77-a590-4e40fafde88d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         83baef07-6c79-452c-adca-e92fff93d2ed)(content(Whitespace\" \
         \"))))(Tile((id \
         4becc9b5-63fe-4890-acdb-438213268128)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9ceaada4-06fd-455d-a74b-4c5a71835434)(content(Whitespace\" \
         \")))))((Secondary((id \
         6f13685d-f3c3-424f-8f26-69aa456e6cd3)(content(Whitespace\"\\n\"))))(Tile((id \
         d2c7330d-13f6-45d2-b69b-2aeab8e617d3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         10eea70c-211d-4f7a-9625-30078b8b3cbc)(content(Whitespace\" \
         \"))))(Tile((id \
         435c3efb-8e64-4892-a262-cd40348f0b99)(label(es))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         285cb946-efe9-43b4-8bcb-f9337612b0a0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a5d3a1b2-9b1c-485c-8ba1-67d6f271d382)(content(Whitespace\"\\n\"))))(Tile((id \
         79d2ebee-bff3-4e35-a993-447cd376a07c)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         12c274f4-c14d-4f62-ad75-f215970762ca)(content(Whitespace\" \
         \"))))(Tile((id \
         6affc4aa-f8d3-47c0-beb6-a8dcab1b303c)(label(es))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eff775c0-077c-4d9a-9d9d-a1f37c1533e6)(content(Whitespace\"\\n\"))))(Tile((id \
         a3f9e8a3-6eec-4dbe-bae3-d0b86103bfda)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         13d1cdef-2640-4400-8bce-97a989dbcc78)(content(Whitespace\" \
         \"))))(Tile((id \
         d4d67507-e369-4221-889f-51eb9719242c)(label(Var))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5e38984d-b3ed-4af9-9bac-366e399e1748)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         2739771b-4e28-4c96-b0f5-adb3eb0a6725)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         7223a512-3a43-4826-9d4d-60e13343b0f9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5dbd149d-e5c0-4f7e-af3e-8201684bc1a0)(content(Whitespace\" \
         \"))))(Tile((id \
         480e9205-d62a-4da0-903d-b91a0b043637)(label(Var))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a8d773ef-9364-4959-aee7-abdf1ba3ad15)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         8de085a7-863f-4b22-9175-8237270fe8a8)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1a9cc9f0-abf6-4adc-a232-7b240e748d58)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         851c2a61-7ee9-4c89-960b-b0d425a8c8b2)(content(Whitespace\" \
         \"))))(Tile((id \
         1ae8a229-2afb-44ca-a40d-9eb7364d3ed6)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ccd03d0-df71-4cc9-bbe5-a47ec5761d32)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c223464-f543-4eff-8306-ba80775f3a6d)(content(Whitespace\" \
         \"))))(Tile((id \
         b6270516-4528-4e00-84bb-8e6abc9d51a2)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6338ef5-ee69-4640-8388-dd3478bd00ab)(content(Whitespace\"\\n\"))))(Tile((id \
         b22d0da3-b8a3-4402-8001-9384678d8c55)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fd492ecc-8a19-4bbf-8e75-98f7516754f9)(content(Whitespace\" \
         \"))))(Tile((id \
         25d46b7b-9cc0-40d2-a736-afffead384e4)(label(Lam))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7b9c6711-e624-4a74-85a8-e6813391dd70)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         d368d836-f3fd-49b5-98a2-3b877ec8089e)(label(x1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7d88b3b3-5a1b-4a2a-93c4-5493a9c236ba)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6971c9b1-407f-45bf-8dec-f393d0b33ffa)(content(Whitespace\" \
         \"))))(Tile((id \
         0fcdc298-a19c-4832-a44c-e44fbdc88fc6)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         4b0d37f9-8c00-4eea-a471-d078704e5d0d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cef449d0-02e1-4cd5-86b8-e37db24f029d)(content(Whitespace\" \
         \"))))(Tile((id \
         ffa5ee92-054f-4143-a5a5-2e1538eae5a8)(label(Lam))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d638e4c9-3412-48f5-87d4-bdd11dd2e75f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         caf9af1a-2ddd-4153-93c9-0b2ce294a52f)(label(x2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0a6311fc-57e0-4361-9372-fae70d3fecbb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f54860ef-d37a-49fe-831f-0b6b95978896)(content(Whitespace\" \
         \"))))(Tile((id \
         55cc421f-59f5-4b84-9b8a-112a965c7e69)(label(e2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         34267b7a-281b-45f5-87be-e72e74f42323)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         39e9cd9f-2d5b-41be-ad8e-f9ca4b310adc)(content(Whitespace\"\\n\"))))(Tile((id \
         8baf909e-c521-4c6b-8d1e-55a252791f5e)(label(x1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3875faae-7c72-4dcc-b297-b8ba85d8c547)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b2b26ef-7e27-4dc4-b44f-46f3f5dac604)(content(Whitespace\" \
         \"))))(Tile((id \
         f618e73e-3eb5-41a4-a603-a255982092c4)(label(x2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ca5163d2-92ab-4346-9d80-02715b104c1a)(content(Whitespace\" \
         \"))))(Tile((id \
         61cea385-c7f0-47fa-b067-317d229eba3f)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84c236ad-5a0e-4d04-897a-2769e5567fd2)(content(Whitespace\" \
         \"))))(Tile((id \
         339cb459-f49b-4444-b49d-11936d3bdef1)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5a361f0-54f9-4a76-95a0-091ecee9409b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         054725c6-f128-4cfd-9f63-1c4031558a3c)(label(e1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         595a6794-541b-454f-a5e9-3c87347db5e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2ed8684-53cc-4f6e-ab7e-46e39e206011)(content(Whitespace\" \
         \"))))(Tile((id \
         4ad64ad7-8816-4411-8ad6-f48bce17423d)(label(e2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         92d05f51-bf70-4c02-8962-64498eb0aae5)(content(Whitespace\"\\n\"))))(Tile((id \
         b9aa38f5-b6f1-41d9-803d-394a24630384)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bbccdb39-8290-442e-957c-dcfad2db922c)(content(Whitespace\" \
         \"))))(Tile((id \
         c563fa31-15be-45e7-a349-dee15dcd05f9)(label(Ap))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         171baebe-1032-4a1d-adaf-25253bd2b978)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a30f2516-6b0f-4eaa-8271-afe908dd8ead)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0c87d608-a53e-4e03-9556-3aac98366cae)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b902c75c-efc3-40d8-a97e-f084cad413ca)(content(Whitespace\" \
         \"))))(Tile((id \
         eba90573-7b70-43dd-a2f0-8e7198399e1b)(label(e2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         594ba3ee-ab28-41cf-9b4b-8985aa91b66d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e0aeb3fb-e8b6-46ea-b1da-449f260da465)(content(Whitespace\" \
         \"))))(Tile((id \
         42065032-f7de-4e4b-af4c-e5114fea7800)(label(Ap))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e09a011c-867e-4310-be4e-63bae175d91c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         26c890b9-63f0-49d8-8b1c-2468d8e05be5)(label(e3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         45001d40-4830-4e91-8d08-96b4d3e1c7cb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d82581e8-a740-45d6-a0bd-a5f51bcb5353)(content(Whitespace\" \
         \"))))(Tile((id \
         6a1d81fe-83f8-4ecb-b344-551d56e80ee3)(label(e4))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3085d56c-fe2c-4ec1-8f2f-557f87df50a8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eca1cadc-4bb6-4685-9885-6883d4c35a49)(content(Whitespace\"\\n\"))))(Tile((id \
         9395e254-9c5b-4bc5-ac18-8665c4bac789)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4ab44dd-b4e6-441f-88b1-3217900c772c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c38e1f8-1c44-441c-a17f-06d1e7132d77)(label(e1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         95fc8967-ccd3-4ece-919f-5cadca6f5a4c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3311177a-0822-43bb-a38c-7aa16acef39f)(content(Whitespace\" \
         \"))))(Tile((id \
         27b4bc10-2ad4-49a3-887d-509ce928beac)(label(e3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b2c4987a-fea6-4c57-b536-f4a77e33d863)(content(Whitespace\" \
         \"))))(Tile((id \
         82a476e6-2bd7-4cb8-a074-48f7b3343393)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a26640f-084f-4f02-9876-1e53bed73b1b)(content(Whitespace\" \
         \"))))(Tile((id \
         bf3509ee-2e1e-48e9-abf7-d5914227a2b5)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4105c346-dda0-433c-8f6b-5e3d2b242bcc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5101a127-f065-4345-abbc-108e36238879)(label(e2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c557fa5c-a19e-4520-8041-67520c764138)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d08280a0-7a9b-4033-b32f-ee58d447052b)(content(Whitespace\" \
         \"))))(Tile((id \
         26b2721f-c05d-4c33-84f9-fd3db38baf04)(label(e4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         da6bf717-74fa-47c5-b30a-aedf903e7cc6)(content(Whitespace\"\\n\"))))(Tile((id \
         72a1fe06-007e-4b97-93bd-c6de7948e299)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b0cec7b2-7108-4b83-9858-b74ddd5ec28e)(content(Whitespace\" \
         \"))))(Tile((id \
         b34da728-1e96-4679-a91f-bddc0e0adf93)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4a097f9f-ef12-44aa-a761-f73b9118c2b8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         462995db-2a48-47e8-951a-00a75aa18fb9)(content(Whitespace\" \
         \"))))(Tile((id \
         4e2c7dc0-ee3c-4015-82cf-ffaf8bea5068)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0cfc46fc-e227-4f33-99c8-287499373a67)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         016de6e8-4751-49b3-a651-efa1a58bf8bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1603600a-0eba-44c9-a0a7-28f67f7c43fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ce26266-56d1-483e-9838-a4d230f3468c)(content(Whitespace\"\\n\"))))(Secondary((id \
         baa957d7-458c-48ee-b415-7aec8ea554c5)(content(Comment\"# Polymorphic \
         Functions #\"))))(Secondary((id \
         fa51efb4-8180-4034-812c-dff481dd2418)(content(Whitespace\"\\n\"))))(Tile((id \
         bf51c1e5-fce2-434b-bab4-48d0019a700a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         083b6539-801d-4c9c-acba-c10718d065fe)(content(Whitespace\" \
         \"))))(Tile((id \
         2bd01ea2-3217-491a-9699-aec9fc6d4538)(label(poly_id))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b88cea54-13fb-46ff-bf01-c2186387befe)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b75fafac-4fae-41ca-b151-15355cb0ad14)(content(Whitespace\" \
         \"))))(Tile((id 82e2f2ec-993d-41d4-b3b2-f06e94d6dde1)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         adb6b7e2-ad0d-40c2-8517-f400fb598c15)(content(Whitespace\" \
         \"))))(Tile((id \
         d1b05e67-beb5-4e54-a572-2a160bf75862)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3d83c141-c88d-40ea-b609-0664b91014ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ff5c5a08-bec6-468a-ae44-fd2dbaec7264)(content(Whitespace\" \
         \"))))(Tile((id \
         83282906-9347-44ba-939b-eecb510503b8)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         96ac5752-2f4c-4ed4-b381-a03090e96ac5)(content(Whitespace\" \
         \"))))(Tile((id \
         48e2afd5-1359-4b69-ad20-88e1f89a151e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         40eaea32-dbb3-4d3c-81cf-86571370ee44)(content(Whitespace\" \
         \"))))(Tile((id \
         3e701cab-30fb-4d6d-9448-db07b3fbc23b)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ec68ddcf-7f26-484c-b502-0d36b5016797)(content(Whitespace\" \
         \")))))((Secondary((id \
         60b6fc35-f0f1-45d6-be46-4b02c41419f4)(content(Whitespace\"\\n\"))))(Tile((id \
         2cc22d88-e24f-4e71-ab70-e4030c22deb6)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         055219cc-733f-4774-a0df-5c81aba34c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         ba50e792-b113-496e-ab59-ef8dcb82e6c2)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d9412b6f-4850-47da-bf68-db791012d01b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6a6d44e9-406c-4d56-9ca2-0056564a25d0)(content(Whitespace\" \
         \"))))(Tile((id 1917762b-a438-4058-8fcf-13a7290d37c8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6bcb9079-1763-4c40-874e-150264ee9877)(content(Whitespace\" \
         \"))))(Tile((id \
         76c203f1-ccae-40c8-bdc1-a446ac2acf91)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         454f7359-894e-48f9-ae9d-0a4872d6ded4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e227f786-db3c-47da-aec4-b860c65b045b)(content(Whitespace\" \
         \"))))(Tile((id \
         fa4603b8-fa72-4642-aec3-c38c4d6200ec)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c4632dab-719d-4f6b-8f06-8136c74255b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         845807d0-4e2f-480a-8394-9f6f5eb2a9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         78365bb5-3654-4a70-981f-a1d96a543dc5)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         124d503b-f743-4d83-811a-cd824870260b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d55cabf1-f2cd-4b41-a053-b8cb37e88a8e)(content(Whitespace\"\\n\"))))(Tile((id \
         ecd4a950-3ef4-41c2-aa8e-0b5acddf9c40)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d98d9321-14aa-49d3-b64f-8e90cc79eed8)(content(Whitespace\"\\n\"))))(Tile((id \
         55c71638-1777-4ed3-9d29-a8566c82329c)(label(apply_both))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f2051853-825f-403f-817f-ba735deec648)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8125e27-899c-4fe7-ac79-5508f5a9f72d)(content(Whitespace\"\\n\"))))(Tile((id \
         eb3ee8fd-b64b-41c4-8e26-910160cff4b1)(label(forall ->))(mold((out \
         Typ)(in_(TPat))(nibs(((shape Convex)(sort Typ))((shape(Concave \
         36))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         f0bd24e2-51eb-4aa4-b6aa-b2f1cd6a9322)(content(Whitespace\" \
         \"))))(Tile((id \
         12250dcc-2a44-4918-83a6-bd9ee7b14d17)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ee5c3695-4c8e-4fc2-b6b7-744281c0ffd4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dcc6fcb4-aeae-493b-a5c6-ecc4e43f7874)(content(Whitespace\" \
         \"))))(Tile((id ac7eadd7-98af-498b-be9c-2c528f1cc5a4)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         de5baca4-3da4-48e3-b163-428a56d74a6d)(content(Whitespace\" \
         \"))))(Tile((id \
         abff8f8a-123f-436e-90c8-a552fa9fe3b9)(label(b))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         9dc3ed1d-3b47-4655-9e45-98f0e47b610a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a9fe1ccb-9daf-4daa-be55-5390e4b787ac)(content(Whitespace\" \
         \"))))(Tile((id \
         5e3042d9-e24f-4002-87fc-3a7e73755aa6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e6abbcfa-a6fa-460f-8723-7f7cd2ef3053)(label(forall ->))(mold((out \
         Typ)(in_(TPat))(nibs(((shape Convex)(sort Typ))((shape(Concave \
         36))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         9051efe0-83c1-4f46-b158-dc6394748155)(content(Whitespace\" \
         \"))))(Tile((id \
         40a8ccda-cf9e-4929-a972-64c78ef78e01)(label(c))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8cd7e8bf-bdb8-4d5d-aead-ed278c3106a3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de9b6728-d135-4069-9137-037b02a31f93)(content(Whitespace\" \
         \"))))(Tile((id \
         29eaaae6-6393-4900-ab63-00cf6d08f430)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         100d5c50-0aca-46a5-b2c0-c887c8615221)(content(Whitespace\" \
         \"))))(Tile((id \
         c2be8033-5d10-4693-bc19-7445366a66c9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         723310e9-23cd-4622-a7af-daf5f90ec6bf)(content(Whitespace\" \
         \"))))(Tile((id \
         3ed209ed-25a2-45b8-9f41-e1bc50926def)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         67dc22b4-24f2-4fcf-9d85-9745dfdd6056)(content(Whitespace\" \
         \"))))(Tile((id \
         b366a3f1-a2d6-4cc0-9810-143897cac127)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a14cc2d-a486-4743-b9aa-6d647f4e4e19)(content(Whitespace\" \
         \"))))(Tile((id \
         7072fcee-af3a-4354-adb0-842714470e8a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9ad311d0-b871-4c7e-aa6d-57c67d290064)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         dffa66f9-c0cc-4644-9929-03b4da65c651)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         77467fb0-5e85-417c-90c8-e931f3fcd104)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         60b54dd3-7d34-4290-adef-f800eca9f3a4)(content(Whitespace\" \
         \"))))(Tile((id \
         d2f90859-f448-4016-aeaa-8c94ff173d21)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0acdef08-2eef-4d59-9d58-c6975132813b)(content(Whitespace\" \
         \"))))(Tile((id \
         27d1468f-307f-480c-80a1-fb514a35d046)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f40e7354-69b0-4154-8d4a-f400f395f4da)(content(Whitespace\" \
         \"))))(Tile((id \
         2fcb15ac-b292-4ed6-8c80-eed78a2f0fd6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0178d65f-11b0-4f42-91ac-7ce750006bec)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b4dfcd33-4a9d-4bce-824a-ab1ac4488282)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a2ce748c-0cf9-4a77-abcd-17d52dbfa3a0)(content(Whitespace\" \
         \"))))(Tile((id \
         45b2808f-dfba-4733-b6cc-935a471a3b63)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         6c43dd47-888d-42f1-8116-ebabfba58c81)(content(Whitespace\"\\n\")))))((Secondary((id \
         99cd7402-4275-4622-9d46-f2d92ac18ad3)(content(Whitespace\"\\n\"))))(Tile((id \
         3264b5a0-be11-40e1-b7dd-0a33ea5c99e4)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         19880b71-46f9-4008-bbb3-8548f8f67cb1)(content(Whitespace\" \
         \"))))(Tile((id \
         4fc5a2fa-76d5-42ba-88f1-d6ce4dccdfb8)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c23f1b38-298e-45ba-9c7a-f2ca801190fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1bfdd9c3-8fdc-4665-a981-5c3559461270)(content(Whitespace\" \
         \"))))(Tile((id d066df3d-de20-4c9a-9024-384717f56b4d)(label(typfun \
         ->))(mold((out Exp)(in_(TPat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f8d0b8a4-0808-402d-9d8f-8c1aad24d8af)(content(Whitespace\" \
         \"))))(Tile((id \
         ea882012-22e2-4354-80d1-464c54a29a28)(label(b))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         dbbf4213-8209-4325-9a5e-1af6b00c1548)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b4e7ce31-325c-4461-9897-0733518b79d8)(content(Whitespace\"\\n\"))))(Tile((id \
         ade0315b-7e7e-4f92-b3d3-1a631ad3a08a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         67c70690-eb22-478f-8f56-a3a6b394fc94)(content(Whitespace\" \
         \"))))(Tile((id \
         4610f5b0-fbd1-49c6-82f9-9fd586b44e1a)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         23de204c-93a9-42f5-86d2-80d0744c2f4c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb7c398f-c9cd-475d-8edd-60dccd50cdf8)(content(Whitespace\" \
         \"))))(Tile((id 0cc47f40-4da5-4b11-b58a-faa5a1f0fc7f)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         44a0caf9-bbfe-445b-9ca6-92326b104ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         b2cacd37-8643-4aab-ab29-371408558976)(label(c))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         dd1625db-146c-4ee8-b953-b7550e31d027)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33e534d7-e16d-46ed-beca-84f03084439c)(content(Whitespace\" \
         \"))))(Tile((id \
         6cadefec-53d8-4994-8250-a001f92c5500)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         fb026b85-20cd-4a6d-8b9a-ff51ae8f5186)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b22fe1a1-5758-4d09-8d70-c41f11cfa3a6)(content(Whitespace\" \
         \"))))(Tile((id \
         daa4547c-d466-4e19-a5c4-514a0f60d7cd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6279476d-e76f-4136-a1f6-6f9c3b122335)(content(Whitespace\" \
         \"))))(Tile((id \
         0e6cda62-d907-415a-b97d-017df552a74a)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c751cbf0-52ee-432d-947d-32b52eccfbae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         791ba73b-7bdb-47e9-b2dc-e336e95c01b8)(content(Whitespace\"\\n\"))))(Tile((id \
         7d2740e6-3d6a-4633-a50d-7f34805bbc35)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f7020314-ef42-4588-8688-5ded13eaf208)(content(Whitespace\" \
         \"))))(Tile((id \
         04c6505b-b587-48c0-8d05-0882504c6d88)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         87ed07fd-b577-40af-ac6d-f8b959c79fef)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         befd16b9-d812-47bc-9b7b-d724ce91b6e3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         42985a9c-fbcf-480c-8ab8-c07566958457)(content(Whitespace\" \
         \"))))(Tile((id \
         a5b03077-3d74-4beb-9882-7e3fb415affc)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         71e10e86-c839-4407-aba1-e30cdd0e35e7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c603023e-f73d-411d-9604-5a9147e821da)(content(Whitespace\" \
         \"))))(Tile((id \
         ec5729ba-30bb-4e5a-b0fa-b21b665db33a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         177621e6-82a7-4416-923d-06b398864f83)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0848d70d-7b93-4ee7-86d5-d750c0038f74)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fbd12d34-0da6-4b7f-92b6-f114060202f8)(content(Whitespace\" \
         \"))))(Tile((id \
         32ab793e-e430-49f5-a20e-6a421d237c09)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2f8d49de-2c63-48a8-a87b-128a613c12d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c0b51c5-3188-4f09-ab90-f346f0408e65)(content(Whitespace\" \
         \"))))(Tile((id \
         2e92d8e2-44ac-4822-bfe8-a571708b1852)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         15ac7db4-d498-48e3-b0aa-e959ad0fd35d)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f45b213-29e8-4536-98c2-0e8ac4a2ac95)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14013947-979d-4174-9aa9-df527e502738)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         4105efc6-92a9-41d9-b257-ade9b1f1efa2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2c5deca-79ab-4eb5-afb9-23be5f73fbae)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9186e36d-a29a-48e4-9ca1-a545e6dd7307)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddea6084-9462-4b7a-8d62-55fcfb34a3aa)(content(Whitespace\" \
         \"))))(Tile((id \
         6f478a64-5866-4dfc-99c6-8566e0135c52)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a48f6ab0-b612-43aa-b3b9-f216484c5281)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b78bbb43-417f-4287-84f0-2404bbb3e933)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         6ac2f730-3051-41d7-83aa-f68e7e5efac0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3bdba9d3-4e98-4506-965e-77b32c1b048e)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e69aae9d-28e5-485c-b6ca-262c78b7d9a6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e6093722-c9c1-41bb-a5f9-e09b6c516d39)(content(Whitespace\"\\n\"))))(Tile((id \
         16758a85-95bd-4220-a84a-0fae99908972)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2416118-8e3a-4e8d-a3c6-7eae7aa90e92)(content(Whitespace\" \
         \"))))(Tile((id \
         b8fc0fa3-aafe-4269-a976-2a4285322aec)(label(list_length))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d8cc039f-d46d-48ba-9d77-4a1226bce0ea)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         466061f3-c1f8-4c14-ab36-75d11c867e60)(content(Whitespace\" \
         \"))))(Tile((id fd86ea26-4995-4208-a3c1-05fd5ea9f839)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         db071bc6-e217-4722-bc39-10a6d3b3f893)(content(Whitespace\" \
         \"))))(Tile((id \
         d74794f0-d1db-4da0-9a19-36bc2c463761)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         87af2786-d9b9-44e4-afd0-711629a4c2c5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         54ab125f-00c0-4a6e-bc5d-3f5fe7342e72)(content(Whitespace\" \
         \"))))(Tile((id be09ebab-7014-4a5e-930e-fc6e0ab61d27)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2d848da2-d436-40c3-8b29-66ec07d5a561)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c44f8f59-88e3-4621-964f-59cf91900ba4)(content(Whitespace\" \
         \"))))(Tile((id \
         8d7c076d-b51c-4188-8e18-1845e45ab90e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a61473c0-4e2b-4946-a82f-15fc5eb8088b)(content(Whitespace\" \
         \"))))(Tile((id \
         f07ad428-1408-426c-8558-bbe4edc1080b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c50ea628-b9ac-40da-b542-2051ccced09f)(content(Whitespace\" \
         \")))))((Secondary((id \
         10a3403c-56ac-4375-82b9-89541a0f2b09)(content(Whitespace\"\\n\"))))(Tile((id \
         331d2720-1ced-4a19-b897-f6ec062f679b)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c48f31ba-d151-4574-87ab-965f16c8813a)(content(Whitespace\" \
         \"))))(Tile((id \
         a132170c-95cc-4305-87f9-614216f7347c)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         04296a5e-fcba-45a7-96bf-4ceef5e015d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         037e5278-bd28-4c1c-9d52-c95a42b14a5c)(content(Whitespace\" \
         \"))))(Tile((id 43041ab7-aa68-4432-8c4f-1574cb1cc783)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dd8470ec-afaf-434d-b2a2-3291eb4d857f)(content(Whitespace\" \
         \"))))(Tile((id \
         93c72c01-7582-4988-bfa4-f3fdf7df7120)(label(l))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9b735929-c46e-41d5-93cc-c95028cb08e6)(content(Whitespace\" \
         \"))))(Tile((id \
         4e68e655-fd93-4891-b090-6c9526ed9cd2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         358e47a7-c6e5-4135-8fdb-f400d9883f3f)(content(Whitespace\" \
         \"))))(Tile((id eb42334a-8123-4c84-a069-4f932fb233eb)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f1427463-b373-4715-8c5d-7312e24f89d3)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d4d72950-abfd-4840-a839-07e83c2d0977)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         763c30d6-f2be-40a5-99fa-01a7f950911a)(content(Whitespace\"\\n\"))))(Tile((id \
         78a78e54-726c-4bf3-b894-b1e3617dd99d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6661931a-cfac-4478-a03a-1eba22bca3ea)(content(Whitespace\" \
         \"))))(Tile((id \
         40df8da6-0278-448d-b45c-db6cfac0f62e)(label(l))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5f4d5efb-bb2e-4a87-a8da-328580d9031f)(content(Whitespace\"\\n\"))))(Tile((id \
         c6fcd8a2-63d2-42e7-88c6-87d62bcea037)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         99b57758-2da5-4727-a830-d6006490103d)(content(Whitespace\" \
         \"))))(Tile((id \
         538e1b2b-b511-4a92-a5f9-c7dc56004e59)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5eb008d5-bc18-423b-92b6-72493ef4847c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bfb43bb6-6291-47fc-b90d-90132682a00f)(content(Whitespace\" \
         \"))))(Tile((id \
         e96f5900-b538-4fe3-b4d9-24d5e510fcb0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9cfe326-511a-4013-93e6-c0c897c145a3)(content(Whitespace\"\\n\"))))(Tile((id \
         6512a84d-f0d0-4c38-8a68-35d394877d8e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0302bfc3-2d6a-4b97-a270-77ae6e9313da)(content(Whitespace\" \
         \"))))(Tile((id \
         4a98bbdd-845f-40bb-92a6-5b3c0fa113d0)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         13a4fc76-e4ae-4364-a052-6c8e0dae344f)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         2e8770a7-3e47-4d52-bb47-cb132c627244)(label(tl))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a9728415-2612-4dfa-a362-a099ddc732de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b891bba-9b11-48e4-ba04-885be723a042)(content(Whitespace\" \
         \"))))(Tile((id \
         d4092230-b798-4089-8102-86e21f4cce4d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         595681be-79f1-4867-a4e7-59490459b69b)(content(Whitespace\" \
         \"))))(Tile((id \
         6850598a-d0f1-40ac-8df9-23137ca7845e)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         110311d6-91e5-40d9-a745-57a93d51fa03)(content(Whitespace\" \
         \"))))(Tile((id \
         7a209588-75d9-4c7b-a8c4-44b41d2c8169)(label(list_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02d27d67-acb5-461f-8e5a-b6b3e5ad93b1)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3869d89a-524e-4188-8f10-b78a728f845e)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         6cb685a6-daec-4645-bb3c-ea16a9b00e59)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6bcf89b6-2a8b-46d9-acc2-173d47b51225)(label(tl))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bd645177-9d46-4dcc-bfe8-a3baec36bdc9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f531be00-00bd-4177-b086-3cf9a777c192)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1b8907c1-be06-4f07-8dc2-07e6f9073c91)(content(Whitespace\"\\n\"))))(Secondary((id \
         76f640f5-17ae-4569-9ba3-da25a8e607bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eafba02-c29f-4565-943a-45cddef16307)(content(Comment\"# Tests, \
         separated by semicolons #\"))))(Secondary((id \
         b0ba1a75-4d12-4c2d-bb41-de5f117e024c)(content(Whitespace\"\\n\"))))(Tile((id \
         36c0b4e1-11c3-4eaa-91f3-70ea1d6bde62)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e911839b-6277-49e1-ab0a-1f47a63fe17a)(content(Whitespace\" \
         \"))))(Tile((id \
         5b3a8287-c336-4c76-9387-0ef8e9afd7ad)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         db5ce8ef-aa0a-44cc-bbc6-f2f2c8053db0)(content(Whitespace\" \
         \"))))(Tile((id \
         8ce8b2ac-a488-4110-8532-49eb32bec0d2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b5d321e-a696-43ce-94e6-bf8580d48a9b)(content(Whitespace\" \
         \"))))(Tile((id \
         ffb36764-a2a2-4e69-8cee-2aa76686618b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e935ebc0-1d78-41ab-bf23-cc36a70ca2cb)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d6285f-6d51-4065-8a0f-6cc66a665022)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce0bc234-f3d5-4e67-a426-7e2e9617fde6)(content(Whitespace\" \
         \"))))(Tile((id \
         79b68ef1-0584-4c91-a0f3-aa8e67f12553)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0553e8dd-d196-4e81-be1b-c87981cf5158)(content(Whitespace\" \
         \")))))))))(Tile((id \
         9d2c0010-bc71-4541-98ef-4ee25b91d1e2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7637c6f-5d61-4b27-acb0-53918ffc24ab)(content(Whitespace\"\\n\"))))(Tile((id \
         8f2c0ade-f386-4fc0-b623-e53fee60ac59)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ce2fbcdf-884b-4952-bcdb-c1e54ef3dd80)(content(Whitespace\" \
         \"))))(Tile((id \
         f278d67a-74f6-4357-b63e-3a420d73389e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1d455a59-903a-4e70-ad7e-e80df2aed797)(content(Whitespace\" \
         \"))))(Tile((id \
         03e44f65-897b-4db1-bd0c-a2fe3d4c9350)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbab0720-cea6-429c-8f87-5bc03c60959b)(content(Whitespace\" \
         \"))))(Tile((id \
         ab498043-1f9b-4163-b853-d9641f87522d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be91c40b-f1ac-493d-8bff-b5c50b4d34bd)(content(Whitespace\" \
         \"))))(Tile((id \
         0a16c7c3-f894-4c2c-8978-2a70aee3c357)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cd6551b-aafa-4f86-8d6a-c3393b381784)(content(Whitespace\" \
         \"))))(Tile((id \
         dbc8ab4f-d288-41f3-b2a7-4ebb1e0a30a9)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         315f9c6b-6af9-4882-bddf-a6d6c33a7cfd)(content(Whitespace\" \
         \")))))))))(Tile((id \
         92d51459-62db-4c3e-868f-499ba30ace29)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d7e5568-17d6-4960-a740-746c9d2440e3)(content(Whitespace\"\\n\"))))(Tile((id \
         37fefd18-e48d-47f7-9f8c-b7901ecaf1fb)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         440f231c-5281-4c50-86ec-b705e1e73484)(content(Whitespace\" \
         \"))))(Tile((id \
         d4b6dff5-c94b-4ec0-8cf9-f08525171dbd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3cd0d394-4698-4a6f-b8c7-b006f97f6828)(content(Whitespace\" \
         \"))))(Tile((id \
         ef646ac8-0998-4ccb-bce8-19ae9304f919)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9326cdb0-f589-4523-b659-a06bfdb22de5)(content(Whitespace\" \
         \"))))(Tile((id \
         d143bb4f-dbdc-44dc-afda-cf32af146b96)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f4db20f6-4231-479a-83cf-76cf545559e8)(content(Whitespace\" \
         \"))))(Tile((id \
         6fe0453d-271d-4da3-8d69-4fc515014cb3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ffa275a-541e-44f5-8e8c-e7b85e1d67c3)(content(Whitespace\" \
         \"))))(Tile((id \
         bc59fae5-c10d-4f4d-b21c-fd609f840a95)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3d8d9c05-1e21-4d54-a00d-a769751d2354)(content(Whitespace\" \
         \")))))))))(Tile((id \
         620a4dca-5ea1-4d69-806d-2da42a5ecb52)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8da786d-a7ec-4139-bbe4-b68fd84c8331)(content(Whitespace\"\\n\"))))(Secondary((id \
         172922ee-7a14-4094-9705-eeab560fd6bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2807b68c-bb6d-471e-a236-8fb35540b800)(content(Comment\"# The value of \
         the program is shown at the bottom #\"))))(Secondary((id \
         640f3e08-b0fc-4001-a583-867f622e4129)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce7f5268-7440-4456-89fe-6998d8e8d352)(content(Whitespace\" \
         \"))))(Secondary((id \
         62cf0fc9-be40-4c88-812f-b7834b63029a)(content(Whitespace\"\\n\"))))(Tile((id \
         26cb3248-db75-4fd4-83c2-c29e8d616e5b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         37e5c07e-b39f-47ed-88e6-8113053f9d8d)(content(Whitespace\" \
         \"))))(Tile((id \
         c444b538-c152-4609-8c6d-23bf6aa2050b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5745cbc8-350c-4aa2-b3ba-656cc84e0046)(content(Whitespace\" \
         \"))))(Tile((id \
         79cdae6f-64ae-437d-b6eb-ae6ec050e7d0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))";
      backup_text =
        "# Hazel Language Quick Reference #\n\n\
         # Empty holes stand for missing expressions, patterns, or types #\n\
         let empty_hole =  in\n\n\
         # Non-empty holes are the red boxes around type errors #\n\
         # (you can still run programs with non-empty holes) #\n\
         let non_empty_hole : Int = true in\n\n\
         # Booleans #\n\
         let bool: Bool = true in\n\
         let operators = !true && false || true in\n\n\
         let conditional = if !true then 1 else 2 in\n\n\
         # Integers #\n\
         let num = 1 : Int in \n\
         let arithmetic = -num*1 + 2/3 - 4**5 in\n\
         let comparison =\n\
         (0 == 0, 0 < 1, 1 <= 1, 2 > 1, 1 >= 1 )\n\
         in\n\n\
         # Integers are unlimited by default #\n\
         let big_num: Int = 10000000000000000000000000 in\n\
         # Use SInt for fixed-with system integers #\n\
         let bad_num: SInt = 1000000000000000000000000 in\n\
         # Use Nat for non-negative integers #\n\
         let nat : Nat = 5 in\n\n\
         # Floating Point Numbers #\n\
         let float: Float = 0.1 in\n\
         let arithmetic = 0. *. 1. +. 2. /. 3. -. 4. **. 5. in\n\
         let comparison =\n\
         (0. ==. 0., 0. <. 1., 1. <=. 1., 2. >. 1., 1. >=. 1.)\n\
         in\n\n\
         # \"use\" lets you set the default number format #\n\
         # for literals and operators. #\n\
         let natural = \n\
         use Nat in \n\
         1 + 2 * 5 \n\
         in\n\n\
         # Strings #\n\
         let string = \"Hello, world!\" in \n\
         let concatenation  = string ++ \" Goodbye.\" in\n\
         let comparison = string == \"Hello, world!\" in\n\n\
         # Tuples (Destructured with let expressions) #\n\
         let tuple : (Int, Bool, (Bool, Int)) =\n\
         (1, true, (false, 3)) in\n\
         let (a, b, (c, d)) = tuple in\n\n\
         # Functions (Take a single argument which can be a tuple) #\n\
         let y : (Int, Int, Int) -> Int =\n\
         fun (m, x, b) -> m * x + b in\n\n\
         # Recursive Functions (Arrow type annotation required) #\n\
         let double_recursively : Int -> Int =\n\
         fun n ->\n\
         if n == 0\n\
         then 0\n\
         else double_recursively(n - 1) + 2\n\
         in\n\n\
         # Mutual Recursion (bind tuples of functions) #\n\
         let (even : Int -> Bool, odd : Int -> Bool) = (\n\
         fun n -> if n == 0 then true else odd(n - 1),\n\
         fun n -> if n == 0 then false else even(n - 1)\n\
         )\n\
         in\n\n\
         # Lists #\n\
         let empty_list : [Int] = [] in\n\
         let non_empty_list : [Int] = 1::2::3::[] in\n\
         let list_literals : [Int] = [1, 2, 3] in\n\
         let length : [Int] -> Int =\n\
         fun xs ->\n\
         case xs\n\
         | [] => 0\n\
         | hd::tl => 1 + length(tl)\n\
         end\n\
         in\n\
         let has_at_least_two_elements : [Int] -> Bool =\n\
         fun xs ->\n\
         case xs\n\
         | [] => false\n\
         | hd::[] => false\n\
         | a::b::_ => true\n\
         end \n\
         in\n\n\
         # Algebraic Data Types #\n\
         type Exp =\n\
         + Var(String)\n\
         + Lam(String, Exp)\n\
         + Ap(Exp, Exp) in\n\
         let exp_equal: (Exp, Exp) -> Bool =\n\
         fun es ->\n\
         case es\n\
         | Var(x), Var(y) => x$== y\n\
         | Lam(x1, e1), Lam(x2, e2) =>\n\
         x1$== x2 && exp_equal(e1, e2)\n\
         | Ap(e1, e2), Ap(e3, e4) =>\n\
         exp_equal(e1, e3) && exp_equal(e2, e4)\n\
         | _ => false\n\
         end\n\
         in\n\n\
         # Polymorphic Functions #\n\
         let poly_id: forall a -> a -> a =\n\
         typfun a -> fun x: a -> x\n\
         in\n\
         let\n\
         apply_both:\n\
         forall a -> forall b -> (forall c -> c -> c) -> ((a, b) -> (a, b))\n\
         =\n\
         typfun a -> typfun b ->\n\
         fun f: forall c -> (c -> c) ->\n\
         fun (x, y): (a, b) -> (f@<a>(x), f@<b>(y))\n\
         in\n\
         let list_length: forall a -> [a] -> Int =\n\
         typfun a -> fun l : [a] ->\n\
         case l\n\
         | [] => 0\n\
         | hd::tl => 1 + list_length@<a>(tl)\n\
         end\n\
         in\n\n\
         # Tests, separated by semicolons #\n\
         test 2 + 2 == 4 end;\n\
         test 3 + 3 == 6 end;\n\
         test 2 + 2 == 5 end;\n\n\
         # The value of the program is shown at the bottom #  \n\
         2 + 2";
    } )
