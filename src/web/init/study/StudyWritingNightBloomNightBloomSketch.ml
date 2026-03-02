let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / night-bloom / night-bloom-sketch",
    {
      segment =
        "((Secondary((id \
         990559b9-9576-4e36-86dc-cbd44d21c11e)(content(Comment\"# NIGHT BLOOM \
         FILTER TASK                        #\"))))(Secondary((id \
         3571f2f6-df9b-433b-ab6f-ee99796bc383)(content(Whitespace\"\\n\"))))(Secondary((id \
         b21fafae-df30-4f6a-b260-52e6556c4c6f)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         e7033c76-9880-4d88-9074-0da0c1750ccc)(content(Whitespace\"\\n\"))))(Secondary((id \
         9399f81b-5d23-4761-8d20-2e86343e89c2)(content(Comment\"# A plant \
         catalog has entries like:              #\"))))(Secondary((id \
         a7add91b-149e-401a-9233-23b4f01fbd58)(content(Whitespace\"\\n\"))))(Secondary((id \
         7bf52d6e-1c45-482b-8638-8d02fdeca7ee)(content(Comment\"#   \
         \\\"Moonbloom [night] 200ml\\\"                    \
         #\"))))(Secondary((id \
         c7088f23-8493-4653-a3fc-0d1b0597d7e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         69559b7b-f97c-4970-8d95-783497a1cf17)(content(Comment\"#   \
         \\\"Duskrose [day] 150ml\\\"                        \
         #\"))))(Secondary((id \
         05e900fc-e3f1-47b3-ae60-5974d287b0c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         b653a2f9-ecab-441b-87fa-ffc7ab243a3e)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         29fb5b1a-93ea-4896-8fe4-fb0070ccf2de)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f582f17-a2cb-4247-ba84-3ed94153c9ba)(content(Comment\"# Filter to \
         night-blooming plants and extract    #\"))))(Secondary((id \
         3b28cba5-8642-4c66-8fe1-f9ee14f74a7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bab2aa3-7a0c-4704-b322-d2a33c594348)(content(Comment\"# just their \
         names: [\\\"Moonbloom\\\", \\\"Starfern\\\"]    #\"))))(Secondary((id \
         619ba727-171a-4353-bc03-52e403232cf0)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b02ec1a-7389-480f-a3bb-6f617ec16bc3)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         17f8e2df-09a9-48d6-8a58-78a2094a8493)(content(Whitespace\"\\n\"))))(Secondary((id \
         03638fd7-0dd4-44c1-b437-20266fb6a865)(content(Comment\"# \
         Steps:                                         #\"))))(Secondary((id \
         0f9cb9ea-89bd-4e92-bd87-fa12dfde8c19)(content(Whitespace\"\\n\"))))(Secondary((id \
         6754269c-d977-419d-8030-ae24f6747db5)(content(Comment\"#   1. \
         is_night: check if entry contains \\\"night\\\" #\"))))(Secondary((id \
         d73d1c5d-2622-4dac-bf78-0ffe717c898b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4c1a054-5ccf-46b6-8a33-153d88558a04)(content(Comment\"#   2. \
         extract_name: get the first word          #\"))))(Secondary((id \
         c616eb10-fb25-496a-83c1-d3e469de0430)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d64c156-6156-4601-a217-9891b70dc096)(content(Comment\"#   3. Combine \
         with filter and map               #\"))))(Secondary((id \
         8bae6abf-30c5-4657-8cac-039ce720b486)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9a7a681-2f02-4241-8e07-ddaeaee7a255)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         a90d7ee9-11a6-4dfb-ab5f-07ff56b1dfaa)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5178035-5a0f-4293-8c1a-e66921e0fe0c)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         08296ef9-2169-4904-bf03-d0246f01cf82)(content(Whitespace\"\\n\"))))(Secondary((id \
         2232c680-a079-4a87-b585-dcb176b43eb4)(content(Comment\"#   \
         string_match(pattern, str) -> Bool           #\"))))(Secondary((id \
         c8ce4592-aee0-4828-adb4-640a8cb7024e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e6df695-d630-4fbb-afce-98400346d84b)(content(Comment\"#   \
         string_split(separator, str) -> [String]     #\"))))(Secondary((id \
         5ff2b7df-7d77-4eda-beb7-9dc6d2e52378)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6615a89-63c4-4b18-a3f6-fb850466af2a)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         cb27b31e-979b-4e04-a79a-a6347900efcd)(content(Whitespace\"\\n\"))))(Secondary((id \
         68425cce-e7ea-4278-9b03-fac4707cb368)(content(Comment\"#   \
         filter(list, predicate) -> list              #\"))))(Secondary((id \
         c7b1cf57-62b8-4505-bd79-7a2f093b4f2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0de04da-d462-4989-860c-55b288c762bb)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         86a7539c-477c-4dc4-b88d-b6f98c6676c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         05fcb2e4-ca86-4cc6-8e09-1d407bbd09b3)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         e0fe5601-4401-45fc-8c6d-50109a4930c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         df5f255e-a02b-4baa-88fa-7c0f0b936a3b)(content(Comment\"# Note: \
         string_match uses regex patterns.        #\"))))(Secondary((id \
         572d06b8-6249-4141-b3cf-a190c06de7b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f781e280-377b-478f-aad5-34885d0d2f6c)(content(Comment\"# The pattern \
         \\\"[abc]\\\" matches any of a, b, c.   #\"))))(Secondary((id \
         07a89d9e-1b97-4459-a6f4-534e2e9d542d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f41dd80-08be-43fc-9553-55afcfb426cb)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         70263b4c-89a3-4d97-aa2d-e641fa828dc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2101c721-3e2b-4e26-8758-5c78c11eb2e2)(content(Comment\"# Tip: Use \
         probes to see what your pattern       #\"))))(Secondary((id \
         cc4f8bbe-98e6-42c3-ae34-6806a0c271a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         20a772bd-0e93-4cdc-af0d-9c439a90c47a)(content(Comment\"# actually \
         matches -- regex can be surprising!   #\"))))(Secondary((id \
         b96438ac-d511-41d8-b894-797afe7849bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         02a90554-f767-4079-b58e-231f05690e76)(content(Whitespace\"\\n\"))))(Tile((id \
         78d04a92-3c3d-40fa-ba5d-a9743d435e18)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6cf8ba6-17ad-420a-849a-b0efcf89fbfd)(content(Whitespace\" \
         \"))))(Tile((id \
         d35e7731-371f-4de8-ae48-c39192ae1208)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         127d0bef-eb9d-4ce1-bd7c-f32075a6ac25)(content(Whitespace\" \
         \")))))((Secondary((id \
         736ebd14-93c5-43c5-a26a-79cd81a71b94)(content(Whitespace\" \
         \"))))(Tile((id 4e51e020-a0ff-4f8c-b7b5-09ee7c8ddc3e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0b73a080-7547-4402-bce1-3ef1644c8a69)(content(Whitespace\"\\n\"))))(Tile((id \
         dc89f905-31b0-4a6d-bc40-4d46ecf03c72)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         0f3f84f3-275d-413b-ab93-b9f7f2f77cb3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8eecd786-714b-46fe-89eb-03671c894646)(content(Whitespace\"\\n\"))))(Tile((id \
         0f9ba5fd-bbf8-475e-8124-a8ee20a00adc)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1e9c68cf-62aa-4ad0-ad08-fd1a6497835a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a91d55b0-d67e-4fc0-9bf3-6b8c2f7de5ee)(content(Whitespace\"\\n\"))))(Tile((id \
         b8fece4b-5481-489b-8b44-889e5b9afe88)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         21ae2770-2bba-40cf-b89b-0c176fdb7a36)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2df8b98-31d7-407b-8c42-cbd95f83e715)(content(Whitespace\"\\n\"))))(Tile((id \
         e0857726-060e-4fb6-9b53-af57022f3ca7)(label(\"\\\"Ghostvine [day] \
         100ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57e3d243-b4d2-4915-b8df-c4868dab3140)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2f78fc26-7ac4-4fc0-a511-af004c3427e4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6a955d55-cc8f-47af-ba4f-ccb424c239b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         233f6685-3f1c-4b78-86ce-65e332ce70dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         95898b7a-b926-430c-a112-a48ac3a4a491)(content(Comment\"# Check if \
         entry is a night-blooming plant #\"))))(Secondary((id \
         01076467-4d74-46ca-b1a3-980bc2949e95)(content(Whitespace\"\\n\"))))(Tile((id \
         eb73665c-acc4-4cdf-9556-6a2b3f86fb03)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7dc25966-d1ab-4794-a3d9-10e23c4013ba)(content(Whitespace\" \
         \"))))(Tile((id \
         f4dc1839-314c-4d1e-b795-173dbefac0f5)(label(is_night))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         989b8f5d-1b09-4d0b-80c6-8ab5c88c5757)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6b80c2d9-b482-45fb-9405-3ac258b1c43e)(content(Whitespace\" \
         \"))))(Tile((id \
         cd9b2c94-dff9-4186-b782-dba8e7b1b811)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9c8408df-9735-4d10-9ab1-fa9fa6a76131)(content(Whitespace\" \
         \"))))(Tile((id \
         868bdce3-1897-488e-b14b-f848817617e1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         26d758d8-00ea-4ac7-b245-50d40cdc78df)(content(Whitespace\" \
         \"))))(Tile((id \
         6ff31bc8-a7da-4436-9508-d59781d4ca30)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         03599e38-65e8-407c-b919-7bbabe10a433)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca60cccf-e2f7-443e-a45a-1043287097c1)(content(Whitespace\" \
         \"))))(Tile((id 60c4f870-d051-4fe2-9386-448e6d9d0dd8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         41c9ed2e-52fd-4b53-a8ff-156e3e074c03)(content(Whitespace\" \
         \"))))(Tile((id \
         9663591e-8f59-4e34-986b-0f2d90daae2a)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2aa101d1-e366-4017-99dc-e97060960ba2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         07b44292-a3a3-4a66-8609-bd80e5c40e07)(content(Whitespace\"\\n\"))))(Tile((id \
         e0d4d59d-9c89-4c05-b93e-9062af47600a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d46a66a0-09c5-4620-bc0b-5ecf3a651d89)(content(Whitespace\"\\n\"))))(Secondary((id \
         977c0ea2-c2f2-4b56-9033-84c40c19e9a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         66620340-3f7e-4a44-9b58-4c2506b8b85e)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe376316-c6c1-4b16-bc9d-517a2c9c4d89)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1de28814-29ce-4d7a-ae2c-9a7919b59c7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         09e646c3-cd1d-4d15-9cc8-960dc4cfe943)(content(Whitespace\"\\n\"))))(Secondary((id \
         ceb9fb60-ce55-4fb2-b74b-433dbc67579d)(content(Comment\"# Extract just \
         the plant name from an entry #\"))))(Secondary((id \
         ee27eb2b-85e6-4012-8c2c-83bf829fafab)(content(Whitespace\"\\n\"))))(Tile((id \
         cfd72363-c6a2-4868-994a-86332b0ca4f5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e43c7db1-947a-4348-bf89-ebc0f22870c9)(content(Whitespace\" \
         \"))))(Tile((id \
         0de1750c-cdbe-4da9-ac01-4b16f4844fc3)(label(extract_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2641b9ea-23fe-401e-9f89-2c4695299289)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d2072119-f111-4b5e-a1b9-66f996beb133)(content(Whitespace\" \
         \"))))(Tile((id \
         53d39425-71d9-49a3-a7cc-417d8a1ad287)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         07a312bb-47e8-47bc-aa21-8852abecadfd)(content(Whitespace\" \
         \"))))(Tile((id \
         1f4ea455-4666-427a-bfff-fe5a863a6747)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a81b1712-b0f6-4a37-a419-c744aedec54a)(content(Whitespace\" \
         \"))))(Tile((id \
         18557084-93e8-46d3-8976-0f36696d4009)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0fd2711c-1108-4938-b024-986f0da8c0a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         4bfd9d9d-7830-4a03-a58b-f2a0450218a9)(content(Whitespace\" \
         \"))))(Tile((id fb8068b7-d42a-4520-afd7-682901a003fe)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         79d82e26-eb7b-46df-b435-c6041c859ef2)(content(Whitespace\" \
         \"))))(Tile((id \
         f9a163e9-d119-4b67-953d-e7391cb9fb87)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eccddd6c-f1ce-4597-9f4d-a17da1a41a07)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d5a9a9fc-6c4c-489d-93fd-12dc57effb9e)(content(Whitespace\"\\n\"))))(Tile((id \
         6473ce0e-dc8f-4b6b-bbdb-16b824c9efd5)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3672d79c-f168-4768-8ce1-69f4d62cfe69)(content(Whitespace\"\\n\"))))(Secondary((id \
         25a1c77c-00d9-4bb6-850a-a75af9a6d02d)(content(Whitespace\"\\n\"))))(Secondary((id \
         abe25278-9838-4660-b837-37e4d930c589)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2fc6b31-635d-4867-9df6-aa3ff4ca0167)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7cd96572-fc0c-44f8-8510-a35cdaa2019c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cd1b9c8-e80f-4cf4-9c27-8137f3f1dad9)(content(Whitespace\"\\n\"))))(Secondary((id \
         92a2519e-d380-43c9-aeaf-ad8ede2a225e)(content(Comment\"# Combine: \
         filter night entries, then extract names #\"))))(Secondary((id \
         921f6e62-c4e4-4d6a-b12c-4625714eac3a)(content(Whitespace\"\\n\"))))(Tile((id \
         87e151fd-8d7b-4ce0-baf6-f5eb196cee78)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7caa2e6c-176e-41ea-975e-8b47587613e0)(content(Whitespace\" \
         \"))))(Tile((id \
         836da38c-69b1-4dfe-9806-1c6f13449c2c)(label(night_names))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         01726d3c-8835-42e9-a162-4539c7351749)(content(Whitespace\" \
         \")))))((Secondary((id \
         67f78733-7f9a-402b-a0e1-438ba256bd30)(content(Whitespace\"\\n\"))))(Tile((id \
         d00d1fc4-138f-45d3-b3ae-5d1571e253ce)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5bb303d-22cb-4e8d-a29d-facef84d0508)(content(Whitespace\"\\n\"))))(Secondary((id \
         59d38010-d9e0-4140-a2dd-9a32c6a50f70)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a98a5a5-1641-46b9-a4aa-9157273233fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         70d45521-60af-4eb6-bbdc-edb7b23224a5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ba03b821-94e4-46bd-86f1-f879c0243174)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c8d7bfc-9b96-48bf-8448-a3ec929891db)(content(Whitespace\"\\n\"))))(Tile((id \
         29859035-e359-4786-bd12-cfb53c62f82e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fa39deb9-7948-4e0b-9bdf-f073194bf445)(content(Whitespace\" \
         \"))))(Tile((id \
         035632d8-18dd-482e-b2b1-53ca2c50b056)(label(night_names))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2af6f43-d4b5-4cca-8e07-07e9bbad1aae)(content(Whitespace\" \
         \"))))(Tile((id \
         fa7929ae-431c-4d88-be4c-4213e342f4d8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e71a3ac2-8adf-4b4f-b971-a333945a09c2)(content(Whitespace\" \
         \"))))(Tile((id 78b6a0c7-57bb-4f60-a8b9-2dd8c7c7ca67)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         05fecb70-cecd-485f-8819-186d6b2ecd44)(label(\"\\\"Moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b624a1a1-c68c-456e-86e4-7ac95ec8741e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42dd5a9d-e48b-4303-9422-359de037cef4)(content(Whitespace\" \
         \"))))(Tile((id \
         e8131e99-6e2b-4d3e-8c1a-cd9738779321)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b0990ad-9673-47d8-937d-4757cb3ada2f)(content(Whitespace\" \
         \")))))))))(Tile((id \
         1372891f-5716-400c-9b23-238a137adfcd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dedd0e7d-2bfb-4382-b2e3-ff7b31fed82d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fef1f0d-81dd-4004-8a81-2a9128c5a134)(content(Whitespace\"\\n\"))))(Tile((id \
         0778d03f-635b-4c29-8b55-eb26e48a26ca)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         622a0ac3-8878-4df0-9db7-6d60e0078cb6)(content(Whitespace\" \
         \"))))(Tile((id \
         9cdefda2-697e-4604-b5c1-ea6023f023af)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1463505-e705-4d34-82ff-713b655784df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf9ad6df-ac1d-41ca-b871-6452fb025065)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d67298d0-7109-4d0d-843d-8dd7192d0260)(content(Whitespace\" \
         \"))))(Tile((id \
         d718f237-e7e9-43ec-aedf-8f23b42f1bba)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88dabba4-3a52-4da5-ba34-f20e5cb58c13)(content(Whitespace\" \
         \"))))(Tile((id \
         9944a4f3-b816-4c18-84b0-089a30a6c4c1)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c0d3c91e-72d3-4c11-a439-66b29112e19e)(content(Whitespace\" \
         \")))))))))(Tile((id \
         6513fb59-0b1d-415a-9c32-b68e5cfd2a59)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07638262-05e0-4dee-8776-1259f4bba9cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ed27bf2-41ee-44b2-b860-3b8e12dc96ea)(content(Whitespace\"\\n\"))))(Tile((id \
         0381b6e6-b9a7-490f-a456-8eee08013306)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         27f0409e-208b-4063-9f1b-0e90b51b1d4c)(content(Whitespace\" \
         \"))))(Tile((id \
         14474822-2411-450f-a2a7-0d4d82d4720c)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66563e81-e6b2-4e37-9f51-31f4e9eca34d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8be3fc74-0d18-474c-ab2d-1fff8b701cf6)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ce024137-a55a-4bb5-b57d-4262198e0168)(content(Whitespace\" \
         \"))))(Tile((id \
         02544740-366d-4d56-babf-31845bb04532)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94bf6013-a370-4f46-9125-5342599bfd82)(content(Whitespace\" \
         \"))))(Tile((id \
         651fe1fb-5195-4989-93f7-6a303dc67a05)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b6d7fad-5d68-49a5-bc21-ec68bda597f5)(content(Whitespace\" \
         \")))))))))(Tile((id \
         8b7de4a6-1bd7-4ff8-9e83-53b70e180a05)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c53c5824-fe2b-41e3-a006-54f53de1a46b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d780ed9e-d886-486b-a239-fa64bb374449)(content(Whitespace\"\\n\"))))(Tile((id \
         81551768-cf9b-4ae9-85dd-911b058418ea)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         080427b4-33e9-49bf-a74b-775bd7f921e2)(content(Whitespace\" \
         \"))))(Tile((id \
         ee393b8a-736d-4e73-8fc4-f104dedebb4f)(label(extract_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         699428a3-ab4f-427f-b5cf-9ddfd5caeffd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c919858-4e7d-4a69-ae4e-b4be61816314)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c09baa21-32ad-40a2-829e-9af1dab3478e)(content(Whitespace\" \
         \"))))(Tile((id \
         80d6e082-cc0c-48e9-8ebf-80db716a35c1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07aa418e-3820-484a-841c-efa7fbcca86c)(content(Whitespace\" \
         \"))))(Tile((id \
         e1e4e421-b727-4fe5-863e-9824e36cf943)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34a85f0f-6086-4276-8793-a7fa55f6c7e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fe614837-b44f-4606-891d-cddca61c27ec)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# NIGHT BLOOM FILTER TASK                        #\n\
         #                                                #\n\
         # A plant catalog has entries like:              #\n\
         #   \"Moonbloom [night] 200ml\"                    #\n\
         #   \"Duskrose [day] 150ml\"                        #\n\
         #                                                #\n\
         # Filter to night-blooming plants and extract    #\n\
         # just their names: [\"Moonbloom\", \"Starfern\"]    #\n\
         #                                                #\n\
         # Steps:                                         #\n\
         #   1. is_night: check if entry contains \"night\" #\n\
         #   2. extract_name: get the first word          #\n\
         #   3. Combine with filter and map               #\n\
         #                                                #\n\
         # Available functions:                           #\n\
         #   string_match(pattern, str) -> Bool           #\n\
         #   string_split(separator, str) -> [String]     #\n\
         #   nth(list, index) -> element                  #\n\
         #   filter(list, predicate) -> list              #\n\
         #   map(list, fn) -> list                        #\n\
         #                                                #\n\
         # Note: string_match uses regex patterns.        #\n\
         # The pattern \"[abc]\" matches any of a, b, c.   #\n\
         #                                                #\n\
         # Tip: Use probes to see what your pattern       #\n\
         # actually matches -- regex can be surprising!   #\n\n\
         let entries = [\n\
         \"Moonbloom [night] 200ml\",\n\
         \"Duskrose [day] 150ml\",\n\
         \"Starfern [night] 175ml\",\n\
         \"Ghostvine [day] 100ml\"\n\
         ] in\n\n\
         # Check if entry is a night-blooming plant #\n\
         let is_night: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract just the plant name from an entry #\n\
         let extract_name: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Combine: filter night entries, then extract names #\n\
         let night_names =\n\
         ?\n\n\n\n\
         in\n\n\
         test night_names == [\"Moonbloom\", \"Starfern\"] end;\n\n\
         test is_night(\"Moonbloom [night] 200ml\") == true end;\n\n\
         test is_night(\"Duskrose [day] 150ml\") == false end;\n\n\
         test extract_name(\"Starfern [night] 175ml\") == \"Starfern\" end\n";
      refractors = "()";
    } )
