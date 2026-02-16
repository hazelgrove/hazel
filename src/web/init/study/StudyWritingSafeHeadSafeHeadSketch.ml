let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / safe-head / safe-head-sketch",
    {
      segment =
        "((Secondary((id \
         cf38d849-8a7d-4a50-836f-183551a7b3f6)(content(Comment\"# SAFE HEAD \
         TASK                               #\"))))(Secondary((id \
         6d653f1b-7615-4a55-9831-50e06521209b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f43d9734-9f96-47ff-ade9-ac070a8d5d91)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         cbd5b776-9ad2-40a7-a464-93b992322eb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         05b51507-d580-425f-8b35-6f979a67d62e)(content(Comment\"# Implement \
         safe_head: get the first element   #\"))))(Secondary((id \
         e5b0d659-6529-4c00-9225-707403cfaf60)(content(Whitespace\"\\n\"))))(Secondary((id \
         1069b9cc-c621-4582-8f9a-b2b0201c4078)(content(Comment\"# of a list, \
         or return a default if empty.     #\"))))(Secondary((id \
         e8ac1ae2-0f40-4ed4-a03a-aac9768ab444)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4055b7d-392e-4d4a-b957-0862dbb85825)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         032747f4-08c7-4ccd-a675-77744833aa84)(content(Whitespace\"\\n\"))))(Secondary((id \
         7670fd40-833f-4b88-82a3-5d2022a8d406)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         c7f61aef-8aa7-4784-ae09-a6ef5b71decf)(content(Whitespace\"\\n\"))))(Secondary((id \
         43ff2eee-aea9-45f9-bcd2-2b02719a8327)(content(Comment\"#   \
         safe_head([1, 2, 3], 0) == 1               #\"))))(Secondary((id \
         4be8b33d-29a7-4d84-873b-3ed4c7e5e19f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a00697a-f9ef-4249-937f-cd2362e5572e)(content(Comment\"#   \
         safe_head([], 99) == 99                    #\"))))(Secondary((id \
         3218ac8a-19de-4968-b09a-c04980fa845d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a98a00a-13f3-4588-b44b-cb2c84aa2277)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         4b142f87-f8a8-4a30-9c99-86ef433a8eda)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffc37f25-e55f-4817-b3fb-4066ad439b6f)(content(Comment\"# Available \
         syntax:                            #\"))))(Secondary((id \
         d68c538e-636c-4d67-bd3a-2fdeff1d3915)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6cdfbe5-969b-480e-a669-204fb5948acd)(content(Comment\"#   case \
         expr                                  #\"))))(Secondary((id \
         7faaaa66-81b9-4606-a322-1a5e875ad1e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         59df9139-b896-49a3-b1f3-22dee6723745)(content(Comment\"#   | pattern1 \
         => result1                      #\"))))(Secondary((id \
         79ce6f7e-bb0b-4e92-98f2-7cc7cdbb97a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f4839c1-b2d5-4f33-86ab-b3997420238d)(content(Comment\"#   | pattern2 \
         => result2                      #\"))))(Secondary((id \
         b18e7d9a-9dd0-4c56-830c-4c4351bb5e62)(content(Whitespace\"\\n\"))))(Secondary((id \
         68af8ad4-9bfb-456e-8bed-ebc39459cb83)(content(Comment\"#   \
         end                                        #\"))))(Secondary((id \
         9a7073b9-92d6-4dd3-ad66-dfc52dc56e4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6859b63d-4965-42cd-a681-15cff1084a0b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8971cc55-200b-412d-aab4-062b12d24642)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca3cde9c-f667-4396-9eb7-010bb74e1c60)(content(Comment\"#   List \
         patterns: [], x::xs, [a, b, c]        #\"))))(Secondary((id \
         f36d8665-9d5a-4a93-93c7-94880fe62e9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfd8b4aa-c193-44d0-9ae3-f497dcbad5ec)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         70c1f7b2-058c-4731-af27-a56969383f6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e0bb467-a6d5-4ca4-aa57-f0fdfeb34a5a)(content(Comment\"# Tip: Turn on \
         auto-probe to see which branch  #\"))))(Secondary((id \
         000683a7-8ace-4b8c-9081-f0fc9881d945)(content(Whitespace\"\\n\"))))(Secondary((id \
         77722e3a-9d6c-4bc6-b0b4-243d7beb683d)(content(Comment\"# is taken for \
         each test case.                 #\"))))(Secondary((id \
         5a344313-0e6c-47d1-9588-f21ba9c6ccf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebb7e13f-44bb-4704-840a-fbe67b274b8d)(content(Whitespace\"\\n\"))))(Tile((id \
         5d0146b4-8610-46e0-9dcd-c2c97240c28b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ca0fc30f-6801-4fc5-9493-f447a428413b)(content(Whitespace\" \
         \"))))(Tile((id \
         447ccf70-015d-4897-83a5-35c018a83460)(label(safe_head))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b6e2ba09-aacb-4625-9989-c2b730cc5a35)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4c6ef01-7054-4158-9799-e6f87edbfc9e)(content(Whitespace\" \
         \"))))(Tile((id 27019f3a-2b43-428c-b926-475d41dac195)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         05302375-22ef-460d-becb-5907c88ecf92)(content(Whitespace\" \
         \"))))(Tile((id \
         2fe31ede-0a82-4eb7-8aee-c44b6bc1053f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         45a3b620-e5fa-4a67-a1f8-092e828d0c4c)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         592cc501-c580-4bf2-a619-1269665de141)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3bef583a-fba6-4401-9666-379d8ce4683d)(content(Whitespace\" \
         \"))))(Tile((id \
         a9e0b4e8-69ef-4be3-8a8f-9673d7df1d3a)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9d46bc6e-4397-4a9e-b5b7-5152d6303c60)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         984b3aef-dbc5-4500-98fe-f53c2a6912c3)(content(Whitespace\"\\n\"))))(Tile((id \
         6b75725d-a6bd-4124-86f4-470b66e97ce2)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         888fe0e2-56df-4bf4-82ee-77dc696a69f3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a70e900d-1835-4c2a-9137-42344fc9d9d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3c7d2c9-ed3b-47f3-8470-4491ab27f3e2)(content(Whitespace\"\\n\"))))(Tile((id \
         3e3d2784-76d0-475b-83ec-70a2201bc180)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         caf73bf4-7e44-475c-a752-20d6fed2224b)(content(Whitespace\"\\n\"))))(Tile((id \
         981ff4a4-fb6e-4484-a121-f2944ea2b691)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3cab59e-2778-473d-b2b9-7fedf8d562ca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4a7ff108-227d-4384-9621-00a64e11a4f5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fe81abba-65f6-4851-bba4-ff6f175a2428)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3604d46-814f-477a-9f6e-7e416ed80ad8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89d46306-2cb9-4801-aca2-e8950752d2f1)(content(Whitespace\" \
         \"))))(Tile((id \
         abc4978f-cf73-4b80-b08b-1f2b5b9ec2f5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61240257-740a-49af-af5e-a5687b857d71)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c053dfc-6eb5-44a0-8eaa-be3c2f518699)(content(Whitespace\" \
         \"))))(Tile((id \
         23be3806-640c-4da2-9079-b6ccf35f073d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         eba8a13b-7466-4940-af86-271307738131)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a12c87a-4669-43e4-a02e-d40a91854f32)(content(Whitespace\" \
         \"))))(Tile((id \
         d051c08f-095d-45df-912a-e0d422ae990b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4aa22a0b-b7ed-4faa-a54d-7edec0fa4de2)(content(Whitespace\"\\n\"))))(Tile((id \
         e228ee6a-36fd-4fd4-b4d8-84e7ccbca327)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7511192d-e90c-443d-81e8-82a932d8d03c)(content(Whitespace\" \
         \"))))(Tile((id \
         e61289dc-749c-4067-a635-dfb44bd8d463)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         831250ad-36fb-4a0e-b340-41a5ecf4a8fd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         096c085d-3d38-4dbf-b66c-b651fe8f7650)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f1f0c8f-db13-44da-a2fd-c32150182283)(content(Whitespace\"\\n\"))))(Secondary((id \
         888cd371-c74b-41d6-a9eb-ee1056ab9eb8)(content(Whitespace\"\\n\"))))(Tile((id \
         170160d6-3fc1-4874-b314-23e9f0ee0f0d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         968d155c-7fec-430c-8874-9ae489acff84)(content(Whitespace\"\\n\"))))(Tile((id \
         968216f7-2ccd-44ee-8279-e31463db5f0f)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f85b3b88-c1ca-4647-920b-77214f9acc41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b8da98de-395d-41c8-bb4a-4ce321d39ebc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9454bdf4-46de-453a-9b86-c8d016bb169f)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6ef575b7-48db-407f-b5e5-14d3536bd16b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddc43d39-5523-4e8e-823e-b14ee2198ad5)(content(Whitespace\" \
         \"))))(Tile((id \
         e7383af4-5809-460f-9766-bab1ed1cf5eb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5611078b-5a8d-46a1-9662-ec0847b16984)(content(Whitespace\"\\n\"))))(Tile((id \
         bbcc2229-f53b-459a-8f66-1b8a78dcdc6a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc3b40bc-d1f3-4857-bd17-7895f45b2501)(content(Whitespace\" \
         \"))))(Tile((id \
         b208e7da-e29b-4e90-bc7c-0e69fa2be470)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         254a3ed3-fe05-4d1b-91ba-c543faeba5cf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         de333da7-e002-43cc-90cf-3e5de9c3c800)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32e7a078-3e14-4da9-a29b-9adeba41dd1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         45d331b9-f2e7-4318-8ec4-1cfc13ab7f51)(content(Whitespace\"\\n\"))))(Tile((id \
         fe812fd7-4a30-4405-8c8b-b02bb7e9ccf4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c6d64bb5-c9c5-4cc3-87c5-eb8734380b51)(content(Whitespace\"\\n\"))))(Tile((id \
         008b8e22-45c4-4d07-a416-8882a9bbd426)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fceb70f5-294c-4425-a89f-3f1ae0e00034)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4d8e64b4-4d6f-44df-8e26-ba668e4dc18f)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40729f76-9aee-4bad-a11a-9a8aad432977)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8d2ed04-5727-48a2-8134-c13fdc92c79e)(content(Whitespace\" \
         \"))))(Tile((id \
         71b03d35-d0b3-4465-bd49-6963403603cc)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f646cb1e-f7ad-40f7-92f3-ca83ad8d9b09)(content(Whitespace\"\\n\"))))(Tile((id \
         f8038868-f11e-4c18-9328-ca3473f8803c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c624040-9e20-47ed-b0c2-07c8d3b1c86e)(content(Whitespace\" \
         \"))))(Tile((id \
         c16d7b3b-b92a-46fd-90b6-62ab803ef1bc)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         883201f5-a231-4580-aa65-fa1827ce37a7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         788cef02-9acd-4933-8acd-36f9d04e6666)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f3fae4b-ca8f-46e4-92f4-538896807181)(content(Whitespace\"\\n\"))))(Secondary((id \
         861c1497-f96e-4520-9c2a-b29eafc9c0f3)(content(Whitespace\"\\n\"))))(Tile((id \
         c7c893d8-9aa9-4f34-8718-dd8e8f4f0c6d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f8850bd8-7887-498d-a21e-8f6bcb889107)(content(Whitespace\"\\n\"))))(Tile((id \
         2780cdf9-d165-450b-bab9-11770356d0e6)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9ea6a26-6f7e-476b-bca1-293b8c827e52)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e6268ae9-ad1d-4ae4-a312-7d05f1a8de90)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         572de01d-a173-42cf-be52-2612fea57eee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e545a98-ee7b-49ca-b1c2-beb1f099dda4)(content(Whitespace\" \
         \"))))(Tile((id \
         c25113da-0282-416f-b355-62a3e6bbbbf6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f795774-b288-4bde-b1c7-8199825ac078)(content(Whitespace\"\\n\"))))(Tile((id \
         daedd4b8-3d8c-430d-8fdb-cf787de0a819)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a9efd02-42a8-4b42-9bc4-29be4f409cf7)(content(Whitespace\" \
         \"))))(Tile((id \
         94c0780b-38f8-4b97-a0c6-913fb7bdfa1f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab52af59-e580-4ae9-ac3e-0bebcbe882a9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9f4340b3-3bac-49bb-94e3-44721156416c)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# SAFE HEAD TASK                               #\n\
         #                                              #\n\
         # Implement safe_head: get the first element   #\n\
         # of a list, or return a default if empty.     #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   safe_head([1, 2, 3], 0) == 1               #\n\
         #   safe_head([], 99) == 99                    #\n\
         #                                              #\n\
         # Available syntax:                            #\n\
         #   case expr                                  #\n\
         #   | pattern1 => result1                      #\n\
         #   | pattern2 => result2                      #\n\
         #   end                                        #\n\
         #                                              #\n\
         #   List patterns: [], x::xs, [a, b, c]        #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe to see which branch  #\n\
         # is taken for each test case.                 #\n\n\
         let safe_head = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         safe_head([1, 2, 3], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         safe_head([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         safe_head([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         safe_head([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
