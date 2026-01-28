let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         ea69825d-8634-49d5-9583-b251da070dc6)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         79b23b26-f9a7-4e4b-a987-1b43abd03ace)(content(Whitespace\"\\n\"))))(Secondary((id \
         279831a0-aff1-4ad9-82f7-9af8fd931a3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         daa3b598-0bda-4d2a-a957-7306c422fe95)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         0e927ce5-f605-4292-9ef1-86989ed84f04)(content(Whitespace\"\\n\"))))(Secondary((id \
         44059a05-43eb-4ed9-a964-fb4bf31155a7)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         8329ab16-3aa8-440c-9f04-cc5dc453df42)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8b9e1d1-5785-40fc-9b97-5f48f38a5da4)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         360f894b-2f37-4ec1-8f8d-7005aebb67b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         68458633-af58-4461-bd43-91e053bae2e7)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         50f49c42-afae-4585-9ca0-6a2d5acc52e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa359279-2651-4261-8124-d6e4b67b87e3)(content(Whitespace\"\\n\"))))(Tile((id \
         c2ebfd15-3cd5-4074-841e-3247a54f5222)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5d07849f-5b7a-4afb-ae3f-6fc672665551)(content(Whitespace\" \
         \"))))(Tile((id \
         af39e031-401a-4475-bc96-096202c31afb)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00cf0405-2f89-4fc8-ab44-048c9f820c4a)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5c1f429-eb92-424b-84b7-0ad5b9b21470)(content(Whitespace\" \
         \"))))(Tile((id 9ca67f95-9112-4961-b821-dc9c20a1588e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f838f7f6-8cb8-4761-99f8-d5c766375772)(content(Whitespace\" \
         \"))))(Tile((id \
         d183c2a5-7590-4474-ade3-284566814db7)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0199e8f8-5787-4cbd-ab91-9be2e0511c30)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         38a4d7e1-5a54-4e3b-85d6-7d0a6ecc9697)(content(Whitespace\"\\n\"))))(Tile((id \
         800b0a30-67e8-42c8-a03b-baf91c97ce01)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ccc9e205-3e0e-4d3e-8b07-7efb71595284)(content(Whitespace\" \
         \"))))(Tile((id \
         7c3bb66d-b175-45d0-9f8d-cefad4f286b6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         16c16df3-a641-4659-92a7-6905e7da15f4)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         63a1bb4f-2e52-4e34-86d4-7ea59eeea8da)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         692a7fd6-0be9-4937-a880-597b20a5cad1)(content(Whitespace\" \
         \"))))(Tile((id \
         e5273eb7-9030-4dfc-9e8d-0a2574e14959)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d8b45c81-2935-41ca-91f4-dc20a68fe472)(content(Whitespace\" \
         \")))))((Secondary((id \
         8fc1f0c9-43e4-4300-8d28-9f259261e157)(content(Whitespace\" \
         \"))))(Tile((id \
         e76c918d-21bc-41c8-a511-d99049fe52eb)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59f53106-f271-479f-8b59-72c9dd544214)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ddb6b967-e6da-4c27-a1de-3aba94fb2b3f)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ed41626-870d-45fa-80a1-9b2ec9b1188b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2616fafb-aedc-4d82-9847-463345775343)(content(Whitespace\"\\n\"))))(Tile((id \
         3723ef39-ade0-4967-b987-e6cd03221947)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         322306a6-c72a-4cd1-ba21-8723ba540edb)(content(Whitespace\" \
         \"))))(Tile((id \
         2ff3bc3b-573e-414f-b9f4-e6b37102b898)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5ffcf57b-4668-4833-bd3e-ca55342c03b5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a84dd97f-94b5-429f-b218-6d853ec1fa0b)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7c07e6ac-20ab-4b4b-b57a-f3f3c1b16b20)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ce3fc5eb-0346-4166-bd81-1d1feb707271)(content(Whitespace\" \
         \"))))(Tile((id \
         074e374b-2860-486c-9aec-22a4cd44bc1c)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         a99b79f2-eb31-40bb-95c0-069eebee35f1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         49a55440-4aea-4759-baa2-c35770262b06)(content(Whitespace\" \
         \"))))(Tile((id \
         485f4750-1799-4a64-bed2-654bb0f8b7b2)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5f11c4b1-d41b-4132-a991-f6d0303e7251)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b3d2b03d-ca17-4187-b8b7-fba9f928051b)(content(Whitespace\"\\n\"))))(Tile((id \
         23d642bd-7c56-4f4c-8922-f55a07846c14)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6b97d90b-4600-4972-98d6-801bcabd0ee6)(content(Whitespace\" \
         \"))))(Tile((id \
         0b3c5b61-0d91-4854-9eda-d63598917376)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7a18ed85-c949-4ce2-869d-a98bbdfb4dd5)(content(Whitespace\" \
         \")))))((Secondary((id \
         b69ae453-eb5f-47aa-98ef-6a6fc867a840)(content(Whitespace\" \
         \"))))(Tile((id \
         160a997d-f064-413b-bcb4-304b0f45f103)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7ea442e-3e4d-41d0-afd6-e882a010e602)(content(Whitespace\" \
         \"))))(Tile((id \
         a32f0226-89ea-4af6-a8b4-0a150aa17514)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         113bf3ef-4351-4c2b-9518-7fef36b332d9)(content(Whitespace\" \
         \"))))(Tile((id \
         4c2e9867-0894-48d4-86ff-1724e9d3a1dd)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         afb3bb62-ddd9-4594-bdfb-4864ef951bcf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cbfa779a-bc3f-4921-8977-693e5e1a130a)(content(Whitespace\"\\n\"))))(Tile((id \
         b39b24ea-3489-4154-a023-ded22fe1f8f6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c3b9f2dd-6ad4-490f-8813-365d78fba19c)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba90f9f6-5e1e-4a37-9bda-28581465b76b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73799d31-6598-4a47-8866-812a23cd8679)(content(Whitespace\" \
         \"))))(Tile((id \
         621c269d-ab45-4bec-86de-5c94427e38cc)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b806bef-cf38-4982-94a8-f7b9186d672b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b4bc130-fbe0-4c25-be91-c075b3804e4a)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24def1c5-1644-4e62-b33b-3e7e4bc5e34e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6bedde8-3c6e-4aa2-b850-061dae68910f)(content(Whitespace\" \
         \"))))(Tile((id 46958072-44e8-4717-84b5-615a54d73a9b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         823172e0-7389-4b4d-8137-733290f3afff)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
         5136dde4-e9ed-48e7-aeee-74e6135c6590)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ecfa39a-21d9-4074-b410-4bbef9bb607b)(content(Whitespace\"\\n\"))))(Tile((id \
         227809f8-54dd-4aeb-b0bb-df7130ce3068)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c5eef873-d6d0-46cd-9c3a-d2eb0165b5cf)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1093a89b-5f6a-4462-b54c-5f81af031ff8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c266401-d2f5-4682-b1b6-76483708297c)(content(Whitespace\" \
         \"))))(Tile((id \
         6cfec1d8-c4dc-48d2-b2a5-a16e309b01dc)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7395c823-5895-47b8-9798-2a5c822df8ee)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         05538221-ae0a-430e-8747-4cfbd3225a76)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4155d577-45c4-49e8-8fef-e772badadf26)(content(Whitespace\"\\n\"))))(Tile((id \
         772dac67-dd07-4c1b-a5fa-97d9a6300bda)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a7d4899-270e-454d-af5c-b06f16c2f6c6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2fee5a6d-a902-47d1-984c-91d7a9f5b783)(content(Whitespace\"\\n\"))))(Secondary((id \
         193d3c91-accb-44b7-871e-aae55c018926)(content(Whitespace\"\\n\"))))(Tile((id \
         2da3e923-3301-4e43-a8a9-221b201fc46a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6fd2983f-aee5-4354-a075-0e2687829056)(content(Whitespace\"\\n\"))))(Tile((id \
         b2011dbe-ebfc-4843-9b84-60cc9a4a5e19)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a427c82b-cd25-48c2-9163-cbd4ce30911b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e7a19665-f971-492d-aca2-e54706d9b7d4)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6251ec48-203e-4488-ac92-8a7c7bc0651f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44eba263-e244-4340-9046-c7e422f24a1e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         905ad4f1-8a47-4330-8f3c-969d41449875)(content(Whitespace\" \
         \"))))(Tile((id \
         b3267257-3d96-4b6d-8b83-53cd006a3234)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a4a3a63-5add-4e48-a982-0f3bedb5d1d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9af4820a-6b7f-4dae-8c41-ac392d71b0e2)(content(Whitespace\" \
         \"))))(Tile((id \
         a8899a23-07af-419f-8ef4-8495c35013de)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7feb6f1b-5595-4b2e-b40c-dd75863e3134)(content(Whitespace\"\\n\"))))(Tile((id \
         92b08b13-8ed9-4ecd-a0c4-b3017a28576a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c09f09e-003b-4da8-90d5-28cf4ef9e32d)(content(Whitespace\" \
         \"))))(Tile((id 6318a276-3732-4a67-82c7-2fd50f7d3992)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee6c6600-cf17-43e1-8769-3279ec6990c9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dca01eb3-7cc2-42d5-9879-c22b32dc7060)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acaeadf4-5f60-446c-a79e-4fef0680d790)(content(Whitespace\" \
         \"))))(Tile((id \
         1c1e135a-ef98-426f-8e90-f34334f84f32)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         513f0af3-a7ef-4164-9cb1-8e62d8fc9125)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3676bc1-608c-4a0a-bc6f-3967608b008f)(content(Whitespace\" \
         \"))))(Tile((id \
         d2d7b36f-2d41-42d5-bba4-da3cbc9caa28)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5576bd06-4cda-4ad8-bd0d-45167d57eb27)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e8b5fc84-95d6-4379-bd17-d9f419b75d95)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c0fe381-6860-4fca-b7e7-ccd49fa23e32)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8479e1c-0764-42f4-9abc-8691e7f1ecec)(content(Whitespace\"\\n\"))))(Tile((id \
         793d93e0-8f93-4a9a-83ac-50313e261877)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         298757a7-b59f-4d05-8d37-eca3090353eb)(content(Whitespace\"\\n\"))))(Tile((id \
         d60ff5e9-28d0-4bc4-aa8e-942b17d33a1b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         763c69eb-5f38-4771-b6dc-c1974f9c9f1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6e02cea0-2466-4edb-b6dd-bb165f6da747)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1b57c498-2cc5-4757-bd86-b68b6cc65ef9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         db77fbd6-3a9d-4000-910a-0717272ee1e9)(content(Whitespace\"\\n\"))))(Tile((id \
         75227d6a-634e-40a7-8bcd-b2df98e20527)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef7bdbbb-0e7e-4946-95c3-bccd6486498c)(content(Whitespace\" \
         \"))))(Tile((id 0fdfea71-56f6-4c1c-9e69-2b5f4d1284fe)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c868c7c3-a46e-49ca-93bc-ac1336c0d671)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3e3177ad-29bf-43b8-9cbd-f7dd352e57c5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         94fb9c64-963e-44de-8e8c-47cd4fc83cd2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1035c2e-75c9-4576-986c-8e66ed92a599)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbca7b23-8e73-46d9-97d1-838976fffeac)(content(Whitespace\"\\n\"))))(Tile((id \
         1cad40a8-a729-44c8-a888-c17266ec470e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b17a8d6b-91ef-4815-8150-b6bc076a3276)(content(Whitespace\"\\n\"))))(Tile((id \
         f98c4b9d-4e59-46d1-8672-11038bef3797)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e35b940-2f9e-4f75-a6c2-bea9c081312f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e3d16e0-c5c9-4674-bfe3-6ea2cbf8dc05)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d2860353-db58-4398-abcf-c2f6082796c2)(content(Whitespace\"\\n\"))))(Tile((id \
         6bcd596c-47b8-485b-81bb-1077e678eb84)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c837744-db09-419c-b135-4b7eafcef3bd)(content(Whitespace\" \
         \"))))(Tile((id \
         8abc547b-704d-4867-a561-f5adb7582547)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28b6b08b-ac58-4895-84f2-54469b563935)(content(Whitespace\"\\n\")))))))))(Tile((id \
         90fac11e-e03c-42d3-b0be-d35b584e7e9e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3aa5b2ee-6705-4842-aab9-ef2c922062b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         eed3fe4f-a55d-43a3-805a-8f7de2e71606)(content(Whitespace\"\\n\"))))(Tile((id \
         14147d72-11d2-4654-a1af-a66bc5d40795)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1fc0fc68-8f02-4a90-b75e-418b740ccdbf)(content(Whitespace\"\\n\"))))(Tile((id \
         d59def7b-6260-403e-bb02-67b312877ed7)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af521b13-7251-45aa-9f5f-f75b11e5d0da)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e6b37f06-e51a-4372-a307-002f9b38c1c7)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         23d8bf79-68f3-4452-9483-9678fcf03320)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         076135d4-f1ae-4920-a7fd-a9cc31962ade)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b61d2db0-ca0b-4171-b469-5747a66efdbb)(content(Whitespace\" \
         \"))))(Tile((id \
         352bd1eb-34c4-420c-81ab-d5aa61fe1ce0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4356af7-7290-4db0-8951-7bba3956b57b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21def298-d76c-4d4b-b2f1-1de822e498e8)(content(Whitespace\" \
         \"))))(Tile((id \
         71782e9c-cbc1-4347-9a6b-974ee4ad57c5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5f63966-21df-4e6a-9a2e-16afa05c6bac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25d229da-9bbf-472e-a369-fddf8dea5320)(content(Whitespace\" \
         \"))))(Tile((id \
         7e568358-8ce9-4664-9383-c74b0faa320d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1e32217d-ca70-4153-8a60-f0f574dc5fea)(content(Whitespace\"\\n\"))))(Tile((id \
         f6002d69-68dc-4387-b1fc-53087e6e5298)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b045aa1c-bf62-4026-95d9-df8fc2b7d1e3)(content(Whitespace\" \
         \"))))(Tile((id 154f03e2-8361-467c-a72b-f394ff9e31cd)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         abbb21e2-04c0-404e-8932-d6eb546f4faf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad0cce61-d508-4fea-a024-42bdd4593dda)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e48614df-b714-4d92-a7eb-0585aeb2d8ed)(content(Whitespace\" \
         \"))))(Tile((id \
         23e8d16c-476d-4d15-83b7-0475bf9aad63)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac164f0f-0d49-4daf-ae73-1a7caf429b04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5eb52704-b8fa-417a-8285-888e4ec6bc38)(content(Whitespace\" \
         \"))))(Tile((id \
         56fc7f9d-c5db-458c-89ce-d5853e6c3e52)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f63f4bd-1f4b-4fc6-960e-d45755235231)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfebc066-1924-47a2-bb4a-1a317d89eecf)(content(Whitespace\" \
         \"))))(Tile((id \
         26033663-6401-4c30-9065-e04ddbaab03a)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d2d902b1-83f0-4328-9d3c-faaf0a11d1ff)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4416f2eb-d38e-4cfe-babe-e884443f6a75)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM - SOLUTION #\n\n\
         # Uses fold_left with a tuple accumulator:         #\n\
         # (running_total, result_list_so_far)              #\n\
         # On each step, add current element to total,      #\n\
         # append new total to result list.                 #\n\n\
         let running_sum = fun nums ->\n\
         let (_, result) = fold_left(nums,\n\
         fun ((total, acc), x) ->\n\
         let new_total = total + x in\n\
         (new_total, append(acc, [new_total])),\n\
         (0, [])\n\
         ) in\n\
         result\n\
         in\n\n\
         test\n\
         running_sum([1, 2, 3])\n\
         == [1, 3, 6]\n\
         end;\n\n\
         test\n\
         running_sum([5])\n\
         == [5]\n\
         end;\n\n\
         test\n\
         running_sum([])\n\
         == []\n\
         end;\n\n\
         test\n\
         running_sum([1, 1, 1, 1])\n\
         == [1, 2, 3, 4]\n\
         end\n";
      refractors = "()";
    } )
