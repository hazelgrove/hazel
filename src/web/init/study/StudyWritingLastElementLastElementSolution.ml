let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-solution",
    {
      segment =
        "((Secondary((id \
         37d95fe8-4eac-43fc-b720-b9cec9ad68cc)(content(Comment\"# LAST ELEMENT \
         - SOLUTION #\"))))(Secondary((id \
         b492e267-a590-4592-be4d-c0d08c4fece3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7310c05-42a4-4c65-b79e-b5c62b51a2e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfe165df-49f0-426f-a505-838f9e2b561e)(content(Comment\"# Each step of \
         the fold replaces the accumulator   #\"))))(Secondary((id \
         34498416-a629-4bc5-a728-6f0da72ac93b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee00fd5b-50e6-4acf-b6de-3a1e371fda73)(content(Comment\"# with the \
         current element. The final result is    #\"))))(Secondary((id \
         e8f94e16-ad09-42a2-afc1-3b38e459f35b)(content(Whitespace\"\\n\"))))(Secondary((id \
         47d6334d-c24c-42f7-90eb-b0ed0ea0455c)(content(Comment\"# the last \
         element seen. For empty list, returns   #\"))))(Secondary((id \
         8aa90d98-c4d3-4b5f-a001-331d9557dd78)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa9d482e-882b-4835-8967-af9d532ef394)(content(Comment\"# the initial \
         value (default).                     #\"))))(Secondary((id \
         14c60bbe-586d-4283-a5a2-3faecba229a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae0d43f1-88ed-427d-a8eb-55c771af0d71)(content(Whitespace\"\\n\"))))(Tile((id \
         96188306-7ae7-4e88-81de-a6105f3b6040)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb05ea8a-f2bd-4178-8052-0275969a7bd8)(content(Whitespace\" \
         \"))))(Tile((id \
         4dbd5fcb-9a84-4e6e-902b-e28064bdc09e)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00476477-1bd3-464a-90c7-ba748a2cc4a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         6cb0cca3-b63b-47ed-8dbb-082f630b42a0)(content(Whitespace\" \
         \"))))(Tile((id 9c746bc8-586f-4f70-9ad3-d1e2d6cf264f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4d910d11-0cd5-40ea-a689-e410ec3e85ef)(content(Whitespace\" \
         \"))))(Tile((id \
         c34e2f3b-1d6b-482a-8d13-7709ff829f96)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         edce3c12-a420-4c52-9548-f1998655083f)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         eb154268-d27a-4291-aed7-0f61fbe75ba1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         27818773-d6e8-4f1c-87ec-3a135ef3be25)(content(Whitespace\" \
         \"))))(Tile((id \
         c5322199-db2c-4331-8359-86a06b043e04)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         27d8bc23-3136-475e-a408-1ead206fd9a0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5a13d9fe-ac9b-4f32-b00a-19212d265ac3)(content(Whitespace\"\\n\"))))(Tile((id \
         c770a2a3-a348-4c29-b9a3-151f240a8d0e)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b20f117-62fb-45ee-8a87-561654adc988)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fee4a236-4016-494c-bb45-5262b4f396f6)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8761c357-7a57-4438-a65c-283b32ddb635)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b257e42f-9abe-45bb-a820-d894de3233b1)(content(Whitespace\" \
         \"))))(Tile((id a8cf8064-7a8b-479a-b270-e6a31d585b04)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f7db3edb-e52a-4976-adef-008080337f79)(content(Whitespace\" \
         \"))))(Tile((id \
         d208a551-845d-476c-b75d-6135e50a32d5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6a97cdba-690e-4fcd-b526-b225ee974d37)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c7834a09-288e-416c-8e6a-77686969a5bb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6c3da461-7af2-40bd-a367-ff397ed18ef3)(content(Whitespace\" \
         \"))))(Tile((id \
         d1236ac2-795f-4d8e-904b-ee519673657d)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         bca51954-5e2c-44e2-8eff-4108aadcb071)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a361abb1-8a5d-4741-9eba-c10ab18c4f10)(content(Whitespace\" \
         \"))))(Tile((id \
         0a59244a-cfc8-41de-aa4a-feba7337f5b0)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6402f64f-d7e5-4005-98c3-3179bd27eef1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38de102b-913f-4fb8-9240-cfba559b330e)(content(Whitespace\" \
         \"))))(Tile((id \
         173393f3-02bf-480e-86d4-e9513baf484b)(label(default))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f5af8f6e-8a58-4743-9532-e5cd2a7df1d0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4c90151e-5a49-4549-a1de-9365febd8a44)(content(Whitespace\"\\n\"))))(Secondary((id \
         2081ecb2-9556-4e6b-88bb-df1bef51bc79)(content(Whitespace\"\\n\"))))(Tile((id \
         494c5249-a090-47b1-b494-00596c301c92)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e36aaec7-9ac1-4518-ada5-5f1f3b351f1f)(content(Whitespace\"\\n\"))))(Tile((id \
         678a9142-0dbf-418a-9dd2-baf26420984c)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97d7d55b-6225-4947-a5ba-263d7c447af7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         79e48441-6d29-4ddd-b251-62ec70c1852a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f1708381-1e40-48b9-8848-3f737fb66ca6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7b82fd4-da99-42a7-b0a7-314c6d70beba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d246f810-354d-4317-8ee9-893cf220e34b)(content(Whitespace\" \
         \"))))(Tile((id \
         9fd8c490-f466-45f6-a69e-c98790ce2fe6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0107e44-7430-481a-932d-209e64279734)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2e136ac-b7fd-4efc-960b-21bffee33ccc)(content(Whitespace\" \
         \"))))(Tile((id \
         6eab0700-7939-47a9-ba35-07540dcf7350)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8f50db48-04c3-4d56-8060-f1283c209edf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4fcbda3-5f93-44d1-90f1-9b39b51ead56)(content(Whitespace\" \
         \"))))(Tile((id \
         c7c1a5af-a920-46df-81ee-33b2683c9ed3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bdcf9b58-6d1c-4c89-97c9-37431ba3ae07)(content(Whitespace\"\\n\"))))(Tile((id \
         289ead53-45f4-4973-b109-d8740149cd31)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46864e20-d295-45f8-bb0a-6d76c493d61e)(content(Whitespace\" \
         \"))))(Tile((id \
         f93ff36e-b048-4dc2-8905-c67068160b59)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1addca68-32d7-49c6-b815-4a9ba49fb228)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8c97c923-9b2c-474f-aa5f-827cce19062b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40aaf0e4-f2bd-4053-82d2-37a2bc317ffc)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc1a30eb-4cf8-414c-be41-2d96d2bb7768)(content(Whitespace\"\\n\"))))(Tile((id \
         d95ef069-5c41-41e1-af1d-a80438bf9dd6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ee5c27b3-1b2c-4fb5-8517-814e08e6e045)(content(Whitespace\"\\n\"))))(Tile((id \
         584dce70-19b6-4483-bb1e-ce6aa02cbc9c)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93f9ec95-de0a-441b-b7f4-72ce3c905d34)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0d31cec-8135-46df-a9e3-a5a45a53b856)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         20d1c916-4ed0-40de-b004-05daf3975651)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         20cf5cd2-3905-4c71-ba24-00bc81c16728)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85667da6-d3c3-4dc6-8154-055aaebd9b98)(content(Whitespace\" \
         \"))))(Tile((id \
         3e3bda4e-5458-4251-99eb-be4294e6daf0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a54b7cf7-f009-4145-8394-5a5acbdc041d)(content(Whitespace\"\\n\"))))(Tile((id \
         049f6179-7566-469a-9f06-e77c26ab1410)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d88d5acc-3662-4f0e-b556-89ca192c7a76)(content(Whitespace\" \
         \"))))(Tile((id \
         d9c63bca-6b1d-451f-b786-93d2acb0ee0b)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1eb47389-6198-4cd6-adcc-f2ff093ed601)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6311a3a1-3e62-4517-a9f0-12f3ff15da0e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b8afedc-c963-4152-97ea-4d394151b859)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6c557fb-6c36-4f32-b56e-783575a8dd99)(content(Whitespace\"\\n\"))))(Tile((id \
         ade58451-ebf1-4add-a329-9683df1fd43e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a027584d-7961-44e8-b6c6-109924f26dde)(content(Whitespace\"\\n\"))))(Tile((id \
         9c3f2983-7313-4680-a724-299d60466b50)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         864626c9-05ec-4ee7-815d-3180211faca9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         523f59e5-59e7-4a4a-bde5-d777c8624efe)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         666a8ee4-39f4-4ac8-b49d-e777385f4486)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2459dcd0-7a5a-4265-b82b-91a3b0312b77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87992655-a83c-4fb9-8768-d6388cabd700)(content(Whitespace\" \
         \"))))(Tile((id \
         2895e2a4-e4ed-4206-bebe-800bc4aaf203)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45c5ef93-35dc-440b-8ed9-53a366311364)(content(Whitespace\"\\n\"))))(Tile((id \
         0b1ee5ce-57b5-451b-88d2-f827aac549af)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52e1155d-b033-456e-beec-0f5918c54520)(content(Whitespace\" \
         \"))))(Tile((id \
         1386bf8b-8a83-4cc8-bfa5-77fbab752a0e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b5455e51-0e31-441e-b3c5-4625b9d87c09)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1c6fc763-5399-4199-9263-a3ca79720bcb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f3d64ad-830f-4261-b77b-a3fee17ba6e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         22675182-f01f-4cf8-a252-40142f309517)(content(Whitespace\"\\n\"))))(Tile((id \
         3e8b8021-ae37-4e0b-a5a8-0fc3ef5920a2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a6b9a751-a838-4958-94fd-162cea1b9bfc)(content(Whitespace\"\\n\"))))(Tile((id \
         992064f7-c389-406c-a9e3-6d59d692365e)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         777cc380-179a-4c5d-b8a0-93afda5838ba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42fd6d47-1904-4939-a681-9770f592d4a6)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab2d92e6-66ca-446e-9a00-986909a5c78f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7877e479-94c8-4f7a-a600-4bf21dd520fb)(content(Whitespace\" \
         \"))))(Tile((id \
         b01a561c-08e8-4b8e-8d02-6eaab5587c02)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1f82c1ad-801a-4f15-87cf-58f7a0fd77e2)(content(Whitespace\"\\n\"))))(Tile((id \
         6d546eec-6793-4f8d-b651-a486fd5a4586)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7be4adaf-22d1-4e9b-9180-b69d574ed233)(content(Whitespace\" \
         \"))))(Tile((id \
         89c1018d-6c7a-4f55-a11d-8989d65f2675)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be4ceaf9-7c3c-44b9-a0c1-866773245de4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4e7e79c5-2e8a-4874-a8b9-6ee77a1a86de)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed3f31e4-dc85-4250-b298-3c56ae0d8bfa)(content(Whitespace\"\\n\"))))(Secondary((id \
         e58f98ce-8c0d-4e85-902e-79e3cde48c3f)(content(Whitespace\"\\n\"))))(Tile((id \
         96bf73e3-9dc7-45b3-a243-6942667d2327)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e742f419-6c82-4048-a401-a674bed34056)(content(Whitespace\"\\n\"))))(Tile((id \
         01b5c5a9-da42-4341-9bc4-bfce49c72c09)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         51173d64-7d3f-4268-ba7a-8f97cc4a7cbc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af715699-bfae-4077-8ec2-5c3636696a20)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1d2e5ad-80cd-4278-b815-db86c42d2141)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f74d2aa-ed15-4068-869b-d7ca150cef41)(content(Whitespace\" \
         \"))))(Tile((id \
         f92911f7-a701-452f-b91a-fbaba2dd635b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         89b76d6a-7caa-4798-9e0a-38000ddb23ba)(content(Whitespace\"\\n\"))))(Tile((id \
         3597f929-ac42-4f51-98ac-91ca865a72c9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b40723fc-104c-45e7-adcf-68b847db3e62)(content(Whitespace\" \
         \"))))(Tile((id \
         d377d445-fe61-4e21-9b9c-c2c19ae8ba26)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         341f86dc-a01e-4c79-b3d9-0550e76cdc88)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4fe259ef-1a59-44ee-b5ae-c1ae23ba3006)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# LAST ELEMENT - SOLUTION #\n\n\
         # Each step of the fold replaces the accumulator   #\n\
         # with the current element. The final result is    #\n\
         # the last element seen. For empty list, returns   #\n\
         # the initial value (default).                     #\n\n\
         let last = fun (xs, default) ->\n\
         fold_left(xs, fun (acc, x) -> x, default)\n\
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
