let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / last-element / last-element-solution",
    {
      segment =
        "((Secondary((id \
         1bf1835b-e51a-4973-ab0c-0e75d3a97341)(content(Comment\"# LAST ELEMENT \
         - SOLUTION #\"))))(Secondary((id \
         2a9b9145-30ff-43b1-ae7b-60fd526c9bcf)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b171628-4a11-4e90-a036-12a183bcda41)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd3c7f46-a133-4e9e-a542-0af0ce10c2d0)(content(Comment\"# Each step of \
         the fold replaces the accumulator   #\"))))(Secondary((id \
         52cf8621-ffb3-4198-93da-b0b082ab2999)(content(Whitespace\"\\n\"))))(Secondary((id \
         6209da62-61a6-4856-a3b6-035652eac6a3)(content(Comment\"# with the \
         current element. The final result is    #\"))))(Secondary((id \
         fd703515-5bbf-453f-8a78-3df6347a890c)(content(Whitespace\"\\n\"))))(Secondary((id \
         f94ef926-06f0-4813-9116-9fb2b3d9a746)(content(Comment\"# the last \
         element seen. For empty list, returns   #\"))))(Secondary((id \
         d56e200c-fe29-42ae-ae43-ee93b2c3e169)(content(Whitespace\"\\n\"))))(Secondary((id \
         afc5ff05-28b5-4aca-b20d-66002f11c84f)(content(Comment\"# the initial \
         value (default).                     #\"))))(Secondary((id \
         653c2bc3-e5ab-4782-90e1-faf020da7592)(content(Whitespace\"\\n\"))))(Secondary((id \
         adc91bbf-e16d-467e-b4a5-00cabd902fd9)(content(Whitespace\"\\n\"))))(Tile((id \
         1ca4c1f4-3615-4d64-afb9-049363575bd4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6451f1a1-6bf5-47cb-a958-0796f64d1046)(content(Whitespace\" \
         \"))))(Tile((id \
         2a75f224-c414-4125-a418-3e0c453c9c46)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6df19f7b-c851-4518-ad7e-4c6b69d1e7c5)(content(Whitespace\" \
         \")))))((Secondary((id \
         555cca1f-4285-4553-bc78-ead50ea4eeaf)(content(Whitespace\" \
         \"))))(Tile((id 121756c1-a93d-4eb6-9792-00928bcf043c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6ab16eb2-a6bb-4146-bc52-8f38ee012822)(content(Whitespace\" \
         \"))))(Tile((id \
         896d7652-d12c-4ad2-b7d9-b677b29250bb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         be02dfde-587c-4aa1-a0e3-42b9d51fcd98)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e3611473-6e92-40d4-9b77-6b52020b3a39)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7934e3ff-1715-4f69-b1df-9afa89fbae9c)(content(Whitespace\" \
         \"))))(Tile((id \
         0ff45175-293d-4b6b-94b1-9867321f8a55)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         bfd05050-54d3-414c-8900-4c19d2635790)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a1cbd33-541b-49ae-800b-c0d34189f8ab)(content(Whitespace\"\\n\"))))(Tile((id \
         b0413039-84a8-4122-a912-530b3ebef97b)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56570dde-3051-489d-8557-0755f17b6817)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0925179e-4517-4099-9f3c-068550ef5d35)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         845d04fe-1c4e-457f-90b5-3bf3a36b75ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0e92e5d-d3ea-493b-b70f-d1c4b15ff0f5)(content(Whitespace\" \
         \"))))(Tile((id 91d36b6b-5812-4449-b150-c349d7786b81)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c352eb70-fb80-40f9-a89b-9c2b5af85c1a)(content(Whitespace\" \
         \"))))(Tile((id \
         f140c5c8-6f26-4b19-86aa-9395d27b6707)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         760ad240-8346-4a4e-acde-3890f71ff6c9)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         eb6dee9e-13f4-49f7-8b3b-8eeba09ed305)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9d658278-df35-41ac-a042-9bc40e0f9617)(content(Whitespace\" \
         \"))))(Tile((id \
         ae1adf62-0953-4bbf-8100-54930722a931)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         091e58d9-2469-4b25-98f2-bb4449a7cbd1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9225468a-0815-4e79-83c2-a5be5442f74f)(content(Whitespace\" \
         \"))))(Tile((id \
         51b4270d-d064-4b9f-85d7-dea9d64ba691)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b21ad159-cce7-4b46-8af9-b930df6b152b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e9e2008-a695-43ab-b542-2ca328efcfd2)(content(Whitespace\" \
         \"))))(Tile((id \
         448e8ef8-4715-4918-b57d-126528317681)(label(default))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4d5c6b3d-2259-4948-82cd-75b411d74b94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e0e36585-65be-4952-b2a1-022305979c5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         761389e0-7093-4227-a15c-bc62447f0e77)(content(Whitespace\"\\n\"))))(Tile((id \
         b04aab7e-2c04-4631-a121-5ca7191ac91a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         171d9714-11a7-48a3-8bcd-12482f31210b)(content(Whitespace\"\\n\"))))(Tile((id \
         6209b68b-6cc7-4b01-b022-c42f6d8d9265)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41e32874-5ace-4ba6-8e2b-6f2b6a1133a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a79d55d-0636-4094-8785-ba8d93e69504)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         76167555-4888-4f62-964d-5487f94bc128)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a378be4-8826-4af2-9205-24ed85cd2bae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40a56ba0-b8d2-4e90-8f50-5029338386a7)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed17db1-bbcb-4ddb-a6b0-6099103f851c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c2ad431-a5ff-46f1-b726-676bd738180b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         240385b0-235a-4eb9-9732-f53c1d26321b)(content(Whitespace\" \
         \"))))(Tile((id \
         425a2c0f-c28a-4ad3-ad66-35a1a82ce5b6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6ea5f544-d63d-43e2-a70c-f085cf51e76e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce612831-d153-4fbd-902d-b0c4a7ddb25b)(content(Whitespace\" \
         \"))))(Tile((id \
         d159a437-3c3b-45ff-ad3a-46b91a5444de)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cdd23edb-0f39-4677-ae84-0c061fdf1c7a)(content(Whitespace\"\\n\"))))(Tile((id \
         2cb95bc9-bc95-48a0-85bc-53528c273c32)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfb6f135-c6e4-4e9c-a199-b679ff469e77)(content(Whitespace\" \
         \"))))(Tile((id \
         a1d49a50-8a83-4c0e-ad53-2775ddc02f8d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         331245cd-4ee7-4119-a7be-57e6c2301229)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e43b9834-5a8f-400a-8cea-656a3dc6d2f1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         290ea113-a43d-4f92-8231-ac689e98e003)(content(Whitespace\"\\n\"))))(Secondary((id \
         46ad9c0e-9978-4a9f-9121-9faa12c2855c)(content(Whitespace\"\\n\"))))(Tile((id \
         ffe71de7-d64d-4ced-851d-8baabf511257)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fc083cea-ceff-47b2-a758-11c97ed5cdfb)(content(Whitespace\"\\n\"))))(Tile((id \
         a7a7a91c-9e01-4585-ba18-950085f49b99)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4fdb605-c71d-44c1-a79f-0c5a5a1b40fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4d029607-2594-4dce-a10e-8e6543b7e06f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2c92fe78-9b6f-4b1d-ac7c-0cab1abcecd0)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3e2de194-9586-4cc1-a154-d685493e73e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7722a2a7-33af-443d-b8d8-7ffce8fd4b55)(content(Whitespace\" \
         \"))))(Tile((id \
         a9971514-bfdd-42bc-b74d-e8cd10243d37)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c30c6bb1-0aac-41b5-82c1-a31aea222e8c)(content(Whitespace\"\\n\"))))(Tile((id \
         9cdbd460-255c-4740-802c-6048cb87f80a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         570b83ce-3013-4780-9f59-ac530923f8c1)(content(Whitespace\" \
         \"))))(Tile((id \
         43781ec9-e08a-45d0-b732-9348b35c2dc6)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c498d84-8843-40f9-9f67-57f338bcdeee)(content(Whitespace\"\\n\")))))))))(Tile((id \
         144fb982-8e4f-49cb-a459-2ddb1ad031ec)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a94b05da-ae5b-4ad0-ae7f-20369fd3d18d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c55dc84b-5d16-4c66-8541-3345510930f7)(content(Whitespace\"\\n\"))))(Tile((id \
         430eff85-ab8d-4afc-91fd-d30a0f1b19ae)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6db88fcc-23e1-4f7e-a1e8-72c4ef0825ec)(content(Whitespace\"\\n\"))))(Tile((id \
         32570062-d7dc-4f71-944e-ba46a9df12b9)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7761978f-e103-4182-b8fa-7e86c4e56af8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ed04c77-eddc-4b02-89dc-ef618dd5bb59)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bb9e206a-dee5-41f2-96be-d1ea6fcbdcfd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1dec7d57-0370-453b-a11e-e637056e3afd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1aee5d2-376a-43b7-a25d-4feebfdf397c)(content(Whitespace\" \
         \"))))(Tile((id \
         a5cf2da8-1229-4ffa-9d4f-ea638e9bd574)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70b2cae3-be0c-4b53-a747-765692dd1c3c)(content(Whitespace\"\\n\"))))(Tile((id \
         3f36f453-d072-4084-a3f8-a2bc51c5b3fc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         050d2b43-819d-4332-9c1f-cc6e19d7eb49)(content(Whitespace\" \
         \"))))(Tile((id \
         9062711c-e84b-4ad0-976a-e9f49f07fa62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b963cc9-1a4b-4b2d-a9f9-8b96ec6bf9ff)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c62b7a97-1b1c-4cc9-9a8c-6f18d4ea8041)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3495e621-8457-49e1-b1bb-223e3b0233e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         1aeb6e35-0845-43fb-adf6-c3b5bdfc6bfb)(content(Whitespace\"\\n\"))))(Tile((id \
         58b1f1fa-3d68-42c8-bda9-d5ac7378d3d0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8b6253d5-df26-4d7c-8e21-7e5ff6f4fd9b)(content(Whitespace\"\\n\"))))(Tile((id \
         ead46d49-6031-40fc-88b8-7e6e726ec15c)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b8626bb-defb-46cb-92cd-546103e040d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab8dcd93-48df-4e83-90ae-00c3faf3d65d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90b8526a-3d0e-4bb1-aa58-269e8d9db76d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19f91968-7efb-4185-b56b-cc0a15fe6f6c)(content(Whitespace\" \
         \"))))(Tile((id \
         39b8cb2a-9027-491f-a3fa-c9296124954e)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5134989d-16b9-4e2b-841f-98080c4a7550)(content(Whitespace\"\\n\"))))(Tile((id \
         1456a545-82a1-43d0-ae76-eb2a3df85794)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c24d69e-d1fa-4dae-b061-baf14b5fa87b)(content(Whitespace\" \
         \"))))(Tile((id \
         4fd45f27-afa1-43a3-8e3d-c751f1a56331)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f5cf982-88a5-4d61-9a6a-b0564294927a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5af5ccad-ef1c-4e80-89b6-08bca740ad51)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d45c3d8b-d05e-45d3-a1b2-77f61740556f)(content(Whitespace\"\\n\"))))(Secondary((id \
         20cf0863-63a9-4792-b039-59bbc19b8113)(content(Whitespace\"\\n\"))))(Tile((id \
         9f9056df-1a26-40d1-b523-4e0600d5ed4d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f7088ab8-d344-44b2-a743-1e96b37dfd42)(content(Whitespace\"\\n\"))))(Tile((id \
         13663567-6b9e-49b1-8bc0-2a52b1ba0938)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ece58e9-f055-42a8-bccb-60df54421616)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c1dd2d54-c39f-4f7f-b477-ac5ee3e96103)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47b076f0-74b8-4a73-b64a-84a54c725c75)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fac132db-427d-47dd-b2ae-0e222b5955c4)(content(Whitespace\" \
         \"))))(Tile((id \
         774f3636-3293-409c-bab3-12b9216dd8ec)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         99a629a1-71b6-4f19-917e-b3e77590b614)(content(Whitespace\"\\n\"))))(Tile((id \
         b93f5107-fb2e-4399-890f-e693892733e0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b31151e6-b6f5-4f95-943e-8e98ac3bba65)(content(Whitespace\" \
         \"))))(Tile((id \
         fa8627b8-de2a-4a41-ba55-0f8dc06baff4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f556306-7284-4fd2-a717-99e9c3db9c48)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0ce5a356-28ac-465a-ae26-eb5f408686e0)(content(Whitespace\"\\n\")))))";
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
