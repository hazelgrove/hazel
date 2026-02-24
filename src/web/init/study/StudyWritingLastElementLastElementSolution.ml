let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-solution",
    {
      segment =
        "((Secondary((id \
         28bc3fa4-7f04-4983-b162-cc8715aa55a8)(content(Comment\"# LAST ELEMENT \
         - SOLUTION #\"))))(Secondary((id \
         4186e3c1-2b47-4fce-9a49-3d26f030ac27)(content(Whitespace\"\\n\"))))(Secondary((id \
         0590cad1-a68c-417b-a8e1-b68a919c405e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b17619e-fa5e-4654-a34f-95b587a4f196)(content(Comment\"# Each step of \
         the fold replaces the accumulator   #\"))))(Secondary((id \
         24c7804d-18e8-4f78-ade7-ee75875f5f09)(content(Whitespace\"\\n\"))))(Secondary((id \
         187750bf-2f6c-41ab-abe6-42f7099950ad)(content(Comment\"# with the \
         current element. The final result is    #\"))))(Secondary((id \
         ee969c4a-c714-4591-bac9-7ba7119e87c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         00a2f44a-832a-4d01-bc6e-a327319e97e5)(content(Comment\"# the last \
         element seen. For empty list, returns   #\"))))(Secondary((id \
         f8c05033-10b8-4c39-aef4-c8250e3aeef9)(content(Whitespace\"\\n\"))))(Secondary((id \
         804b6010-f4b9-4926-9186-f2ede402ab15)(content(Comment\"# the initial \
         value (default).                     #\"))))(Secondary((id \
         97e6dad8-89bd-44ca-80e9-927ec8af2f34)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3d5a622-1183-4822-9752-48d8b22f026e)(content(Whitespace\"\\n\"))))(Tile((id \
         63e33c58-bf9a-495e-898f-324143eee560)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         126b036c-0586-4e8e-9681-d0893565652a)(content(Whitespace\" \
         \"))))(Tile((id \
         af0c75fa-135d-48af-a802-1a572c91fbaa)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9118a295-1d22-40ef-af89-46f21cad6b1f)(content(Whitespace\" \
         \")))))((Secondary((id \
         a410b71d-1499-40c9-a070-197ed3a1df88)(content(Whitespace\" \
         \"))))(Tile((id c6da9842-553a-4b9f-8b7d-ebf9f7b4a04a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         98bffd0a-5e7f-437e-8574-9f908c346c66)(content(Whitespace\" \
         \"))))(Tile((id \
         7ee9a71b-8aae-49f1-973b-9ef3e2233cb1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         09f849e3-07d2-4e2c-a27a-f552f938e8ca)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d4e8f7e7-0c48-42cb-b054-4ed8d3d6ccab)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0d3b4191-a4bf-4607-af9a-91ab244220f6)(content(Whitespace\" \
         \"))))(Tile((id \
         38bec9b5-aa88-4175-9147-d88ec6f927d0)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         00d0eba9-bb64-4772-ad79-06cf760882c3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8e3c769e-10af-482b-aba1-90c69aa91b50)(content(Whitespace\"\\n\"))))(Tile((id \
         301761ed-61a1-4a5a-b824-5ec885d61f2b)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cef9a710-421e-4c61-91f1-014246637332)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce8ecf99-6495-4342-a801-d0eb0dc547d5)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a6138ed-1b91-4731-b318-62f671cc302d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         608d70e4-f29b-4024-9df6-5408d3ccc6f2)(content(Whitespace\" \
         \"))))(Tile((id fa65b85f-b3ea-4fa4-8a07-f67d26c2e9f0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         eda64b76-1063-4e51-b696-d272978ce1e8)(content(Whitespace\" \
         \"))))(Tile((id \
         4253b50c-1801-4bde-b620-c18e32cf2e28)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c6b2263e-72b0-4f04-89b6-6da709a7e7c2)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b4dad7ac-cce1-49df-9b7c-4fd9130c6425)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b70c33b6-3cd2-4f0c-9c09-2bf5e1b90b13)(content(Whitespace\" \
         \"))))(Tile((id \
         78d854c2-3735-4e6b-a84d-513ab5c58b66)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5c9919f5-3768-42c9-b453-deed44cb7d6a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15584969-2dc7-44ea-856f-c08a094f97f7)(content(Whitespace\" \
         \"))))(Tile((id \
         8c9474e5-9009-4fb0-8e79-a70130837abf)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c6638fb-c00a-49f4-950b-17805d047c0a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7caa6984-3e3f-45ea-a93b-b81f8c84890d)(content(Whitespace\" \
         \"))))(Tile((id \
         22cccbbf-ea1b-4a53-940f-81f11d94f2d0)(label(default))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a3498d7-2c66-4cb5-8cec-768e6fa91ef4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7f1487b7-8af5-43cb-b956-a5b4d3bbd0a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         0672afce-8d4b-418f-acde-608d5b30e19b)(content(Whitespace\"\\n\"))))(Tile((id \
         d2075a8e-0a53-48d2-9519-fcaef1c48ced)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6bad4340-c5b5-46d5-839f-c5803ca8374b)(content(Whitespace\"\\n\"))))(Tile((id \
         9617a2f0-352a-4c09-892e-e1339725bde5)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ff03174-68ba-41ca-8569-8362ad960916)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7ce0be8e-8699-4b54-b18c-19c06d9fd256)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d269ab92-7f07-48d9-b6d0-d0f5a804dd6b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ec77283-49a2-4bf4-a8d8-779bb0eaad47)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5dc2b74-f081-470f-b0b7-0e6381b06771)(content(Whitespace\" \
         \"))))(Tile((id \
         722731c1-aad7-4192-aa79-c146f3d4cb92)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6636b8ba-ab9a-422f-b2c4-827950f34a1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aeca70c5-3be8-4e40-acd1-60fa0618ebb8)(content(Whitespace\" \
         \"))))(Tile((id \
         68d8c869-6bc2-496f-b1f1-f07fbe91055e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         80ca7a51-d894-4211-bcf9-8af6cdb9d3bc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b4f34f7-f19d-4e86-9d52-f82e32081c28)(content(Whitespace\" \
         \"))))(Tile((id \
         3deda590-12e7-4200-9986-d327ef47c646)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c4419939-b2f0-467b-9db6-e96fd4cbab87)(content(Whitespace\"\\n\"))))(Tile((id \
         ce1821e7-30ee-438b-899c-9cd65a8a4eb6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df902f12-cafe-41b4-907a-afc8e3d8556d)(content(Whitespace\" \
         \"))))(Tile((id \
         3a6c23fb-eec6-478b-8b09-4ed45765a1ed)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8d39128-a6db-46d1-b94f-10d5f80c808a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e730a119-e33f-4bfd-bc27-10927aee6fd9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93f2bc9a-6069-425d-b355-36b10271133c)(content(Whitespace\"\\n\"))))(Secondary((id \
         35dbc6ef-1f45-410f-a67d-b37b3e2647cf)(content(Whitespace\"\\n\"))))(Tile((id \
         e6936c24-0b35-443d-9e29-cdcc0057e714)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         243c3f21-68a1-4175-b87a-58cc6839a8b4)(content(Whitespace\"\\n\"))))(Tile((id \
         f7c544a2-c6af-4e04-8720-3fb1f5c3bf84)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6c79d89-635b-46cf-9fdb-0d4c6df8b2d0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4645728b-2d87-4fcc-99c9-21090b8f2108)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         62af412c-edaa-45db-8a15-0006cdaeb2c2)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e58dd81-8928-48eb-88eb-7c293868e0a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57659bfb-231b-4728-88e0-82b066566f64)(content(Whitespace\" \
         \"))))(Tile((id \
         a4864751-bbbd-461a-b6a4-bc9f16a75276)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         54fd91a4-aaea-4af5-83c5-a035b42ab9d7)(content(Whitespace\"\\n\"))))(Tile((id \
         08711f95-67c0-4f3d-b56a-cb183d2e978f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33c96c83-695b-4dbd-a5a9-88f5e01cf7a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b2f09f34-c027-4c06-968d-fb7a90aac8d9)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36c6f740-ae01-410a-8c70-c4416e5d667b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         94574d46-cf64-44cb-a26d-361763a26501)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dab2172-216a-4ccb-8ac2-356bbb33eaa3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffdc7271-71d4-4d8e-b561-7e9d5148790f)(content(Whitespace\"\\n\"))))(Tile((id \
         ccc3d63a-c149-4f3e-8736-c7773677f5cf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9b8b6731-70a4-46d2-998e-cef411678933)(content(Whitespace\"\\n\"))))(Tile((id \
         0f9704d2-d96f-45bb-be60-183af1ae7464)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cf6e1b3-4961-4799-9f6f-da07d8de0f3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         83b55ed7-6067-47e1-a2f9-87584e038560)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c6501861-93b5-4a60-9e5d-e704dd3ece35)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a05abce1-6126-4901-b55e-3fd35f2ba6bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ba9af00-2bbd-48f1-a826-e38385be99ea)(content(Whitespace\" \
         \"))))(Tile((id \
         8da66ec8-3019-4732-8b60-2009706d6f2c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cecf1750-4a48-4442-9f82-0dc460f711c8)(content(Whitespace\"\\n\"))))(Tile((id \
         aeea9abf-a423-4bfb-a71e-1a1ea0a8b84d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7dbaa5c1-abec-49b5-94a4-a66b3b38787f)(content(Whitespace\" \
         \"))))(Tile((id \
         15918d4f-4b17-472f-afef-2e97c9e3afe9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         440977de-0ee8-492e-bc8e-c3db28601557)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e721dcaa-d1f7-4f96-bf48-e83dfaeeec69)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f96eb527-0e45-4b30-a847-8a470a483859)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9369753-1069-4fc8-9935-69a81a21c7b4)(content(Whitespace\"\\n\"))))(Tile((id \
         ddf0ce72-cfc5-4f5e-acce-76dc4a68e761)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d9a5b11b-b104-4783-9739-04f6449dddeb)(content(Whitespace\"\\n\"))))(Tile((id \
         d2aeb978-5e52-4610-8ed3-ba1aa6a4a8c2)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef594f53-75b4-46e7-9e91-2c6c84b2d729)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb5204ec-974a-4b37-b051-493085557870)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6daf93c9-2d9e-460c-add3-de354610e593)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cd88d0a-1aed-42ed-b5c1-8c244c06736d)(content(Whitespace\" \
         \"))))(Tile((id \
         ddfa369e-d995-4d3f-9898-c51c0b2aa8a9)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e4cee106-aa69-44a0-9c7e-f490d31fd411)(content(Whitespace\"\\n\"))))(Tile((id \
         3340b0ad-a941-435c-ba65-7a9d59ffb402)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c680ad6d-afb2-4db0-8962-7f7f0d7cfd3b)(content(Whitespace\" \
         \"))))(Tile((id \
         60bb1941-8dd6-4f18-af53-bdb967f2cb0c)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9b19d71-1503-4e85-9327-099ef5e10441)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f3bcb5db-21f7-422a-9d3f-c67143c9c047)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bc974ea-b957-4271-9642-45ca72a1e452)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad493802-e217-4588-94a8-3711bbe31a98)(content(Whitespace\"\\n\"))))(Tile((id \
         835ad95b-d04f-435c-b45b-b9e94382962f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         610136ea-173e-4530-b38f-4bfccf00ca5b)(content(Whitespace\"\\n\"))))(Tile((id \
         1dc7fd64-9e3e-4501-9195-57e55435a92d)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37303915-a30d-4463-a6c9-4f866155a756)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dae5db91-21c8-40bc-9970-91ef2b734bb5)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d45fd99d-a7c1-416d-9c12-c100a4404853)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f245f12-bb6d-4192-9448-3ba9e5c390d3)(content(Whitespace\" \
         \"))))(Tile((id \
         69145f36-8998-4667-9d2f-d373a9cb994b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3e82f605-37b0-4c78-b770-dc4ee6bb937c)(content(Whitespace\"\\n\"))))(Tile((id \
         d7907635-1a81-492f-981e-760caf590649)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5ef999a-ff8b-46ac-99cc-6f76e0e6c49a)(content(Whitespace\" \
         \"))))(Tile((id \
         ebbfcc6d-6827-4658-b667-b5b0cfbeca6e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6abb7359-fc92-4aac-b560-7d9c70ffadce)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         db53a1d7-3fdb-4ebc-b558-a75ee67571c6)(content(Whitespace\"\\n\")))))";
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
