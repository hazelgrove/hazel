open Haz3lcore;

print_endline("Building common context");

let pz: PersistentZipper.t = {
  zipper: "((selection((focus Left)(content())(mode \
       Normal)))(backpack())(relatives((siblings(((Secondary((id \
       b09e9f67-5042-4290-8c20-51b764808c02)(content(Comment\"# Basics \
       #\"))))(Secondary((id \
       c39840b0-976c-47dd-8c67-4e2a8de455e0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       535f88f4-3758-4d91-b061-38de808dec9c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       407c9d46-1fc1-4e42-8f2b-89ff0a59233c)(label(type = in))(mold((out \
       Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       c16c175c-4f8c-4f8b-899c-fab671c52570)(content(Whitespace\" \
       \"))))(Tile((id \
       aa94c657-a329-4918-a6db-6dc3fa020af6)(label(Option))(mold((out \
       TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
       TPat))))))(shards(0))(children())))(Secondary((id \
       4f5b1fca-ab58-4a21-a3e3-108292ff5e56)(content(Whitespace\" \
       \")))))((Secondary((id \
       dc725a35-ff7f-4b9d-b8a5-6500c3a0608e)(content(Whitespace\" \
       \"))))(Tile((id \
       f759f32c-344f-4935-8c89-7e71b025c273)(label(None))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       8b1665e8-7722-4776-81c0-2b420a88ca22)(content(Whitespace\" \
       \"))))(Tile((id \
       e02fb964-fd57-4bdf-99f5-3718ad128aae)(label(+))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
       10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       8affce32-17cd-4b27-8275-82062069729d)(content(Whitespace\" \
       \"))))(Tile((id \
       3d95a849-efe1-4427-94c5-70e611a1a142)(label(Some))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       7410619b-a6db-47bc-b5c9-9f63d8cb4db3)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       98b14f8c-c66e-42ec-91a3-9aa264ffb681)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       746dba30-0737-4261-9057-eef001782a0f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       06232d3c-3efa-4e0d-81f3-71c519fa3186)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       d1f6ab02-fc63-4a04-bcd6-6319f19d0c97)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e055ecff-370d-4b2e-b0c8-2b8609f6ea4c)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       7a6083ea-9a00-456b-9a4b-6ef7fefb16db)(content(Whitespace\" \
       \"))))(Tile((id \
       dbd132a1-b2b3-4059-a69b-b8330266e139)(label(fst))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6517c8ea-6ddd-4ca5-a6c1-23d1127fc00a)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a6f5f3a7-8bbd-4cdd-af6a-a41cc61750c0)(content(Whitespace\" \
       \"))))(Tile((id \
       e5c9afa5-1b3f-4288-b393-1a9a7734506c)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       621dae6d-a835-4992-917f-a0eb314f9c57)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       d3fd1d74-0404-47df-8bb5-1505f8696ba6)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       7cc040ac-f507-450b-8306-f864171cbe9c)(content(Whitespace\" \
       \"))))(Tile((id \
       688b2103-056b-4c91-9059-32f75f4c11db)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       61f72ff1-75cb-4732-a61d-42d6a7fac945)(content(Whitespace\" \
       \"))))(Tile((id \
       b3b69f4e-5675-4c28-a851-86114bc87666)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7048595c-0fff-4a4d-8529-3f8e6ca19703)(content(Whitespace\" \
       \"))))(Tile((id \
       3ca32192-619f-43af-adcb-eaf327eebb46)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a47d0b58-0fe1-4d78-8aa4-53296d08eaa4)(content(Whitespace\" \
       \")))))((Secondary((id \
       9ca32a96-437c-4eed-b71d-de1ccfd1493b)(content(Whitespace\" \
       \"))))(Tile((id d00fcc1a-99d0-477f-a2ab-54171bacb665)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       3920d64f-9602-46d7-bf86-b04108745444)(content(Whitespace\" \
       \"))))(Tile((id \
       22f691db-6141-4709-9148-5c23d5d95dfa)(label(a))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b74f211e-f6ef-435b-a8ce-3238333f398a)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       b9d6205c-61fc-4f31-bdcd-e1fa4148aaba)(content(Whitespace\" \
       \"))))(Tile((id \
       3067064e-2fb8-4721-a19f-b80ad53969bf)(label(b))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       27273a9c-0b79-4484-8738-a0f5235d0c56)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f1a98876-423c-41f1-a6a9-e8ff84a24153)(content(Whitespace\" \
       \"))))(Tile((id \
       260fc413-2e20-4292-84f3-52bcf46b93ff)(label(a))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       542e19a6-4bd3-4271-9df5-87f89e3150c2)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4cbf775b-c382-40bf-ac3f-19fe02083257)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6677d945-e4f6-48e2-8a6f-5b9ef5c1e4ad)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       d2850597-adc3-4fc4-bd73-f54717eca7b6)(content(Whitespace\" \
       \"))))(Tile((id \
       1b471632-9b9e-411f-961d-1637a057b884)(label(snd))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       21fc078f-921a-4cd5-b125-b1cbc08fef61)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       07198713-47b8-4900-a12b-b7c2ec6b95c9)(content(Whitespace\" \
       \"))))(Tile((id \
       ff6f3d4e-1fc2-47d9-91b6-9082a40be12e)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       42120b49-6335-4327-937c-ed2217aed8e8)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       cbe9cc88-24ee-40d4-ad2c-b7af1d1cfdc5)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b33fb89a-4e6b-4b28-a3aa-620c7eed981f)(content(Whitespace\" \
       \"))))(Tile((id \
       ed8c1886-8e7d-4280-845b-0d9c8e95bf69)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       d77dd05a-06ed-423c-ab45-5ab8fb8d6882)(content(Whitespace\" \
       \"))))(Tile((id \
       8519b597-e7cc-43db-a638-412e4463294d)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       70d2fa6f-9e88-4169-845a-41c99e5d5dd2)(content(Whitespace\" \
       \"))))(Tile((id \
       6c6c7939-2ac1-4509-88b4-609b92bd7786)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       fff4db7a-26f4-4d7e-95d7-f1aa44a1c5d8)(content(Whitespace\" \
       \")))))((Secondary((id \
       0712fcfe-a09e-4cd6-950f-cde06cc2cd10)(content(Whitespace\" \
       \"))))(Tile((id bffae465-b083-44c9-8829-83d992c96656)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       765f7e23-1a15-45ee-90dc-b408775f001d)(content(Whitespace\" \
       \"))))(Tile((id \
       c6f8cc76-1047-4423-b5ef-da8dcf6ed1aa)(label(a))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a534663d-0fac-40a8-ad22-50973e6d8f43)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       8568e24a-6b74-48a9-a20a-86d3ff275ba4)(content(Whitespace\" \
       \"))))(Tile((id \
       924e9ca8-e712-4c2c-ae06-cee0b7fe5779)(label(b))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       be11d9e3-6cf1-4e26-9e80-c516d6dc0885)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       a01dbad4-5d60-4369-9ed5-89bc693245c6)(content(Whitespace\" \
       \"))))(Tile((id \
       82bbbce0-d65d-4baf-8a4a-27423410b779)(label(b))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       787d5362-87ea-49c6-8226-4270365b8dfa)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9dd8892b-1954-4a9b-9ad3-279dceb8f912)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       cbc18990-dbe2-416c-9f79-63a6695b3bdb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1d390ce7-877d-457f-8d6b-b15c63afe203)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       3fa40e60-f981-4051-bf1a-11e2becbe3aa)(content(Whitespace\" \
       \"))))(Tile((id \
       11128974-c8aa-4b05-b532-35fda3617b53)(label(not))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b9560fb3-6858-4b99-9f7f-0abc4c133974)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       0711d238-106f-4b51-a3cf-0bca6fd12caf)(content(Whitespace\" \
       \"))))(Tile((id \
       a761c8b4-c6e3-4bee-b26b-023a1083cc1e)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       beac94cb-5eca-480d-a2f4-3e97ad0c278d)(content(Whitespace\" \
       \"))))(Tile((id \
       d9d5f8cb-d77d-45bf-809e-f88ba8204af6)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       33cebf8e-2604-4ea1-8b78-17f62ce088d3)(content(Whitespace\" \
       \"))))(Tile((id \
       d92bebb1-c2ce-4173-8bd4-7693caf4328d)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       9eb6c7de-cbaf-4e3e-9ee6-4126d222abb7)(content(Whitespace\" \
       \")))))((Secondary((id \
       af72fada-b233-4f33-9e3a-0b7b935b4c7c)(content(Whitespace\" \
       \"))))(Tile((id 8b705fe3-dacc-4eff-94dc-33432b0a850e)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       7732a3fb-9583-4042-a771-c15b61913b8d)(content(Whitespace\" \
       \"))))(Tile((id \
       7101e17a-9c8a-4156-a2e4-c665b9085c66)(label(b))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       0faca024-557b-4396-b278-0b82ad0c60ab)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       7aaeaa47-55f8-4390-9a84-2b6afac70a16)(content(Whitespace\" \
       \"))))(Tile((id \
       73a50581-d192-4e70-90f3-ef00f0090f70)(label(!))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       049fbef4-b68e-4668-a4af-50c16fb57140)(label(b))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       fa5f9ce8-1d18-4d2b-85ec-19c6975153dd)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       cb2e902c-203f-4a98-bbf3-a6a07407b39d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       9a1829d3-bf22-43b6-b6a0-eb078a460d08)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       5ba19186-023c-46b1-b2ea-25d2f29f24a5)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       06ab300a-ba21-4637-9568-9ecc23c35316)(content(Whitespace\" \
       \"))))(Tile((id \
       3af1a10c-e1ca-4afd-a91a-892950f7d978)(label(bool_eq))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       06e3c425-a380-4833-a591-e74d0de68386)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f203eb64-0d94-49c6-9659-fdda41815694)(content(Whitespace\" \
       \"))))(Tile((id \
       57780fac-5409-4d85-888e-43224ce276d2)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       c4c9748d-1851-441b-97f3-fb6b49956fb8)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       38582cdc-2177-464c-8e69-e8a1af68c560)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       d694bb93-ed5f-4466-8cec-196c14ff45f0)(content(Whitespace\" \
       \"))))(Tile((id \
       6c48a5f7-e46a-46d5-8ad8-488eda2d4a91)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       82f3b89b-0e0f-4db3-a5f3-af91b53f0f35)(content(Whitespace\" \
       \"))))(Tile((id \
       6a44e99a-e800-4d75-9ed2-ccdf5ea1e194)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       aeb7c0ee-e913-40ff-9641-ec6fcd6bcc0b)(content(Whitespace\" \
       \"))))(Tile((id \
       4d31c1f6-00c3-43a1-a78b-c1a8f103361f)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       faae4939-126b-4736-8187-ec083a18cb7f)(content(Whitespace\" \
       \")))))((Secondary((id \
       0804f2b2-71f0-40d3-ae77-b2ee2e074379)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       4b338d4f-50fb-477c-b7fc-ed1e304d4abc)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       7879d377-0b53-440f-829c-7366193f4cf9)(content(Whitespace\" \
       \"))))(Tile((id \
       5ad3decf-11ae-4755-b91b-99a47e8be667)(label(a))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       abc37ff1-240c-4eb8-a63b-410349e7d237)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       bac39a8b-6755-492b-8cad-72b2cbd2a036)(content(Whitespace\" \
       \"))))(Tile((id \
       d6834487-dde2-4fd3-8d57-7c89c65a1db7)(label(b))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       874b2c21-388b-4bc8-9a81-e13c3c6078c1)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3091437c-3f2b-4e15-8ecb-9e09267a769e)(content(Whitespace\" \
       \"))))(Tile((id \
       66aa6f0e-069e-47a2-a7fd-b17af6c9c884)(label(a))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       b830b65f-de5a-4090-b6ce-f572ab307d73)(content(Whitespace\" \
       \"))))(Tile((id \
       efe956ea-e0a8-449a-ab66-04d36478891d)(label(&&))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 9))(sort Exp))((shape(Concave 9))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d1fee3b9-65d9-4f28-949c-84f1131977f4)(content(Whitespace\" \
       \"))))(Tile((id \
       5dc19ed2-a213-4de1-8b5c-4b9ed22d0e93)(label(b))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       af74cec7-8d69-4ede-b861-ef64921bd1ad)(content(Whitespace\" \
       \"))))(Tile((id \
       8165ed2d-c2d5-48bf-9316-f9d316d4db42)(label(\"\\\\/\"))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 10))(sort Exp))((shape(Concave \
       10))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f77ed009-67a8-4c54-9346-7c9624f596d6)(content(Whitespace\" \
       \"))))(Tile((id \
       bdb1577e-fbb5-4f92-9aca-25bb6dc8dc91)(label(!))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       af9a7563-5cfb-4458-9418-fc108ad88647)(label(a))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ec44addf-4c71-4add-bab9-95fb41041864)(content(Whitespace\" \
       \"))))(Tile((id \
       d5175ad5-f97d-4804-9e24-62922ea9f7b5)(label(&&))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 9))(sort Exp))((shape(Concave 9))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       e0a4b33d-4f87-4ab7-9270-d73aefc91653)(content(Whitespace\" \
       \"))))(Tile((id \
       26efaf5e-103e-4203-9d9f-f3117507b2ce)(label(!))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       7a5e40bb-0d53-4173-a105-1c2a4adfdcff)(label(b))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       172638b8-2a4c-44a6-aa5d-4102fad4ce14)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       c8823c84-2354-4376-8da9-6ae8ccd2b8f1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       092cd92a-8a10-4ec5-b889-3168ba00a9f3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       e75e363f-e9e7-4b97-9e66-2b5a71b255e7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       b5b70b79-f591-4eb8-b435-eabb0f203810)(content(Comment\"# Lists \
       #\"))))(Secondary((id \
       284312d8-736b-43fb-bb03-03c037dbeac4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       532d99bc-fd51-4c5d-b309-937a04290747)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       551c37cc-3d5c-4a5c-be32-535f879365d6)(content(Comment\"# Add an element \
       to the front of a list. #\"))))(Secondary((id \
       87380a8f-8036-4a43-8b41-a333d0d0fe8d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       23779cd2-c765-4a55-95ed-29cbed8396db)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       fdf983ca-b4fa-4f4d-86a3-998d25887be7)(content(Whitespace\" \
       \"))))(Tile((id \
       0f122195-ad41-42be-bba8-106604fa4996)(label(List.cons))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6e233908-3728-4fe5-b217-a1be9f81ac0c)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       5687d6e2-2621-405f-b14b-cee6f2b4bc00)(content(Whitespace\" \
       \"))))(Tile((id \
       da8e2be2-4b02-4f86-aab5-5af8262ce83f)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       c63fb390-e2a6-4148-bf1a-83777e2d5c22)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       c99fabb2-c981-4a54-b833-879984c4dab4)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       98913cee-a75f-4904-a384-4a9b0dbd2f4f)(content(Whitespace\" \
       \"))))(Tile((id 36fcf146-6ab9-4ca7-b69c-acd21f88b910)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       3d9d9d72-faaf-48fb-8581-d7c17c2ac73f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       dbc87171-1f53-42c6-b608-cf698e2050cc)(content(Whitespace\" \
       \"))))(Tile((id \
       c715cab1-791f-41b2-97e6-9b98d73609c6)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       2b90de03-4469-46f5-b4bd-cecaff574e22)(content(Whitespace\" \
       \"))))(Tile((id cceb9437-f7c4-4628-9338-ca61a2a115a4)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       6fb73036-8ac2-4638-8313-e5741fe88a62)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       580e9394-bbe7-468e-aec0-5a341853a861)(content(Whitespace\" \
       \")))))((Secondary((id \
       fc90b83c-0981-43b5-b6d1-c07b9073647a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f71b3115-dc09-4612-945b-cfc3da6b9ead)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       569d5f15-0930-4a19-b2db-5dd55f5d6380)(content(Whitespace\" \
       \"))))(Tile((id \
       ef283fc4-784e-46f0-a8e2-9b15437cadac)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c1e76e95-5525-4c6f-89d8-88b95e0814f9)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       bc0e5828-f784-4c30-90d5-839cb71e9c81)(content(Whitespace\" \
       \"))))(Tile((id \
       8a7463dd-4d62-44f2-b32e-8d0a1e230f58)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       087eee99-8da9-4a70-a56c-ad8571720332)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       cb523370-11e8-485a-ad41-5274a4d757ce)(content(Whitespace\" \
       \"))))(Tile((id \
       1972cf69-ccf3-4bf3-864f-dfc83249faca)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d2ad755b-32a4-47f5-8d91-723ef163c4cc)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b9befbac-b3c2-478d-81ba-268bc4c68bb5)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       de8aaa4e-bd04-43ee-97fa-60c01c88beae)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       85ab08f5-c5b0-44cd-bfc8-cd96de236d1c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       9e8904bf-020b-4aac-8db7-5d657d2e1df3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       7b2c5d3c-ae11-41a8-84a8-afaff7b49f52)(content(Comment\"# Determine the \
       length of a list. #\"))))(Secondary((id \
       180215b0-8448-45d0-a270-98661863ccaf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ee2c85cb-0bb5-48cf-9616-a526cf184d8b)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       aab3a344-aa94-4165-ba8b-454f99c1cbf7)(content(Whitespace\" \
       \"))))(Tile((id \
       7ae3971f-d160-4484-88f6-360f64f0afe1)(label(List.length))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       71b8969a-3762-46cc-8a89-5abbba242ecb)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       47775dc6-58d1-4256-8158-a72aef5b1607)(content(Whitespace\" \
       \"))))(Tile((id 6b06bd06-6d32-4fd8-aea7-17807d33392c)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       e78f7bc3-10c1-4b01-9b87-6b78f59e1a17)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       0393691f-72bd-4837-887e-b1a9eaf632c1)(content(Whitespace\" \
       \"))))(Tile((id \
       64ac79ce-9d9a-4e74-a818-c64961785d26)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       ca6bb245-98a8-494a-bfbb-a2e1d94f5c80)(content(Whitespace\" \
       \"))))(Tile((id \
       3dc62f27-f32a-4571-979b-642dd5e9af35)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       650da512-c70e-49f7-946e-19c7a4936910)(content(Whitespace\" \
       \")))))((Secondary((id \
       0c01fff3-617d-4caf-9e46-ac98a3b8531e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f669eba5-42e8-43be-808b-470ae3cbf8a0)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       0c369a3f-c3c1-4b8a-8182-a449c9789523)(content(Whitespace\" \
       \"))))(Tile((id \
       a0cf986f-0194-4531-bc37-8d0ac403e779)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       c1c55702-6cad-4aa9-9f52-c9470d80c0cb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       369d55fa-feea-4604-a050-582566c48aba)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7fe72ce6-6fde-42c2-9fe5-20ffd8faffa6)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       85d6b922-2d7e-4f35-b70c-127ddcd7447c)(content(Whitespace\" \
       \"))))(Tile((id \
       f99622b7-13dc-45ea-9b6f-a5ab8d828efd)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ea4cd92f-de57-48f1-b5bc-7ec22bd9590b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6a8599b8-5623-4a09-8857-d9decf3aa74c)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       9b7767f8-bf18-41fc-a62b-f4bed62b6967)(content(Whitespace\" \
       \"))))(Tile((id \
       88fa9554-0319-4321-b3fe-435377dfda53)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       89bb5c27-3530-49eb-9834-83c8dd3ce993)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0def483b-b445-4b53-9c81-af4afacd8206)(content(Whitespace\" \
       \"))))(Tile((id \
       7a473532-3b57-42c0-af46-6c4d22246248)(label(0))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       db41b971-8290-4b33-a2cd-424d69b8da71)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1bd0d54f-f478-4ec1-ad25-8ac7b7b4529c)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       da85e3d5-60df-4de2-a14e-c357a819f68c)(content(Whitespace\" \
       \"))))(Tile((id \
       a97d3976-afbe-41c4-936c-ff6fd83eb03b)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b938cab4-4110-495e-97dd-ac4ade04d5a7)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ef6bd0a1-5112-4d06-9362-16aec66edbf3)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       16e66787-c6fb-4405-87cc-ce2525b24e32)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       d311519b-3dae-4578-80d4-137f358343af)(content(Whitespace\" \
       \"))))(Tile((id \
       8b909261-7bab-453d-a684-bb6fd4fba512)(label(1))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       e2dcf68b-f70f-4803-b506-7cac9c3f2e14)(content(Whitespace\" \
       \"))))(Tile((id \
       80e8080f-0111-470b-8c6e-9d3563aa7eb6)(label(+))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       196b0d8b-7015-4447-906e-56b6fe0a97d6)(content(Whitespace\" \
       \"))))(Tile((id \
       5d98118b-b284-409f-b768-10d619f1ccf5)(label(List.length))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6a818923-0c9f-4d8c-a577-c0ebcd5c6e61)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       6f8c36f4-125b-4a66-8741-6c86df89eabc)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       b4cbede6-2479-48c7-bb21-bf19460fa0eb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       947232c2-54d1-474c-8b0c-82b03b30cabb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       fd63a33b-4822-4c2d-b7df-375b13a647dc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       1e258a1f-3c1f-45af-916c-2a4eeaacabcc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       eb687e24-069f-4022-84ba-e95a7603ea12)(content(Comment\"# Extract the \
       head of the list. #\"))))(Secondary((id \
       1951301a-c8f6-47c6-abe0-312447a6637f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       aa19a489-6dce-4548-a40e-d1e89a927e26)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       46f9c982-14b5-461e-9dc7-74f5fece1e85)(content(Whitespace\" \
       \"))))(Tile((id \
       397d3936-9aec-410f-92fc-b4c5b3dc15dd)(label(List.hd))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       edaf571b-35c2-4e37-9739-5b934270b2a3)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       030acbb4-6dfa-4125-8331-cd3d45fa8c99)(content(Whitespace\" \
       \"))))(Tile((id 69b0bde9-c3ed-4cae-9440-41a6575bb5c0)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       05501d5d-5331-45f8-88bc-5ad019aa7eba)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       36962487-686c-47d3-b986-1ef9f848dadf)(content(Whitespace\" \
       \"))))(Tile((id \
       237f1f6f-0419-467c-9e93-3a78e9536061)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       5dbbd22a-9dd2-4d06-b58e-2426c8f81386)(content(Whitespace\" \
       \"))))(Tile((id \
       418aace4-0a5a-4aa1-a53c-7afd15e3a1af)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       49459269-8de0-451c-9b0a-3ba7f3a92d3e)(content(Whitespace\" \
       \")))))((Secondary((id \
       8f3bf88e-6b2d-4219-81f1-605abbc6c044)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       00001a38-1bd4-49a1-9c99-e74708d0292c)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       190d4b97-c9a6-4ce7-bf52-330bc13a1ab3)(content(Whitespace\" \
       \"))))(Tile((id \
       3d8f23bc-6b0e-46c1-8ff6-18324683d2d5)(label(l))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d957e90b-9623-42a8-a0a0-0d2ea5180995)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f250e9f5-0f1d-4c7d-935b-4b33699d49d4)(content(Whitespace\" \
       \"))))(Secondary((id \
       e865cb6a-5bb8-49bb-a5b5-9e99cd34e469)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7e7a52a6-efdf-4923-86eb-c49fa8390ee6)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       40057a60-9800-4132-8155-7f478e506150)(content(Whitespace\" \
       \"))))(Tile((id \
       2f0ed471-9321-4c36-8a2a-b3bc00389832)(label(l))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       715d8b71-9c54-4f90-baad-94ee05701301)(content(Whitespace\" \
       \"))))(Secondary((id \
       6f9a809e-4e3a-400c-bf79-5f33f356fa00)(content(Whitespace\" \
       \"))))(Secondary((id \
       587278f9-339f-4766-acf3-33ba7f5b0f00)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6c2c8157-d367-49d3-9e18-f20ecd196819)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       80870f0d-1ee9-4364-a840-99af495c6741)(content(Whitespace\" \
       \"))))(Tile((id \
       2cb50c50-1bb5-4afa-abe5-41f5b40944ae)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d9074fa5-2421-409b-bc82-343fe46d18d7)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       11469acb-b93e-4ac1-96a1-ba0687f80b97)(content(Whitespace\" \
       \"))))(Tile((id \
       83097570-ef64-4796-a7df-4e1fc673a1a9)(label(?))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       23b7de1b-301e-4155-98d5-0fdec0e15c04)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       910aef51-55d7-4297-9300-a0ca738a357a)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       7b6a1f37-a41e-4e93-a74d-f9750f394e90)(content(Whitespace\" \
       \"))))(Tile((id \
       1c95c7d3-f689-4d42-a6d8-3efbdb45d4cb)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       fdf7e68d-4258-4579-b377-8aa491c8b3f6)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       cccd56a9-b3f2-4b9c-ab3d-dc2e9b73507c)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7b88c443-64fb-476d-a13e-9aa46ac5615a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       b93f2aa0-8b89-484e-82c2-8c5ecf6a60f3)(content(Whitespace\" \
       \"))))(Tile((id \
       df2ee3de-e1cd-4417-9878-ceef14a810ac)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ba7a5dc0-ad54-47ee-847a-b408f9a4791d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8384adf8-3f16-4c09-a928-34b850aebb20)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8210341b-3769-413b-ac80-0e18f888b589)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       c8a2ee33-3767-4400-ae5f-dfeaa3c7d011)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       844c26d2-2a68-4a63-bc1b-f51692e81c71)(content(Comment\"# Extract the \
       rest of the list. #\"))))(Secondary((id \
       d20f3321-ac3c-4f33-bb46-660686986e56)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f94cd095-2fc0-4393-bf9f-b97b0fafca29)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       cd66c3c3-e6ca-48fc-a862-f5835846a9dc)(content(Whitespace\" \
       \"))))(Tile((id \
       85ca2dd6-8b73-4080-aec6-738301e080d9)(label(List.tl))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1459d141-6082-49bd-8878-5467e65570fb)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2031c41f-3d33-4e74-8b85-94fd29fba162)(content(Whitespace\" \
       \"))))(Tile((id e14c2a47-fbd2-4df3-b70d-6442ed82c431)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       0e016be5-4a2c-4365-b7b3-d8ae13215fd1)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       7c65c24f-cf86-4af0-aedd-9892f277c3fb)(content(Whitespace\" \
       \"))))(Tile((id \
       e885964f-7597-4139-8768-72161726410c)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       85875d23-e207-4778-a25c-a002eb4d241f)(content(Whitespace\" \
       \"))))(Tile((id bc71f8a9-3829-493c-9e47-be966c94668d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       56276ade-dded-4d1c-996c-18d026289460)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       130b8a32-4db0-456f-954b-89e4e2b4edd4)(content(Whitespace\" \
       \")))))((Secondary((id \
       a0aae6be-96e8-4691-a32e-10ab1efdca62)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       5a96a133-6707-4ab5-9366-096e25de1fb8)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       38be2a3a-b64c-453c-82b1-906fc2a99c4b)(content(Whitespace\" \
       \"))))(Tile((id \
       47b4ab32-fe03-4197-8b3c-c8934ba7cec9)(label(l))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       339c9e83-0c4c-40cb-8b96-b8f05f150f62)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8a91e7f4-507b-4c8e-9707-dba2231dc802)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1783db51-25e0-48fc-ab49-27c81f8fc4a1)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       34337f90-d173-4829-abf2-4d8434c02b95)(content(Whitespace\" \
       \"))))(Tile((id \
       7cd88e57-7bc1-4eef-bb15-6a58ad4f68b4)(label(l))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       e97f5fac-482b-4c8b-af29-90a863124b56)(content(Whitespace\" \
       \"))))(Secondary((id \
       3a26cbf3-30af-4435-b75e-80bf3ddd8c41)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       b56f74f6-7376-4a6f-992b-4e56e859780e)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       c128e351-d88e-45e5-a0ad-96aab23afb87)(content(Whitespace\" \
       \"))))(Tile((id \
       9fd0ef16-2c15-4df2-9eb5-959006f8c0ee)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       237cea6f-5c8b-4d91-86eb-b6b8c31f69cf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       54d484a1-644e-49c1-9b2a-0bff612f0891)(content(Whitespace\" \
       \"))))(Tile((id \
       fcb703de-cac7-4447-ac36-299beab281e8)(label(?))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       02c63f14-6779-41ea-9588-b890a0abdb35)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       06c77551-69d2-417a-938c-aee8b51c92d2)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       aa324c4f-2137-4094-9312-f65323569c07)(content(Whitespace\" \
       \"))))(Tile((id \
       e2d9d0bf-8e0a-460d-b239-c8f94d4da0ac)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       fc119baf-bd4a-43d9-a48e-5d9831de3e46)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       850cee30-ad91-484e-922d-4c01fa83aa2d)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       c423bb65-5c65-4fdf-aab0-9dfb46be4434)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       563283ac-bf92-438e-90db-39f47d42089a)(content(Whitespace\" \
       \"))))(Tile((id \
       8d38bd81-1647-4474-8f6f-40cb228553c6)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d69d06c3-2ab4-48d8-b64a-2642165c4d26)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       328e54fc-f747-4f80-82e7-a97602ad4835)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       43b224df-202d-4f85-ad6b-b77b0210516e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       9947b32b-3a4b-412d-a1d2-80e14a13df61)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       6008ad20-5af9-44b2-989c-96ff27f86e44)(content(Comment\"# Determine if a \
       list is empty. #\"))))(Secondary((id \
       c3535dbf-cc67-4cde-a8c0-6e0becbb7d86)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       71b70846-6a04-452f-b8b8-a6c1db377f8c)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       b88a5e51-69aa-42a3-84c2-d71b2a93bca0)(content(Whitespace\" \
       \"))))(Tile((id \
       5841d3f1-5dba-4e08-81bf-d6a2864e8396)(label(List.is_empty))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       36ae5347-932c-4fd1-8b36-110fba72128b)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       39fac74c-1a13-4a40-ad52-6e3ad6bcf360)(content(Whitespace\" \
       \"))))(Tile((id d5e40cf8-ac41-4a8a-8288-3509cc33c9ee)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       f8687f61-3af3-4880-9b69-49f669dae9d2)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       abfb6373-75ac-4c7b-bcf3-2f3b5f244d6b)(content(Whitespace\" \
       \"))))(Tile((id \
       03fc0875-03b5-4e6e-ae04-de96f84141a1)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       3caebdbf-1595-479a-937c-fe96abd957b3)(content(Whitespace\" \
       \"))))(Tile((id \
       7a526f04-6623-4ab9-94b5-bc622809739c)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       224a0bfb-e4c4-46eb-8903-d9c40264a511)(content(Whitespace\" \
       \")))))((Secondary((id \
       036ba81d-5128-4882-aa1b-3c8bf07342d7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ed6dd0e5-54e3-4c42-8989-4361d81862de)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       c42989e9-a31c-4c80-a270-6e2a807195b0)(content(Whitespace\" \
       \"))))(Tile((id \
       705336bf-ac1b-425b-8778-4e31d99caa67)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       ba3b42ea-f221-4359-96bd-a84ac112e536)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       68a35bc3-7997-47a3-83a0-01cf0b408224)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2f21f193-f9fb-4f52-b9fa-9abb7d6d29aa)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       f009a5cb-3aed-47d8-a2ac-82d15016cf4a)(content(Whitespace\" \
       \"))))(Tile((id \
       8694fbde-670c-4590-bc71-9a6ddbaa6a3a)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       049ea2b0-051c-4fc5-83f2-ab32a3b9a4e2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d87fcad6-ea75-4150-b147-d9ee4ec3f62f)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       2733c852-96f9-4433-88cd-3a88d8cd4c8f)(content(Whitespace\" \
       \"))))(Tile((id \
       7f7c778e-0dd1-4c54-85b5-81a5f7c0223c)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       3e53b7b7-847d-4841-8d9d-f1cf82e22ae5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       fc9a255f-a931-4694-a63f-a4226e8696a9)(content(Whitespace\" \
       \"))))(Tile((id \
       83f886ed-901a-4458-830a-7e3b071a558a)(label(true))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       746d6fb6-870f-4b7a-b20a-86c78a84ddff)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e5603ab6-d481-415c-b7a6-89211130d857)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       01779976-6976-4551-87e4-84b6b856d235)(content(Whitespace\" \
       \"))))(Tile((id \
       b755bea4-0c4a-483a-ae8c-0f7a987e88bc)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f4e6279e-3b31-4973-8dbb-cfc2150cdfb4)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a919269c-fb3a-4221-a9f9-ae2af9106689)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d8f16ad0-70a9-4595-a8a3-b1869b04ecff)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4841e930-6509-4eb8-8e76-0d6d0d062d59)(content(Whitespace\" \
       \"))))(Tile((id \
       58f675f2-0279-41be-889a-63e72e2abf70)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d70ea485-94c5-4c87-8819-ad0e8e9f35ba)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       ed6384ec-3d20-471b-bead-db10954b2d4d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3262c943-63a9-4130-9b94-9cf6b6495d6b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       5e6ab17a-ac99-4595-8723-870a91bb0289)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       86a8ed39-e344-4f6e-9c88-986d5b124e21)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       bf68d56b-de62-4e88-bf96-24b38fab720a)(content(Whitespace\" \
       \"))))(Tile((id \
       b20402cc-3e88-4aef-922e-dd437f401462)(label(List.nth))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       81daa21c-7074-4a77-9c3e-91ae2e44ebf6)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       72d8a6ec-4884-4c5d-9905-a2d98b22a3b5)(content(Whitespace\" \
       \"))))(Tile((id \
       6ac4bcee-2b59-4007-9b4a-b038b5dcb3de)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       6c9c0833-8762-4ee1-b01f-b971f9066bbf)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       d0820d66-8854-4e92-82ad-af185ca72e07)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       ca6be0f3-a199-423d-9522-91bf5db7b31b)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       36d36678-08df-45ad-9769-e1bcc97ddc5b)(content(Whitespace\" \
       \"))))(Tile((id \
       48687b05-7dac-401b-9d29-582440fe8e47)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       79b97de7-62bc-4a9c-8d50-e4b7171597de)(content(Whitespace\" \
       \"))))(Tile((id \
       16d9c626-c85a-4320-ae72-692c66f53bc0)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       ffc7446b-1972-4f1f-a5bc-bc1dcc412d13)(content(Whitespace\" \
       \"))))(Tile((id \
       dbcfbbf5-89b7-4d4b-b012-77b27e98a3ba)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       65c5e3e7-01c7-45a8-a10b-c1d0e3f0fee9)(content(Whitespace\" \
       \")))))((Secondary((id \
       c602aa7e-4410-4146-819f-acebd9882bd8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       43981a13-cd03-4590-a40f-93933bae6f22)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       b66abcb2-7d10-439b-8b62-0aa393d7322d)(content(Whitespace\" \
       \"))))(Tile((id \
       da8e9a7c-39fb-40d2-8012-81c79e4359e4)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       5e296649-9fa4-4556-91fe-20023bbce004)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       40adb552-f48e-41e4-b275-e372c3348468)(content(Whitespace\" \
       \"))))(Tile((id \
       ceab78ed-2aa9-4c9e-8086-c3ca50ff9a40)(label(n))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       4a466735-da5d-4e3f-a76f-849a4d36b9a0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       7eb98009-111c-41e3-a54e-fd2fcf5e9384)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       4fe78a0d-69a5-47e1-ab3b-58045478b651)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       3f32a444-3e46-42b1-bc45-9503e53605f0)(content(Whitespace\" \
       \"))))(Tile((id \
       188222ab-8a3b-4f73-8b3e-76692317abb8)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e03ff2f6-3871-49ba-ae89-5c25b98c7852)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       310d9a92-0c21-4989-ab35-71b627ffe830)(content(Whitespace\" \
       \"))))(Tile((id \
       d32d242c-176e-43ab-afe3-20cc6db67c36)(label(n))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ba46868a-25fd-4fed-b715-cb380cfaecfb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3f06fa42-6cb9-4c33-b2e5-ffd0d79295a0)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       a09015c1-31da-4bf2-90b6-a9f57574ca5f)(content(Whitespace\" \
       \"))))(Tile((id \
       ede002e5-7784-40d0-a5bc-687a10b143bc)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1e841b99-ff66-427f-b12d-793db9f17c34)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       35506834-0b99-42c2-8093-728b7ebd21ad)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       5c7277d3-fcf7-4b04-80fd-ef363cfe3b17)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       fa4e096f-76f3-430f-9895-a5c1396f1c33)(content(Whitespace\" \
       \"))))(Tile((id \
       abc891a8-2c5b-43ec-9440-3080561c6ae5)(label(0))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       a0152c7e-c129-4985-9f55-4d90f909bcf2)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       d8240c29-4eaf-45ec-892c-d523f7045ab4)(content(Whitespace\" \
       \"))))(Tile((id \
       3abb3b61-1954-4517-9e38-bae2d3661962)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ee97fed3-e9d8-453d-b431-aa53b0cf2201)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       9ff8f18a-3885-4001-93f3-e47adb1666c3)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       e6fd7682-423d-4a10-a99a-bdfa9f88af0d)(content(Whitespace\" \
       \"))))(Tile((id \
       28b47511-d503-473a-ae4b-7d0bc53cac63)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       44d80a12-aacb-4ff4-8091-53dc1ba92a8a)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       5c242f0d-a21f-4bf0-aca5-471a650c567c)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ff18d135-082f-44c6-badc-ed757c6b86d3)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       599e4005-2b6c-41e2-8f1f-5703e9ae7c43)(content(Whitespace\" \
       \"))))(Tile((id \
       ffaad327-9220-4e70-809e-839fcd06fd40)(label(n))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       2da123ce-6fd1-420f-85a5-2a9def10a00d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       45412e8c-9169-4c0e-b900-49c2a084fc2d)(content(Whitespace\" \
       \"))))(Tile((id \
       61a0be11-be01-43a9-95b1-abef7733d0b7)(label(List.nth))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       8a1de120-84be-4aad-adae-718f82f98b66)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       1b1601b2-76b1-40d6-a782-a99a0cce4157)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       77de280a-51fb-4cc3-84c9-7bba9ae514a1)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       a6ffc768-3d69-4aff-a422-446c48d057af)(content(Whitespace\" \
       \"))))(Tile((id \
       0a487f30-f56e-4dc4-ae30-c72a4ad9a074)(label(n))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       61656654-7f85-4572-949d-45bfa7d8cd13)(content(Whitespace\" \
       \"))))(Tile((id \
       9cc539ab-08db-4b64-b1de-ccb19ceee86d)(label(-))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       7060282e-91aa-4e93-9568-207229dc80d6)(content(Whitespace\" \
       \"))))(Tile((id \
       96425734-4425-48f9-bc08-1eb1024fd811)(label(1))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       4e2bb46d-63a7-4c92-ae49-dd63f3c63423)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       296c4618-95de-41f8-b0e4-2d8e5c52a418)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       79fa5d6a-256c-4fd6-b6c6-59d60a9548f1)(content(Whitespace\" \
       \"))))(Tile((id \
       f94337f8-e552-4865-9649-6e8fa64a853f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       76214679-705e-4bd9-aae9-d5b54ec01791)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       ea7e00f5-1d52-4d60-b9e6-96398219deb7)(content(Whitespace\" \
       \"))))(Tile((id \
       6e2255d0-18f6-47ac-9f14-f3ed37ed26a0)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       e5145c27-8da6-47f0-af9a-bc49829ad189)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       db1bf3c4-562f-4f6f-8fe4-4cbd986e68c0)(content(Whitespace\" \
       \"))))(Tile((id \
       4b7e26ac-097f-431f-bd8f-3cf569625779)(label(?))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       20d3c818-ed22-486e-93e1-e44d8601104e)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       195455f7-970f-4dfc-a412-c9ba778edbaa)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9b98f78e-fd32-42fc-8221-dcaad2d95faa)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       6e997de8-4555-43b0-8a85-178be6ab4277)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       0097d90c-75dc-4ef3-b21e-062d60698255)(content(Comment\"# Reverse a \
       List. #\"))))(Secondary((id \
       cf4a28a6-eb9b-4699-95e5-7a06713e3666)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6a45bd22-eb86-4ff0-93c7-17e35eb1ef02)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       ee5bb07e-cd8e-4ffe-8b38-d9813d7cfbbe)(content(Whitespace\" \
       \"))))(Tile((id \
       17309765-1e56-4f38-b3a8-d6977055b3b1)(label(List.rev))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       df78be60-aa54-4b48-bf55-70bad58ad924)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       8e459e20-9a79-455e-86c1-8dc420c13c98)(content(Whitespace\" \
       \"))))(Tile((id d69f715d-bd7c-49d6-a534-ff06366fbfdf)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       1337c7d8-1dea-45d5-9d11-611a1e1f6e34)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       90a3a5bf-52f0-43c0-b6a9-edd9dd235d70)(content(Whitespace\" \
       \"))))(Tile((id \
       9a559356-0e30-4f58-9fee-4075be0a26c3)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       cf3cd5da-7325-4160-b024-e031d785d2bf)(content(Whitespace\" \
       \"))))(Tile((id fe5f1424-f475-49f4-b973-5484d42c4743)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       aa51a082-1c86-474c-b209-82c7f824236f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       d76f2a63-72a4-4dbe-8cde-d50984ee1a00)(content(Whitespace\" \
       \")))))((Secondary((id \
       e398ffae-6576-402a-a2f8-52c91642d9ab)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       cd605366-110f-4388-b6bf-ae6260f6ff48)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       0d6d0744-464a-491f-bf16-f781f7ae9517)(content(Whitespace\" \
       \"))))(Tile((id \
       7dcf23fb-69c4-4412-8a8b-30b26741f275)(label(l))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       1c479783-9b3c-4265-9f61-7d9ca9b1089a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3e24abbd-e3b4-45bd-a5c1-cd118247c979)(content(Whitespace\" \
       \"))))(Secondary((id \
       6c7eb9d3-24eb-471c-9891-4d0c903dc612)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e84e25b6-3c29-47d9-85c0-98249c488f43)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       0dedb669-7a63-4a73-9944-879e3223dead)(content(Whitespace\" \
       \"))))(Tile((id \
       a99906e9-9903-40c6-8ec9-d6e83d9401f0)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       3078b6e2-32dc-465f-978d-699c9448e4b5)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       9074d9b1-a238-4b60-8e44-83699409d681)(content(Whitespace\" \
       \"))))(Tile((id \
       480bdbe6-89f2-4288-a358-5b5820615176)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       84298e5c-31ff-4fb4-a6b6-1da6a8640d7e)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       ecc39560-e957-4045-b068-e507e4bbb809)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       7695611e-18de-4328-8925-c64af911c12b)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2aca4395-f2ca-404c-83e6-a148b3352303)(content(Whitespace\" \
       \"))))(Tile((id c10bdc71-5dc4-4514-8158-fec13b957748)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       14ea5f6f-b66f-4e85-9350-611b6b1812f4)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       00cf75b7-1e73-4faa-a34c-919aeee935e4)(content(Whitespace\" \
       \"))))(Tile((id \
       902332ee-fb97-48e3-95b8-d16560836ef5)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       ec0c3d2b-870d-4b10-bfe3-8a15c48619df)(content(Whitespace\" \
       \"))))(Tile((id 9d7a7696-7506-49e0-9e93-4ee8a8004be0)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ebb712d9-1040-4337-8abc-cbead838ea71)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       e5392a38-081d-4c5a-993a-8fdd67ddeba1)(content(Whitespace\" \
       \")))))((Secondary((id \
       536c80e2-341e-4569-a110-59089b28dfd0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       bc3b2030-2f70-4cdf-ab97-80ebed54da2a)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       e8b60b9e-70aa-4bf4-8585-5dbfebbb681c)(content(Whitespace\" \
       \"))))(Tile((id \
       29465c36-9903-4512-86d3-c3e49351f0a3)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       554dcc9b-7050-483f-a66a-b5538d2f7b85)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       1783c778-52b7-4d94-9c82-e33e0b938fa5)(content(Whitespace\" \
       \"))))(Tile((id \
       5497405e-24a6-4cb4-99b9-6a733719247d)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       2976bdc7-10f7-4fad-b04c-43147ee9b62f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0c1eeb85-ad28-42a5-b49e-82a7f40c4bd2)(content(Whitespace\" \
       \"))))(Secondary((id \
       469f030a-99fe-4163-ba26-f4d99743aba8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e46c073f-e84d-4e87-bc6a-c7d204227f28)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       3e0bd757-d309-4cda-9048-80e63671ae42)(content(Whitespace\" \
       \"))))(Tile((id \
       6a5b63ad-a15f-49be-9fdc-80d58a4a9ef5)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       36cd9d4c-6ae2-4f2d-9fa1-3ba8316be3cd)(content(Whitespace\" \
       \"))))(Secondary((id \
       fbc7d87d-4dae-47f1-94f1-3046111b2361)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8957b157-12c5-42bc-b6ea-320a9617b280)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       51d814c2-d14c-46c3-8b31-1ca10b7cef38)(content(Whitespace\" \
       \"))))(Tile((id \
       741c876b-e199-4429-ae37-53bc5ee9fff3)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       de3c4b1d-d62f-4060-a91b-0a585afc077c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e2dbb788-b769-4528-a7d1-23e685406187)(content(Whitespace\" \
       \"))))(Tile((id \
       3a477de1-8537-4759-9400-d82084f7d81a)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d071d208-2ce0-4032-a23b-4a575df7f859)(content(Whitespace\" \
       \"))))(Secondary((id \
       47046152-ca8e-4a95-8141-c586a329d53f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       39f76273-e80d-4eec-8c7f-ab6e6e208ae4)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       d0c02c5d-9d09-4910-8032-473d5f6c3e5f)(content(Whitespace\" \
       \"))))(Tile((id \
       4c2729d8-14d2-4a5e-ad2f-405afcc1d5bc)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       bd0f8b97-ed33-470e-bb76-c43ab4fa0fc7)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1ff6f701-7e6a-4626-9882-5823a3cacf1b)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7ad24ea9-bbda-4c45-979f-0946b61e5cbd)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       515b4169-ec90-4aff-ac2c-611bfb95a34a)(content(Whitespace\" \
       \"))))(Tile((id \
       760f75fc-4fb7-4308-bf4a-90b492144d46)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       1ba1880d-a35d-494d-a5fc-dacc161a5709)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       5fb4acbb-8027-4b73-b5d0-352d27a3760d)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       1582639a-5a89-4f84-b6fb-1b4d1811f06f)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       00c975c9-f9b4-4f91-838f-0863eb50f1eb)(content(Whitespace\" \
       \"))))(Tile((id \
       666f7c44-5af6-4b5f-a1c7-411ed5a58764)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5e5fe9f2-27e5-4c15-ad1b-ba904e71f650)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f56d020b-0965-414a-a057-f6cf139ed11b)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       3856fbae-5005-46c6-920d-db71d9af2d83)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8b152b60-350f-43df-a16f-7b596a572a5f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       747b93b2-284d-4c13-8d79-64d7136e7db2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       cdb561c8-10ec-4486-b718-f95bc14c2296)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b212c511-90ce-429e-9549-b089d1b252b3)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       c5b20a00-beb8-4f69-a48a-2231c7b59d6b)(label(l))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       dd6933b4-c0ad-4cec-a332-c5007949aa29)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       371f2258-b775-4231-925b-98f80b87bcee)(content(Whitespace\" \
       \"))))(Tile((id \
       6acc0a28-b5c3-47d4-b8e2-f4cfd0f50cb2)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       38ad3121-fc0b-4e23-993b-fc09304ec31d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       88340ad9-b02f-4072-b050-8472d1ea40b0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       63ead6c6-6047-4ffd-80ac-6209c5f32873)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       236872f3-c9f4-412b-9f3c-adb25399cecf)(content(Comment\"# Initialize a \
       list with a given length using an initializer function \
       #\"))))(Secondary((id \
       bc8c1624-60b5-4883-b38d-7cdbdf347332)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       415ffeab-8da8-434a-b223-216ea696ecdd)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       19f54822-9277-4eb0-9073-c126948ba5ac)(content(Whitespace\" \
       \"))))(Tile((id \
       e6b06d04-5710-4ab2-8647-c7e88e7d9a5a)(label(List.init))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       bb704792-8f22-4f5f-b878-92bc781e26ec)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       e044e115-bcf6-425f-a98a-b58739e74a6a)(content(Whitespace\" \
       \"))))(Tile((id \
       9bd8035f-bede-42f0-8845-8ff5630c0c3f)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       2181d13f-d49e-44c8-8e3b-ad13dc9575e9)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       c56c2991-f6a1-487d-a0bf-43f939a2d8fb)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       fda3e172-d5ec-4935-bacc-f4b8f30cf32b)(content(Whitespace\" \
       \"))))(Tile((id \
       6bc7c623-1925-407b-a2d0-79efe2c5e2fe)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       52408c5a-c4fc-4deb-9c5a-7f0f3b33e386)(content(Whitespace\" \
       \"))))(Tile((id \
       d1fde81d-2667-4c5e-ae0a-a7892b158d31)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       0f3b5b71-59c9-45c8-a1c4-63be6afd0913)(content(Whitespace\" \
       \"))))(Tile((id \
       b53d5681-facf-4a4e-9852-eed14b6217f6)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       bee8de0a-4a99-4a09-9394-e9d87250a631)(content(Whitespace\" \
       \"))))(Tile((id \
       d082f12b-c2c6-4106-81a8-6b94d4b6bce4)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       2ff6a4a7-d06b-431f-8e72-683b998db19a)(content(Whitespace\" \
       \"))))(Tile((id 2ea36690-ad56-4092-a357-4ef6d1a4541d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       9a4fb2d5-8ffa-455e-9ff7-37e16be9d1a5)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       ef0eeaeb-e607-4d50-ba4c-fdcd793a65f8)(content(Whitespace\" \
       \")))))((Secondary((id \
       8b175f70-91f0-45f7-91ff-57e055f6fdb1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       aaa764b8-5bc3-45ef-85bf-6e2c8788b4be)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       eb6f310e-d9a8-40ae-b870-fc959d91fde9)(content(Whitespace\" \
       \"))))(Tile((id \
       7dbcf8bb-7008-4b6e-97b4-c24cbedb97c9)(label(len))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       628e9dff-35e9-4ee0-bbb5-42c1d8447aed)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       f832bda2-dae1-4d5b-90fd-fe9f6b82f73d)(content(Whitespace\" \
       \"))))(Tile((id \
       028a38e0-741b-4d1c-ad0d-33148bec9e93)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       b8bbf75c-7a05-4783-8b70-779fa585775d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       faf4325c-a70a-46a5-b234-6789f23dc44e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       4c4ea233-78ff-4354-b1e8-d623c41f5a1f)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       2f4c48f3-b025-4709-bb6a-77fa076687b4)(content(Whitespace\" \
       \"))))(Tile((id \
       032db45e-fb8c-49cc-b3d3-794c61abbb57)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       9cb7fc4c-907d-43db-875c-4c8b63a51fc4)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       8d11ed3b-b22d-4a79-8b20-451e13224a94)(content(Whitespace\" \
       \"))))(Tile((id \
       62e3536b-0426-4704-bc90-ba27c5ddd7fe)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       5de56752-84e6-4778-bb88-48a4a4dbb0bf)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       a26698a7-5a98-4f4f-ba2a-4e852aace3ed)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       ca97278a-bc59-4be5-bcc7-03e3337b7569)(content(Whitespace\" \
       \"))))(Tile((id 5bfb1804-65e6-4551-aced-c52ac4b83413)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       b1673c2e-03aa-4f23-b250-11cb4a3c1e36)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       2ff39a3c-ab7b-49f0-a42d-0c3373c2434a)(content(Whitespace\" \
       \"))))(Tile((id \
       85504ba7-98de-439d-96e6-a29b12aa4a47)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       4c2712b3-2297-46b1-8cf6-51ca7f583d34)(content(Whitespace\" \
       \"))))(Tile((id ca60bab7-009b-4236-86cc-34f883692535)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       55516cc9-dce4-408b-ab26-0761adb07901)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       94596845-b892-4147-bd02-4418b394440e)(content(Whitespace\" \
       \")))))((Secondary((id \
       d056bc65-b1a1-4643-a7e0-845de2e0ba2a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0d48398c-2dde-486c-8d11-fa7b9d31f537)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       8cf751ed-8801-496d-a064-fcc43f60cecb)(content(Whitespace\" \
       \"))))(Tile((id \
       19d4dcec-3e12-4a07-99d7-14f7476dbea9)(label(idx))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       bf350967-62e8-4ddb-92cf-cafb84c12422)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       db68f9bd-4b09-4e9b-9009-aef50c09f579)(content(Whitespace\" \
       \"))))(Tile((id \
       0c56e009-2a6f-40da-8299-ccf25aa6b7fb)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       f8e75315-a08b-4abf-8f7e-ee8c3fd2b0c8)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9cff1cbc-56f8-49ff-9d25-aadc3658db33)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       826196dc-3231-4e36-9242-869a5b61ca38)(label(if then else))(mold((out \
       Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       6cbb4d18-7387-4b3b-a245-69887c58a98c)(content(Whitespace\" \
       \"))))(Tile((id \
       c7bf0e97-3b9e-4961-9895-16ef7f2a98a1)(label(idx))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       8611a412-a91b-4901-b31c-7954fc63acaf)(content(Whitespace\" \
       \"))))(Tile((id \
       dd4ae2a7-1ca1-4263-b817-b8b251a6fc2c)(label(<))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave 8))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       05bcc9a1-dbb3-415c-909e-bb8b9e9f7c7e)(content(Whitespace\" \
       \"))))(Tile((id \
       a671ee01-42eb-4263-afa7-71a5fd872c8b)(label(len))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       0e334da5-25f3-49ba-82c5-ca79948e11e3)(content(Whitespace\" \
       \"))))(Secondary((id \
       d901585a-afc3-4744-85e6-8fe2a3809ad2)(content(Whitespace\" \
       \"))))(Secondary((id \
       a05e1fab-5a3a-4fb8-995d-252760b378fe)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
       5f7d4ed2-1432-428b-9311-f0c6268a7501)(content(Whitespace\" \
       \"))))(Tile((id \
       89996d85-2776-43c0-9f9b-5fa6ab8d3223)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6b7d73fe-946c-4744-80c7-eca49a96bf00)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       7cca8c9d-6c44-4d45-98a3-4e7e9435c975)(label(idx))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       dbd8871c-3698-4915-b684-fd194550b0b1)(content(Whitespace\" \
       \"))))(Tile((id \
       12135abd-5d76-4017-9c31-c398c3b552df)(label(+))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       7a795100-4686-4f9a-9fa1-35e56bc4a5f1)(content(Whitespace\" \
       \"))))(Tile((id \
       9cfec40c-326e-41c4-bac3-5eadea00e4da)(label(1))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       23d55a59-aac3-4a50-ad6f-7dfee9df241b)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       53143be1-aa71-4240-81ff-d04c5c80503c)(content(Whitespace\" \
       \"))))(Tile((id \
       983272a7-6aa8-4281-8653-1ec737b72542)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       dde87a19-d2a7-47f7-9b52-3e2548bc9dab)(content(Whitespace\" \
       \"))))(Tile((id \
       977503b6-dddb-44b2-afca-5ddcc7187622)(label(@))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       4d731e18-1ff2-4836-9047-2635ef0a3294)(content(Whitespace\" \
       \"))))(Tile((id d4c5c1ad-1a6a-4982-8ce2-a5767428397f)(label([ \
       ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
       Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
       63ce46e0-6858-471d-8988-489462250e87)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       bda96617-1004-4e47-81c6-bdabf2376a14)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       796eace5-81e6-47dd-ae63-8e95ced91679)(label(idx))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
       5c02c526-42d3-457f-ba96-608d3873830f)(content(Whitespace\" \
       \"))))(Secondary((id \
       fa2df0b4-ee92-474a-8d56-5fa0b3e7c895)(content(Whitespace\" \
       \"))))(Secondary((id \
       4fde5c37-aef3-416c-b83e-e8aca6ceaec5)(content(Whitespace\" \
       \"))))(Secondary((id \
       35a6c611-60c6-434b-8583-ec3d1d3e8c18)(content(Whitespace\" \
       \"))))(Secondary((id \
       f588ebc1-4980-44d7-9dc8-09661371fe5e)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       18222a7f-7b56-4da4-8eb1-e788b67443ce)(content(Whitespace\" \
       \"))))(Tile((id \
       1013e35a-9bc3-44c1-a8b9-914fee962508)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       f794bffb-f3b8-435e-98a2-67e3606e6e82)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e1271523-8906-4fba-844e-8ec7d390c2de)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8b2b3173-b609-44fb-abf7-c85ef072b740)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d86d7c18-2e83-4a6e-9937-253878ec78e7)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       92dbbd3a-74f7-4bd1-959e-2d32554461e9)(label(0))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3146abd5-3932-4de5-80e7-b2044725430e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ca00301b-2cb0-49d9-92cf-a08750b581c6)(content(Whitespace\" \
       \"))))(Tile((id \
       8a0f42fc-8e3e-4bf9-99e8-db4a847e0e57)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       7977f5c2-41d3-4ba5-8db5-b50e7588b148)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       b4127508-9836-4e36-a170-72cc125f3608)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       3a75f521-a283-456d-9cae-d2ac1673f435)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       f0ebae85-5ab7-4399-9fec-c31edbd079e3)(content(Comment\"# Check if two \
       lists are equal #\"))))(Secondary((id \
       96dcea7a-77c9-4468-9994-9ec5f57b1fa8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0eb11443-7002-40e7-990b-244d87a5b64d)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       377678cc-30cd-4639-9735-b3bbf098890a)(content(Whitespace\" \
       \"))))(Tile((id \
       97d1ce8f-6736-4a0b-9589-3f445861003e)(label(List.equal))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6292db03-8789-4c57-a522-b09a9c8a2ce9)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       3ebfadd3-459b-42d5-a235-aee0a206f95b)(content(Whitespace\" \
       \"))))(Tile((id \
       c88cea98-6c3a-4fd0-a4d6-5b9d3d0761f7)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       605664cd-05ab-4398-bf1a-7cd5c394e558)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       09a08a26-4e19-447f-82a8-a2885efa26e2)(content(Whitespace\" \
       \"))))(Tile((id \
       877779b9-de78-4c9c-ada5-ea42021ebe0c)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       1f6ebd9b-a871-47a8-a92a-3951bf1740ef)(content(Whitespace\" \
       \"))))(Tile((id \
       3de42be1-1247-43f7-891b-28ef561755b1)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       efc7bdfd-848a-41f7-babc-c4f302e5d286)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       45fedc86-f187-42f1-9606-bb7a67174a55)(content(Whitespace\" \
       \"))))(Tile((id 36dad777-b64e-4ba8-8980-28de1dc65577)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       a3cc7f69-1613-4c2c-b88a-484082d44a27)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       ae3a7592-16c6-46bb-bd2f-6e4e7a7c925e)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a8e2af1c-b49c-43b6-882d-2ef79ea12a44)(content(Whitespace\" \
       \"))))(Tile((id c75263d0-cf3c-45a2-89d9-472a18125d17)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       845babf8-9663-4a86-bbcf-373e375be3b2)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       c3b704fa-5b29-499d-9de8-8e4fd1afffa9)(content(Whitespace\" \
       \"))))(Tile((id \
       f64b5698-2e4e-48d1-b591-5422f3e71405)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       4f45de2d-b7db-4765-a9c1-dd962759f909)(content(Whitespace\" \
       \"))))(Tile((id \
       0df054b6-ef94-4a81-9c45-6aab47145686)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a1513b4e-3f3a-4b5d-8553-dccb2c0caebb)(content(Whitespace\" \
       \")))))((Secondary((id \
       d4cf87a9-2fa4-453b-88bf-1279259b9d03)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       95c54cb7-85be-4021-a092-7c2b3204b781)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       861f17be-5cfc-4895-b1cc-e46bdc96b1ea)(content(Whitespace\" \
       \"))))(Tile((id \
       7b640232-7d29-4fa6-b7e8-3778f4429c04)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4f223d3c-3a92-469a-8237-e47d3b107d40)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       dcccba2d-055f-4d18-acd7-beb8349c835f)(content(Whitespace\" \
       \"))))(Tile((id \
       ed7735c6-1ca0-4e45-bea2-f1ea3fb9640d)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       dc89b20f-7645-4053-83d8-a46f3909e0e8)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       44f1a6c0-144a-4a64-8187-137fa1960e19)(content(Whitespace\" \
       \"))))(Tile((id \
       b9144416-9390-4828-b5c9-8c6c3bc9f315)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       b8c71fcc-979c-40b9-bc6f-e2d5a548403e)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e0eec426-e8ad-4e24-83c7-a9534d9096eb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       a52977da-f737-4d9f-afaa-24e6add6f81e)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       92bedba1-662d-4eba-83a5-e6d92e840402)(content(Whitespace\" \
       \"))))(Tile((id \
       a98c4b75-3900-42e4-8a4b-75cb61707779)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       556a3354-f55e-4d07-92d3-be8113859413)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       bc8686cf-0449-4d0f-b66c-e6180371f755)(content(Whitespace\" \
       \"))))(Tile((id \
       b17932cf-5595-41d6-9bb1-b1d3a2b361c1)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       7f2f2eea-7d61-4938-b051-8dc1c574c637)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       01a36f64-60d2-42bc-a7e9-73a9ef132772)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       30ce1b01-61e8-411b-85e6-ac7001281cc1)(content(Whitespace\" \
       \"))))(Tile((id \
       e1982bd4-eb44-4b00-bfee-2bd1d2e6d222)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       74600ba2-3466-4d69-8d86-e99217f3e70d)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       ed98419c-7cf1-4fda-9a05-2c41f6b454a9)(content(Whitespace\" \
       \"))))(Tile((id \
       2140939b-5b07-4338-a4ac-2234cf7b7c38)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       91847696-6528-4e63-9b12-806521770ee0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       06c73ac3-9fcc-4b2a-a118-083fd073d2a8)(content(Whitespace\" \
       \"))))(Tile((id \
       08400f3e-2758-4bed-abc2-227b587dfe63)(label(true))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       6b2caa6f-cc18-4bfd-a5b1-115a4d4fced1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ba919ecd-510d-4113-9f95-fdb52135d256)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       ebacec01-d382-48fa-b1b6-bc2646944dd6)(content(Whitespace\" \
       \"))))(Tile((id \
       17ed25a8-c111-4679-b7b2-03bd3bf95278)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f62eaeac-c14c-4270-b847-15eeb873a2d6)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a9c7715d-9d9b-409b-985f-e73c88ac57e9)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       65c74259-cff8-40f0-905e-6bc2671f2e38)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       8c7a2b02-7c75-496c-827f-3ab1b536dace)(content(Whitespace\" \
       \"))))(Tile((id \
       67a1ff56-8336-43e4-a1df-06abd830c07a)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       8a34246d-681c-4268-b356-c0f241965ab2)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       9d741301-b400-4b28-b864-5cfbc098d774)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       cca33057-c94d-4288-a611-78e5f1e28983)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       edcb2915-ed09-44f1-ac2e-120a4636c474)(content(Whitespace\" \
       \"))))(Tile((id \
       42cae114-a1e0-4b97-b263-d71013efae10)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5d035aff-d594-4ea3-93e8-c6766d6fe3db)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       1b2fbe8c-0593-45a4-8a06-c789a8d3f96d)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3254af53-474d-4fb2-87dd-55cc0d8e0170)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       bc26e87b-32d6-4980-9748-8a28c07b7aaa)(content(Whitespace\" \
       \"))))(Tile((id \
       caf47a97-a8a6-432b-9dac-9459e236399f)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       6501f328-b2b0-416b-aa74-f64256c114b8)(content(Whitespace\" \
       \"))))(Tile((id \
       65696f8e-a401-4eb0-9c5e-708c976d4b9c)(label(&&))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 9))(sort Exp))((shape(Concave 9))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       0529682e-289b-4dfa-865a-3ca35da6eb0d)(content(Whitespace\" \
       \"))))(Tile((id \
       200665e5-8ee1-47fb-8130-9c97c2810075)(label(List.equal))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b551e422-e226-4375-8e2b-8c775fb1b632)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       934e4bf6-3642-48cb-a3c7-ba650bfdca0a)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       69fe918c-250d-4333-acd6-042784990311)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       c6aee57d-22ba-44ed-8f46-a17131a63ebc)(content(Whitespace\" \
       \"))))(Tile((id \
       42f00f8f-bd0d-478d-ae74-09c383492014)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       0fabd1c8-b5c1-4a5d-ae59-518467839e2a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       a6f6d42e-b846-431f-8eb3-1a39e50d5014)(content(Whitespace\" \
       \"))))(Tile((id \
       01927f6e-6771-4dcd-b09f-4ba104771cbe)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       4b307143-1c59-458f-976b-3f2835573992)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d6daec7b-6b6b-4f7a-a967-394b3ee4cac2)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       4a0f9794-76e5-4be6-a580-4776d7bf3686)(content(Whitespace\" \
       \"))))(Tile((id \
       293522c4-c94b-43c2-868b-c25c9c0dfd01)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       c0450f9c-2c1e-4b70-9860-9629e1478849)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f7919a0a-facd-4efb-b080-38d5cdf4a585)(content(Whitespace\" \
       \"))))(Tile((id \
       bf2a2ae3-9d52-499e-bf64-1c0b7844a3a8)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ef27592b-86bf-4cb4-80c4-b3efad817d5d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       bf326fe6-9d70-4018-9e6c-136eca850aeb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       416a30ca-14eb-4f02-afbb-649b4365fbbf)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       0fcfa4c0-281d-43ad-810e-67608e7b5874)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6fa6355f-ba2c-4c85-920b-255e19c87a16)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       06682c8c-8166-41c2-abfc-a63649545038)(content(Whitespace\" \
       \"))))(Tile((id \
       214b0812-b984-426b-9651-c07a94a490e0)(label(List.eq))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       33997e0f-ea33-440a-98c9-5182de9d4a38)(content(Whitespace\" \
       \")))))((Secondary((id \
       b92abb78-1a99-4c90-a8e9-e5df3df6eca0)(content(Whitespace\" \
       \"))))(Tile((id \
       61f4aefd-78d7-4dec-afa0-ab8b34e39f73)(label(List.equal))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       04d40d3f-9dce-45e6-a115-b404ed68a2df)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       701987d8-8a57-4472-a08f-65823fd3ef9e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       ef6baee6-7f9b-4d22-84aa-6520b0b19580)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       1d777330-cf39-4f4c-a90b-ce8b19c8fa9f)(content(Comment\"# Reduce a list \
       from the left. #\"))))(Secondary((id \
       f3d27a61-03e5-4862-a756-2b7fbfdae5b2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       01fc3267-44cb-417c-966d-d51082797b20)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       2919e59c-4174-44b2-94c2-774a90f8ce9b)(content(Whitespace\" \
       \"))))(Tile((id \
       c6ca8a01-9614-4183-a132-a472f7ae4c1e)(label(List.fold_left))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       cb38891b-aaab-41ca-9895-28b6a1f8e130)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       546a9866-e01f-49c3-80e6-5e91a883cb77)(content(Whitespace\" \
       \"))))(Tile((id \
       62fe526f-fcd0-4103-b634-23085fddc77d)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       85e5d875-6a80-4d09-95c8-b888ea0b05aa)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       95581140-bf80-4f7c-8cb7-0d7a482102a5)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       77b58efe-80cf-4d47-9605-a079b19f42b8)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4bf422e4-06a7-48c4-bed9-cbe260d47a09)(content(Whitespace\" \
       \"))))(Tile((id \
       0b443a2e-532f-44e5-84d9-2f155ded438f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       976c158e-4ca5-453f-917b-dc670df50ff1)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       96022d6b-d6e2-4e51-a049-0e91006051e8)(content(Whitespace\" \
       \"))))(Tile((id \
       757bae37-9888-4a03-b2c1-41e8834a35ea)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       5444ecec-5a2e-49de-9fc8-db7486669284)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       65e8e077-9727-4fcc-aea9-0d9355794d89)(content(Whitespace\" \
       \"))))(Tile((id \
       5ae209a9-7dd8-4165-9017-bd9e1425e326)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       7aa24950-8f4d-4026-aeb3-b9aec2f86911)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       544811d9-4074-453c-b24b-b812df5a6f65)(content(Whitespace\" \
       \"))))(Tile((id 900b4aa3-6b6a-4fb3-8c99-83d7dcb1e0b6)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       73fb0001-3af6-4b60-832d-1d73f9c27a84)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Tile((id \
       a5355d71-39b4-4d97-ada0-9269fe4113d9)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       1f4e005c-d0a6-404c-bee5-ef8ad7da9111)(content(Whitespace\" \
       \"))))(Tile((id \
       22426753-51bf-48bd-a7b1-c222dae5f935)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7c824c29-875e-4562-89a0-8a657aaaea0a)(content(Whitespace\" \
       \"))))(Secondary((id \
       eef9ee98-255c-4269-9172-99a86590a171)(content(Whitespace\" \
       \"))))(Secondary((id \
       ab313603-ac81-4839-b3a4-35535c41e1ce)(content(Whitespace\" \
       \")))))((Secondary((id \
       8e46b627-5e1d-4b71-831a-afbb69714ccc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e7ec134d-a191-43e7-af73-cda829c33de0)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       6319a51b-d85c-4933-9d0a-d4ad5bfaeb7a)(content(Whitespace\" \
       \"))))(Tile((id \
       c1c06e87-7842-43b5-8603-fc8b3937a324)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       92768781-ae93-427e-95fa-d14d826633d5)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       f42566b3-fa02-4811-8be7-a634d78b18f0)(content(Whitespace\" \
       \"))))(Tile((id \
       d64d6f95-f9ca-4449-accb-c05f3a42e903)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       727b4208-3ca7-40e1-a909-08b2c42b0e4a)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       47b34334-b531-4b91-9a9a-9233054ce469)(content(Whitespace\" \
       \"))))(Tile((id \
       029583d8-0013-4381-9582-c75106675eac)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       ae165e31-1e72-4d6f-abaa-ab0c43838acb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       98270ad3-34ab-46d6-9c9d-e6657671a07b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       662c3051-7bce-4e3b-9fdf-3e72d59c6958)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       e050012a-4f18-4741-87a2-3d8318e9888f)(content(Whitespace\" \
       \"))))(Tile((id \
       fbc06ae2-0ad1-4ec2-a370-b97cc6b15d7d)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       a1df96a6-ef44-4fff-b6d2-2fd0608d0674)(content(Whitespace\" \
       \"))))(Secondary((id \
       e31c664f-cc16-4006-8ab1-a8c8674dd4cb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8e462737-7d69-40dc-9425-90ad089c9c77)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       15c42690-32bc-4e60-b790-528a1a5d7fdd)(content(Whitespace\" \
       \"))))(Tile((id \
       de48fa55-70bb-480e-a4d4-d15ab9262f5b)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       3083f691-f9b5-4128-8690-c80cb57ee85c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9c4e390b-bb9d-444f-ba5d-db214635b343)(content(Whitespace\" \
       \"))))(Tile((id \
       8cadb73a-31e4-46b2-a9c8-6dfa2cf646ef)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       4ecb7236-a67e-44e7-986f-ffbcb3da3c99)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       31ec8433-978b-4884-ae3d-3774a7ac91d7)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       308f8c06-274d-447d-a89b-719c837ff812)(content(Whitespace\" \
       \"))))(Tile((id \
       f641448f-9ae5-4ad5-8298-3f31ac64fb1b)(label(hd))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1ac72297-362a-4d8b-bb32-65317c02c389)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       20e583e0-fd79-4f2b-b8e1-59315ea4d9d9)(label(tl))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       01fb8323-9526-4111-badc-f7b56afba4cf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2ef32a28-9b33-4196-97f3-729ed90e94f3)(content(Whitespace\" \
       \"))))(Tile((id \
       919df809-b867-46ad-8428-2347c996dc67)(label(List.fold_left))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       69c1e25c-af45-4829-9e0d-278e44526909)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       d213bf39-1c7b-47b1-960e-8fa5ff8121dd)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       347902f9-2f6b-475b-9cd0-81a0eff49157)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4b834a55-9e1c-4c77-a63d-eeecb41fd46c)(content(Whitespace\" \
       \"))))(Tile((id \
       e3b02bbe-fd6c-4e1f-ae2b-f70135c05d2f)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e5d87b81-01ab-4f54-9f43-46c5508e75c2)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       65e05f9a-9156-4406-9d01-bfc7f5fa9b1c)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2ae06618-abae-4d9b-b9f0-88c7c6d3e337)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       612ac2b0-3e0f-4094-9031-71c0a427a87d)(content(Whitespace\" \
       \"))))(Tile((id \
       72ac2d1e-bde0-4f1c-b707-8eb1e890c4c5)(label(hd))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       a3d6dbab-2d62-4962-8a87-855e3f409342)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       3cae3840-f784-40fb-b012-9e475fcd6451)(content(Whitespace\" \
       \"))))(Tile((id \
       b7b3231a-c1e1-4970-b356-7f4f75c61080)(label(tl))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       a2fdb520-ecc9-4302-b80e-e03e48e280d5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0cd62714-0d56-4450-95ef-12866c3073bb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       850ff777-acb3-4de2-8fb3-ba40bc6340d1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       c811a7e1-be92-4ed7-8ee9-de72bf13399e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       fd316a63-6da4-4a68-96f0-2839d815be6b)(content(Comment\"# Reduce a list \
       from the right. #\"))))(Secondary((id \
       1b3a7b09-4c78-4903-a645-e52c70f09bda)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       5e2c2213-e60a-412e-88d2-8c33215c64e4)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       9001f6aa-1711-4b68-8603-611fcbdc3488)(content(Whitespace\" \
       \"))))(Tile((id \
       a6a212c5-db24-49d8-89c6-43c10429cdcc)(label(List.fold_right))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a01cdefb-9f91-4e64-bbc6-8d790719030d)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       6759a5b9-6145-4fe6-ab8b-85da62328396)(content(Whitespace\" \
       \"))))(Tile((id \
       423cb4fa-2120-4aca-ab9e-b97480cb70a2)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       ede371a7-f6d9-4302-a5e8-d9da40726f5c)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       6ebe4f7f-cea1-4adc-befd-1b00c15c1efa)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       0e60513a-c98a-423a-9c7a-65b9b487c30e)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b2699bbe-a65f-4bb7-af04-07eefe11d05c)(content(Whitespace\" \
       \"))))(Tile((id \
       30979a06-cd08-46f0-9256-cf8a8368b0a3)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       a14ed0b7-ea3b-472f-9e31-ca9a5c68ad34)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       13757fb7-164d-43a3-ab6b-1a32610fbe60)(content(Whitespace\" \
       \"))))(Tile((id \
       66bde2ae-9474-420d-b353-06b63adc371a)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       0195a3b4-c6af-4cac-94c8-fd00f8c53606)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4b982aff-1837-477c-b9f6-9c6025732095)(content(Whitespace\" \
       \"))))(Tile((id 9113ea06-dda8-4a3d-9ab8-8e59c1a982b0)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       6c4734a7-0c8e-4c42-822b-818e660fe41b)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       a01117fd-dd59-4866-9130-a54dc6c00524)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       97bc1833-b50a-48b1-9e8b-a8cf6112f985)(content(Whitespace\" \
       \"))))(Tile((id \
       1acd0f01-dbee-45a4-9e2f-36a6456f47cd)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       411a4dd7-32b7-42e2-a1a6-d2cbce02a17d)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       d47784af-be0f-4f7f-9acb-7c41ccc93dfc)(content(Whitespace\" \
       \"))))(Tile((id \
       6107b587-104d-4ebc-a46e-698a55f2ae2a)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       8c18aa0f-e51a-4dae-99e5-07d6c500d791)(content(Whitespace\" \
       \")))))((Secondary((id \
       7da6c77e-6abf-4e73-b2ab-f1ab0adf4f53)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       845968ea-444b-48be-874b-cf1eae3a9ce7)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       60481074-82bb-4113-8a26-eb5ae75a3285)(content(Whitespace\" \
       \"))))(Tile((id \
       d6497ad8-1dab-493f-b8b7-9724a41d3721)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d856878c-ef8e-487d-9d77-fbca8809b4fc)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       80c8015a-914f-42b8-a89e-df4b7d12979b)(content(Whitespace\" \
       \"))))(Tile((id \
       ddaad17e-480e-498a-99fb-0502746d535f)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2e966831-505e-445d-8e81-1e02610e8d52)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       24f81bfb-3cd7-4683-babf-4d5a378e354d)(content(Whitespace\" \
       \"))))(Tile((id \
       ba9c9acb-ce4e-4036-86bf-11807395deef)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       656c5166-730f-46b5-8193-129646ff13aa)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e843e842-58c7-4b8d-b8fd-84aa87c08cf7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3a75dc72-4d71-4d37-a195-0e633d61a481)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       2e7f223b-56a7-453e-b0b5-36fc4e31b493)(content(Whitespace\" \
       \"))))(Tile((id \
       3d159979-6fc9-445b-b130-63b609aab32c)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       e6de6648-d2a2-4625-8b35-5b49b27c68f0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       b1e6e894-35da-41e4-8b30-59ca74423f16)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       f538d14f-395a-483a-802b-f63d2bf0d8c4)(content(Whitespace\" \
       \"))))(Tile((id \
       813f91fe-c87c-40b2-b696-896a89de080f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       942efa40-e047-495f-8888-2e6a68f85a92)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4fe3d6b2-8c8f-4a3a-b291-fa9873be0483)(content(Whitespace\" \
       \"))))(Tile((id \
       33979eca-3f51-442f-9983-7d583598a6e7)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       8f7783bb-b8b6-4df9-8f3e-1150398fd938)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e09519aa-5810-41ae-ab93-79d683943b17)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       60f94f1c-bd0a-421d-9a75-125c3e247512)(content(Whitespace\" \
       \"))))(Tile((id \
       73b2fb61-a6e6-4508-a6be-00820250beca)(label(hd))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       11bf3380-c4c6-4fbd-a0e7-04914a8f18bc)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       95478653-49c6-49f3-85dd-1efe6f75ddbc)(label(tl))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       91ba087e-d01b-4bde-a548-5ecba409f5dc)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9313fd70-5d6f-4025-bd50-875407981691)(content(Whitespace\" \
       \"))))(Tile((id \
       21e8f338-6024-4cd5-aa6a-abdab60977d7)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5242a5f7-bfe4-43ae-a9f9-c4e28dc909bc)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       a7ccae25-e12a-443b-a9c0-c7829a145b45)(label(hd))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d3e2bd59-e473-40b5-b13d-80ce0cc290fc)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       b6303cd6-09f3-4f7c-80d3-29c8c4058cd9)(content(Whitespace\" \
       \"))))(Tile((id \
       e49b81ab-c339-4f55-819a-117a8d7f80f0)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c1e1b327-b14e-45c7-a84d-433c3e97c963)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       af38506d-bf92-406b-9a68-86f326e1e07c)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       193657a4-9c7a-4c16-bc03-c0814143c244)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       e39f796d-f599-4e96-8925-eb8a21f70922)(content(Whitespace\" \
       \"))))(Tile((id \
       6979efae-2fe5-4e1f-8ad5-df0a4d0502a6)(label(tl))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d8126df8-a8ac-4e8a-bc4a-68c1d3e449db)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ff040f93-73f9-4d27-8e9a-d52f3f5ef0a3)(content(Whitespace\" \
       \"))))(Tile((id \
       459b85af-184f-49f4-8711-37cbebbc7dc7)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       1162be98-0b23-42a8-99ff-d1ffc44704b7)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4c3a7f38-feec-4ddb-9757-cafc2392523b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       1c6a9a7c-ed98-4c1a-8f03-bbd6693aeea1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       7c5ead5a-6397-42c4-aa5f-87a82076cd30)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6e91cd60-a60a-4093-ba54-82022416f503)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       54554e85-1e8b-40c5-bad6-a79d17a37ed5)(content(Whitespace\" \
       \"))))(Tile((id \
       c615eb9f-c48c-4bc3-af90-0c63a7429ed7)(label(List.fold_left2))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       852395aa-a964-41b3-9eec-29f850633848)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2e0eb251-d902-43c2-8a0a-0b325b80d6e1)(content(Whitespace\" \
       \"))))(Tile((id \
       5cf5902b-baf0-4d59-a059-8781dfa819cc)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       4401dd96-c2ca-4e76-a711-d5251ac0c523)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       eb22796c-dddf-4c8c-8ed5-167450df0200)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       87cc54e7-f1e9-4ecd-913f-45302c5ade6d)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b6056c5c-f12f-43dc-ba61-96115e33e3cb)(content(Whitespace\" \
       \"))))(Tile((id \
       ef3c143e-a105-4422-850b-96aaece4b40f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       2bf63bab-9d9a-45fa-a44e-8d96cfd41f94)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       868949f7-0c4c-4a91-98e6-547d9d72bd94)(content(Whitespace\" \
       \"))))(Tile((id \
       bbd8c64a-e506-47ed-942f-248755e583f3)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       1d227a09-6288-43e8-9931-7eef93194a34)(content(Whitespace\" \
       \"))))(Tile((id \
       8e33b2f9-5fd0-4f6c-a064-bec3f5079f2f)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       5011fec7-0837-4af4-85be-38aac2a556e7)(content(Whitespace\" \
       \"))))(Tile((id \
       7d796206-aa14-4efe-8d4b-d537e65ebea1)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       1f248e46-c5cd-411a-b94e-3c7ff0ed95cf)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       ab2a5aa3-7443-486d-a5af-51c0e5c29562)(content(Whitespace\" \
       \"))))(Tile((id \
       d2ee8c62-8f59-4ae2-9e99-3246baae6eb2)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       bc4db758-0b89-4884-944b-8c85989fb39d)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       0e37bdf1-9e46-4992-bfca-9391fe2c005a)(content(Whitespace\" \
       \"))))(Tile((id dc496b8b-cd09-4d80-9ded-2a1cfd65c59b)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ccef9fa5-6177-4b1f-b768-116a72dd775d)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       9e1b8a6e-ffca-419d-8b19-0bf5a8a46777)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       5dccea80-08c6-4d07-aa41-2d0727a0ac8e)(content(Whitespace\" \
       \"))))(Tile((id e891918b-499f-4316-b75f-0071c76eaf23)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ac68c129-e260-43c4-b993-8f9505cc49bd)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       162ff746-857c-463e-8792-cc544466457d)(content(Whitespace\" \
       \"))))(Tile((id \
       17ee41de-c000-46e5-94ab-aada9f6122a3)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       89ee1443-e869-4734-af19-4b9f256e67f5)(content(Whitespace\" \
       \"))))(Tile((id 2b4d5b21-9120-4ca3-ad1d-f08bb980b108)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       8cc72906-5d6a-4464-9bbf-ad9b7218ef67)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       081cbce0-457a-4889-9862-fca7ba49af4c)(content(Whitespace\" \
       \")))))((Secondary((id \
       80abda90-4685-45d9-b179-0fb36d659139)(content(Whitespace\" \
       \"))))(Secondary((id \
       d0a40263-2087-4af1-b586-24786c087141)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       047697e9-b109-4378-a3c6-fb51ee2b8321)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       2e2d40c7-2f4e-4c7f-9f8e-be7b6420c489)(content(Whitespace\" \
       \"))))(Tile((id \
       2498ca59-2af0-4d0b-ac54-278699d7e55b)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b87c6a6b-1df8-49d6-a936-01b9c632572b)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       7210ad4e-f5b8-476b-a30b-ab503c6096f4)(content(Whitespace\" \
       \"))))(Tile((id \
       5d90fe1a-c02c-41a1-8b7a-4bd905eb75ae)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       014ea9cb-5a47-4113-acef-792644a3ea17)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       bca59bbf-076d-4166-8a0e-76967f854383)(content(Whitespace\" \
       \"))))(Tile((id \
       36d3fd45-35a0-4cd2-8287-390f1d98c4a2)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       cc167cc8-9579-495e-ba70-fdf3f39114bc)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       cfa89b73-e065-4ce9-a57c-7959a5ba1d7d)(content(Whitespace\" \
       \"))))(Tile((id \
       25a56f5e-292e-4181-8f47-ffd580f636a8)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       477ac05a-3409-44e2-886d-3eda69a573cf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8029d2a7-a018-4c12-b81d-61fc835ffa59)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7f57e4fd-d707-4f5b-bbc3-f94bcfc4c1bc)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       6c9b70bb-1cc6-48cb-b166-436d4036ef5a)(content(Whitespace\" \
       \"))))(Tile((id \
       ae10e03e-a464-45f8-81ad-84e96cfdfa9a)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       263aa61c-748c-4845-bbf1-675abb157e9e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4e871cbb-3051-43ce-8179-2a6555fc5535)(content(Whitespace\" \
       \"))))(Tile((id \
       9d6c3981-9b75-4ddd-b042-ee85c7165bfa)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       1b54086b-cbd4-4b33-823f-38176a0f0a4a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       61fa39e0-b391-4bf6-b71a-fc7bcc7b798d)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       36beef3a-935b-418b-b2b3-79d0a10e6df5)(content(Whitespace\" \
       \"))))(Tile((id \
       b7bf5a5a-1ab9-483c-87da-cafb15741a7f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1b8f3269-6c6b-4870-a3f6-0047d64e6ea3)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       21c615bc-a27c-4fa2-bc3c-a03282f536b8)(content(Whitespace\" \
       \"))))(Tile((id \
       0f214d88-81b0-4bf8-a906-fd3265a9817f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       a88946e9-c1b6-4b41-b6b1-1901c078f15c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       cdce5e61-4116-41f7-a08b-56dc90c6bff3)(content(Whitespace\" \
       \"))))(Tile((id \
       0616d312-d35d-4141-ad86-836c8404761b)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       a88acd34-2466-4c0c-a6bd-f49e3fa5b178)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1eb73417-30bf-4f52-b77b-be7b354a240d)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       a403943c-f62d-4a5a-aa74-b9fa54c590db)(content(Whitespace\" \
       \"))))(Tile((id \
       0ad6cf2d-5362-46e4-9ea9-6f7200b1ced6)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c774daa6-708e-4a59-a8a9-3381ee8149a6)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       3b56bae8-c6ec-4119-9569-32e8c2346fc2)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ae648126-ac34-4e79-9a23-099ee2d9647b)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       09fa64f2-d75e-4916-9f85-f48e607a25f0)(content(Whitespace\" \
       \"))))(Tile((id \
       08da9cd3-7b71-499f-95fc-93682a8053cd)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2786d7e3-47bf-442d-9f09-26bc945af024)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1a9808b6-7ed2-4117-b396-139795fec3d9)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       cf184dfb-2b71-44cc-b14b-95cc3a36f9aa)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       7c1b158e-e429-43ad-ade2-91b1ea1cdd39)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       639855a8-cb39-467c-9580-070a3f050e92)(label(List.fold_left2))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b54330a3-ae1b-4887-9a5c-619668258b72)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       46de784e-393a-4756-998c-6db33eb785d4)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3525066d-740d-4d6f-bb12-a1ab0fb43633)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4297d270-62d2-4518-afcf-8a1268f1f351)(content(Whitespace\" \
       \"))))(Tile((id \
       f99507c3-d13c-4304-b14d-9cfea90db859)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       7b705fff-f23a-40de-bef8-998ba85b8ab8)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       126b5f32-64b1-4c31-825c-a8bf39b2f895)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       a6c69473-11c5-427c-a69a-2bb1ea7624e7)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       89753655-4633-4b14-a2ba-b380389458e4)(content(Whitespace\" \
       \"))))(Tile((id \
       dc77aa16-7c5f-470d-8070-9343c2e4af87)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       afa2fc2a-cc47-4254-88e3-acc35c5d1871)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       860ac583-42e9-4e82-8c91-8ec64d47ff36)(content(Whitespace\" \
       \"))))(Tile((id \
       0798f375-4d49-4d69-9972-052bf568e89c)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       7f52311b-ffc0-4981-913e-427055d22ac0)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       cfa35a40-2446-41a0-8abf-4ace6c2ba5c4)(content(Whitespace\" \
       \"))))(Tile((id \
       4f48332f-7484-497b-8ae0-8b6ac7189c92)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       1e213cde-d197-4be1-a20a-04d1daaeaa1f)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       9d12addf-323c-4385-bf1f-2aa21215b183)(content(Whitespace\" \
       \"))))(Tile((id \
       8f00a4c2-f8b1-48fc-9cb7-044fcb65b5fd)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       a30f5db9-fa5c-4062-a2a4-10467b56568b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       60d0ee13-3f50-4056-b66b-ec3ba0a07f4e)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       94f2c42a-37f3-444c-b662-d062f946959e)(content(Whitespace\" \
       \"))))(Tile((id \
       2974cbec-c0d9-4cf8-9f9d-9e8d07d00a6e)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       f035cc95-0c71-4946-92d6-4b8efc74ac85)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       82fcbb9a-8c94-4b3f-a54e-2046f4fac3c0)(content(Whitespace\" \
       \"))))(Tile((id \
       e5f4707c-1c8b-40a8-b769-743be0944057)(label(?))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       4f8bc5b5-6d33-42b3-8c2a-60408e80f464)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       d68503a2-9b38-47c1-88d0-1d865969762c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       057edae7-a1b5-4bf3-b708-3ede6046c940)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       c27eb8b7-6b7d-4d1b-ad98-2f201ec4044e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       410da897-0ae6-4bee-b6f5-d59641edb99d)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       cca75b43-4334-4e23-ba48-e8ef671a8a81)(content(Whitespace\" \
       \"))))(Tile((id \
       1685c8e2-dae7-4a1e-9250-0e5361edf869)(label(List.fold_right2))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a39f87df-81a3-424d-bd14-2ea6686a1159)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a20d1aa3-47a7-43a1-a40c-2065eb4c84c5)(content(Whitespace\" \
       \"))))(Tile((id \
       cc9505e6-48fe-4ed8-b96e-3030a57384ef)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       1726d22d-d67f-4587-90a2-f74618df7689)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0532b1c6-50f1-4da5-842e-e73c7a8cfeec)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       ad55a4ca-8cb8-4af0-bcca-17336ed36792)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       caa2e601-cfdd-4b3b-881b-28f6cdb3ae0c)(content(Whitespace\" \
       \"))))(Tile((id \
       1aeb2851-4497-4f0d-b538-c8991c917fdb)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       9a618b33-bf87-4221-a6b8-5e09e3cb4e79)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       8e4d4a94-98d0-4903-b9f6-787530adc897)(content(Whitespace\" \
       \"))))(Tile((id \
       2e46f70f-537d-468a-be30-235eb129736c)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       1c372e37-cde4-4026-87a7-cdf8b5f29e27)(content(Whitespace\" \
       \"))))(Tile((id \
       069b357a-4bf0-4ae0-8148-2dcd0fbf466a)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       49009cb6-007f-4b95-847b-51c7bf45d54e)(content(Whitespace\" \
       \"))))(Tile((id \
       656fd6cf-fe1a-478f-80fc-d7fbddc688f7)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       7e95428a-492e-49b7-9bb1-8cc185d92870)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f6dd73ed-5cca-481a-9bfa-b232b46057e1)(content(Whitespace\" \
       \"))))(Tile((id 7d8ac8b0-d7dd-427f-bdf1-d770b8b58ed1)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       0c765759-d90d-436e-a3f2-1a19de45d7c7)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       02b9e5d3-8bd7-403e-8b1e-89ed0d429ca4)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       738fa2e1-6fd2-4464-90c6-ebda2261214b)(content(Whitespace\" \
       \"))))(Tile((id 6e57e284-7ee5-49c9-92b0-c41ac58f2666)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       a4561535-5197-4ecf-b0ec-f6e557655edb)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       7eed91dd-4fa2-434d-8d2f-a33cbdf14a63)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2736dc9e-f638-4658-b9e7-6188399c9ad5)(content(Whitespace\" \
       \"))))(Tile((id \
       ee059b2a-27ff-4b39-b35e-ea51bd066d85)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       a3adb137-b402-4981-9db1-b2f792a52bc0)(content(Whitespace\" \
       \"))))(Tile((id \
       1dd07a49-e1d5-4085-ba68-821a439f1811)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       dda959f1-284b-47ca-b7c9-0bbbae4482e7)(content(Whitespace\" \
       \"))))(Tile((id a746dd2c-009e-45ca-b36b-d43b6a79b73a)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       a1a9bef1-a425-4329-b69e-0d11c15d1c7e)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       3ce800e6-18ba-4469-a862-a95241e82a69)(content(Whitespace\" \
       \")))))((Secondary((id \
       67bc824c-0a44-4a42-9690-ea391fb2c30b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       fcc2bac9-e153-4d18-b836-274c914715d4)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       ecd67a00-dc58-43de-a1f9-f0839f5db6db)(content(Whitespace\" \
       \"))))(Tile((id \
       3391b6d9-6c90-4d55-96de-9e8f7bcedc56)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       aa3f30d7-d965-4f6e-8f82-8f9da4589d99)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       ce57ffcc-c7b6-4aa7-91aa-b58b19c658f2)(content(Whitespace\" \
       \"))))(Tile((id \
       4e295460-e3fc-4d30-bee2-1394ddf320ad)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a74ffaa6-e6c9-4fbb-902a-fd122658f353)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       edcdf1b6-34ea-4bb5-83c4-f594397d86f0)(content(Whitespace\" \
       \"))))(Tile((id \
       20ea80ac-f602-4f0e-bb5d-46ef228f13f5)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       0f50bd7a-85f1-472b-a44a-126560b41067)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       2fea88ad-9e15-450d-a3d2-16f5ec07f5e2)(content(Whitespace\" \
       \"))))(Tile((id \
       16d9d6e2-0d03-4ac6-8709-1f980db8e457)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d8e29026-12cc-4554-8ea2-47470808da05)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f2abf661-1e31-4eeb-86a5-cd0556d87236)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       a00d2c21-3033-48c2-a396-5dd89144a671)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       4af508c2-9f88-4ff9-81af-09dc9860da26)(content(Whitespace\" \
       \"))))(Tile((id \
       e3ebf083-b04b-4bc4-805c-d07de1e4e434)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       57255a1b-0d36-4948-b265-900ad6102168)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ac4b32f5-4e86-452b-baaa-101cae61f9f4)(content(Whitespace\" \
       \"))))(Tile((id \
       cc43381f-2d03-4496-a34f-aa14028847f6)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       bc6fa258-cd25-4c51-92b9-bac521c3dbe6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1b3394a2-1d02-4150-96dd-2749627da552)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       67568492-ae90-467a-96dc-a607a248ea37)(content(Whitespace\" \
       \"))))(Tile((id \
       7b99b3b0-95e5-42eb-b70b-d5767eed06a6)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       0732f8ed-affe-4892-bd1f-7b6d4d62dda6)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       9cfcf4dc-4ebb-4f54-987b-d2bb0a5564cf)(content(Whitespace\" \
       \"))))(Tile((id \
       db4299dd-97ba-4475-99fe-fcf391e053c4)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       fe32cc5a-7895-4498-8308-88fc1cd7bd42)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       622ac196-a04d-4b54-953b-445ae39a4f2b)(content(Whitespace\" \
       \"))))(Tile((id \
       2dc6fa0a-c73d-412e-8669-4c1c1c35397b)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       26872951-ed21-48a0-941a-103529f8e498)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       65807ce9-3aa4-44e8-984f-fdc5accd8c76)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       21ff47a2-aa81-4764-870f-1f50620ea718)(content(Whitespace\" \
       \"))))(Tile((id \
       6de2e64d-a91d-4497-bf89-09764765bd03)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       30217a3e-5486-4f62-b58f-b9ff4538660d)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       5dcd291c-6f96-4d42-9db6-c1a1133d0190)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       013dc489-88eb-42c7-ba79-d969b33f6710)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       140a609e-87bf-423a-9bc1-6720a2175e68)(content(Whitespace\" \
       \"))))(Tile((id \
       aba879fc-c1c0-4734-8870-850523ead3a4)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d24995ff-0600-4331-b5bc-952caec0c58d)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c313228a-a823-4d36-bc2a-23e8a310eebf)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       db26d17e-0aa9-40d5-8fe7-f014a2784283)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       cd54c060-43d9-424e-8c65-58ea965a11a4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e5b0fbbc-1212-4ce8-96f9-2dccdb2e5bbb)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       0fd37285-336f-4764-91a9-6092ec74da2a)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       653578ca-410d-4a71-b3ef-00e782cc4c1a)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d7a7412f-3ca1-43d2-ac41-44ed2945e5b9)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       d8535b91-cde8-4f6c-843a-083e34822fc3)(content(Whitespace\" \
       \"))))(Tile((id \
       052ad009-9e71-4ab9-9f3d-24a3d19b8079)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       976eb181-3dea-49f7-93a5-98dc166ee5e5)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       de17a9c0-a6f8-4755-952c-ce9d6124f93a)(content(Whitespace\" \
       \"))))(Tile((id \
       85345000-cad0-40c6-9b8d-889b49c93234)(label(List.fold_right2))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       540d1a2c-c7c0-491e-b6e0-3c8a4f0dc367)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       dfe30be0-6961-43d3-8064-5c15c9eba4eb)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       9f8d4f1f-c209-428e-b382-a6b6a4bd2d70)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       0261e4d7-5a93-4fae-9a21-2dfb825eeb69)(content(Whitespace\" \
       \"))))(Tile((id \
       f6bf3426-c0be-4f79-ba23-9e811c7bba4e)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       00dfb57f-b84c-4a39-8ca7-5fe8a18f963e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       c7820ac0-1585-4409-ad97-43423528e9e7)(content(Whitespace\" \
       \"))))(Tile((id \
       916a6adf-9098-4d0c-b7f6-0c117c896318)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       45c0f636-4f6b-49c7-9af8-bbb7dda9a06d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       c38323a2-5a16-4ee9-b21b-80920d9f820d)(content(Whitespace\" \
       \"))))(Tile((id \
       c47c7c46-2e7f-4af9-a321-3517d2e57f64)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       aa701dbe-c6e2-4233-afbd-35154f8f9883)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       5aa06be7-7c31-413b-8432-e8c6b24387c8)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       c34715b8-5c9c-477c-a405-d678b0b05c61)(content(Whitespace\" \
       \"))))(Tile((id \
       205d047c-8f4c-4c43-ac6c-583bfd72f199)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       52e06c1e-a6c0-45c2-b627-05c8c55b9121)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       97961260-ce17-418c-a907-6a9db7635d91)(content(Whitespace\" \
       \"))))(Tile((id \
       a701b0f5-a39c-4366-8188-69692bdcea77)(label(?))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       6825a312-bf62-4c61-8b9b-12219059503b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       6c1a0ac8-834c-46d4-ac1c-0d4793dd7d68)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       078e1ceb-6acb-4475-95aa-0da5952d788b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       a04f05a4-b1be-431f-a8d9-74099ffd103c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8b0b4ca5-a7c0-4bcd-a93b-ab4823d14536)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       74afbb67-c6d9-4f65-8e24-ce5f54568385)(content(Whitespace\" \
       \"))))(Tile((id \
       44ab70cd-de79-47d7-af06-3a883a9e929d)(label(List.map))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       575235a9-23cd-4c67-9c5d-a901580d7a60)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       70975268-218f-4200-b5ed-5b13d5bd12e9)(content(Whitespace\" \
       \"))))(Tile((id \
       f962431d-a9ae-46b3-8c27-1881cee7e278)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       f1a82224-24f7-44b8-baee-2a37278b75e0)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       2fdb73f0-4f7a-438a-94b8-bd743f3aa998)(content(Whitespace\" \
       \"))))(Tile((id \
       cac289f8-241f-4052-a556-c426b9f5b70e)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       092d6f3b-5e30-41eb-bb55-67139adcc9ad)(content(Whitespace\" \
       \"))))(Tile((id \
       2ec90c87-5dee-4574-b387-ba2539271eb3)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       2a21c79f-5ecb-4890-a8f0-d93eb5feff1c)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b7e2d9ac-4fd6-4b66-a43a-67137632f25b)(content(Whitespace\" \
       \"))))(Tile((id d876636b-b7bb-48bc-8acd-38ebe4e4dba8)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ca5b3806-6b25-441a-b424-3d2f76e56e87)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       80ba159f-77be-4fb1-b357-2b6702b5033f)(content(Whitespace\" \
       \"))))(Tile((id \
       5b0370e5-71b7-4274-aa80-0494a16437ff)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       9bb2f6c0-8e04-4a16-a48c-90d1731da6c9)(content(Whitespace\" \
       \"))))(Tile((id \
       e6b89924-0181-4e5e-a552-08e0e1d275cc)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       cb0948c7-bcb4-4afc-acdf-74338c641cac)(content(Whitespace\" \
       \")))))((Secondary((id \
       2c45a40d-8594-4924-99f3-92d58909526a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       58655fcd-e122-4cd3-9dd6-57a789d6b33e)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       3c191d0a-029d-4346-9ba2-0b4a47a560b6)(content(Whitespace\" \
       \"))))(Tile((id \
       a73264a9-09f6-43a1-b774-2e7c76c0749f)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a6329cc3-349f-48b5-b9ef-1174f2f0c271)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       77f8e566-95d7-4265-b5e4-7ea77bd1ef21)(content(Whitespace\" \
       \"))))(Tile((id \
       c3bfc5f4-d4bb-4b10-9b3e-f4b546d3a1e6)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       b03df1da-1a0e-469e-be41-91cabe7be922)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       89771337-306d-4774-9e53-22d226818388)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8f0a2841-cfdc-4fa5-bbc0-b1729ed26442)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       bef5f27c-27a2-4c21-bb3d-23c91c46bdca)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       12661b55-dea4-4309-98d5-84de89fc50f3)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       c46997c9-f948-4294-8e54-75a5212c59b9)(content(Whitespace\" \
       \"))))(Tile((id \
       d8ffdb60-f2e8-4d60-9493-c9bccb77bca0)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ccdbfdc7-4b95-49f0-aa43-c19a15d72ebb)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       71433c8e-c8b3-499c-81aa-eef67e2ce5ff)(content(Whitespace\" \
       \"))))(Tile((id \
       a59e477f-b02b-4a78-9e5c-65c78c3c49bb)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       38445248-1d6e-405a-bd6e-b5a2de24dd29)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       42f3988c-01cb-4473-adc5-6543412db83f)(content(Whitespace\" \
       \"))))(Tile((id \
       71facc98-0a56-42fa-820b-b23a5c1c74e5)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ada5f385-41ad-422d-8f90-89ecfd2d253d)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       3b37a0a7-1449-43c3-85fb-74e4928823d7)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       b7279684-1b2e-4559-9fd7-176b96492468)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       eb8f9e46-2b05-445c-af41-7bd061112c82)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ccdd372a-c2ad-4550-a52b-b0a2d02be70a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       d56987d3-6315-4be4-8215-21e1c6c664b9)(content(Whitespace\" \
       \"))))(Tile((id \
       e6be359c-4c9c-4a9c-a552-56f43b4aef5f)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       be0389cd-a29c-4887-8f0c-36d71e19f2e0)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       90d5bfab-8405-41d4-ba3a-f1608a3c9d32)(content(Whitespace\" \
       \"))))(Tile((id \
       a8ca843a-8be3-4324-b9ef-a2405247b0f9)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       b55cdf60-f0cd-4ef5-812b-d7882641d9d7)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9a4e14b2-99d8-4c66-8157-7309d71525ad)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       57f643a0-4e49-4f64-993a-fbb24bce4a99)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e27df17f-b8aa-4727-a569-06b5033dcdcb)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       72129371-e589-471a-96b3-adf935d93789)(content(Whitespace\" \
       \"))))(Tile((id \
       164abc53-145e-4190-b256-5c92ee04e333)(label(List.map2))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       19095083-ef8f-4ac2-805e-e4031fd61bb0)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       888332ec-4a5c-4d9f-901a-9fdd0c2e81e6)(content(Whitespace\" \
       \"))))(Tile((id \
       e0e2807d-3ed6-43d2-ae74-7dffa27bfa3c)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       c83eb9ec-2778-4e01-9724-9eca26c3dc53)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       08875fa9-5320-4bf7-b0d2-f956bb20ba6d)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       11c5db7b-0c56-4542-807b-e69e2643a216)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Tile((id \
       0e40002a-2965-4ff2-9ef6-f071e4308271)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       ac0a976f-797a-4c86-befa-c37f9a9af11e)(content(Whitespace\" \
       \"))))(Tile((id \
       2bc943f2-31c6-4d7d-94fc-a231f1994876)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       dc4b03a3-592d-42d6-91f8-5c69e2eae091)(content(Whitespace\" \
       \"))))(Tile((id \
       4c020137-8c21-48df-81fc-1ec5eceff426)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       9330a0e8-1811-4e41-a863-a30f656d7b56)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       94360885-aac0-43db-893c-514d307d2926)(content(Whitespace\" \
       \"))))(Tile((id ea5ac017-7f47-4a52-9028-7097bdc3bc99)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       6506c350-18ea-489a-85b9-e96f9d07fa05)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       9de19e05-6dfc-4359-9b74-7f28c0b6406c)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       c14fad46-17fd-4a00-9dce-c36797bcd332)(content(Whitespace\" \
       \"))))(Tile((id 1bc62dc2-4acb-4aaf-b3eb-1ead40703af2)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       d0b58a6f-8550-4dff-a07b-df117eba277c)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       842055eb-d57d-4d62-a950-276f178ad018)(content(Whitespace\" \
       \"))))(Tile((id \
       3f6a0b8e-b1af-4385-8a01-8d58bc61e951)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a284c3f8-2fa0-4655-838e-b9e97c23abee)(content(Whitespace\" \
       \"))))(Tile((id 6925d05e-53f9-4027-b739-a3d13a5bf46f)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       87dca4cb-f698-45c0-9e0b-272b5b902c80)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       039012e0-fbc6-4b2d-ad79-60976c8b1a73)(content(Whitespace\" \
       \")))))((Secondary((id \
       da1d3b6b-92e0-45b4-954e-9978041c68ff)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       b9c10f0b-f74a-4113-b172-48394097ce36)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       9516ec1e-dcff-4043-8fa9-d009c8c77301)(content(Whitespace\" \
       \"))))(Tile((id \
       587be5f6-1e08-42aa-9183-7ec83707be78)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b3c756b6-ddbe-47ae-88cc-371edc87052f)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       7589deb7-49e4-461b-bdaf-d85661b9c64f)(content(Whitespace\" \
       \"))))(Tile((id \
       9297caf5-28cf-4233-a311-173e8bf07c08)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       89bf716f-19fb-41c9-a5d3-f2159123c4e1)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       c54c772f-5de7-423e-a041-f0a87f6cadc9)(content(Whitespace\" \
       \"))))(Tile((id \
       9874e48d-ba6b-48ba-8d80-2ac36e937cb8)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5ddcaf77-1910-4302-80ab-810a15cf44aa)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       1c11a98e-0f8b-4acd-81db-329962531994)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d8c8b7d1-2ad0-499b-b231-4ece6c78b2ad)(label(List.fold_left2))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       72edd2ec-0d14-4228-8a63-eb022d56dcad)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       30f0f96b-167b-4221-a40a-80f0e120cb5a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ee50d803-d52f-43b0-8d5b-a70a20a29ee4)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       4fd99802-f003-4933-9be2-63ab6351f5f1)(content(Whitespace\" \
       \"))))(Tile((id \
       bee0f96a-789b-4aa8-ae69-c643f8ee6cea)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       af822e15-55c8-4c3f-ae65-a96f039145c8)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       0f9ccb5c-2e40-4132-93fc-10e820cc7b7f)(content(Whitespace\" \
       \"))))(Tile((id \
       4a4c1909-748f-465e-a4a4-4fba5362e2bb)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       702c4930-b8e3-4a16-a132-5f2c1d8836dc)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       61fdc398-7b74-46e0-a6ae-8ae509c0b0af)(content(Whitespace\" \
       \"))))(Tile((id \
       269b6b9c-2b7c-478b-919b-8179910b0079)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5e80df08-6386-4213-9fac-294f72f01ec5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3f8ae36b-d1f2-4f98-a9f3-bd348e07b414)(content(Whitespace\" \
       \"))))(Tile((id \
       460add79-1d50-48ac-ab3d-9076fe401f82)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       07f9abd8-800c-4f53-b740-d5654568e6a6)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       d437308b-5ffb-4578-b4eb-51867050fc26)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b47c7f0f-8015-4ef1-92d8-dd2250f4c4dd)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4df8daec-84a4-4446-8a99-0bf95d82f565)(content(Whitespace\" \
       \"))))(Tile((id \
       a3e0bfe2-f38d-45b2-9c9b-ff2c2b8f8588)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       812c9bed-40b1-4cc3-8f35-4ed6be827908)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       47293a82-c38d-4257-ad96-d76ce75c70e2)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       8db85f5a-9838-4a81-8af5-abdc161bc795)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       22768d40-f02b-49a0-950f-586d3ea50bb2)(content(Whitespace\" \
       \"))))(Tile((id \
       b7bb3406-b81c-4307-8df7-498944c22b72)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b071ea5e-d04c-4d02-9876-96e8217e5daf)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       c9bc7a48-7fc4-44b0-ae47-722b0fd59f8d)(content(Whitespace\" \
       \"))))(Tile((id \
       69c8cab3-c353-49f2-a0d0-3309bf115732)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2dd3df83-74b1-4c16-8c89-1fe11eec4cb6)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       0e898930-601a-48d0-94da-e5e4f63bdec2)(content(Whitespace\" \
       \"))))(Tile((id \
       b5c880c6-2d34-43d4-a95b-f208903b48fb)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       da2b4c49-cf1d-455e-b02f-74f15dfb1a0b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       01665bf5-9668-44eb-851c-1096d382f885)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       7922a0e0-45aa-4364-af53-4e2c005d9ce8)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       0bc31e1e-de98-402e-9ffe-e7cc421ef27d)(content(Comment\"# Keep elements \
       that satisfy the test. #\"))))(Secondary((id \
       43f6e7b4-b9f7-47d0-ace6-06792625efc3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f187e166-819c-4540-96c9-40c1a2f0fffa)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       e8012e68-ae4b-40e0-8f97-139b65a6e85d)(content(Whitespace\" \
       \"))))(Tile((id \
       a1df94f0-6aac-4104-b8aa-32713d3f83c0)(label(List.filter))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a8e39412-4f17-494f-8733-629858e4295e)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2fe56551-aabf-4c73-b70d-52f5191b5c60)(content(Whitespace\" \
       \"))))(Tile((id \
       2072cb4e-e913-41b6-8feb-6d372335d600)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       329f7801-b72f-4009-b2d2-cb8a08763666)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b75e8278-96f9-4f89-afc1-1dd6a0bd67d6)(content(Whitespace\" \
       \"))))(Tile((id \
       50e6765e-c4ad-455f-ab98-e021db11186a)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b31b7d80-e1b8-495b-a246-2ef718de09db)(content(Whitespace\" \
       \"))))(Tile((id \
       a008541c-b2f9-4b91-a73b-b151467ebc09)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       b49ccc8a-8fc8-436e-9674-d04609e61ccd)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4af96e16-0083-4ecd-9c1d-c2704f7d11dd)(content(Whitespace\" \
       \"))))(Tile((id 398102bc-0933-4423-ad99-26628d792cb0)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       78b781ac-6804-4c16-b333-e18bc240895f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       5d38ae5e-ab51-47ec-b6d5-92ad2d2438e1)(content(Whitespace\" \
       \"))))(Tile((id \
       1e16df57-c58e-4cc5-869c-d74fe4a5a608)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7885fb96-28f1-4988-9417-ab7e51eeeabf)(content(Whitespace\" \
       \"))))(Tile((id d08950ad-de50-4c5c-9325-6d474f296d9d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       d5c71a07-8a29-4656-9716-cc002412a879)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       f45a4214-25b7-41a3-b416-73d9566ca392)(content(Whitespace\" \
       \")))))((Secondary((id \
       40258272-466a-460d-8845-af5f6505e781)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       bd77e552-0e24-4122-b7d5-7e57cccc4501)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       9592a9e5-401a-4ec0-8e1d-d3eb59d37f5d)(content(Whitespace\" \
       \"))))(Tile((id \
       e19f18e4-aa96-47bc-912c-c670e0ebc392)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d82cf21e-9515-4dde-b0b2-797d3a6d6a52)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       a3122487-4903-402e-8894-0769336e386b)(content(Whitespace\" \
       \"))))(Tile((id \
       50ef1224-e639-4acd-a8be-3a649b7a8b6e)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       49371122-e010-4f08-9f94-fff3fa9df84f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2deda621-2c0a-4ed0-96bc-a88ae43f6bb4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3528f013-c57e-484b-89c6-a5b3a5167b03)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       754fcb6d-ad4a-423c-a310-03d8f08f006b)(content(Whitespace\" \
       \"))))(Tile((id \
       07e2a069-c2ae-4520-afad-55089e98ab1b)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       4d6f56ae-6feb-497f-be37-bd48882fad81)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1bb10aef-f4da-45e7-b078-0765a0aa9470)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       1989a7c6-a077-4bf8-887f-4fd5490cf856)(content(Whitespace\" \
       \"))))(Tile((id \
       c4a5cab8-25c4-4a4a-b02c-d56ff16da98f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7eb249ae-1dd4-426f-807b-0da28aa6afe9)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       356d5128-a303-4304-b6a9-3fa0dacc4468)(content(Whitespace\" \
       \"))))(Tile((id \
       a9ee042c-fbd3-4f86-8837-aced7123c53f)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       e41e019d-b6f6-4e7c-92fa-6457b400b39f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6786bac1-d522-4dc0-b784-3ec163bea880)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       4a066100-f048-49f3-bdd4-e03375d09fd5)(content(Whitespace\" \
       \"))))(Tile((id \
       f294d1ab-ab48-4c88-b983-f4c17802203b)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       964a3777-224f-4812-bb67-dadabe6e330e)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d47aa90e-2be6-4e2d-abdc-cb87511a6300)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5927cf78-ee31-48a4-a639-48d247639fe6)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9e0d3bc9-98f4-42ed-8472-145301de67d1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3bfff4be-6f8c-4696-98fc-e68fa32502b5)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       60f04f49-52a3-47ab-af33-678300185d1c)(content(Whitespace\" \
       \"))))(Tile((id \
       8c4f9dcd-1fb9-411f-bc38-78de8d273c6a)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       a5cee80f-3f94-4a70-815d-a89e73e5e4a2)(content(Whitespace\" \
       \")))))((Secondary((id \
       6d97fcd8-7d57-4e65-81b0-66c538624e38)(content(Whitespace\" \
       \"))))(Tile((id \
       2ee8f18e-9b76-4149-a1ef-9b97f89deafd)(label(List.filter))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       166d3df3-59bb-4105-b1c1-eea2c831854d)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       9be87763-0dfc-4919-96ba-cdc8d932d58e)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3eeb23db-2444-4eaa-a940-eaf920cf9561)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       35c749e3-12ea-47ac-8ee7-e3f18cf6581f)(content(Whitespace\" \
       \"))))(Tile((id \
       ff8b69c7-93c0-4d3f-8768-bf7bb623e264)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       75cf96fd-5220-4d70-b2fa-27c5e7b74984)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       a23b1b46-3e3b-4887-bcbc-ce96af93a2dd)(content(Whitespace\" \
       \"))))(Secondary((id \
       36d76fd4-8e03-4f0d-a426-82886deeaa4c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       c8e62018-c37d-4379-a77d-166707ae13cf)(label(if then else))(mold((out \
       Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       53c44116-b52c-4f70-adce-9a8461a1ae74)(content(Whitespace\" \
       \"))))(Tile((id \
       6064fde7-e010-4ce7-9a2f-ac2613f340e4)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5dd3e5c9-7df9-44e1-a5ce-0bc59f23fa61)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       dd9a72da-2501-4ab9-94f8-35ec859dd5b8)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       759a2e9f-8e47-4815-9a62-88788cb0aaba)(content(Whitespace\" \
       \")))))((Secondary((id \
       a09fca7c-1ab2-4b78-aa68-0a3945a4d053)(content(Whitespace\" \
       \"))))(Tile((id \
       fe7ca023-5e8f-4bb9-b6ce-1b02e532b198)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       eabd37e6-e4b6-428d-9938-9de6415622d8)(content(Whitespace\" \
       \"))))(Tile((id \
       e4bb41be-634a-4292-beca-e02870660ca3)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       106e2673-ed71-4ba5-8d99-f69b604812a7)(content(Whitespace\" \
       \"))))(Tile((id \
       50a3028e-5692-42b2-a583-4f1c77f8f413)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       a83bb553-afe4-4877-a85e-d1c2581c7087)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       343259d8-6c02-484d-b984-a36777424a77)(content(Whitespace\" \
       \"))))(Tile((id \
       b83e3f57-8896-44b4-9d69-b4601db0c304)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       c74b0e7e-bae0-4d24-b63a-068fabe427ec)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       474abbfa-18d8-4fe1-bcc3-edf699e0e139)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5e333df8-b5dc-4c9b-9b1d-8deb0fb48712)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       cf7d7aa7-fe3a-40ca-9c8f-fe625f4a3304)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       31e0f3d5-b00f-4644-8309-c3d21e45d6a3)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       6b5d8b02-a1eb-4344-877a-4e47a5610b38)(content(Whitespace\" \
       \"))))(Tile((id \
       e806abba-38d3-461d-b322-4211a6855961)(label(List.append))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2e4d6cc4-83ff-4986-b8b6-8c7fa5fba7db)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f0ac4ee6-cda2-4bcc-88d2-e26cb2c56f24)(content(Whitespace\" \
       \"))))(Tile((id \
       519d0dca-81c3-482d-b075-9fe8e3d04f2a)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       e5adfa36-4aa4-4eba-a16a-8121c9449d70)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       b1a0eccf-3ab9-4eea-bfde-6b0d1b66a898)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       8082d1dd-f88d-42b8-830a-6a610f6cdfcf)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       4ea872e1-7e5f-4610-9b87-2b52bfdec6bb)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       d465782c-a426-49db-a045-c57a21155d59)(content(Whitespace\" \
       \"))))(Tile((id bf0b8190-c8f7-48f1-93cb-5464602c7e72)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       13491ef7-90ff-45e1-be38-a5e0e1711144)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       1e43abfb-9a1d-4265-b577-824bfb2ac8a8)(content(Whitespace\" \
       \"))))(Tile((id \
       4878ea00-8d30-41fb-ad60-5e9f9a04a002)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b96a3dfd-5fb1-4e4c-bcab-85fe4dddb0af)(content(Whitespace\" \
       \"))))(Tile((id 64f4cc66-f82d-4f1b-89c9-988b9fd53e59)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       6e4dfad6-e1db-449c-b211-1c446887fc63)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       a06e6e28-eeed-449e-96f2-92adac70b533)(content(Whitespace\" \
       \")))))((Secondary((id \
       ba2e96e0-0a34-4f11-b8b0-1f1774583bf8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       10f9666e-32e4-41b3-a3e2-deb87355bf85)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       293daff5-8d86-4a79-8681-515026b516a8)(content(Whitespace\" \
       \"))))(Tile((id \
       f71a8de5-4504-414a-9f20-aeb1532d277a)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       647cd2b4-e7cf-4b2f-9590-6583778ffcd2)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       ae1b3b33-38f9-4af1-ae76-98c62747f69c)(content(Whitespace\" \
       \"))))(Tile((id \
       065c11e7-3bb1-4a32-a5e9-0a40b96dc601)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       c88773a3-c067-4577-9db2-d88cacb950fb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2abcbecf-4c0d-4191-b676-c011266f0c01)(content(Whitespace\" \
       \"))))(Tile((id \
       247c8f1d-234b-4cf8-b925-93d34a07a31f)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b674258e-dc64-4a55-a4ee-9ac406e51ec7)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       582f50c9-7386-48fe-9a9a-c82f5b56165b)(label(List.cons))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6210c27e-b56b-4893-bba4-f7b151a9ff96)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       b281bbed-c9a0-4352-bd37-96fa9c323132)(content(Whitespace\" \
       \"))))(Tile((id \
       64666979-1424-487b-9d28-92130c73e577)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e6761764-4a92-4884-a8f7-c9ee331dfc4a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       db3c0657-b1c1-4ac1-a100-d82056edabfb)(content(Whitespace\" \
       \"))))(Tile((id \
       b72e6815-c7e1-4b19-abad-83502caf426d)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       fb9caf6e-5677-4cd3-9c76-103aa0ed59da)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       6c5e0b42-5b96-407e-9f91-0e7f62b97e84)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       7b30ad63-41e6-42b7-ab06-955b5f73591f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d0977afc-e4ad-43f7-a18c-26eedf8685d4)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       aa3adb9d-bdd8-4322-882a-5667a5de5087)(content(Whitespace\" \
       \"))))(Tile((id \
       28bc05bc-9649-4d63-ae21-62ed59bba9f8)(label(List.concat))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c42003f5-96ae-4acd-bae6-fb40eb04e4ad)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       242fdd43-d8c4-4cab-a2d0-ac69190845e1)(content(Whitespace\" \
       \"))))(Tile((id ef07548b-9c33-4785-827a-38d7aeca581b)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       6539e596-7204-4982-84ab-691531ad6109)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       8d1e524a-79bf-4d2a-a428-6e36b31a5572)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       4a56ebd6-ccf1-4e66-9390-48bcac3780c9)(content(Whitespace\" \
       \"))))(Tile((id \
       a46b5102-cb25-4002-9f83-506709410251)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a477a792-7c99-4e03-b14f-f2936db0d11d)(content(Whitespace\" \
       \"))))(Tile((id 9dc39a81-1f85-4820-823f-b43f67e7fe78)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       4b2ad24a-010c-4f0a-bc25-62f2f3b03d94)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       9039b0e1-342b-4761-afc7-8175c7a103cb)(content(Whitespace\" \
       \")))))((Secondary((id \
       420dbe8a-74c5-4173-aa3f-382ecb9e65e1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1f812a84-80aa-4cea-be7a-8ed847789463)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       c1ac7780-c871-435c-a24d-6bec2f849e5d)(content(Whitespace\" \
       \"))))(Tile((id \
       8583f40d-dabc-4c40-a8a5-e4d7d2bd4157)(label(xss))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       529e8878-2249-42d3-8aca-14326319ca34)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       1288f289-c32f-48ba-8688-ec07215b6554)(content(Whitespace\" \
       \"))))(Tile((id \
       ef1bb0d8-614a-4ab6-b4a7-69604418ed94)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2a05ae6d-6632-465d-b637-0d1916fd9701)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       bfa06e47-3f6c-4c0f-b12a-54dc86c274b7)(label(List.append))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       9b2d2356-a328-4cad-bf52-a612be52bf10)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f33091b7-0212-47c7-8e28-f703201bb8ee)(content(Whitespace\" \
       \"))))(Tile((id \
       014a3384-789e-4b4a-85e2-cba3e13686d2)(label(xss))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ac6277c4-7fb0-4731-b41b-6c18e7cfe12e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       47d4e569-a9a3-4382-a47d-97088183d4db)(content(Whitespace\" \
       \"))))(Tile((id \
       6afcd8b6-a498-46f7-8595-ec5b2b78c8c1)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       74db2d00-34f8-4f94-95ab-70ea1e75bbcb)(content(Whitespace\" \
       \"))))(Secondary((id \
       16205e01-faac-444e-b531-14186cd8562a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       887f0c92-dc6d-468e-b583-158bdca02015)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       db6e18f3-762d-4102-a6bd-2f376929de66)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       37d592cd-ba3c-4b96-bd09-6cbf5d8d1975)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       7139d234-7b76-4988-80dc-16830ad00ea0)(content(Whitespace\" \
       \"))))(Tile((id \
       a8f6c67a-9674-48d5-8a3c-0ca6c6394f42)(label(List.flatten))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       83c60451-8111-4457-befb-7939def6474f)(content(Whitespace\" \
       \")))))((Secondary((id \
       f7420f16-e132-4953-a834-bb9087dfe8ba)(content(Whitespace\" \
       \"))))(Tile((id \
       bf1aeaa3-c887-4ca4-ab9c-5e723077dab1)(label(List.concat))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       f9810d97-f4cc-40fa-b946-526670368894)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e127c073-8e7c-403c-a7ea-dc9267e6b072)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       bc1d9035-44a8-4af5-b255-f7c83832564d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ab245f56-2ca6-4de5-9c26-76d96b4e5c71)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       518d790b-40dc-4344-85e2-9374b9c1bcba)(content(Whitespace\" \
       \"))))(Tile((id \
       3dd68901-ab6a-4e16-9e78-7fcfec14dfdc)(label(List.mapi))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       e6682305-d425-4b4c-b55c-93f6a12babe7)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       8992f68e-3f9a-463f-bf20-5e23450a4320)(content(Whitespace\" \
       \"))))(Tile((id \
       bd6b1c32-6839-4267-9a58-dfc1db3ffcb5)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0a19f7cc-0784-44d0-8a37-a7dfbc4a451e)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       2c15af81-dc6f-46a1-b27d-ca903b7b69b5)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       d0625a21-837d-4181-93d4-ddd17981b2ce)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4cfbe054-af52-4b9a-89c9-46b6e39c59f4)(content(Whitespace\" \
       \"))))(Tile((id \
       7b6a4c31-bf5f-4e43-8aba-92228c37e52d)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       24af0923-daf6-47c4-bfad-49c39c0fa598)(content(Whitespace\" \
       \"))))(Tile((id \
       301022fa-bbc0-4920-ba28-edc374804726)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       feb000d7-debf-4fb5-93d1-911cbb75ba2d)(content(Whitespace\" \
       \"))))(Tile((id \
       797d65fc-4507-40ec-9e75-3c0cff1cf1f4)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       1b0fb158-792c-4a16-bea2-4055e2302234)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       d5db64a8-db53-4444-9e10-c0bb33494809)(content(Whitespace\" \
       \"))))(Tile((id 004bc87c-3012-4bbb-913a-ec9d7c8368be)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       bf50ad4d-3a4b-489e-aa74-7aa7230e0e30)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       23e24f62-c905-4358-b642-f8930d9de170)(content(Whitespace\" \
       \"))))(Tile((id \
       cffe8c0e-4b85-4874-82b0-5ed168f2bac1)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       c0b6c157-99ee-4893-a2a5-6979bb3adbd8)(content(Whitespace\" \
       \"))))(Tile((id 5956d529-d1a7-4020-9799-f270198812f9)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       c6fe0960-659f-4b85-b367-94792c73022e)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       ef9e62e2-0c13-41e1-8fd8-c92ea0598f28)(content(Whitespace\" \
       \")))))((Secondary((id \
       27e73a5c-95dd-4479-905b-c45245df6477)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       5c60b63b-e0e0-4ab1-bfe3-cd9ba014cc01)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       c52c2a96-c5c7-4ecf-90bb-f7eaea24c376)(content(Whitespace\" \
       \"))))(Tile((id \
       88367b5d-8cf6-400a-90c2-09e97bf451eb)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       0399d9c3-bf89-4293-b7c0-6df05c8d3d42)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       e0020918-6f07-46ac-be47-edf40b07cbbb)(content(Whitespace\" \
       \"))))(Tile((id \
       a4c43491-1ba7-40d4-ade5-a2f9d0d4faf7)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d7198de8-35e2-44d1-a4ed-796bdb4b2b3f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       cc4e3cd9-4aee-4383-909e-d766230ae5c5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6827ee45-cecb-4ffb-bc92-2976219b86f6)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       d64f2f39-3ca4-4241-89c5-883f4be40084)(content(Whitespace\" \
       \"))))(Tile((id \
       116bfce7-c586-4d95-bfe8-2912b69a0f8c)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       09e730dd-9653-4e96-aceb-41939f8048a1)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a80c4012-696f-4344-a37c-00323d4748ee)(content(Whitespace\" \
       \"))))(Tile((id \
       6286d73c-b98e-4f04-9bbb-af4092fd8e19)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       8061c272-9c1e-4ba7-8ad2-77839651952a)(content(Whitespace\" \
       \"))))(Tile((id \
       464c60f7-7602-485b-8c99-3e92fa9cc08a)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       f7d90801-0d4a-439f-a4e7-64e598e37903)(content(Whitespace\" \
       \"))))(Tile((id \
       de8d372e-b820-4e01-ac02-b09e40b50994)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       66324e4f-0841-4f70-bcaa-928786fcf26e)(content(Whitespace\" \
       \")))))((Secondary((id \
       de7bf76a-7722-4800-8bce-dd94c2b8925f)(content(Whitespace\" \
       \"))))(Tile((id fefb7cb8-4d99-4d4e-a4e3-9002060e6496)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       2adab172-e1af-472a-b2a0-ec07a2e59d62)(content(Whitespace\" \
       \"))))(Tile((id \
       96b463ea-9d7b-4cee-ae74-f70b7abdca84)(label(idx))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f054689d-cd14-4dc8-8873-11b57fbf8778)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       4adba4d7-0a05-4f1e-a6bd-5d3bca16d240)(content(Whitespace\" \
       \"))))(Tile((id \
       921cfcad-faea-48d9-8b8c-b13a7ed9d3fc)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       52c21e43-dac7-483b-8f92-b3c444000102)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       ead56ad0-aca8-4242-885d-2bc9c0d842bf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       b170f5e5-dfa1-4ca2-ad96-ec680b97f6e7)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       a4ab2eaa-ae1a-4066-a681-d0a131269eb2)(content(Whitespace\" \
       \"))))(Tile((id \
       23f71185-db19-49dc-ab21-0a1f76552a42)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       ce6ef122-a155-40ff-a6fb-7fd3660ddc75)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0f02556a-dad6-486b-b834-c32065662ead)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       5d5ae26f-1d2a-4cf4-8713-8642df97f4a3)(content(Whitespace\" \
       \"))))(Tile((id \
       7ffccb0f-ee77-4d36-9004-372f50717e40)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       13794727-7f9d-4da1-b4cd-80e299705f79)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       67ebc512-9b82-4e2e-97cd-65b922e14897)(content(Whitespace\" \
       \"))))(Tile((id \
       0e35198c-c697-4914-93f6-090ec5a0f97a)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       9f921a50-7bd7-4039-ac79-d9516e13cd03)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3c3f4ae3-0687-4001-9283-32b513038bb1)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       84c5fc53-8bab-4e4b-9158-b6ed2f332b2f)(content(Whitespace\" \
       \"))))(Tile((id \
       de730187-0cec-4820-bac6-bdf7c6b09c7d)(label(hd))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ed5158a0-101e-4852-8177-a02f305fe26c)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       fb276840-68c8-468f-9524-107db5126540)(label(tl))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       22bb83c1-48e9-499f-b049-917e14ba58fd)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8fd0afe4-0b13-4ab1-98dd-c08e5a504130)(content(Whitespace\" \
       \"))))(Tile((id \
       df6ca675-fe9d-4b90-bb32-d54d84953648)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b950f454-cb12-4585-b37b-4fb9c39e939c)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       f1963a14-ae62-489d-a877-e9d87ff07027)(label(idx))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e7106fc0-385e-47ca-8cb9-575a38f27d0f)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       84dd9189-360e-480f-a0aa-b152f2cf3f74)(content(Whitespace\" \
       \"))))(Tile((id \
       8a089cd2-4dbf-422c-b898-58f77bb48cee)(label(hd))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       38ca6121-f58e-4590-bbc0-ffaf637d1687)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       500c24ea-05dd-4812-870b-5f9aca542b50)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       682c7ef9-0f77-44f3-a653-628590e9f1be)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       614d9f4d-4e02-4587-ae70-aa7ee27d7278)(label(idx))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       87d4acf3-8a3e-48f4-afb8-54d53d4058c6)(content(Whitespace\" \
       \"))))(Tile((id \
       a869996e-e55c-4dcc-82bb-8cfabd9e8778)(label(+))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       f9aad649-e172-4168-850f-d6440cd3a360)(content(Whitespace\" \
       \"))))(Tile((id \
       ecf3f99a-fe6e-4af6-b303-28f8def3f68c)(label(1))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       39d0ec7a-518e-4606-9c5e-1846f9a811f7)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       7a9b2a63-be0b-46d3-a72d-f39e0b696a62)(content(Whitespace\" \
       \"))))(Tile((id \
       de1edaa1-e012-4091-a9b0-fb7c5d98fd5b)(label(tl))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       36700ce9-9820-4206-82ba-46f95c857cbf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f1de9661-c1ea-4615-9fe9-aa8ef379659e)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2b4195be-65cf-4037-849f-eae320345dca)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       615f1ef1-fc19-44ad-ae58-8b5b86116dde)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       27e66eac-5746-4432-974f-9049ef264bf1)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       212bfe15-c1a9-4492-b37f-20f9e78e2c6a)(label(0))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e5a03f89-9577-4dfa-bbed-39faa7b92ed5)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       9616d6a5-39c5-47a1-baec-ccc18e26f172)(content(Whitespace\" \
       \"))))(Tile((id \
       d072259e-e1ad-4c79-bf6e-40252bbfec47)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       bcd3ec8c-887b-496b-bb9f-1ba477bb1318)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f6ddf189-5ccc-43ce-8ace-9f5582076095)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       1bb73a2b-634c-4c29-b3b1-9d88ee913753)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d813202f-3714-43c9-865d-963044dcf1c2)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       f861e6cd-266d-4e37-907c-e3ad26fa84c0)(content(Whitespace\" \
       \"))))(Tile((id \
       023a5a93-d67a-4945-ae8a-9b3960a7e6b0)(label(List.filteri))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       8636d4fd-c64f-4664-8bc3-082ca8c0c56b)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       fa784f12-fa0a-41b7-876d-3407a3c236b7)(content(Whitespace\" \
       \"))))(Tile((id \
       5804ff06-c0d4-47a9-a750-f8268ff741ba)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       31bfd418-86c2-4927-82c3-a6b4d9fb62c4)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       ec3df9e0-dc97-4e1c-a4c4-5dea6fd342fa)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       8f8d897a-14fb-4747-81c7-7a7e6e3130bb)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b36bc651-98ab-45c9-811e-665f3b5e497c)(content(Whitespace\" \
       \"))))(Tile((id \
       0bed77f6-6e17-4d05-9ef8-6e75c17cc51b)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       ab433cd1-88d2-4117-8f23-b37c40be361f)(content(Whitespace\" \
       \"))))(Tile((id \
       fcfccb52-fe56-426c-bdb7-9b94f6a638d1)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       28862831-e7d3-414f-b16a-c7466735568d)(content(Whitespace\" \
       \"))))(Tile((id \
       f70541dc-b8ba-4e70-a775-b35373426a9c)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       e7aadc9a-b617-4195-b5ed-500214355068)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b7e52383-de7b-4108-9836-94044a11deec)(content(Whitespace\" \
       \"))))(Tile((id fa509b17-d1bf-43e3-8b1d-28ea7177e6a8)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       4082d735-266f-460a-8fb3-ffbcf9cb6694)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       03aea960-1c62-4c23-afb1-650337ef55b8)(content(Whitespace\" \
       \"))))(Tile((id \
       e3c6e266-7b59-4b9e-b41e-a86de3000094)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       698eb784-6c94-46d8-83b0-2859e1e438ba)(content(Whitespace\" \
       \"))))(Tile((id 01d2d602-8cfe-4ec5-a03c-b5a8c464ef2d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       a11b0b5e-2b7b-45b4-b420-5d1245fb95e5)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       7c1a07d5-469d-4d51-8565-2f1649233402)(content(Whitespace\" \
       \")))))((Secondary((id \
       91b6d59b-1dbb-45df-ac8a-dd0ff0d409aa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       571557f5-12dd-4b05-b012-4a0edcc32c42)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       17da7218-f41b-4cf3-9334-fca41779727f)(content(Whitespace\" \
       \"))))(Tile((id \
       a858cbd8-5adf-4776-a943-59eafe2a232d)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6726c136-75ef-4333-9bdc-72e7397d55a0)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       86cb65c0-b1d4-43bf-b902-ba13fd7edf35)(content(Whitespace\" \
       \"))))(Tile((id \
       123e8835-9f9f-4c79-9551-25c1fdb301a4)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       abb1e9f0-965f-4b4f-b595-9c59447a5312)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       266e50ed-b76a-4004-97c5-0b54fdd67a08)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       fdb8a1d2-f828-47e5-8171-ae8a83d1ab11)(label(List.concat))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       170e57cb-ab6e-48e3-be84-e393cfe0ac18)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       80bf15d0-f547-4732-bcc0-46790b05d634)(label(List.mapi))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       49c26098-90f2-47ba-bd22-b8c9c0560bee)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       1cfc8b49-23bd-473e-9754-52d32ae5dd6c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ffa3d12c-dc3b-48b3-8255-c9e96aa067b1)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       8fc4c9f5-7b7c-4090-b471-f8a9af1504b4)(content(Whitespace\" \
       \"))))(Tile((id \
       dc8a536c-00dc-4d24-bfa4-60b64dd20683)(label(i))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       041cdda5-b22e-4526-84ee-aa6be3c9a89e)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       460888c1-1878-4127-9f0b-093e992906e5)(content(Whitespace\" \
       \"))))(Tile((id \
       2d6f6083-6422-4c84-8b6b-2c03a2849714)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d670d257-213c-4f85-bb26-2f2ddfba1735)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       c4af710f-d9d7-41e0-a7fa-4659f93fd4fc)(content(Whitespace\" \
       \"))))(Tile((id 423ae3e0-2605-4d26-bbf8-46a23f70ee6b)(label(if then \
       else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
       2))(children(((Secondary((id \
       33853f29-0620-4262-ae45-07da3ca89050)(content(Whitespace\" \
       \"))))(Tile((id \
       ecc14f16-e34b-4a53-be4a-d63ad622a658)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e01bbbff-842e-468b-9db7-5697f8ce7c9e)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       37a3ef2d-26b6-4d88-9da7-2ded310e17c6)(label(i))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       0e09cd07-0295-403a-89f1-43d540baba28)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ddd392bc-7ed4-4913-8e22-f0471caa9723)(content(Whitespace\" \
       \"))))(Tile((id \
       70bbab7d-7015-4c33-8ac1-e2fbbb2fba61)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       e4b5171b-5496-4f91-9e05-7a5009d99baa)(content(Whitespace\" \
       \")))))((Secondary((id \
       b39c6a26-7fd1-457e-9c37-4f2cc1164c4c)(content(Whitespace\" \
       \"))))(Tile((id 6dacda09-572d-4429-a128-9af272cfd807)(label([ \
       ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
       Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
       41fade74-b510-4f9f-8266-0e059f332d49)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       b5579839-4b2d-4ad1-983a-35331dd30a9f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       00b6978f-435d-432a-ba28-b62b21af82d7)(content(Whitespace\" \
       \"))))(Tile((id \
       82b721a5-be88-4b26-876a-0e0199d4fe55)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       75570e0f-7fae-4e2a-a549-1e5b0cdfdebe)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       5f6ac91b-5071-416b-bec5-4a68869f3a87)(content(Whitespace\" \
       \"))))(Tile((id \
       03640018-b91b-42c5-843a-8e9f85ee58cf)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       eba38e7b-0d76-4c46-bbcf-b4fba03e0723)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       785b7212-7f99-4cf8-928f-6ff8684d60c5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       320a5346-cb73-4e7b-843d-7b2e4ce6f6f0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       012170d1-5e1c-43c8-ae22-9544f596bcfa)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       aa95db0c-d35c-441d-8649-f1b1de18f919)(content(Whitespace\" \
       \"))))(Tile((id \
       8465e135-8be4-44c4-af36-704fa1d6f438)(label(List.exists))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       41418e6e-3c92-4297-83e0-c5ae5a5c7ff8)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       9a9fd9cb-d287-4787-8920-23adeb159cdb)(content(Whitespace\" \
       \"))))(Tile((id \
       582294be-6851-4ad6-af8e-3272cd6eb528)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0aaae1c8-25f6-4d25-b360-7df60267079f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       0a5a354a-4bd1-4a64-a056-41aa5eeeabc1)(content(Whitespace\" \
       \"))))(Tile((id \
       d5e90217-f769-4e46-b076-6121a3f3956d)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       18b6a9db-0b88-49c6-a95f-3542bc98735f)(content(Whitespace\" \
       \"))))(Tile((id \
       761b7a5b-658f-4e63-bfd1-d929809baf5e)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       4c049385-1d3f-4949-b8d5-9d5015d1e9f0)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       56e1f39a-a354-416b-bedc-64c934058954)(content(Whitespace\" \
       \"))))(Tile((id 28d44999-1f3f-4c1b-b6a0-c77168ba33bf)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       f387cd9a-f07e-4c04-b4de-cf128cb2b7e4)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       67469082-2114-4140-b2b3-ffde724ff9fe)(content(Whitespace\" \
       \"))))(Tile((id \
       889caae2-a251-45bf-a16d-cc28e3f9845b)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       79b17b5a-1639-4287-8eab-6114a20fd528)(content(Whitespace\" \
       \"))))(Tile((id \
       38b5e31b-7af2-4f39-b0d4-0b285f9cecb9)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       cc6e2a00-2932-4d73-8540-140a3c3e0ec9)(content(Whitespace\" \
       \")))))((Secondary((id \
       c0a489dd-edc2-49bd-80bc-4e3803cd80b0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       579a44e0-416b-4dd1-829e-7c26771cbcb8)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       3a6af4d9-b62f-498c-8d5d-0c91fbb5edc3)(content(Whitespace\" \
       \"))))(Tile((id \
       e619babd-8c2f-4235-8e89-49d7c31f07b1)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       9cb42b1a-c82b-4829-a9ba-0e5b2f2b244e)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       e229d7a5-c03d-4cc4-b218-3ed5076fac5f)(content(Whitespace\" \
       \"))))(Tile((id \
       ebcb7372-973a-4c6d-bf69-70bfba751c1c)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       f9ff2ec8-5d5f-4c6a-b563-ed530a14e2f0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       93980b18-a602-4a48-99ad-5fbb5ca4aa15)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       58be8334-17e9-4775-89ea-28d3a3706e61)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       f6a87ee2-b9dd-4c6f-9c0e-0bed94881a23)(content(Whitespace\" \
       \"))))(Tile((id \
       d425a52d-6e1a-4677-b02e-05dbcdb2fe07)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       0ba137e1-191a-463e-bbe0-05cb7c8c1016)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2874693d-d425-4b1f-8e52-9c13e487d483)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       c60e7459-3425-448c-97f7-9bc7dc0c2357)(content(Whitespace\" \
       \"))))(Tile((id \
       b2437910-294d-4e38-bf46-85180f74095c)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       97d1bcee-52ca-4c3c-8170-f3cb8309a1c4)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8d790cc6-9eab-4668-813c-d0e02b55eb3d)(content(Whitespace\" \
       \"))))(Tile((id \
       4af4a5e5-2fd5-40b4-80b5-f39fd1f7861d)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       3eaa7b98-3a1d-47b9-bcf6-410846ca7638)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       4bb4fd26-1ba5-4b39-bc5b-e66c1ba0056d)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       fb3eb1e3-334b-4603-bc74-d4ed9718d7ac)(content(Whitespace\" \
       \"))))(Tile((id \
       6ad600c6-4432-4048-bc2b-bc5477443c71)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       902e62d2-c52b-466a-a973-f5ae1ec77acd)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       34f1e6c7-e345-482a-8bf8-c6dc3eb39c78)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       cdeb4b0f-2ab1-419d-ac2c-560f445d0cec)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       22ad77dd-5eb1-43f5-a024-0b9c76ee6c0c)(content(Whitespace\" \
       \"))))(Tile((id \
       2cebb87c-3d96-4960-964c-4864d21678cb)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       90bc009f-5f43-43e5-9abd-0c1c847ec615)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       f7c1961c-2030-462d-875f-40a326305ea9)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       28fdf606-65d0-4be4-859e-57c3b98c4018)(content(Whitespace\" \
       \"))))(Tile((id \
       1fcfd97d-153a-4036-8263-cd46f678b9c0)(label(\"\\\\/\"))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 10))(sort Exp))((shape(Concave \
       10))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ff523f34-9849-4eae-8d07-6eabc87ccfee)(content(Whitespace\" \
       \"))))(Tile((id \
       1c9c3894-e33d-436e-a165-21bb0aab01bb)(label(List.exists))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e49fe942-1b91-4c3d-9f86-925f1b93d9d7)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       250417e6-030b-4977-9536-2ae1659d8f28)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       297f3096-0995-4700-bad7-4aa796734f69)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       fcf23d3c-cbb1-4c37-a52d-b14087f74732)(content(Whitespace\" \
       \"))))(Tile((id \
       8fd5d5d4-f9b2-48d8-80f9-606ad708242c)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       fe862c64-ce4a-4249-a1b6-4ff3a5ef78fb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       85f2f9b8-0a79-457f-b4da-71e124b4aa32)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5df23e01-0ae4-4f43-be7f-4094c5b1026d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       c341fbb9-31e4-4095-b07a-2bfd464ff9df)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2fe9b04a-b84a-49af-bd45-8e8ae42e7303)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       9048a7e8-820d-476b-9f56-64c794d1d8cc)(content(Whitespace\" \
       \"))))(Tile((id \
       bc138331-8516-481c-a41b-23fa20a03fbc)(label(List.for_all))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2d67ceca-c07a-40c1-abe6-a946281141d0)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2094c10f-4d95-4a7a-9e5e-ef9847e4eccc)(content(Whitespace\" \
       \"))))(Tile((id \
       6888c853-ee3c-452a-8a27-a88613978ad2)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0da37ff4-efa2-4569-b3f0-5d3d6c4d47dc)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b603fa1f-e1b7-4a5a-aeb0-b2b34b841c4c)(content(Whitespace\" \
       \"))))(Tile((id \
       00eced7a-ac31-4f96-b71a-8cde26989077)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       5192584f-6f3a-4486-96d8-f462c538c7a3)(content(Whitespace\" \
       \"))))(Tile((id \
       3ef7d680-1f2c-4827-9a34-9abcfe9d766a)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       4d91c2d6-9f87-4dcb-97e5-fdff8f5a8c39)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       3ded0b59-631e-40bc-826d-05db70769a5e)(content(Whitespace\" \
       \"))))(Tile((id 93a7a414-10a3-4ba9-beb9-900406220e64)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       531a8697-f40d-4051-836e-55b5d7b2cbc0)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       9a2aa292-8eb4-4fc6-a157-b0529b6a90c0)(content(Whitespace\" \
       \"))))(Tile((id \
       7fe66ee2-8985-4c36-b1d6-949f165f94ec)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       5b42f811-2575-487b-a757-56681ad35c02)(content(Whitespace\" \
       \"))))(Tile((id \
       19e5cf67-6560-462e-8883-134b3ac489b7)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7d2aac05-4276-4db3-8c5f-ae73ba92abcd)(content(Whitespace\" \
       \")))))((Secondary((id \
       07f32b7b-30ad-4e33-9699-7a2aa36c8ff7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3e8c589b-878f-4918-be20-7502de87cc7a)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       0c61883a-0e9e-4bd3-86b7-0514fb85ebb2)(content(Whitespace\" \
       \"))))(Tile((id \
       3260b9bf-3483-4956-8208-88e1c07e3075)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       8dd4cf53-19e6-417b-9d4c-996dce5dfc5a)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       849affa7-5618-4ba5-959b-6e05f8b10b5c)(content(Whitespace\" \
       \"))))(Tile((id \
       627fb2d8-dc26-4780-b5f6-798597350acc)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       55a9af53-714d-4eec-884a-4ec794546e93)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       eca62b82-da09-42b8-8cd3-61491d2d1b92)(content(Whitespace\" \
       \"))))(Tile((id \
       20b709a3-05a7-4d94-b489-6ce0a4594cce)(label(not))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       a63d62e3-bdfe-46ed-be31-4f8ae2903ba8)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       1889b6a3-c2df-4a41-a34e-06d711d1557f)(label(List.exists))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       602024f5-ca62-462f-acfa-4716ac229ffe)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       d9929f87-40d0-4908-a720-58e77b34dd82)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       f3fabcfd-7970-4ff2-8892-c29bb7de5f2b)(content(Whitespace\" \
       \"))))(Tile((id \
       0e84bbe0-d4f1-4a12-b9a8-5f16c74af022)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       71f80648-3307-4c4c-8f6e-77255adfee83)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       6c8c0910-e862-40e4-9974-750faf72d7fa)(content(Whitespace\" \
       \"))))(Tile((id \
       927f4d2c-0573-4927-bb3c-2834d42c4168)(label(not))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c1f0f478-da8e-4a5e-9bc3-1da6d6d757bf)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       430e6829-3e78-4a09-9e8c-97780135283f)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6cfe4ac9-1dc4-4a4b-884b-ebb6b1e2d869)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       e3bdddd9-50bc-4b21-90b8-270db51e37bc)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Tile((id \
       19f36754-0bf1-4c64-a3db-980c9225b876)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       dbb8e473-35c4-478d-b867-1d0eb9c7a98a)(content(Whitespace\" \
       \"))))(Tile((id \
       9b57750f-e83c-4cdc-9940-cf1ec040f863)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       ca7dc3fd-d0af-464f-bec5-abadabde26ee)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       26824a75-ac6f-4176-88f6-3588da7b9b34)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       a31a06df-7775-4d38-badd-f5ff1bd4707b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1539837e-fc6f-45f7-b7b5-eb0ebdb3d4e0)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       dd4d88d8-fa6f-4eb1-9f31-1706216ea738)(content(Whitespace\" \
       \"))))(Tile((id \
       ac5e8de6-c73c-4444-9dde-42ce7d8cb189)(label(List.mem))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       2ea4a554-9701-45cd-a8e2-3fe1357b3045)(content(Whitespace\" \
       \")))))((Secondary((id \
       3dc7eb77-c3c8-4a95-9598-e5318399d481)(content(Whitespace\" \
       \"))))(Tile((id 898a8539-a204-4040-bdf2-e1cef402482a)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       07a25da0-c610-4136-817e-527d123fa01d)(content(Whitespace\" \
       \"))))(Tile((id \
       fb843045-3008-474f-bf86-5efd3caa96bb)(label(eq))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f8092e15-32e6-4af5-827f-3f219c6dc895)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       cd199cd9-7039-4ff0-97d8-9d95c0c9bfb6)(content(Whitespace\" \
       \"))))(Tile((id \
       ef3047a4-af2c-470e-92e0-e390fd2973f8)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4df4e488-9c37-4d86-b5d7-a0f085e8f508)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       68f96368-245d-4a3f-a406-a2ca0c07e6a9)(content(Whitespace\" \
       \"))))(Tile((id \
       f57b32e8-8dc2-4122-8a73-44049d1b855b)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       bb635fa2-11e8-4fa6-adea-695507fdb9df)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e0708d3b-eb48-448e-b9c7-03317f64c311)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7b87cb80-69da-48d6-85e3-e5a785a37933)(label(List.exists))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       555eee64-c8dd-40e2-8311-ddbd42e97d99)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       aaafba7b-02dd-47d5-8419-772a2f92eabc)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       1e22b6f5-ae84-40c3-ae28-094bf1a79893)(content(Whitespace\" \
       \"))))(Tile((id \
       56705d86-63f8-4354-b3e4-3b04e4379351)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7b63afb3-ed4f-46cc-a98a-671e0f8e37ca)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0912afaf-4d5f-4e60-9069-16ae2061b427)(content(Whitespace\" \
       \"))))(Tile((id \
       99bc3eb2-9d5b-4871-ba14-8a198dd6dc70)(label(eq))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       7bc134e5-3480-452a-8109-3434cf8cbc53)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       6da6337d-d92b-4be8-b92f-fd4a7649aa26)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       79b3533d-991f-45df-a6af-c273b48bf43b)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       6f89399d-d64f-4d45-beee-340f79424256)(content(Whitespace\" \
       \"))))(Tile((id \
       46c724e3-e5ab-4b5b-8254-3823d9b222aa)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       e26199cc-4608-498c-a42b-6c053c0de41e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       219b1e63-ae33-4ebe-a76e-9d2c98f4fea4)(content(Whitespace\" \
       \"))))(Tile((id \
       7e1d58e5-3b07-46b7-8f65-fbe99188b506)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       f8e7da2d-b4ac-47f5-aba9-f21d088fbd13)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0f822037-3bd4-4b21-9836-d84f95a53639)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       910f44ac-45c2-4f64-bdad-41631fe72368)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       07cba0ec-e20a-4a8e-9666-d01d7efeeffd)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       5d43d7a7-9ab1-4449-a96f-a0276ed63ac1)(content(Whitespace\" \
       \"))))(Tile((id \
       c7ee807c-2584-437d-89e2-1c5690342b69)(label(List.filter_map))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b99cc594-9c1e-4aac-9744-8c40d73dc4a8)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       79db0285-a30e-433e-9661-ec54ba76a397)(content(Whitespace\" \
       \"))))(Tile((id \
       c3715b5e-ffd4-413b-9b90-c15ccd947ebd)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       3a9b56db-69be-45ca-81a9-d6df914084f1)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       5c0c6eb0-180c-45ca-b7cc-86b703249a28)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       76885e2e-2412-4c38-9e49-6d4cf7db5281)(content(Whitespace\" \
       \"))))(Tile((id \
       4be98296-23c9-4711-95ea-0be628e2ba97)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       39714a74-7065-4bc0-954a-4a23457a9ea9)(content(Whitespace\" \
       \"))))(Tile((id \
       cafddda7-9be5-4056-aec5-e6b7ca14d6ce)(label(Option))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       ecc5adcd-0a51-46ff-95ab-4d552f69d5c3)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       924785f0-7b30-4e95-9cae-694cc5701c4e)(content(Whitespace\" \
       \"))))(Tile((id 32625589-07b2-4a5a-ac60-770a50c0b108)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       25bb6186-edf8-4e30-9cd8-bd6e18a08cc6)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       91b2ec47-9545-4288-9151-128cbcf271a3)(content(Whitespace\" \
       \"))))(Tile((id \
       d72c7f02-59f3-404c-84a9-9f50ae527bc2)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       9b91780b-cf3a-4f4a-83a1-359e132d6915)(content(Whitespace\" \
       \"))))(Tile((id 69718d35-a4a0-4629-90f5-3ba44321c8b1)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       4f0dfd95-a2cb-42de-a604-09674ba6f63a)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       fe52d0a1-c6bd-4d01-9484-be2fc1b009e4)(content(Whitespace\" \
       \")))))((Secondary((id \
       07e048a0-b0ca-4c56-82e4-0a39bd2e70fe)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       97f56040-a6c7-4ac1-b389-79aa80a0dece)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       2a468d24-aaec-4847-9de7-ec0d4990e9a2)(content(Whitespace\" \
       \"))))(Tile((id \
       008a880b-83b8-4d64-96b7-61420ca23137)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       3c3a712e-8802-4d9a-ac5b-606c3461d580)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       3c2859c1-b1f8-44c2-b5b1-13ccd476b2ba)(content(Whitespace\" \
       \"))))(Tile((id \
       37869fcb-a870-4574-9ec9-90b9ac783547)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       07e476f2-ee4e-4798-9514-dcd1e4d1bb38)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       72a7377c-e743-420e-b73d-3e62b7b0a52b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       eb3c2a57-e3ad-47ee-8d99-9cbc55a9c37d)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       cc37cb94-2dfa-4728-9dfe-df03fb33cb2f)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       15bdb0b1-4fd4-4fec-9f32-ed6989627a3b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6acd5997-1bfb-4850-ad93-be01167c7581)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       0e02e120-d6eb-433c-b703-73fd7e61e24a)(content(Whitespace\" \
       \"))))(Tile((id \
       330d04ff-30b9-4468-afe7-7adc381bfd55)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       fbb5ab80-dade-488d-8c74-8f656e350740)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       55354dd1-f810-441a-8462-8141e1f7a5b6)(content(Whitespace\" \
       \"))))(Tile((id \
       b468aead-1414-4bf6-a7c0-f637ce8947a1)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       215f5337-43a3-40b4-af0f-07ebf490e2d1)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f4446642-c018-481f-b106-ea306d282bce)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       26446efa-67c9-41d6-aeac-1f4a6c796653)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       32355f3e-a087-427c-85ff-6f4640c196d7)(content(Whitespace\" \
       \"))))(Tile((id \
       160eaa36-2c56-44b2-985e-3ae6bad80941)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e8194bd9-d63b-4369-96f8-508e9af109eb)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       eb6b851a-ac7f-4971-ac4e-5296704de76f)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       77f87696-7771-42ca-94ba-3797a597c212)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       788d95f6-330a-4f74-87a7-f83bfbe97ae0)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       44c84228-c293-46d7-a2b7-a45459ab95a1)(content(Whitespace\" \
       \"))))(Tile((id \
       e89de076-9150-41ab-9cb5-4503888bc925)(label(None))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       dd53d356-9249-4765-95a7-3ac8e6d4d640)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9c7e52c0-c6e2-42ff-b22b-2cb406459fa3)(content(Whitespace\" \
       \"))))(Tile((id \
       57ce22bd-090e-49d0-aa69-fe73dc1f9775)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       87ca1303-4ad8-432f-8923-ff77149a2e33)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       05016a61-52ef-4fc1-85ad-c30d5738ab00)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       b78703a7-457f-48e4-9ee0-2f03bf5b05de)(content(Whitespace\" \
       \"))))(Tile((id \
       35fb7525-bc28-4644-824e-7394af4efb88)(label(Some))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a3dc489d-9878-4817-acf6-89f82476c409)(label(\"(\"\")\"))(mold((out \
       Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0 1))(children(((Tile((id \
       8b96f3ea-1444-41bf-a0d4-77b8796054f4)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       458a6e56-d373-4563-b33c-e48518151ce5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       c2289e13-1d44-4e1d-9434-117811755465)(content(Whitespace\" \
       \"))))(Tile((id \
       2d55fd04-4f01-49a8-94ac-63b252d5fde3)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f501a74d-a560-474d-90d8-68c136be69ec)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       1ce5f0e8-3ae7-425a-b1e8-e9391d36c102)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       58c3e8f0-e216-4781-91e2-913936711bc0)(content(Whitespace\" \
       \")))))))))(Tile((id \
       80c0214c-3522-4ee9-bad7-0ed684bcea6c)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       6e943780-6e38-4105-90e7-3e02c15d6352)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       a093d4ab-58a3-4ca5-9611-189377612531)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d5c96d14-dd01-4226-a98a-365bcc2e3b6d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       890eda66-7270-4fbe-8ed8-905f2a29965f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e8d401fb-bf15-4d88-868c-a5ef1ea3d73d)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       42df39cd-0712-4cdd-a100-b1d3d691f635)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       42b20ccd-699f-4ba7-a279-c7db4ddc31d3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       82f047b0-bbb9-4a29-8cb8-0e297823ca2e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3fd356b7-cd8e-451e-a7c6-3b46014fe957)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       4a3b324a-16c7-41cc-902e-21892ff30fd3)(content(Whitespace\" \
       \"))))(Tile((id \
       f21405b0-8e0a-499f-b0c8-1ee3cfbe3d09)(label(List.concat_map))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b260b1ff-8925-4a4a-a156-af5ead8baa20)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       d8c0e5f2-35d1-4a7a-820f-300913536e6c)(content(Whitespace\" \
       \"))))(Tile((id \
       93dea122-c103-4527-aea5-7b9f52a11540)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       44ccca59-319e-4304-9d5c-c52ad3345fca)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0cdc428c-ed6a-4b11-b970-d4a0dda55155)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       33c0d4e7-54b9-4d2c-a6db-0f490a5a763b)(content(Whitespace\" \
       \"))))(Tile((id \
       f7f0b7b1-7999-4cd5-90c7-4ef93e9d6524)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a2562652-c171-4677-ab11-2a50608ce2a7)(content(Whitespace\" \
       \"))))(Tile((id c6f2ffbe-c65b-4411-a5a3-34803091c14a)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       e3741e47-1f1a-4b2c-8dd8-8304b6e30c86)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Tile((id \
       47984afb-dac4-43ff-98e3-a05c90beb64b)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a33a620f-1eec-4358-b4db-67598a07677b)(content(Whitespace\" \
       \"))))(Tile((id bdb03a50-8672-4aa6-a7d4-d2171c70dc8a)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       d9db5762-f9f6-4a7b-9c1a-6a0c48411dd0)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       882edef1-c7e5-4711-9019-bbfa373e7881)(content(Whitespace\" \
       \"))))(Tile((id \
       a83d0b6d-1eeb-4339-9ae3-ec5ef12f7bd7)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       1f48f429-34ac-4626-84b2-baa3356c2ea0)(content(Whitespace\" \
       \"))))(Tile((id ef584e0b-3ab9-4b49-8061-a72d6dd5c3ed)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       c6d7f634-795e-436f-ad12-4bad1f8f965f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       0dad4342-31bb-4580-936b-df3b493f32b8)(content(Whitespace\" \
       \")))))((Secondary((id \
       897125e7-e287-4c3a-8073-f82ea11552a9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6ec41733-b08f-42ac-aa1f-c3c63a958986)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       212e5ebf-9356-462b-9912-333d812f4522)(content(Whitespace\" \
       \"))))(Tile((id \
       b6370a5c-289b-41e4-abcf-cc718402bf75)(label(f))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6271a06c-42b2-4ddc-bd3c-1cbb00863886)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       2e676117-69f8-4b55-b381-44739f532f8b)(content(Whitespace\" \
       \"))))(Tile((id \
       ca69322a-e344-4818-934a-370a95d9eb82)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7572cd02-f1aa-4a73-bf5e-f5ce6e4fddc5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       76f375d7-8b7e-4bf0-a760-74559333a2af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       bb64a54f-9c11-4216-b735-e4b24408938d)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       91b94feb-32e1-41d6-a8b1-92f977cc0f5e)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       0f3715a2-f085-4044-a18f-6c08a652f85b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f24ea694-4d87-4b47-a154-757ca16ecc79)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       3a6dbac8-5369-4e87-84c6-601ec030ac0c)(content(Whitespace\" \
       \"))))(Tile((id \
       42b44e92-fbce-4fd7-b777-6b23fad88232)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a3a45008-dc45-433e-b2e5-be3573af91bf)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       cceee857-6093-404e-bd51-ecd49e8ab1fd)(content(Whitespace\" \
       \"))))(Tile((id \
       87e5864e-f5cf-4b64-a8dd-2e96af20289e)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       70523fa0-2575-4006-b0d5-e49683af80e0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       1d533eca-7077-44bf-b70b-8b57f77831e7)(content(Whitespace\" \
       \"))))(Tile((id \
       d12d569d-6de1-4520-9bfe-460d29235dd8)(label(List.append))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f75ef9f3-f54e-4075-af08-d694ed636c12)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       06fceb2b-ac48-4deb-bd73-cf69ba6143a9)(label(f))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       4a39ae10-5df2-4433-bf54-2073dfd811c8)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       2c1da8b4-d988-4e43-8f1e-64a6fcce0a7a)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       2acaa331-c1f2-40e1-b42d-3855bcd688e4)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       002c0ac5-2c8c-484d-8caa-e97dab643d5e)(content(Whitespace\" \
       \"))))(Tile((id \
       728a1813-6210-4acb-890b-a5a6ec1429a9)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       b1f55162-d842-4722-b55f-91d43350595b)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ed7b1e28-daab-466a-a0ab-68301fa5311f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       830d3fd9-3027-49bd-a353-9dd9fb2f1e40)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c1fb58bb-b594-43d9-a219-3f53257337e2)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       b54dc988-db10-46cf-a95a-29858a8e4247)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f98a6d81-9e43-4f38-9716-895c9dbc9ee7)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       c39e4833-b85d-4ef4-9016-d620fd2f04f3)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0236f49a-c85d-40fd-81ee-fa320b712527)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       4eea5020-8672-4278-86c1-24f2b3592406)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       61f01469-f8e9-4b06-a6a8-187265a85070)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       b9b00d00-1dd5-42e9-af40-a7a0d6a9f28e)(content(Whitespace\" \
       \"))))(Tile((id \
       7debfcf4-61a0-45ab-af36-061d9f32d923)(label(List.for_all2))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       0d6aa9c2-731e-4413-8344-2ec921a8ddf1)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       b6defb77-d5db-4d91-8b4e-4cb1962771ba)(content(Whitespace\" \
       \"))))(Tile((id \
       b5d69e4e-5498-4b80-81eb-c96647e5c49f)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       7a12f727-69be-4c84-8b8c-52e162e8dd60)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       f9ac0396-3979-484c-9167-8b674024e3c5)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       039e829a-53a7-4659-9d89-b9d88c7b7796)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       8fc63928-48f7-470f-8981-d37160d506ce)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       0e0fafb7-0803-4b20-b35e-e736414bdcf1)(content(Whitespace\" \
       \"))))(Tile((id \
       18fffdc2-d3ca-4fc2-8ce9-cba791a7c436)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       90d7fc5a-186f-43f4-afdb-be27ea0a09c0)(content(Whitespace\" \
       \"))))(Tile((id \
       9673223d-4016-49af-b826-622e73e9e5d7)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       e3647734-9112-445b-bebc-8740abe5d75c)(content(Whitespace\" \
       \"))))(Tile((id \
       aa1f1bd1-917f-41a6-8fd6-51b16b9cb8d4)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       b27c9987-087e-4697-a188-98b30334b4ae)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4b91807f-beb0-4b29-a98b-d0790d44e49b)(content(Whitespace\" \
       \"))))(Tile((id 155667c1-74e5-4147-bd26-02baa5b4803f)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       f754bce1-0e79-4d82-b3f5-1fb48803894b)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       dd09eb76-95fb-48bf-a9f3-aea4fad0ec61)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       99601bea-dd84-46fa-91e3-5b1fcba29ed1)(content(Whitespace\" \
       \"))))(Tile((id 9976f4b3-3a2c-49a0-8f1e-004451dff5a5)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       9963a651-c083-427f-9f18-50addf79e129)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       5fa4f305-e431-4de2-b83d-1c8ba1d81f69)(content(Whitespace\" \
       \"))))(Tile((id \
       0bd296c0-2c4f-4630-8293-391084c5ad28)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       f99d6384-679b-47c2-b195-3c8ccff6e2d6)(content(Whitespace\" \
       \"))))(Tile((id \
       372c344e-fb48-4855-b231-60f7108b3897)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       5b704d0c-0484-4c08-b7c7-6d02f2a97272)(content(Whitespace\" \
       \")))))((Secondary((id \
       a240e9ab-8880-4e29-9e7a-e1a4c8c65c26)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       212eb137-02d7-453a-ae4a-1843df47d2b2)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       a3656c54-e3f7-4388-a39e-2f67bf2e4dd4)(content(Whitespace\" \
       \"))))(Tile((id \
       7f7f15b4-f6fa-494e-a098-b70853ef0c66)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2816c205-5878-4f6a-9b63-38e056bc499b)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       de261f22-81af-4e57-81ac-5b47abc4e54e)(content(Whitespace\" \
       \"))))(Tile((id \
       5852617f-48d7-4582-bf81-aec04621a91c)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b1d8d759-4f53-46db-a055-72d4cd1c8c33)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       3af403dc-4887-4529-8096-bf0684d63506)(content(Whitespace\" \
       \"))))(Tile((id \
       29f5256f-a236-4c8a-821e-8cb320e7cf1c)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       6ca9a644-5a4d-48e7-ba7e-7f7c0ca2c1a7)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       365c5640-0b57-4d50-b068-a414e7eb5c73)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       9c2cc186-2eb3-4c46-a6a0-cff4001ae885)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       f91a0840-9207-45a2-aa70-e8058e6eee2e)(content(Whitespace\" \
       \"))))(Tile((id \
       90ab264e-7bdb-4fd2-bc6c-cce035431652)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       1f23d685-4379-4550-b5d8-d95b0db99831)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       28a7c0ca-66af-4cd3-90ab-81f28c8c268c)(content(Whitespace\" \
       \"))))(Tile((id \
       02ff753e-9abc-4710-9fe1-e3f8c2bbc697)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       92d2e78e-7f11-451c-adfb-a8483a9fbd0a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3aa34c50-faf5-401e-bc35-014b7ddc7dbf)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       ed74da50-26a4-4a8f-9515-333dccb3e731)(content(Whitespace\" \
       \"))))(Tile((id \
       d5b4cbe0-66aa-43a4-83d9-b890f1d5390c)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       205cecd0-60cd-42ce-b879-a304b4f20409)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       8c7032d8-8491-4c77-aa0b-4e900d5df1df)(content(Whitespace\" \
       \"))))(Tile((id \
       b3bd37a8-f5f0-488c-86f8-4ece62ea616a)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5d734291-cc42-4cc0-b187-11026b121c4f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5cfdb22d-007f-4f0e-8481-fd868777de6e)(content(Whitespace\" \
       \"))))(Tile((id \
       62b09296-f2b5-4490-8d12-b09024769e5b)(label(true))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       7276d265-2dbe-4ed8-92f6-73aa3fe5a660)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       c12723f9-dfb1-42b1-a86a-5c63433f6474)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       ad211ff2-ebba-4233-9f53-9f30b273c98c)(content(Whitespace\" \
       \"))))(Tile((id \
       1d805ce3-6394-4a1f-9852-407ad82de63d)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ede0cea6-a33e-46a8-8490-aec8516ae90b)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       e9333e87-c10e-4ad6-af1a-7097503914ee)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a1dbfa68-e084-42a7-9f44-bf7a2503660c)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       d1572ca6-1e11-493e-b7dd-7de8be143b61)(content(Whitespace\" \
       \"))))(Tile((id \
       a444b47f-a487-4c9f-86a6-93c23e78121f)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f82dae14-11ea-4559-8420-0c7b6d4ec512)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       baafe19d-d0ae-45ef-8ad3-1040a8887449)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       b8dc7c5e-f594-474e-9b38-c6a4b384130d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5ece79a3-a60d-4fb2-aa3f-4c4c1ee3acbd)(content(Whitespace\" \
       \"))))(Tile((id \
       69dd876a-0a24-4c9a-a2fc-d0a732785008)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e943ebe8-93bc-4dd0-baa5-7e33993e6422)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       8fdd0523-19fb-4d16-a89b-e262e3e02e95)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2c3502f2-d1e3-4387-bf6d-1c5dfa2aa7ac)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       19007b33-d5b5-40e6-b0ca-b2b0bb80fcd7)(content(Whitespace\" \
       \"))))(Tile((id \
       86d9146b-c582-43ed-b1e9-9e02b98470d5)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       c11efd65-23f3-4622-a5c0-efe82e93def8)(content(Whitespace\" \
       \"))))(Tile((id \
       0580a46b-c1f2-4677-9382-fa4ba7e0fcc0)(label(&&))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 9))(sort Exp))((shape(Concave 9))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       cb514b48-014a-4009-8e65-7111c63944ea)(content(Whitespace\" \
       \"))))(Tile((id \
       21060667-5cdf-4677-b13b-bf84d4eba34c)(label(List.for_all2))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       9a175a66-5f5f-4ba3-9583-f2c7263ddda6)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       08c27080-40b6-4388-9553-5478d0c9af85)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6ec1ee67-be1c-4d89-bc36-1f051d71b6d7)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       e112b81f-685c-4db2-962d-3c83d3f430fd)(content(Whitespace\" \
       \"))))(Tile((id \
       312aaf71-7067-4e76-b75e-de7b7b415870)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ad6070ce-4a0e-4575-9daf-11280603056f)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       15aa18bd-8a73-4b21-aea6-f355eed33365)(content(Whitespace\" \
       \"))))(Tile((id \
       a616e6f0-3f21-4a22-95ab-e2c105ed7b93)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       235cca32-6afb-4a9a-8d05-dc1e0313f40c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       25893f9a-ac1f-4b7e-b766-ce4b7c09d9bf)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       e79a2b04-049d-46d5-bd42-dcc1a06a940c)(content(Whitespace\" \
       \"))))(Tile((id \
       d0676f65-64f5-485d-8624-b641dcebbaca)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       bb268ed6-4817-403c-8ace-c0caa9d34af4)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       a4be9f31-98c9-4b87-a592-44dcfa058fa6)(content(Whitespace\" \
       \"))))(Tile((id \
       7657bf04-b0f6-4267-9c04-777d19731616)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       06fb09df-ba40-4fb9-a3ee-ca76dea5bb26)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       85a61f9d-6314-4181-bbe7-0f96f6929be5)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4278c3f2-4d54-4756-9dda-a71113805366)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       ed2477d7-7c51-4f13-8e3a-9e7adce76292)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0add1b43-e709-491c-9936-fa0ca1410143)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       43d61120-6bf7-4596-85b4-06209bfea0fb)(content(Whitespace\" \
       \"))))(Tile((id \
       e98452b4-26e6-4b6b-950a-7f0b1432a106)(label(List.exists2))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a7a7844b-18aa-477b-b230-d291446f5f9d)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       0e4a3e18-e5bd-445f-b8b1-3f8a8c7fd78d)(content(Whitespace\" \
       \"))))(Tile((id \
       34e4b250-7594-43c8-9ba4-fb267e39fab1)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       6b6ffd6f-6b65-44d6-a28c-db99f9a49e4d)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       dcf2abce-4cae-4161-966c-318a485e3f26)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       bb087561-7873-40ba-a5c4-93eeab88a10f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       86ef1053-3b53-4f84-b7ee-cac90f38dfb9)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       cf962af3-5ae2-44f2-bc25-7a12d4bfefb4)(content(Whitespace\" \
       \"))))(Tile((id \
       d2734ee8-3d21-4643-8dfe-49b18e1ab344)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       8fbb3cc8-bd3f-44cb-90b1-87c66ebc9a3a)(content(Whitespace\" \
       \"))))(Tile((id \
       e49c736a-7db2-4318-821a-de1cc77ff8d2)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       ec2d8e98-dc85-4a6f-a8ba-af734de65b69)(content(Whitespace\" \
       \"))))(Tile((id \
       87bdeea2-bc0e-4451-b2d0-151d88c13231)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       4ef9e87d-1c64-4800-a294-efc5e3f5b7ff)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       c6f03e23-b25f-4e58-80b7-a1a48424de3c)(content(Whitespace\" \
       \"))))(Tile((id b676eac8-45ad-4f44-acf4-b382ba8ab6f3)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ec3141ea-1d79-46df-acfd-c79b5e369c94)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       4c0edcd8-92ee-4192-af18-3dc0df61f3b6)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       3e7c21c2-5d46-436c-8ea4-32a0273285e1)(content(Whitespace\" \
       \"))))(Tile((id 5a9242db-0253-47bc-90c9-4b86d6052f75)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       13cc627d-1184-42c2-8d35-b73753cdb5dc)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       417f548a-ec5d-44a8-9c85-f06adec633c0)(content(Whitespace\" \
       \"))))(Tile((id \
       d46fd10b-205d-4a3e-863d-cb005492c7f5)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       bf875ec1-fbed-40e9-b416-8b6025c15543)(content(Whitespace\" \
       \"))))(Tile((id \
       58f80536-63b4-4d9a-82f1-cfc161d8eae8)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       2dda6d4a-6fc1-4925-91f0-f0929a28c9ab)(content(Whitespace\" \
       \")))))((Secondary((id \
       3534709d-2461-4853-abb6-b1aa63ecd160)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       16e5ec03-d584-4733-800a-c7b0186985ba)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       c837facf-08cf-4ebc-8370-59fa09e7a191)(content(Whitespace\" \
       \"))))(Tile((id \
       3ef15259-56c4-44ac-9feb-9663fe2c974b)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       60637218-ef11-4b37-a437-abbf330b88cd)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       adbb1088-3fa6-43b9-939b-46a164483464)(content(Whitespace\" \
       \"))))(Tile((id \
       e846eca7-0a5e-4eae-b6bf-3e64482b0439)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       86e6ea42-f1be-4b67-a680-cc5ad8646d8b)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       4ee2b67e-37c7-4bf0-998e-6563442d3596)(content(Whitespace\" \
       \"))))(Tile((id \
       08ea39da-cea6-4bf6-ba5d-0c849508c43b)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       88876f1d-297e-4f82-8733-e82312bb1d46)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       21ec0927-a417-4e96-bc38-963d3321bd92)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       fe308942-b346-43bb-ad67-f22a1f7d7d7b)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       a00b50b0-c734-4818-a5a4-4f6d227a62bc)(content(Whitespace\" \
       \"))))(Tile((id \
       654d6dea-1b13-4637-b8f9-2609f7858a67)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d36e3309-ef55-4822-9ab6-50fbc410530d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       274b5fb5-0bd7-45a5-bac7-7aa7d8ef8165)(content(Whitespace\" \
       \"))))(Tile((id \
       9d04eb1b-e5ee-4efa-8ba4-6908679cf08e)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       23cb72e7-ca17-4466-840c-ab357b24819c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e2885fb8-cb48-45e8-ac82-d5ba92436805)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       29cd96e8-2814-4d12-9e77-f7dbaf5fdb9c)(content(Whitespace\" \
       \"))))(Tile((id \
       c35ca8dd-bd70-4aed-9dd9-38231cfe76d2)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2689e95e-11df-4916-b063-ee49b5da0dc6)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       648171fd-c22f-4d4b-b898-b46689c48ffa)(content(Whitespace\" \
       \"))))(Tile((id \
       a375de6a-f2bb-4b9e-b1be-ca6835dd2751)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       e37cdc37-c3c0-49b7-9c0d-8645eb16d930)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       ccecf57d-46cf-4cc9-9975-3905bef3d4fc)(content(Whitespace\" \
       \"))))(Tile((id \
       5d6634d3-71e7-49d3-93b9-cf52850cac78)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       34b74df7-e08e-4b71-86dd-38498268a361)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       cab66b82-5e14-45dc-8b62-26f56e161d40)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       239c7835-dfbe-498a-8c35-b1bd9a877076)(content(Whitespace\" \
       \"))))(Tile((id \
       a7b7d366-e9df-45d0-96e6-5d4736659b91)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       02af5494-d5bc-4718-b303-27a798396a11)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4f1053d1-d307-4135-8db9-6894f51f83cf)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       e4c52ac6-f032-4e81-bb00-b76ed7042868)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       cf4ede32-9079-440f-915c-b0438cc1f4ec)(content(Whitespace\" \
       \"))))(Tile((id \
       c40d5f8e-dc5c-4d39-affa-e140303bf5c9)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       85628a8a-f57d-4018-8eee-6d66df33b045)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       162b534b-f7f6-45c4-a947-729ecf4375ca)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       ebeb48ed-54cd-411a-bb10-f4907bcf8d46)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       bbd2a0db-51a2-4672-8d13-8d8230d41c03)(content(Whitespace\" \
       \"))))(Tile((id \
       fbec0ead-02cd-4dde-b141-aefb68a1e40c)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b0100545-8923-415f-bc4a-c4ba5d7c2997)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       81da2528-7ba5-4026-92ac-f4805ad6a68e)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       0603200a-5d8e-4ed4-bfcd-bf0757c5065c)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       e6e404d0-4e93-49d2-a01b-d5d0ac524752)(content(Whitespace\" \
       \"))))(Tile((id \
       94aac143-ef4c-4ea2-b02a-22ce75891b3a)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       bc46d19f-0a5d-4f1f-aae0-a3dd49578b13)(content(Whitespace\" \
       \"))))(Tile((id \
       702a5611-1b5a-443f-aa98-947e145f99d8)(label(\"\\\\/\"))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 10))(sort Exp))((shape(Concave \
       10))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       ebebd1b9-ac45-4844-aa18-fa4488d270e3)(content(Whitespace\" \
       \"))))(Tile((id \
       4835c9a8-43fc-485d-b951-c36e8f3b3779)(label(List.exists2))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c913eb6f-db37-4a24-8981-08af9eb00439)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       24362bd5-392a-4b1a-994b-ca882a3054a2)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ffd3629f-74d0-4482-a179-4d0cc9fdba1f)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f42ba05a-9872-4a7d-8d8d-761221e51b99)(content(Whitespace\" \
       \"))))(Tile((id \
       e0c8671c-1b8c-438a-abde-65145fc78ccc)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       4945c5ee-4ccd-40c5-a278-e5ee9b834347)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       13e1a1cd-f5c0-4032-9f0c-9435744e1e76)(content(Whitespace\" \
       \"))))(Tile((id \
       91ef4d57-9b1c-44b8-a98a-d10604eb2e45)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       a5a7db48-b8df-43ca-b188-9441bb641f28)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       c6f81300-1d74-496d-b80e-223a4c1223bd)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       eb182791-572e-4192-8cf3-f3d11cbb16a3)(content(Whitespace\" \
       \"))))(Tile((id \
       7862101f-f1b3-4154-a522-6b4f60221cd1)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       adad4e14-987e-4d1f-b132-ed59f749f21b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       41f13b53-0055-45f5-835d-281c67eafeb7)(content(Whitespace\" \
       \"))))(Tile((id \
       bf63b409-bd01-456e-9be1-1dda2800451b)(label(false))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       370c0c30-d15a-4ed9-957c-39fb364d215a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8ce88e7b-7976-4f12-9ff3-4f6c248b251f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       52fb20a6-8842-4adb-b14f-eef7d023b6e8)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       9243f356-e70d-4e9c-b0c8-462f5e411e18)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       304706ab-3911-48c7-92ef-032f9c567865)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       fa53e465-e0bf-47f0-82b4-2c923069fe20)(content(Whitespace\" \
       \"))))(Tile((id \
       0b3c21cc-6095-4ed7-8b30-ea8e61e13222)(label(List.find))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       71e20bbf-aff4-43ae-9448-712705d5412f)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       3b9aac1c-736a-4819-8118-75c5bcb74355)(content(Whitespace\" \
       \"))))(Tile((id \
       55533a8b-bd45-4d9c-b8d8-e9ec8d46009b)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       91f4f683-38e6-46c5-ae86-961a7912bda3)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       a9ef1312-232a-40e9-b7fc-330ef30fde51)(content(Whitespace\" \
       \"))))(Tile((id \
       c3c15243-caf8-4011-b07b-caf025a21b99)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       0ba8934f-0c7c-4778-a7ed-65d42b0e2584)(content(Whitespace\" \
       \"))))(Tile((id \
       11db6c88-1747-40fd-90eb-13136bd0b8a4)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       df8c7788-392d-4282-9721-5f59884ef001)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       92a02dfb-e737-4180-a4a3-af355905e4e5)(content(Whitespace\" \
       \"))))(Tile((id 80fe69c7-b109-4417-9049-d039de3849cd)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       7ff09c97-a923-4728-bc03-0382f3840aa8)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       7ba5ae8b-c5dc-4907-9ad0-b86420497749)(content(Whitespace\" \
       \"))))(Tile((id \
       e7121d61-5fab-422b-8f66-4539d90f42da)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       cd36888e-7bd1-4219-b3c9-46c0f427dd5e)(content(Whitespace\" \
       \"))))(Tile((id \
       64afd346-b086-46df-bb26-cf0ef1f6e8a2)(label(Option))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       3db0f7cb-2a81-4960-81c6-77a4563ef7b1)(content(Whitespace\" \
       \")))))((Secondary((id \
       7d95581c-66a5-4ec5-89a0-92799d4e2ba2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e9bc3a70-4810-4d97-86f4-fb40bd17ee64)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       742289f6-4d35-459e-a157-40783cea9e41)(content(Whitespace\" \
       \"))))(Tile((id \
       3566f109-3658-4dd8-850b-26caadac8c5d)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       864434b6-7272-4103-8952-05bf89dbb634)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       6aa603b2-0868-489f-a43e-379d075a57cc)(content(Whitespace\" \
       \"))))(Tile((id \
       4f4a9009-b391-446d-b041-3d48233766e6)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5a2ca387-060b-4ea0-957e-8071a722a620)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       e505c2e8-8e06-4fc3-bfa9-56a2e2e43d07)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       c062385d-1f66-4e95-b104-0d0ec95f76cf)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       d6a0c30a-af9d-422d-9814-73c6a1bc2c3c)(content(Whitespace\" \
       \"))))(Tile((id \
       ad75a8a9-f90e-462e-9bb6-fbc203f4c4c8)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       cc40995e-d82c-4e72-95e2-4492f0bc1b8b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       58dc6500-56eb-4ab7-afa1-6ef7af8aa1e2)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       84c8980a-6a6b-4b0e-aec9-48beb0e7daa6)(content(Whitespace\" \
       \"))))(Tile((id \
       8ce0a40e-5fbc-483a-8435-b02579e62024)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       71932ee3-ead0-45ab-be84-4048ac412286)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       bfa38b77-2436-452c-8065-7b2798e005cd)(content(Whitespace\" \
       \"))))(Tile((id \
       7f79259e-fe78-43a3-9ede-9aea19fa73ce)(label(None))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d46923bb-4231-48d3-83c7-01a23b0f4ae0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d7012150-041c-438a-8709-764a668a7296)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       c5179827-d7e6-4609-a21a-66b16683ee1d)(content(Whitespace\" \
       \"))))(Tile((id \
       0980ccda-324e-4637-8952-7b46a98ace6c)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d6592679-5a94-49a1-83b5-95b88e849965)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a5cf07a9-8bd0-497f-bc3b-a5187a964b18)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       43b2ffde-d66a-4142-83ee-af89e41ec935)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4a7014d6-b6a6-4ddb-a5a3-0f600aa0899c)(content(Whitespace\" \
       \"))))(Tile((id fd6723fb-6466-42d2-95bc-4066c81d8598)(label(if then \
       else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
       2))(children(((Secondary((id \
       c7c0f527-9162-4146-8c92-f3e9dbb02636)(content(Whitespace\" \
       \"))))(Tile((id \
       92f47fdb-8d14-484f-80b6-8252176499c2)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       cff6c3f6-e059-4883-b840-5692b9eefa87)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       5654d35c-e47b-4a87-9811-77fed59bb350)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       2497079d-ecfb-4e21-9edd-ef075c76f22d)(content(Whitespace\" \
       \")))))((Secondary((id \
       3d54f882-3fbb-47c2-b8ba-4162b9bed405)(content(Whitespace\" \
       \"))))(Tile((id \
       53ad96f5-83fb-49fb-ba0e-c33b85903b2f)(label(Some))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5146212a-874b-4998-a58d-977d69efb9de)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       66f4c355-459b-4aa0-8f18-469f1ac284e4)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       2d54879f-f8e4-445f-aa12-86cbb575739a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       edb72cea-a279-41c7-8e44-168f4eae99ac)(content(Whitespace\" \
       \"))))(Tile((id \
       bc87d371-86c7-4eef-810f-d143b22ad76b)(label(List.find))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2f58240b-a5de-4ef2-b7e9-b08af3405710)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       cf83cc09-4a12-4a2a-9a7a-e640aba464c2)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       8a4a7738-943b-464d-9105-bf808c8aa130)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       07222c37-45ca-4f39-94da-8ab4b8e13512)(content(Whitespace\" \
       \"))))(Tile((id \
       53eb0edb-b3de-4a9c-b4f1-1870149a154f)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       5da96473-4e0e-42f7-ab98-da144d8ba8d6)(content(Whitespace\" \
       \"))))(Secondary((id \
       545939a1-18a5-4ca5-a702-0fe607d0d8b6)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       f290b878-c934-4867-800a-7aaba00e63e9)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5ef0dceb-950b-4d1b-bf3e-e2e8590ab091)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       2e660c6c-193e-4431-ac79-f3fe11c12754)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6b7b4dac-a465-41c0-86a4-79b438fdeecb)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       96383ae5-2701-4334-bbd4-cd6a895e255f)(content(Whitespace\" \
       \"))))(Tile((id \
       78a07221-50c6-4e12-839a-9d712af7bd36)(label(List.partition))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       e3f43191-fec8-43e8-b92e-4815039a131b)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       a1065350-bcaa-408f-9ea4-d8d690d17c00)(content(Whitespace\" \
       \"))))(Tile((id \
       b7f4529f-3ba3-44d6-9e9c-14a800a229d1)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       7f7c75a6-03ca-4123-b5d1-bff39bef5b10)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       8ecc3967-bc64-41e1-9583-39c40eb1e2ee)(content(Whitespace\" \
       \"))))(Tile((id \
       337ad854-c25c-489a-b2a4-a0c9ea71fcae)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       3e72db59-bd44-4116-8b00-128c385a912c)(content(Whitespace\" \
       \"))))(Tile((id \
       ee69acbe-5cc6-47fb-ad2c-a5dea29f55cd)(label(Bool))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       60648f7c-07b5-424b-ba9d-7d75c737a388)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       50cca79b-449c-4363-959a-8c5240e30c22)(content(Whitespace\" \
       \"))))(Tile((id 0bc244bf-7494-47b7-923e-15593e3e6fd8)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       20dc269e-bf6a-4d55-a484-432a8c21b8c8)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       f0e476a1-8829-480f-9dc7-fabb5be3ceb8)(content(Whitespace\" \
       \"))))(Tile((id \
       7c34c539-1ef1-4bab-9a15-838aaefa39ef)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b6375136-64cf-43d6-8696-cba7fa7aee6a)(content(Whitespace\" \
       \"))))(Tile((id \
       6f205420-79a0-4475-9bdb-eba7e66f3a39)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       f6c4fb69-56bc-41d8-98f1-6fb0e48b5ddc)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       ead0bb23-446e-4cf3-ad19-ea8ca5507859)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       2a1ecfa9-92ec-4029-a5f8-d3373588aec1)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       4d03b4e4-ea46-4489-a5e4-ae540546aa8e)(content(Whitespace\" \
       \"))))(Tile((id 48315beb-58ef-4954-8ffd-45cc91f66e6d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       3f69f755-effb-4aa0-9430-5b014429dd78)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       98e8b3fd-387d-4152-a700-cc9b72e7b2bf)(content(Whitespace\" \
       \")))))((Secondary((id \
       5041e9cd-fdd4-444d-a199-60e4a1809938)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       dc9bf126-3e36-4363-ba58-290c0718157f)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       ddd6ba87-6388-41f8-92f5-f7bb1f9453aa)(content(Whitespace\" \
       \"))))(Tile((id \
       79e3b04f-d583-4ea3-9bac-cfefa8d5126d)(label(p))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2bcea339-8321-4b54-bb31-3ee0e45adac5)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       a7e980e6-b1c0-408b-b4e5-249267c6e4c5)(content(Whitespace\" \
       \"))))(Tile((id \
       bf372694-fd8b-4861-954d-6ce67dd3e85a)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       ae76ebd7-6e20-438e-934d-e58bd26df5b0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       794f76b6-7b6f-4e9a-8466-d7cfeb99306f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       570ddf55-eb76-414b-b885-0861f96c8eee)(label(List.fold_right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ffcd5fd4-1b47-4e8f-a33e-d3d4483c7309)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       36c60235-4d9c-48e1-a293-7ad892b7701f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       44089e58-77cd-4de3-87e6-28b8c362a8fc)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       95a0735d-4d2f-462d-84da-f4c97c43607d)(content(Whitespace\" \
       \"))))(Tile((id \
       7552e83e-3a4c-4b04-90b0-38739c21129c)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c42af7e0-5b56-4aa9-8d58-f7f54aa5e5dd)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       92bd8fb0-3e3c-4687-8012-8b08756c5eaf)(content(Whitespace\" \
       \"))))(Tile((id \
       dd793a74-a81a-4859-8143-2996a47460d3)(label(\"(\"\")\"))(mold((out \
       Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0 1))(children(((Tile((id \
       2f00b8dd-bd94-4ea9-8e9b-d2449be10a29)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ec1f3e79-424c-4032-b5c3-b6d65f12cbc2)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       06092150-b61d-41f8-83c2-1b81eccb8380)(content(Whitespace\" \
       \"))))(Tile((id \
       4356106d-588b-4db8-bce7-6d322c5a6ef1)(label(zs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       6f6dd78e-8dd8-445f-ab7d-05b7bacc299b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8285cd2f-5b95-41b0-8f3a-3d55ea43e87b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6a79b4fe-a099-4f32-9e02-a584a1365934)(label(if then else))(mold((out \
       Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       9c6daa80-0759-4906-9529-d704d989c804)(content(Whitespace\" \
       \"))))(Tile((id \
       9081a7a5-d698-47cc-a9ef-7b96bce24f09)(label(p))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       41801854-213e-45fe-b701-6713bc0ea7c7)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       5dc4f68d-becd-42f1-9447-af679e5df981)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       4490c4d7-8101-4d60-bc1a-8c52aa0494b0)(content(Whitespace\" \
       \")))))((Secondary((id \
       1addfc04-dbfe-4f10-ac08-9dec548e96ca)(content(Whitespace\" \
       \"))))(Tile((id \
       dda85d7c-8c78-47a4-a90b-76ea9190d740)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       f932634c-d1c6-47bc-a219-7e97498d8f1c)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       df117180-fa22-4486-82f6-86219ab0e893)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f67638b8-f8c4-46d1-915f-cb2534eb688c)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2e5b9783-743f-46d2-9b98-c82d2016b42e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       5234efa3-dcaa-4c06-9191-6fc1920dcc1e)(content(Whitespace\" \
       \"))))(Tile((id \
       faadae50-11a1-4ff0-82ad-f0e653a7ddf6)(label(zs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       39c84c20-67d4-44a8-aeac-8444cb647457)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       00a3333a-ff4b-4c4d-9f4e-e1f2c831d7e5)(content(Whitespace\" \
       \"))))(Tile((id \
       bc746e94-56ac-4cdb-8e10-11205b2fa196)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       547da8c9-ac44-482a-9565-2c06cebc9a99)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       13520314-7553-4158-ac76-591e6f539976)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f2b40054-f407-465c-9f97-d18261a94a36)(content(Whitespace\" \
       \"))))(Tile((id \
       5d8794dd-93d9-4264-8e37-89d7e98fe2e8)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       386b5d76-8c81-41f4-82ac-2160af4fcf0c)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c1121abe-d755-4be9-a299-dbab8ce0fbb1)(label(zs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       88dd983e-e963-4ad9-a9e3-5017afea3ef0)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f8b50c06-d59a-4870-84a6-076c8b2832af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f1ed3670-662c-4db9-94f1-f9eda33bfc21)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6bbf748e-e085-4b3f-9924-680225dba45d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       db728091-a607-4085-aa9a-19431ed0acdb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       c8bc8227-8579-492d-a1ee-c5bc2e79efe8)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       63fce703-7991-42c4-92e2-fa7a193c57c4)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       84cafe02-49a4-41e1-9f05-ce27161fa666)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       e57a2177-b366-4da0-8cba-0e36e95f29d0)(content(Whitespace\" \
       \"))))(Tile((id \
       1c1fcc0c-69d7-45fd-8a25-b50ca992ea5c)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       541f5303-663c-40c1-a492-a3d8d5bc1ea4)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       8314d2fa-6f20-4cd2-b76a-3e4d2b641b4d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       2c47c30f-fd22-4812-ab0b-afc70dd56016)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       eeeefb57-66b2-440f-bec7-6900260e1d19)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       b1142d1b-2ffe-4a58-98d7-363156ee3b16)(content(Whitespace\" \
       \"))))(Tile((id \
       6d5d81a0-8a5d-40ea-b444-073509d1d51e)(label(List.split))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       439c1521-8642-48ac-9397-4f811d64ee09)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       baa94580-e731-415f-8675-990a1116ca66)(content(Whitespace\" \
       \"))))(Tile((id \
       8a465eff-c758-4115-b57c-a160b2786064)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       5a821c1f-4cb4-4965-99d7-8f51d3e7f254)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       abe1c568-c94e-4de9-8aa0-cf07da290c77)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       4abba599-1202-4053-a003-2b8d6554f6ff)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       6b35b207-d79a-4550-9b6c-6a5fe0513196)(content(Whitespace\" \
       \"))))(Tile((id \
       3500dc5f-f5db-4b3e-b4f8-7d410c720ea4)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       1fdee35c-fddf-455a-883e-1fcdf64b27ef)(content(Whitespace\" \
       \"))))(Tile((id \
       4ea8c334-1ed3-467b-b3c8-9d5a8dbcbfd4)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       287d53d2-9931-4253-a53b-8e857379b043)(content(Whitespace\" \
       \"))))(Tile((id \
       0185964f-8e2c-40a1-8f90-52fded6f949f)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       59408283-1fe5-400d-8d76-be84dc827257)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       d253625b-8b8e-4eb4-b86c-addf86530011)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       3105a47a-26a7-4f48-921b-1451bca6c34e)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f9e34d53-99ea-4ae1-8de8-6f4cec21b7bc)(content(Whitespace\" \
       \"))))(Tile((id 08718f07-1cbc-4897-8725-8e4b4a1634a0)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       cd82af6a-51df-498e-a06c-cc4504af4307)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       7b7c7fb1-ed7e-4188-b33e-25544f41ebab)(content(Whitespace\" \
       \")))))((Secondary((id \
       297b88fd-7ccb-4182-9f4b-2883dcfa53ec)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f04d36a8-f033-4926-94b7-f43ddc9fdbb0)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       4c221782-91d2-4445-b42d-d771a1f0a34f)(content(Whitespace\" \
       \"))))(Tile((id \
       843c3e85-3861-4776-b169-11946fe5c19f)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b65c2def-5976-4dc8-86af-aafc06cee3e8)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       c927fce0-bd27-422a-9cc7-dd13e338a425)(content(Whitespace\" \
       \"))))(Tile((id \
       be7e4dee-58f0-4f66-a305-9db90e8e7489)(label(n))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       c2cfe5a2-5c30-466c-94c5-f94555d4f1c8)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4e0d925c-7db4-4f67-a053-6e83d482d535)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       59eb6d81-6ca0-4c06-8c66-9226ee3a244a)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       c8666391-1308-439b-9516-c05c9776c289)(content(Whitespace\" \
       \"))))(Tile((id \
       e800e006-7d90-490c-8a18-f31777d9a26a)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a3890b35-3a9c-48dd-b736-f79e1773cbdc)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       6dcf48fa-7754-4169-97e0-237f07e089b9)(content(Whitespace\" \
       \"))))(Tile((id \
       26574076-3249-4fa5-a17a-326a1bfab605)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       3d92b929-f159-4f2b-8911-bf54d7048f77)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       58d1fb75-5ed4-4664-bf88-46238f945ff3)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       c5d9b4f6-45f3-49ef-9390-ca3e1fee2d30)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2c8e07aa-ca1b-4df8-ab24-97d254750891)(content(Whitespace\" \
       \"))))(Tile((id \
       d2bda102-18ee-4ae9-b6dd-b47a25123818)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       d4a7e8b8-f30a-4c65-a7c1-9bd24514d7a3)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       471c3696-7040-4360-8cb2-b64d2e480ec6)(content(Whitespace\" \
       \"))))(Tile((id 3729df0a-732a-4d00-ad8b-86d4d027fe4e)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       cf2256b1-f151-48f5-9234-7d6a68db0024)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       9d53ce14-abed-4d59-abb3-fbc098ab82d2)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       5ba04a3a-431d-4aa0-a001-8270e2a0a01e)(content(Whitespace\" \
       \"))))(Tile((id 78908edd-80ab-49ab-88bf-e52767e8f127)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       dcdec6c4-f4e4-4b92-ac69-24b86ac60085)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       bd0c1e34-e763-4051-a222-a199fb9fd314)(content(Whitespace\" \
       \"))))(Tile((id \
       d26bc3f2-2bd8-4114-8820-f547be584295)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       225917ec-21ad-4ab9-8dc7-3d32269d0c64)(content(Whitespace\" \
       \"))))(Tile((id \
       1f168e03-f71c-4cfa-b10e-faf74af2b734)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       d2a92f12-5da7-4530-a108-0e8b60b7a92f)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       60cb0ab9-7b01-4e0b-a006-c34ea9b5cc9e)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       97c15f57-11b2-473f-b15d-400c42750184)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       57d3f977-87d8-422b-990c-0cf18d98a0c3)(content(Whitespace\" \
       \"))))(Tile((id 9554ffb8-caa4-43b4-85cb-28801fb4eebc)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       a0153c94-2de1-4c37-820e-0da961a838e7)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       0de71219-7a53-426e-9a78-fc776fc0303d)(content(Whitespace\" \
       \")))))((Secondary((id \
       9f0dcac5-ae78-457e-a7ac-3ffe673b04c8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2cd05f4c-c8fd-4244-9888-31405f9ae31b)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       1541cbb1-060d-49fe-82f6-9627a66ee59d)(content(Whitespace\" \
       \"))))(Tile((id \
       9770217b-dc43-4fc0-9d6b-bb5e6d009dc0)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       76bc7586-5ce5-423b-8d3b-1a3c73b9cc90)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       6c5d7d9a-9cc4-442e-9151-e0f5b17e321c)(content(Whitespace\" \
       \"))))(Tile((id \
       74746ab6-e10b-48ec-949c-763bdafdcfe3)(label(n))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4045f60e-2c59-424f-853c-ca34f3891567)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       1027f38c-fe4d-45d7-bf26-d65d468b6311)(content(Whitespace\" \
       \"))))(Tile((id \
       1f2e8e6a-0e04-41d9-a784-fd1efb0eceeb)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       afec64be-b2db-4317-838a-b482744eb8ca)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       f4d286a8-6087-4c79-b1cc-c663cd1d7542)(content(Whitespace\" \
       \"))))(Tile((id \
       ac1d2106-4a0f-4be4-ab9d-2fbde3ff3d0f)(label(zs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       2434d21a-fcc5-4589-ba14-9d12ad862120)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       22f6e174-dbfe-4020-835a-be4a0a873e06)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0136e738-c649-4966-b7e9-1989a532939a)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       12705e3d-0d83-4fea-82c6-ba526e2b4b71)(content(Whitespace\" \
       \"))))(Tile((id \
       342f3993-1ffd-4e91-8e94-a45044141b8c)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       be7240fa-d3f3-4f6e-b08c-15eb8d41a5e6)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       d566400e-8d9c-4cf3-a967-1b5b6eb4a9a2)(content(Whitespace\" \
       \"))))(Tile((id \
       53f3e65f-60e8-4215-95e0-4b877207d737)(label(n))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       442a3456-17ec-49f7-ae8a-cefc29988545)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       9801d861-55e5-49f2-9fd1-dcda9ce7f151)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       5dade0d5-cb97-41bf-b43c-ecabebe04f65)(content(Whitespace\" \
       \"))))(Tile((id \
       67c27a09-5408-454b-be6c-09800a8a7dec)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       cac53583-59c3-48bd-aead-7680ec18c1f6)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       dbe77760-5010-459b-9f47-907202a5a264)(content(Whitespace\" \
       \"))))(Tile((id \
       5b565231-292f-489b-9e89-7512e2fc906d)(label(0))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       5e4f8adf-7cb3-4b18-a2c9-6a85ed667e7c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       144a5074-a023-4363-bae2-a167eddc2d18)(content(Whitespace\" \
       \"))))(Tile((id \
       3d96034d-51aa-4461-867d-a4dcd2dfe83e)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       92244d67-f017-4a1a-a498-3c4de92eaa87)(label(List.rev))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       be5d39a0-c2cf-4357-b00b-2548f1a7a368)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       f6846880-9988-4534-87e0-1422fad97513)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       8192efca-8351-4a61-a5c4-06f6c1e0f97a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       de29e571-ba2e-43df-8c46-59b43aa86029)(content(Whitespace\" \
       \"))))(Tile((id \
       73d1b804-2e6a-4fad-aa43-fda1682979b8)(label(zs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       5357060b-76f1-4106-ba70-76124b6b6fc6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       ffd2de00-2a02-4b7f-bb3b-56eaf5e1efdd)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       164efaec-2814-446e-96d2-eb0736536d33)(content(Whitespace\" \
       \"))))(Tile((id \
       3e561984-cb23-4a47-8be1-7f478fa7f1ee)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       dc93ff48-be15-41c6-863d-21078bef93eb)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       b71c1565-8c68-49e0-9cf4-3e59dc047a1c)(content(Whitespace\" \
       \"))))(Tile((id \
       1750a040-026f-4c6e-a8f1-d78107699e80)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       0b1aa9fa-eb29-46ca-b9d4-f6f266d504bf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       6521b9b9-f18a-4aa7-8346-bf83da036067)(content(Whitespace\" \
       \"))))(Tile((id \
       25a7d892-6d6e-48cc-b6f6-88ab314a7c15)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       9ec31877-68d7-40e2-9481-1d4400eb7c3e)(label(List.rev))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ca18a85a-61fc-4ea3-86ff-eb8542711b17)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       e1b41577-6d09-4b81-974f-661e25a18515)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       044b6feb-f3f9-4dfa-914e-6e4b91c7f927)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       79051697-3748-42c5-a2a8-99138ada3b18)(content(Whitespace\" \
       \"))))(Tile((id \
       0305d6e4-98d4-4f36-9234-437eaf5a8804)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       1f7db64b-3c44-4e8f-a04f-8804cbaa1c96)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7a68138c-93fd-474a-9651-e7c388e426b0)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       cb4f0e23-527b-4f33-bc1c-39bcf7741139)(content(Whitespace\" \
       \"))))(Tile((id \
       46908fcc-2b59-4a06-b8d2-6246e6b54a9f)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       be1cfd17-48ff-4345-8718-c492918c48fe)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       20c3120f-05fd-448c-bb11-f38137a1913f)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4082ea60-ab71-4191-8d0d-5543638ac29f)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       586029c2-65d0-4da0-8656-dc469e1ff640)(content(Whitespace\" \
       \"))))(Tile((id \
       a465eb51-a315-4aa0-8acb-f4e82f9d9965)(label(n))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       92a14a12-5899-4894-8333-fed0824d9561)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5dbb7f5a-59f6-417d-a9db-9ece24204ce8)(content(Whitespace\" \
       \"))))(Tile((id \
       1f0e5e83-e127-4bd2-a0f0-8b966f7a9c26)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       2ac9ebd3-0cd0-4402-98fb-259bd0110546)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       a5da841b-d686-4768-a6c6-688f612a3855)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       257a1e9e-9058-474c-9fe0-81fff4497557)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       aaf7f035-0b4a-4b0d-9d26-b63f9cb86f9a)(content(Whitespace\" \
       \"))))(Tile((id \
       a939fb3e-6c78-4736-9d2d-147475731452)(label(n))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e642f649-babf-48c2-8f35-00851e9e2948)(label(-))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       34ac6a48-1f0d-4122-8451-ecf2fd43443e)(label(1))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e671f2b8-eba4-42d1-be3e-2e258468d26a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       7e1aa0f0-8ecf-40cd-a59d-ea73169437aa)(content(Whitespace\" \
       \"))))(Tile((id \
       9b037085-d1fe-4b0e-907c-9a214e0fa1db)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6af19050-80a6-4aad-b863-8112c0b9b53e)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       13eea440-2f53-46ea-8c94-b091eebae04d)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6f90270a-374d-4780-95d8-a8053f7c4a48)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       bc3e876d-c67c-4707-a604-13a0cc3ef17f)(content(Whitespace\" \
       \"))))(Tile((id \
       992fbe87-f17b-4f23-84bd-30a9817537fc)(label(zs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       aa2f724d-0c86-4719-883e-26879d52b3f0)(content(Whitespace\" \
       \"))))(Tile((id \
       e927c830-85d7-486a-ac62-9bb0171ea059)(label(@))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       c03ffe30-5fa0-4476-9dcc-66b7a8178817)(content(Whitespace\" \
       \"))))(Tile((id f9d9f24a-b629-47ba-addf-a140755b734b)(label([ \
       ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
       Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
       455e34fa-29c5-4ccb-83c3-69b64381e733)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       649b0de8-a1d7-4cd2-b2d3-c8fcfbb526d0)(content(Whitespace\" \
       \"))))(Secondary((id \
       0ea055f7-5ad2-40e2-91a4-92f22f468dfe)(content(Whitespace\" \
       \"))))(Secondary((id \
       60e12883-73af-41d9-958d-d6e97e53de23)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       6a088d05-c77c-451a-bf8e-52469d5353b6)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       422e2488-fd34-43c1-9d5d-c22943e2f127)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       945e7357-592b-4f04-8650-0dc463d1b309)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f43db050-6927-4e9f-9dde-59c258b21ea5)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       07804b98-7194-4c03-be20-7e8a3d17074e)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       85ffeb26-a613-438d-b4a0-9eb0a1f05c27)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       26c9cfea-94f2-48e7-8269-55229deddf3f)(content(Whitespace\" \
       \"))))(Tile((id \
       9cc19efb-e38a-4682-b6e4-fa5f4f3da2c7)(label(n))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       15bd3a12-1782-49e6-8bdf-dcf02674dee4)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       5083aaf6-95c5-4dc6-ad4b-c572fbeb746d)(content(Whitespace\" \
       \"))))(Tile((id \
       414bf6de-832a-4d1c-a526-0b9cb5436f11)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c0239b1e-d388-4e35-96f6-655bf434b03d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       f054f809-3c6e-4269-8bf8-2b7af688bfb3)(content(Whitespace\" \
       \"))))(Tile((id \
       e85bf726-8d02-4f75-baab-8d11d0db1206)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       30fafdea-b9ae-4952-902d-acd38105fcbb)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       f196f025-df62-4bc7-854a-e6f5ce77c26c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       8b15df99-29a2-4fd7-9e18-713462e5bbac)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       317bfaa6-ea57-46b3-87db-e8c486a4e801)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       dfccd190-df5c-4047-9173-94a1bcf60bd9)(content(Whitespace\" \
       \"))))(Tile((id \
       8746861c-011d-4294-9738-5ece10c156f1)(label(List.combine))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ce90eb06-961d-4a35-9834-08abead94cb3)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       46fd9916-229f-46a7-8a98-58c63b525f53)(content(Whitespace\" \
       \"))))(Tile((id \
       6e838e01-fb7a-412b-b61b-f84079fb8d92)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       6ed18676-fc47-463e-b03a-b32625487d1c)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       103686d6-7b2f-4eaf-b290-52b5425e9772)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       d0cbfc89-a01d-4f9f-bb32-673fe54932d5)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f7ddda03-2f4d-450c-b2d7-cd798265026e)(content(Whitespace\" \
       \"))))(Tile((id 8f583a2d-9a29-48d3-a3bf-f14c4f584b57)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       dc3ad97f-d951-48c1-af39-6dc6fa42a2b8)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       057c0764-00c2-4c43-b4be-b6a79c94b1a8)(content(Whitespace\" \
       \"))))(Tile((id \
       5a33874d-a194-4502-aa28-8723176360f2)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       4962b26a-5ce7-4e4c-90fe-fec8342044b9)(content(Whitespace\" \
       \"))))(Tile((id 5ce6f768-b011-4d17-a526-3683b05169b6)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       efa28f01-95a6-4fd3-8b34-7a9e8f823c0d)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       61aa95e5-179c-4281-b6c5-98acef070e66)(content(Whitespace\" \
       \")))))((Secondary((id \
       d0fea8a3-e9ab-4926-b322-d3960d3ca911)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       50f0d15f-e10d-4651-848e-e810b7a98f07)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       cd25bcf8-d35a-4075-bed6-0b7d20b130be)(content(Whitespace\" \
       \"))))(Tile((id \
       a970407a-70f0-496a-81ae-f7d420717be4)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       29917d12-533f-46ec-a6f4-f221e3d6c080)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       d8e6a0c3-60db-4aaf-ab43-fd208dd418b0)(content(Whitespace\" \
       \"))))(Tile((id \
       a3506563-5b90-4f0b-bd9d-829e20a18c2b)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       25e2c9fc-fe33-4421-808e-62f293c69c53)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       987296ee-b922-47e0-815a-e2afef0b6cda)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       fef18019-4839-482b-927b-b9792ecfa2e0)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       b2bb4212-938b-4b4b-b132-098d96d81cbf)(content(Whitespace\" \
       \"))))(Tile((id \
       2fff1f6d-ccdc-4379-91ce-e1dc9150dde7)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d2f578f0-f4b9-4362-9d57-9fe052d7e668)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       02469f3d-0ca4-4a6c-8cc8-b5ef8b71278f)(content(Whitespace\" \
       \"))))(Tile((id \
       305648e9-1908-4ff1-ba9e-c921f316e190)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       45a2219c-07a7-483e-aa67-d209539357dd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0e0ee31e-d1fc-43ab-8784-a2c573511921)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       791f404b-7ca3-436a-b47a-282a94c34a2b)(content(Whitespace\" \
       \"))))(Tile((id \
       7c09652f-1759-4555-8d80-fa242fa6c02f)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       08d299b9-b571-4704-bea2-ee346a64a202)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       16703256-6707-49c8-b33e-5eb57ba54710)(content(Whitespace\" \
       \"))))(Tile((id \
       5be6b94e-aa80-4942-bc96-ae3e64875b61)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       84a138b2-d65e-4adc-8c00-e096ea9c1ed7)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       622d7e68-e939-4ace-9b8c-ea9299cc12b5)(content(Whitespace\" \
       \"))))(Tile((id \
       a7ddde3e-dd50-453f-94bd-f8621a8d9e5d)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       eebcfda3-4b9d-4f94-9001-41befbfd7752)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       3273d655-b63a-42f2-94a2-17768bed7701)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       5e379bd4-e784-4a05-beae-9032ef9476ec)(content(Whitespace\" \
       \"))))(Tile((id \
       5b20a1c9-3566-468c-b286-b100e04087e1)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2d6130b1-4e11-4563-9a51-25354643a2a0)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       299070d7-51df-4d88-abe5-4204166a439c)(content(Whitespace\" \
       \"))))(Tile((id \
       0fab42af-5ca0-4359-9167-e756fb1cc3b5)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       101fdb1f-2315-4949-8e12-6ce4998c17fd)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       673d3834-a495-4f9a-9aa6-f49796d50286)(content(Whitespace\" \
       \"))))(Tile((id \
       eac0ce8c-10f9-45ad-9a5b-dd5e71b6047f)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       b47f3c7e-4df0-439e-befe-e5f691a4eb33)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       d4791a8c-fca8-484e-aed1-fd6a623f400f)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       3c618e1b-cbd4-491d-a5ed-63a7087f91db)(content(Whitespace\" \
       \"))))(Tile((id \
       f4d27be0-1b68-49eb-9311-f52343900d14)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       939d729f-2a39-482b-9bf4-8c0bbd0e617d)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       4d18e2b6-a896-4ddf-99ef-dab91aa77071)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       69ec3aad-70c8-42cc-8c5c-72ce05c530c0)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       945065a4-6372-4c09-96f2-2e2867a4ed9b)(content(Whitespace\" \
       \"))))(Tile((id \
       b63e7a06-0dee-4301-ae0a-4cfe5eb259e0)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       8fcba9a7-06dd-4a1c-b777-e0fbec9cd9aa)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       0d723192-eab0-409e-b1bb-8b3d03f97f77)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7baa27f0-a3ee-4177-afd9-ce95b0668363)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2cd381cf-0cf7-47e6-8eda-e2440bd4b95d)(content(Whitespace\" \
       \"))))(Tile((id \
       455c0fa7-ba85-452e-8d36-f8ad87236c51)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       bc3dfa8a-64d0-43ca-8163-5ac54efd6a97)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       9d2dd10a-b1aa-49aa-9f07-1b756693af9b)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       5d6f1152-2bb8-439b-85c8-d38dc4d3ebc3)(content(Whitespace\" \
       \"))))(Tile((id \
       142f1376-25fc-4945-8d4f-8e19c02be766)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       cbd5a4b8-6c8d-4eae-a0de-55195066f78f)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e4ad9f23-3fe4-4f11-a93a-ee04de1c17b9)(label(List.combine))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       50c725fc-b45c-4375-b37a-d4d9f60458a1)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       cd5fa9bf-8db7-4248-ba2f-262b8a727e6b)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5ad20411-d274-4b20-8021-d768eb9bf9f3)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       c3a4a6a4-31d3-4330-a928-a76f453e1cde)(content(Whitespace\" \
       \"))))(Tile((id \
       f4baa457-071a-499b-bfb1-3ed33cafb27c)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       56c9f1a8-2a6b-455d-813d-365a94e82462)(content(Whitespace\" \
       \"))))(Secondary((id \
       f1650f74-fadd-4723-9a9b-4ae9dc4ece67)(content(Whitespace\" \
       \"))))(Secondary((id \
       44af297f-08d7-40ab-8c42-16e595e2feff)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       2f7f5bb2-6480-4ba4-af85-ebd797f3e0f1)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       20df706a-8752-4549-9ea4-21bb70647e48)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       0bf3b314-bf11-4aa6-93c3-02d19b78dc98)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       db35b87b-3765-4df6-b647-c98b49c23bea)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       607b67d4-41bd-4405-92d7-2cc34989c088)(content(Whitespace\" \
       \"))))(Tile((id \
       80e5ca12-ebf2-4381-8150-87c653cd59d7)(label(List.rev_append))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       907b971a-f5fa-438f-b186-17cd213d4ef1)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       fb1236fe-08fa-483c-8515-3cac40b92821)(content(Whitespace\" \
       \"))))(Tile((id \
       18d9874e-9cc7-49c4-ad2d-e060be27ca0d)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       9c7fda58-d0ce-4410-a74d-06c1967aabff)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       cc033bd2-ca2c-490f-9daf-2e6ab9efc961)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       13290ca8-d824-45d8-a088-64c60cf46d67)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       7a8a38db-5426-4157-a00c-9c23fc4a2f97)(content(Whitespace\" \
       \"))))(Tile((id 451c0751-a3f5-4017-8f10-172b00ff16a5)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       73cdda10-f9ee-4d6b-a7d0-21e6ecf235ec)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       1733bf72-3d26-4b63-a5c1-2e92ae016049)(content(Whitespace\" \
       \"))))(Tile((id \
       3accbe98-b380-4236-96ec-738c91a9e4b5)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7ff9a510-314d-45ff-89fc-bd911aea47f7)(content(Whitespace\" \
       \"))))(Tile((id f8b0eb40-3466-4acf-b619-1bea12a93c0d)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       d16925f7-5d7f-4428-b606-cffe2441b11d)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       1167207d-5c65-40b8-83a9-59ee8c68c85a)(content(Whitespace\" \
       \")))))((Secondary((id \
       f8b61ed5-ea5d-40c9-9ed2-1ee654fef8d0)(content(Whitespace\" \
       \"))))(Tile((id cff98906-0167-4743-ba47-f4c39bac978d)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       85bb9563-26b0-4a51-a0e1-5b455f260694)(content(Whitespace\" \
       \"))))(Tile((id \
       908d2c59-faad-475c-b923-25981353557f)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       75d4d136-5490-4b3c-9daa-0529f47fb87a)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       48e7d2cd-95b0-4df1-b9a6-15aa69325446)(content(Whitespace\" \
       \"))))(Tile((id \
       39218e64-c58b-4382-93a8-9b0519833f5a)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       00cb1d82-6b20-42dd-a749-6d24d4cc7306)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       fc9fbb17-6325-461e-90a6-ea4964661701)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       68029d3d-9e30-4c95-b3ce-d1d556c8fa0c)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       fbda2cf1-71f8-4af0-8663-022a33351802)(content(Whitespace\" \
       \"))))(Tile((id \
       9ae694c2-8afc-43df-9ff4-0bd53a0b07e2)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       36d3893a-42cb-45e5-b146-dac00c1b9ac2)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       062c4c69-9bac-4912-8ee9-1278f22f79cc)(content(Whitespace\" \
       \"))))(Tile((id \
       593edffa-7e51-4781-b326-8628cc98786f)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0d8fe46d-cc99-4f41-aa6d-1f64937ae19f)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       9d41b49c-03f1-48ee-9781-92b9eda3b503)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       f3e873a6-d078-4bb1-a5c9-5adb52e5af9d)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       216033b8-4b55-4be1-ad4c-08793f29ce68)(content(Whitespace\" \
       \"))))(Tile((id 1e009db3-9e09-48f3-ad41-7d6c8460b222)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       8df681da-5a3f-4910-b426-9d358a3cee54)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       1a1cb8c4-60fd-4b27-bf3b-3d6ff3368d9f)(content(Whitespace\" \
       \"))))(Tile((id \
       e1de3885-85c1-43b1-bdfe-ccca21b77fdf)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       df6d2927-9038-43bd-b005-bd0255706494)(content(Whitespace\" \
       \"))))(Tile((id 9a5a1d00-cd19-460a-be0c-650ef2a7ec0f)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       ba7f9ce2-7665-4d0e-a924-2ceacfff0d9e)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       00fa0f59-3d72-4333-97d1-73b0284a4566)(content(Whitespace\" \
       \")))))((Secondary((id \
       7b42fa87-bff2-4d7d-90c9-987a49e0049b)(content(Whitespace\" \
       \"))))(Tile((id f9151c84-0323-4d01-8f06-8d90a3af5b7a)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       fafc7b1f-ce4b-4369-a7e5-491d724f05f1)(content(Whitespace\" \
       \"))))(Tile((id \
       825ac86d-3d40-4eac-a27b-3a2afffacba0)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       dc167143-03a9-4c4e-8206-760d1522e27c)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       60d17acb-bf35-4a1a-8ccb-e9b8bb759e8e)(content(Whitespace\" \
       \"))))(Tile((id \
       aa5a2ae1-d613-4a54-9118-efffe90d35c1)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       ff94602f-ebee-4654-a81d-787418b7761b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       6af972dd-ebd0-4d20-835d-4a5346576153)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       7422f12d-0cb0-459e-ba1c-0871f131895e)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       85deacdf-ae4a-4c66-aae9-8490e973d1c3)(content(Whitespace\" \
       \"))))(Tile((id \
       d0f226ce-e567-4d74-8d42-ea6d98d401d2)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       8ad27875-d0c6-4b02-83c7-1c3320d993d2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       92c86f1e-b10e-4620-8509-3c65e858eeb0)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       2b27fa0d-ea98-4b04-a9ab-8677171ebf33)(content(Whitespace\" \
       \"))))(Tile((id \
       802305d3-76e3-4904-b6ad-598b2bdfcb94)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       82438e26-b4a0-4330-9ec8-91e9e5a12eb1)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       11eb9e3d-e401-49a5-87d0-414e424a094d)(content(Whitespace\" \
       \"))))(Tile((id \
       768b9889-208b-442e-9b53-db43d5f85ff5)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       7313cc0a-b25b-4485-92c1-b23a15f3ba8c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       fb9bb7c6-5d02-4e24-809d-bff667ec166d)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       1a3e197d-bf28-4dbc-9f56-874ff9b7b077)(content(Whitespace\" \
       \"))))(Tile((id \
       7a6d1728-756c-4fef-b4f5-a82dbd62fec8)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       cf4d95d4-225b-43cf-9531-da28d5638a90)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       b10e8e88-1cd1-40d5-8152-fd9ea3a081f9)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       cd2b4298-d9e3-4837-ba66-5eb70530de64)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       244e933a-7672-4f97-9496-38fd85b0ea0b)(content(Whitespace\" \
       \"))))(Tile((id \
       e1699403-7314-4962-a4cd-d7039dd2eaaa)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       108f40f3-f7ce-40ac-aa43-330bc6e19ce5)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       5df77f21-116c-4780-9594-107b003015a0)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d5f36cd7-2c4e-4b92-af15-2374c4ec65f9)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       6eabd420-d6c1-4e43-b473-ff380f3c43f9)(content(Whitespace\" \
       \"))))(Tile((id \
       fa11b579-55b3-429e-b9cb-a1ea5a43ba2b)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e0f4504e-a39d-4c75-8b0e-d7b15cbbc560)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       139757d1-8a10-4c7a-96a9-97dc14735381)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       3895bb4a-8510-47fa-b26d-7710eec3be47)(content(Whitespace\" \
       \"))))(Secondary((id \
       55388b97-bfc5-422a-bc87-46924544a4c4)(content(Whitespace\" \
       \"))))(Secondary((id \
       d25f5ef3-25b5-4a36-bff4-8e4791766780)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       e4f8b9b5-ab40-4613-ac3f-7583f1772aaf)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0a6088f3-d20c-486a-9f55-c16a2b2d4041)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8c123723-af56-4bfc-bb75-28e33e19eb11)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       27744ab1-2eb4-4c6b-b155-925a31e3ca8f)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       7e478093-5f4a-4275-8093-60a19124b2b6)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       98279a22-92df-4635-bb15-57277e25cb02)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       00b754b6-c91f-4bfb-beac-de5a0dadf488)(content(Whitespace\" \
       \"))))(Tile((id \
       e7212b04-4daa-4a64-ae3b-e9f705beb663)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       cd03b130-a735-416b-9bdc-81048c0f157d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9dacdaa0-bbf1-4592-a56f-3a730944b12b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       a0635548-218f-4c34-b504-1ad4d50fe173)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8e813e64-62ab-4251-90be-e0daee99814f)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       05559820-7d0d-4232-a701-0a56e5041a3c)(content(Whitespace\" \
       \"))))(Tile((id \
       4ec03947-10a5-4bcf-bc31-b2195b3c5758)(label(List.merge))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6f74bd1e-7f73-4373-a81a-ab6f94fa4afd)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       34d160cc-543d-4417-a22b-720c699553ee)(content(Whitespace\" \
       \"))))(Tile((id \
       993426ef-7bbf-4e52-9560-82e194aaafc4)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       0d69643a-cd91-4274-b221-6ca3323e752d)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       c1acb573-4deb-4869-b8c5-4030cbd39cbd)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       dbc08424-4d1c-4069-8ac9-3fce54fe3152)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       29301e7a-b11d-4cd6-89ec-32e74942d137)(content(Whitespace\" \
       \"))))(Tile((id \
       93d5e650-9deb-42a1-a8f9-890db64a1470)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       f30d271d-af06-4cdb-9d9c-c1892133acd5)(content(Whitespace\" \
       \"))))(Tile((id \
       52a6e625-0eec-41b3-9cc9-6ea5b7c45c42)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       b4366db9-20b8-4ea5-9c9d-4b0b02af814d)(content(Whitespace\" \
       \"))))(Tile((id \
       d3d0ed9b-407f-4163-8081-229936760e2b)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       dbbba492-5a54-4ecd-8754-a3bad8e8206a)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       52058470-2982-4c0a-b63c-479a602fb5e5)(content(Whitespace\" \
       \"))))(Tile((id 1f2f2201-737a-4187-bb2e-030ec7609c43)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       bafe1db9-f8c6-4249-af74-157d6d738851)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       4164fddd-d482-4a49-9932-e8cd8b4fe459)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       e9ed6358-265d-41b7-9659-7f369d67133e)(content(Whitespace\" \
       \"))))(Tile((id f5d36fd0-41cd-4c90-b2fc-d36e2c7ff549)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       fe0ad9e8-5817-4091-9c1e-3a7db6d5d7fe)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       72791f7b-38ec-4cc0-a48f-91d8bb9857e0)(content(Whitespace\" \
       \"))))(Tile((id \
       549dbbad-f393-4144-aa98-36fd90a986cf)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       c44c99db-5a36-4b9b-9246-ae4cd93d9ee2)(content(Whitespace\" \
       \"))))(Tile((id 2f5f85f7-64af-4e98-9d98-7bbf6efbb667)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       594d9b34-08a2-4dc9-a543-cded172e8fe1)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       fa729d8a-7099-4a8a-91ca-6a8ec4b183ed)(content(Whitespace\" \
       \")))))((Secondary((id \
       74a06900-08a6-4372-979b-966c821fff86)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       57b25f1c-2d83-4fc4-8821-b369de54fa50)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       191b4614-9f21-4298-ac9b-fa26ed24b36b)(content(Whitespace\" \
       \"))))(Tile((id \
       f905c687-fd71-46d7-be73-081c3c89bf5f)(label(cmp))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a585f48b-b409-4977-93c2-9d569d005c60)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       87d0fa3d-1840-4c7c-a455-a78c084e2745)(content(Whitespace\" \
       \"))))(Tile((id \
       685c0b4a-1c1b-4939-a185-53777de18cb3)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       ab4f6734-bf81-46c0-998e-277f7eda887a)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       fbed85ec-6c8b-4209-8ac3-6d43b6efb2d6)(content(Whitespace\" \
       \"))))(Tile((id \
       17a80c81-e1ec-41e3-aef5-06f4e2276042)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       6c5467df-b3be-44ad-82fd-c11605d5c188)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9bb7380f-a944-4ea0-9dbb-939465070515)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       52da24c6-3bfb-4bbd-b02c-63acff77f7ca)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       27ebdce2-dfa7-4c96-89ff-9952616ef2f7)(content(Whitespace\" \
       \"))))(Tile((id \
       baf1d97f-1338-4758-8e47-fcb0c09a2424)(label(go))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       531fdfa4-4622-4bf0-86f1-0f809dd49136)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       663725f3-479e-4ac8-9863-a1b9e922bb28)(content(Whitespace\" \
       \"))))(Tile((id \
       05b123cf-39d1-4f90-b9f6-acda864ade8d)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       c5e852fe-9461-42f8-b8f1-c47e322add38)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       2a54c92f-4f8a-4523-bf3b-178eff0d918a)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       0e8b912a-8777-4376-b5ff-571f5d8014e7)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       80b0e1c9-cc61-4256-8416-0a96bb4c3ccb)(content(Whitespace\" \
       \"))))(Tile((id 401d8313-5555-4537-a2f8-b04f0a7be0c4)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       882c9243-c172-404d-9902-dceb46cc2492)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       02328137-c230-4bf7-b9f5-93070bb76a47)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       f5e71807-17cd-4cdd-98c8-4857411c7b24)(content(Whitespace\" \
       \"))))(Tile((id e6a3c1b1-9d36-4c1b-ba32-7badee1e2726)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       2f79c037-2f64-417e-b227-0a25de1a12f2)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       c8e7b34b-3e06-4928-a46f-f6fedeab930c)(content(Whitespace\" \
       \"))))(Tile((id \
       0e17f0fc-f692-4b99-a0fb-bc5a5e2a6726)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       0ab13162-1be3-4748-a630-db27ddde3a78)(content(Whitespace\" \
       \"))))(Tile((id bc384580-44a3-4fce-9193-bde63222c207)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       bc1c967f-6313-40f7-8a30-603d07933cc4)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       03a04487-3f00-4ff4-808b-6abae9d1798c)(content(Whitespace\" \
       \")))))((Secondary((id \
       401c1938-77c3-4fae-ae4e-14585b91cdb4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6f977be6-c05a-4c48-8dc1-1a7fe95d7029)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       70c05112-eb66-4bdf-9cb5-06de2aa16b6a)(content(Whitespace\" \
       \"))))(Tile((id \
       1c567bfa-a0d8-4227-baa0-17e09702e304)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6789311c-6076-4a11-9f7c-2ea9ca670ebf)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       2bce9ebd-4103-4c6f-8ca2-55a606c6ebd1)(content(Whitespace\" \
       \"))))(Tile((id \
       1db34bce-1b50-4b7d-b2c3-4862f5450c1c)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       a80e8cca-dc13-4d37-835d-b0078387f798)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       f8ec40ae-7ba0-475a-8de1-1ca451d3769f)(content(Whitespace\" \
       \"))))(Tile((id \
       e582c603-591a-46eb-911b-bf395ed9479a)(label(acc))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       8d9836b6-2c15-4c71-a1c3-9e701032fb6b)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5ccc181c-2b15-4ecb-9f86-725502e30864)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       f5cbc7fe-809b-4ab3-9448-e7c5d42f86fb)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       2d3350f1-a69b-4f7e-a23b-b0ef22a072a8)(content(Whitespace\" \
       \"))))(Tile((id \
       80028ebf-0c42-4f41-94d2-bdf81ff501b1)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       908123af-5ed1-4714-bb75-8e00bded4a89)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       b2ea617e-c552-41a8-916b-cc2bb2c20da4)(content(Whitespace\" \
       \"))))(Tile((id \
       ca25b41d-8340-4f22-b5b7-9494fc5f6bc2)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       601b7f72-2bef-467d-b633-a76988f65719)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8137f884-dde7-44b8-8bce-173f8744c280)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       0fcd56bc-f314-4421-b199-9ce62e93a1ef)(content(Whitespace\" \
       \"))))(Tile((id \
       32aa965d-1f5b-4384-9789-5b66838574c5)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       c738a55f-ae7a-43bd-9653-3244260e3669)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       588d51c9-c4df-401d-a6e6-83979d63a728)(content(Whitespace\" \
       \"))))(Tile((id \
       c57130e8-a965-4097-8dae-037911e30264)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       913d1956-647c-4e4b-b2bb-ae841f414f69)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       7cff28ff-f483-459d-a721-dc520ec6b10f)(content(Whitespace\" \
       \"))))(Tile((id \
       2deae111-55db-4050-831f-f7ff349e54f1)(label(List.rev))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ea3b5619-bda5-49a5-8f16-400165a574cf)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       4e9d28e4-3251-40e6-8301-088be42d8df4)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       403cb085-e04d-4e35-87db-f6252e013d7f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       620f39c7-382e-4aea-89ea-15288bf330eb)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       95bdc31e-cc28-4c3a-92cc-caa73f5b8bef)(content(Whitespace\" \
       \"))))(Tile((id \
       1dfa2de3-ad68-4d1b-8dd1-d07437c1361e)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       49691483-2935-4c0c-9c2c-b189070c9863)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       26f8f1e0-1259-4a7b-b1c6-e78ad33ff3df)(content(Whitespace\" \
       \"))))(Tile((id \
       2e1073e2-c87f-4afb-995d-4aad1365f39e)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       f2bc25f9-b693-437e-b171-5285d6b9e498)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       34aed31e-18db-4042-b319-f223b70243a9)(content(Whitespace\" \
       \"))))(Tile((id \
       5d427281-37aa-4f2a-8c2a-112627ebdf19)(label(List.rev_append))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       a90fd49c-aa71-4e1f-944d-7d72ab135735)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       03b53658-47c7-40de-86ff-6aa94ce2acf8)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c3726217-9469-49f8-a371-768e62cdac0d)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       fc473aeb-3ebe-43b3-8b1a-fddc0dd9cb05)(content(Whitespace\" \
       \"))))(Tile((id \
       7f34cbb1-01d2-4e18-acac-5b1de7365c1b)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       411662a8-72a2-4cab-87fc-10bcd98aeef3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       805d14eb-abe8-4b48-b691-a805afe8defc)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       2f52c115-83ab-4788-8edf-ac31f934f231)(content(Whitespace\" \
       \"))))(Tile((id \
       398a7d8f-e9a3-466c-aa06-35a2d9d31ca9)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       25396a41-f402-41ff-af77-b6eaec8fa3c9)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       5995cb1f-513d-4124-a7fe-2593f5e6de97)(content(Whitespace\" \
       \"))))(Tile((id \
       643ad21c-bc77-41e2-b09c-a512f78f59d1)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       9b0b235a-087f-46a5-a4b5-ee4e1559e51c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       77412984-18fa-44f8-ac02-6d2086d7819a)(content(Whitespace\" \
       \"))))(Tile((id \
       dbddc0f0-0def-4fe5-abc4-31b326a0d77e)(label(List.rev_append))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       ed00f9ef-6d56-44d7-9a89-ce472b9dde45)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       6fe2e015-a79e-4e07-ae25-9d5ab2b755b2)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       609d028f-d2f6-45c7-8906-5eb20cc3805a)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       51ad3042-1fac-4205-ab66-7d000d5c6523)(content(Whitespace\" \
       \"))))(Tile((id \
       f3904947-a96f-432d-943f-b46f6580b97c)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       a9adda7f-36b2-457a-8025-8e18b39867a4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2e1edcaa-c07e-47f5-92f8-e85c4d536c31)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       ca54311a-c589-4044-84c9-955d2c699b37)(content(Whitespace\" \
       \"))))(Tile((id \
       ad0cf687-145e-4ce3-903d-2c578699ba46)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6ae17370-1091-43e0-9e0c-c6d68bd9d904)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1a18e030-f509-4be0-99f2-d59262fd5a64)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d5ac6be6-db51-4044-899f-dc1d56c47157)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       657378a6-7c0f-45ae-9989-9030e5832617)(content(Whitespace\" \
       \"))))(Tile((id \
       477e3333-02c4-4ae6-8b3d-a1182e9f0256)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       d052ebd4-ba49-4c13-a448-f194dd5303e0)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       03209657-8ed7-48dd-b6ca-319fe76483a0)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       d369d2ed-64ef-4452-9eda-775945bee213)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9fc48632-2127-4c3f-bbf9-a506c65d2109)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       1c1a8ae4-2d11-41cd-99ef-559cf63125ab)(label(if then else))(mold((out \
       Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       9f457082-0b44-42a3-bdcb-1ea4e696602f)(content(Whitespace\" \
       \"))))(Tile((id \
       48d80077-8b1c-4353-b848-097e0339248b)(label(cmp))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       55030163-c54e-4507-ac96-a203fb5f601f)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       39fa8d81-12bf-4130-8199-683d28defd63)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3382aad2-c797-4354-9294-861bdec2c986)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       0a9d285c-428f-459a-adb3-4d65e4480eb2)(content(Whitespace\" \
       \"))))(Tile((id \
       66c2efae-3723-49ee-b886-7e36909a5f1a)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       dfc59ce6-ec3a-4ccc-a5f0-c594521d469c)(content(Whitespace\" \
       \"))))(Tile((id \
       8d348e6d-d59c-4c1e-bac7-cacb8b5a6fab)(label(<=))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave 8))(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       983471c4-17eb-49b3-866c-b8fbe54aaa60)(content(Whitespace\" \
       \"))))(Tile((id \
       a7f913ce-adb2-415d-87ae-9165ed9df2b8)(label(0))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d4f09d0b-ab98-41d3-96ba-a3c62a4f051f)(content(Whitespace\" \
       \"))))(Secondary((id \
       eeb0d963-2afb-4394-bf00-0ded79da6c14)(content(Whitespace\" \
       \"))))(Secondary((id \
       4891b74a-eb86-411e-87b1-dacafde530ee)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
       441049df-867f-4754-a7d3-e90283d9a600)(content(Whitespace\" \
       \"))))(Tile((id \
       b5d0d8fb-c1d6-4c62-8bff-59af9d9175d1)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b87beb64-a0c0-4ad7-ae7b-ea67ebcf2d26)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       081b67c6-9489-444d-b9e0-ee85eefc09fa)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e863ffa9-dc04-4a65-9456-5f750bb96179)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       576f2169-0129-4aff-8975-5f7ffbb886da)(content(Whitespace\" \
       \"))))(Tile((id \
       84598a8f-2112-4d68-af44-d55324590f42)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       613693a2-6b1d-4215-8674-513c54926d2a)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       0fc83eab-0558-4062-876b-a390da4047c3)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       3ef51def-35d3-4e3f-83f7-df0e62ac366b)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       1c36d1e5-e4dd-4f9b-9cf3-dd9b20f1a43a)(content(Whitespace\" \
       \"))))(Tile((id \
       a6829190-5ced-493d-a90a-3628e0439a9b)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b34b9cec-b310-4a15-b3fc-c5de5a800ee0)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       b3effb83-101b-4785-97df-3ebbfef6d176)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       bd8ca2d6-736e-4819-9c8f-d1aadf9b9772)(content(Whitespace\" \
       \"))))(Secondary((id \
       08d665be-6f2e-4891-94db-64a0f31ada86)(content(Whitespace\" \
       \"))))(Secondary((id \
       f7f9ce1f-c651-40b0-b7c7-92b0aee12fb5)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       5c9b3e9d-7147-4e23-830e-dce987f1ecb9)(content(Whitespace\" \
       \"))))(Tile((id \
       2df70e24-b2e6-4d1b-a0c1-6b196b6d177f)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       7d0667f2-d645-4137-84c1-cac0552c2097)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       b50d4863-0f1f-4789-ace7-1879025a06b8)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       f98c33f4-ff33-4a8d-9ead-b21b5d3b94af)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       9cbb3fdc-a97e-48a7-8230-221f16a6cd93)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       904c57e8-0ccd-4f5d-b678-3707707d7a24)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       85415978-431a-46de-bbbf-e2163cf4ad6f)(content(Whitespace\" \
       \"))))(Tile((id \
       b9894f60-936c-4749-9cad-9986af1198c3)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       41800047-345f-4ac0-997c-70cd4fd050bb)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4ceab4ea-c01f-4fd0-a8f2-6c8207697ad4)(content(Whitespace\" \
       \"))))(Tile((id \
       535d8dd1-1584-4d01-ba59-e5127c7b15d9)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6d8c29af-dbab-4fc5-babe-00a162df571f)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       928617c8-722c-4f55-b570-cd20f7a83524)(label(acc))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       e838a734-dfb4-4f30-8cbf-58bfc2ecad34)(content(Whitespace\" \
       \"))))(Secondary((id \
       0773cb53-b23b-41f4-a4a9-21b1034422b7)(content(Whitespace\" \
       \"))))(Secondary((id \
       4bcca455-3dee-45cf-86af-a3cb5c35d04d)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       e36517f0-5437-4ad0-b263-614892b7391f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       9f688d69-bea9-49d9-b1ee-1e7681108bd2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8eb58de7-5393-4eeb-b65f-abc4b44a526d)(label(go))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c7704916-cdec-40dd-92fc-156703951a5b)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       8702e52c-4dc6-4824-ba54-c0eea743a0b6)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       204a6be6-0b05-4730-a138-b0330ef4c840)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       2a8ddeab-b7a0-4af7-aa85-ba30eb886aef)(content(Whitespace\" \
       \"))))(Tile((id \
       3a698dad-20f0-496b-b1ab-dd885576f809)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       154afaeb-140b-433f-bc89-7a1df4e69cef)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       4495471a-431d-4cc9-807f-9d6e1fae6fce)(content(Whitespace\" \
       \"))))(Tile((id \
       51341690-b958-4463-b0e9-5b321ed81a8c)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       a1a98f9e-3599-4c40-bfe6-23448506c99d)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       265a2174-ee25-417d-afb4-637d57cc8735)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
       a06a4756-63d1-4467-85b3-b77dfd879289)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       331d54fe-e2b1-4ad2-8637-fd9949c7447e)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       990c4c7e-b1c2-4d11-a0d3-ab7c8f8394b1)(content(Whitespace\" \
       \"))))(Tile((id \
       7bbccadd-4825-472b-a62b-254770194ccf)(label(List.sort))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       6106dbd3-546f-4e3f-82db-9d6d9a15793d)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       e4d228a1-1fc4-49db-8d1d-54be58a02298)(content(Whitespace\" \
       \"))))(Tile((id \
       b57b7dc4-8f4b-4db8-9ca3-e31f70887fe9)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       17532d1a-ddd7-4ab0-808f-df9507902100)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       2d5c4ad2-2c60-4959-acdf-96222aba1f05)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       8fe4c6ac-4a2e-4e00-af03-b8e6bd3371a1)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       291568b7-82c7-485d-8c40-aafbb5b2173d)(content(Whitespace\" \
       \"))))(Tile((id \
       3919b3e3-4b57-443d-a648-48b9b357c0cd)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       a72dcf56-ab2e-40b3-bcac-b5cb32314777)(content(Whitespace\" \
       \"))))(Tile((id \
       b7d949bd-f9cf-476b-a06d-0e8b5104250e)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       9ef0ef2e-2641-431a-a169-1cfe904d6176)(label(Int))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))(Tile((id \
       54f130fe-7f9c-40e5-99e6-50e5ad30c1db)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       286c7c0d-b4e5-4806-bb43-48dc3335bc67)(content(Whitespace\" \
       \"))))(Tile((id af8df329-3df6-4d5f-9677-4c8ed3c79893)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       b7c73108-a72d-4521-bff7-c15c6f9abd9c)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       954c861f-3c77-47f3-8922-ccbfa2faba40)(content(Whitespace\" \
       \"))))(Tile((id \
       b8a1c38a-a2ca-4dd5-add2-c15a20f7f8e4)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       4f59c208-953e-4441-acdd-d305745319a8)(content(Whitespace\" \
       \"))))(Tile((id c998f5ee-2c1b-4a3b-954e-b8602d0a7368)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       f57c02f2-2278-45df-8658-14deef263801)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       451e1942-f575-4d5d-9db9-af4a9609a600)(content(Whitespace\" \
       \")))))((Secondary((id \
       f9237ed6-9e8e-41c3-a9b0-341ff515b9d4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       98abfbdb-4490-4d86-9ac1-c370aa5a3c14)(label(fun ->))(mold((out \
       Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave 13))(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       5bd4dbb8-f4db-45b8-9bf6-51377ad1fd94)(content(Whitespace\" \
       \"))))(Tile((id \
       a41c02a8-e076-4356-8329-1a9d4d1cdfdf)(label(cmp))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       420fe8d1-a228-4c81-9b38-e0500db10f46)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       c09d6ee4-d618-4f83-8053-9f977b42f19b)(content(Whitespace\" \
       \"))))(Tile((id \
       48290c6f-33d0-4947-b130-74196bab38e0)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       9e737380-bfb9-4527-9f60-16dcacecaa83)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       5c4bac44-9c64-41ec-b92d-157ef9c1c741)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       6b88a591-0e40-4622-af2d-95f69f2e499d)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       bd0ef8ac-276f-4490-959f-3a61a0f9fbd3)(content(Whitespace\" \
       \"))))(Tile((id \
       3394fe68-fec7-4c5a-b2ae-29294b1b309d)(label(split))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       70a4bc40-f687-417c-bb05-1472a585afab)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       2325167f-b5b7-4853-be47-b771639c2be8)(content(Whitespace\" \
       \"))))(Tile((id 281b0786-4ee9-4281-92ca-fd62b27aa9f6)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       700e9a87-7004-4d4c-a801-6dcbacb6ea14)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       a76c23da-6945-4f2b-b98b-387684773643)(content(Whitespace\" \
       \"))))(Tile((id \
       0c3b4ecf-a570-4696-9ef0-e4239b986e73)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       7fea6651-1ea1-4de4-963b-0ade89e3cd5d)(content(Whitespace\" \
       \"))))(Tile((id \
       a78ddf72-ae6f-45e8-adf8-d5d3b752b17a)(label(\"(\"\")\"))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       4e0ceb2c-e1e1-43cd-8787-4c4bc7d196da)(label([ ]))(mold((out \
       Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0 1))(children(((Tile((id \
       fbf9c0f8-8c27-4903-8e69-243a4d06fe0c)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Tile((id \
       e8464c36-e6b0-42e8-9f35-0f82dfaa4099)(label(,))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
       14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       6dfdb596-4a33-43b3-9cbc-bc2c10352918)(content(Whitespace\" \
       \"))))(Tile((id 378be4a6-71f1-4c9c-a501-a1aae30574d3)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       1d56c8de-8b67-40fa-b8e6-e41c10b472ce)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
       7247d71b-a70e-48ed-8ebd-f8326ee3678d)(content(Whitespace\" \
       \")))))((Secondary((id \
       7d052b26-eece-426b-bd32-37f43d206451)(content(Whitespace\" \
       \"))))(Tile((id 0e1a76ee-c1fb-449a-82f9-30a9ef037f15)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       63d07920-6cfc-485b-afd9-9fff203ba5ae)(content(Whitespace\" \
       \"))))(Tile((id \
       fe369a66-2ea1-4730-9e82-d6e70201c5ff)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       29def6f2-9970-4c1d-85f8-b15678dc1619)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       d812ad74-2512-420b-8fb4-42527fe7072e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       a9838a00-b3ba-4fbd-b326-68ac970d4f56)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       b639fcf0-4de5-4428-a46a-8fbd816b2a6d)(content(Whitespace\" \
       \"))))(Tile((id \
       77111be7-c95f-4017-9284-052718ceff09)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       3ef7d329-3c25-452f-a636-0e48ece43d90)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       a95a397c-5f07-4c20-9ea6-115ca932f012)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       57cb91fb-44e3-45ef-b4e9-82fc5fb0f6f5)(content(Whitespace\" \
       \"))))(Tile((id \
       91475719-ebb5-4b4a-ac8f-4f178f61236b)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       98dadafa-67ea-4715-a4e2-8ebe767671e3)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       0a5746e2-a79a-4eb2-9c08-90f823effd71)(content(Whitespace\" \
       \"))))(Tile((id \
       b2254461-0f1a-493a-9791-6f035b66c763)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       e30005d6-f8e6-4e81-b335-167f1637b9ba)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       86d1014b-e30c-411d-be72-5ba26c273b30)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       5e568bda-8c19-4784-a5aa-fb5bcb85bef1)(content(Whitespace\" \
       \"))))(Tile((id \
       1d0d7086-a93f-4cf2-9970-dcc4562b60dd)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       49b95dfd-e5c8-451d-8bd9-874fed85a484)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       075418ce-d611-42b0-acc1-8398d1d20b96)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       3c008b0a-dd29-43d4-aab9-cb5910645e8a)(content(Whitespace\" \
       \"))))(Tile((id 7e574e60-daba-454a-8e1f-fcde6f0e7ab8)(label([ \
       ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
       Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
       1ebe058c-aaab-4ae6-a5e2-ad1b1415fcf2)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       7451b934-53db-4f7a-8f74-d5ad94b57b56)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       7bb739ee-d2b0-45f4-bd4e-aa4c5d2f0853)(content(Whitespace\" \
       \"))))(Tile((id \
       4be376c7-2eef-4747-be86-9cb083e6dd79)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       10208713-baba-43a0-bee8-94565ab5f369)(label([ ]))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       27cffe18-aeeb-4910-aea2-4799163d6000)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       f422422b-f9cb-45e6-8fe8-173714c8d09e)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       51a64586-1226-4938-947d-0c25a256c5dc)(content(Whitespace\" \
       \"))))(Tile((id \
       fefad412-721a-49de-af56-cdf043b5cb23)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       2ffde239-a414-43d6-8f28-4237b2045744)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       dc70d3e4-fe28-44f5-ba31-ca013e0a310a)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       49163302-fa5a-44ac-803a-3883a52ba3c4)(content(Whitespace\" \
       \"))))(Tile((id \
       cdd69ccc-ac0c-4439-9c95-eb6620d72c4e)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       2092c076-5764-42c1-a8d4-1e92a277b5bf)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f05cc73c-9701-42b1-8d36-f18232a263c0)(label(y))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       f78f877e-8679-4f9f-a7ed-d6b74b66bc5a)(label(::))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 6))(sort Pat))((shape(Concave 6))(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       94beb919-1f1e-4c4e-80b4-7f73305f7450)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       7b0ed41d-0ba0-4fb4-8ba5-5ba033a3cc9f)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       92164f0b-3aac-4103-a549-fffa5a3d5167)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       90db6b7f-aeb5-49d1-9368-6766c6476e5a)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       680d2932-da58-4725-85de-bc4eab44e8ff)(content(Whitespace\" \
       \"))))(Tile((id \
       05dee8fd-f52b-48ec-92b5-1a98193b61f9)(label(\"(\"\")\"))(mold((out \
       Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0 1))(children(((Tile((id \
       bfa69661-c552-4f23-b3a5-810a53667e91)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1bcccd3f-fd7d-487e-9392-939035d02953)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       0e81455b-8f5a-4e05-9c23-e20c8e762504)(content(Whitespace\" \
       \"))))(Tile((id \
       2f1b7498-3692-43c6-803e-5c1586473033)(label(ys))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       31ecd1f4-bb54-4163-a9c9-d8611607471c)(content(Whitespace\" \
       \")))))((Secondary((id \
       6c0152bb-4926-43b8-bbff-a78ec37cb98a)(content(Whitespace\" \
       \"))))(Tile((id \
       4bf891dd-7cfa-4404-8632-38da053aeef8)(label(split))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       d267636f-d119-4838-8012-1be85ab20d14)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       8e65d4f5-cb82-4983-8b07-a4375630ed92)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       ca5eeead-3315-4f44-844b-3a575eb3e4ae)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       832920c0-a548-4761-897a-1cf3345e2658)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2eaf575b-ddb0-42c5-87bf-0ad2a2806b12)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       67e41b91-ddeb-4539-9b2d-8bd3a1eaa211)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       e857f5dd-e171-4eab-8a01-460d5ec415aa)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       a1b73cc6-6527-45c2-bba5-e473a987c52c)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       6eb8156e-cc43-47f4-9f22-dea3b4719f64)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       bd75a3d4-8601-4f05-b672-1c838ba365c0)(content(Whitespace\" \
       \"))))(Tile((id \
       7abf3b23-5733-4967-a191-104674add9dd)(label(y))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       48e15cc9-3f76-4de1-92a6-acb1c3b6fa38)(label(::))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave 6))(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       363d455b-dd20-4d37-8667-e26a7291d865)(label(ys))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       e3eaba24-f227-42cb-8ec8-2817ee366eca)(content(Whitespace\" \
       \"))))(Secondary((id \
       ee4eebbe-b52b-44fd-a836-68188b230d5e)(content(Whitespace\" \
       \"))))(Secondary((id \
       cf732c23-0297-483a-bb26-528f6d906c5b)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       7c8ff436-76e8-454f-8878-3b0d5c58889c)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2dbebed2-8ca6-4ea8-8140-930524a61d2e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       9fe54afe-529b-43a4-8a45-4b8f2ebd009c)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       93581669-5198-4bcd-a339-f53191f611d6)(content(Whitespace\" \
       \"))))(Tile((id \
       053bce4a-0cb8-430c-ae18-0fb83bb0f5f0)(label(merge_sort))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       51ccbe45-310d-4824-b068-af051419bb4f)(label(:))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
       11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
       ed4e661b-22dd-403f-8ee6-88bc20804a53)(content(Whitespace\" \
       \"))))(Tile((id c0cf1e4f-9fca-48a3-a6be-60bc5d8d67fb)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       60bb720d-fe7a-4ed4-b18e-135deb18bf0f)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       02064e61-b98e-41e6-ae18-551ebdddf30b)(content(Whitespace\" \
       \"))))(Tile((id \
       e91bfc9c-8da7-4fba-8671-836bfd922d05)(label(->))(mold((out \
       Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave 6))(sort \
       Typ))))))(shards(0))(children())))(Secondary((id \
       af8c3bff-ce9e-4834-a3a5-95be230d2e25)(content(Whitespace\" \
       \"))))(Tile((id 3a3c40ad-0ef9-4985-bb47-ce608df1b7bf)(label([ \
       ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
       Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
       9c830659-1df4-4da5-a812-73b95cd804ab)(label(?))(mold((out \
       Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
       Typ))))))(shards(0))(children()))))))))(Secondary((id \
       0a6ebab0-74ce-4d5f-9413-62a1029597c0)(content(Whitespace\" \
       \")))))((Secondary((id \
       7c509689-a490-4fec-b1ee-c3a8bfdc0310)(content(Whitespace\" \
       \"))))(Tile((id 018f2a03-b5cf-485a-837e-7fd581020555)(label(fun \
       ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
       Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
       1))(children(((Secondary((id \
       a028f610-a5d7-4046-8dbe-140814bce7ed)(content(Whitespace\" \
       \"))))(Tile((id \
       c066503c-5648-4bcc-9097-908b659f5379)(label(xs))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       a1ffbdd6-cafe-45da-9685-739e06058c26)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       d2bbb4dd-3b88-4b79-88bc-9ed29d810576)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       917b4913-e6eb-4c08-b85a-e33822ecf346)(label(case end))(mold((out \
       Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Secondary((id \
       75ddf5e0-c4de-4cd6-999c-4064268fbb07)(content(Whitespace\" \
       \"))))(Tile((id \
       5e44af37-dedc-44bf-a93d-22e097a144fc)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       bfbdb8d1-e26c-47c9-b2b2-79967b825514)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       e74878e8-0e92-40d2-af50-59a5b7620bfe)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       8f88d95a-0b7a-4876-9ff5-677b55a5a5af)(content(Whitespace\" \
       \"))))(Tile((id \
       4728e67d-5cdb-4ed8-a421-0c95a7e89980)(label([]))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       28bf69c7-c953-4037-86d5-fa4df8298f3a)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       4567a308-ce22-460b-a91f-230387314b47)(content(Whitespace\" \
       \"))))(Tile((id \
       02278ef4-9585-4945-ac62-a01a14a65593)(label([]))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Secondary((id \
       d21dec06-d0ed-4f8b-b455-d7f60d6c3543)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       0a899425-9bf3-4884-8365-1563fe69a72a)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       6ff563cc-4be0-4790-8181-9565257c6d15)(content(Whitespace\" \
       \"))))(Tile((id 020b962e-2157-4e3f-9618-6392133140c9)(label([ \
       ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
       Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
       afd8cb98-9700-4b7e-ba80-c181485f3ee8)(label(x))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       7e0f82c4-1e6f-4458-9e69-a1c051043a31)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3f156b80-0aad-4edb-b2f5-525aa0a64972)(content(Whitespace\" \
       \"))))(Tile((id 3fe10356-aedc-4ef3-b76e-524f850749d6)(label([ \
       ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
       Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
       5fbfb1ae-ff0e-4129-b1e2-befd13c32e82)(label(x))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       d4dc246e-e673-4e9c-9eb0-bdaf45b50280)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       9cb9ccfe-3e83-4aa3-9e22-4fb84441456e)(label(| =>))(mold((out \
       Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort Exp))((shape(Concave \
       19))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
       3a4a0f96-60c3-4554-8c10-59ec2266b7ad)(content(Whitespace\" \
       \"))))(Tile((id \
       7761d839-1124-43b3-ad0d-215723cc4354)(label(_))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Secondary((id \
       51c42825-fc45-45f1-97d4-df73bae1b560)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       3f606733-36cc-4cb1-ab60-54bf7d4c0a18)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2ef14540-1804-428a-955d-0096853902d9)(label(let = in))(mold((out \
       Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
       16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
       a3f2c6ee-9a1c-45af-9c9b-1b5789c80c56)(content(Whitespace\" \
       \"))))(Tile((id \
       5b09698f-e674-437d-b324-87f0ed45de7a)(label(\"(\"\")\"))(mold((out \
       Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0 1))(children(((Tile((id \
       dbf7540c-edb3-4e2e-aa72-af60483fd6fd)(label(left))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children())))(Tile((id \
       1033f24a-f346-43b1-8dfa-5fbe708a85d1)(label(,))(mold((out \
       Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
       14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
       49ec7930-591e-4368-b08e-5d9ec4c80b7d)(content(Whitespace\" \
       \"))))(Tile((id \
       ae0f0ebd-1711-415a-b019-452afcbff825)(label(right))(mold((out \
       Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
       Pat))))))(shards(0))(children()))))))))(Secondary((id \
       832c39dd-0628-4a44-b888-bf5f83b690a3)(content(Whitespace\" \
       \")))))((Secondary((id \
       194c62b4-f784-4965-9a45-06697c743d58)(content(Whitespace\" \
       \"))))(Tile((id \
       abd62b98-3dbd-4e99-b0f8-e29f36e34c10)(label(split))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       5058d411-9fbe-4375-9106-0e255b846f1a)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       996d1724-bf55-434b-bd8e-97c973d878f9)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       b977ff11-f5cf-4419-88a9-c22b1081b2ad)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       dc252a14-831a-4679-9cd0-7e2ec976eb34)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       051bdfb5-a163-44d9-a31d-8709cb3cc7c4)(label(List.merge))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       7f69db5b-c6ad-4b84-afe6-bd51ad9212d1)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       ee13c64e-d662-449f-b0d2-1411fb0e525d)(label(cmp))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       547a8c08-6182-42c0-a623-9d5239f585e4)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       e1000929-6680-4877-a517-ed290999db88)(content(Whitespace\" \
       \"))))(Tile((id \
       f4d1478d-0ace-421f-8131-cba2a714aeb2)(label(merge_sort))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       c41936fb-bd64-4d14-8b87-55f6affe5c50)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       a48c365e-e35a-451f-b1a6-c925b49eaf5a)(label(left))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Tile((id \
       97b609e0-4424-4d33-9fa8-ff45de619a60)(label(,))(mold((out \
       Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
       14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
       b4702837-a5da-4ce5-8f0d-e14bef22d51c)(content(Whitespace\" \
       \"))))(Tile((id \
       2a886917-fdca-4771-b7d5-230fdc048f9e)(label(merge_sort))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       532df7ad-cbf4-4d97-972c-4e2f5ad77471)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       4884e561-efff-4f2b-9a8d-7f3896284080)(label(right))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
       69a5c616-4d5a-495b-aea0-4a3f4b0789ae)(content(Whitespace\" \
       \"))))(Secondary((id \
       b8993001-f82f-4db2-a39b-591bafc4893c)(content(Whitespace\" \
       \"))))(Secondary((id \
       33ca4130-8335-4958-8266-95c32a214a4f)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
       e9fce966-d347-4da0-973c-150bdd7d58c0)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       c8df00f0-e3e5-45ab-b0de-8f8d92b163c6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       8e8fc419-33d2-46a1-b032-b0a61519a9c1)(label(merge_sort))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children())))(Tile((id \
       a635cf43-1f39-4bf3-a498-015fede81536)(label(\"(\"\")\"))(mold((out \
       Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0 1))(children(((Tile((id \
       b81759ab-eaa4-4315-8731-08e563f7b5a9)(label(xs))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))))))(Secondary((id \
       d894c89e-3eca-4ef6-9874-1bd009533d40)(content(Whitespace\" \
       \")))))))))(Secondary((id \
       2fe8ad60-41ec-475c-89aa-8aff8e3cff15)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
       2ef5133c-a769-4141-a951-4a4a898ae3ad)(label(EXPORT))(mold((out \
       Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
       Exp))))))(shards(0))(children()))))()))(ancestors())))(caret Outer))",
  backup_text: "# Basics #\n\n\
       type Option = None + Some(?) in\n\n\
       let fst: (?, ?) -> ? = fun a, b -> a in\n\
       let snd: (?, ?) -> ? = fun a, b -> b in\n\n\
       let not: Bool -> Bool = fun b -> !b in\n\n\
       let bool_eq: (Bool, Bool) -> Bool =\n\
       fun a, b -> a && b \\/ !a && !b in\n\n\n\
       # Lists #\n\n\
       # Add an element to the front of a list. #\n\
       let List.cons: (?, [?]) -> [?] =\n\
       fun x, xs -> x::xs in\n\n\
       # Determine the length of a list. #\n\
       let List.length: [?] -> Int =\n\
       fun xs ->\n\
       case xs\n\
       | [] => 0\n\
       | _::xs => 1 + List.length(xs) end in\n\n\
       # Extract the head of the list. #\n\
       let List.hd: [?] -> ? =\n\
       fun l -> \n\
       case l  \n\
       | [] => ?\n\
       | x::xs => x end in\n\n\
       # Extract the rest of the list. #\n\
       let List.tl: [?] -> [?] =\n\
       fun l ->\n\
       case l \n\
       | [] => ?\n\
       | x::xs => xs end in\n\n\
       # Determine if a list is empty. #\n\
       let List.is_empty: [?] -> Bool =\n\
       fun xs ->\n\
       case xs\n\
       | [] => true\n\
       | _::_ => false end in\n\n\
       let List.nth: ([?], Int) -> ? =\n\
       fun xs, n ->\n\
       case xs, n\n\
       | x::_, 0 => x\n\
       | _::xs, n => List.nth(xs, n - 1)\n\
       | [], _ => ? end in\n\n\
       # Reverse a List. #\n\
       let List.rev: [?] -> [?] =\n\
       fun l -> \n\
       let go: ([?], [?]) -> [?] =\n\
       fun xs, acc -> \n\
       case xs \n\
       | [] => acc \n\
       | x::xs => go(xs, x::acc) end in\n\
       go(l, []) in\n\n\
       # Initialize a list with a given length using an initializer function #\n\
       let List.init: (Int, Int -> ?) -> [?] =\n\
       fun len, f ->\n\
       let go: (Int, [?]) -> [?] =\n\
       fun idx, xs ->\n\
       if idx < len  \n\
       then go(idx + 1, xs @ [f(idx)])    \n\
       else xs in\n\
       go(0, []) in\n\n\
       # Check if two lists are equal #\n\
       let List.equal: (? -> Bool, [?], [?]) -> Bool =\n\
       fun p, xs, ys ->\n\
       case xs, ys\n\
       | [], [] => true\n\
       | x::xs, y::ys => p(x, y) && List.equal(p, xs, ys)\n\
       | _ => false end in\n\n\
       let List.eq = List.equal in\n\n\
       # Reduce a list from the left. #\n\
       let List.fold_left: ((?, ?)-> ?, ?, [?])-> ?   =\n\
       fun f, acc, xs ->\n\
       case xs \n\
       | [] => acc\n\
       | hd::tl => List.fold_left(f, f(acc, hd), tl) end in\n\n\
       # Reduce a list from the right. #\n\
       let List.fold_right: ((?, ?)-> ?, [?], ?)-> ? =\n\
       fun f, xs, acc ->\n\
       case xs\n\
       | [] => acc\n\
       | hd::tl => f(hd, List.fold_right(f, tl, acc)) end in\n\n\
       let List.fold_left2: ((?, ?, ?) -> ?, ?, [?], [?]) -> [?] = \n\
       fun f, acc, xs, ys ->\n\
       case xs, ys\n\
       | [], [] => acc\n\
       | x::xs, y::ys =>\n\
       List.fold_left2(f, f(acc, x, y), xs, ys)\n\
       | _ => ? end in\n\n\
       let List.fold_right2: ((?, ?, ?) -> ?, [?], [?], ?) -> [?] =\n\
       fun f, acc, xs, ys ->\n\
       case xs, ys\n\
       | [], [] => acc\n\
       | x::xs, y::ys =>\n\
       f(x, y, List.fold_right2(f, xs, ys, acc))\n\
       | _ => ? end in\n\n\
       let List.map: (? -> ?, [?]) -> ? =\n\
       fun f, xs ->\n\
       List.fold_right(fun x, acc -> f(x)::acc, xs, []) in\n\n\
       let List.map2: ((?,?) -> ?, [?], [?]) -> [?] =\n\
       fun f, xs, ys ->\n\
       List.fold_left2(\n\
       fun x, y, acc -> f(x, y)::acc, xs, ys, []) in\n\n\
       # Keep elements that satisfy the test. #\n\
       let List.filter: (? -> Bool, [?]) -> [?] =\n\
       fun p, xs ->\n\
       case xs\n\
       | [] => []\n\
       | x::xs =>\n\
       let xs = List.filter(p, xs) in \n\
       if p(x) then x :: xs else xs end in\n\n\
       let List.append: (([?], [?]) -> [?]) =\n\
       fun xs, ys -> List.fold_right(List.cons, xs, ys) in\n\n\
       let List.concat: [[?]] -> [?] =\n\
       fun xss -> List.fold_right(List.append, xss, [])  in\n\n\
       let List.flatten = List.concat in\n\n\
       let List.mapi: ((Int, ?) -> ?, [?]) -> [?] =\n\
       fun f, xs ->\n\
       let go: ? -> ? = fun idx, xs ->\n\
       case xs\n\
       | [] => []\n\
       | hd::tl => f(idx, hd)::go(idx + 1, tl) end in\n\
       go(0, xs) in\n\n\
       let List.filteri: ((Int, ?) -> Bool, [?]) -> [?] =\n\
       fun f, xs ->\n\
       List.concat(List.mapi(\n\
       fun i, x -> if f(i, x) then [x] else [], xs)) in\n\n\
       let List.exists: (? -> Bool, [?]) -> Bool =\n\
       fun p, xs ->\n\
       case xs\n\
       | [] => false\n\
       | x::xs => p(x) \\/ List.exists(p, xs) end in\n\n\
       let List.for_all: (? -> Bool, [?]) -> Bool =\n\
       fun p, xs -> not(List.exists(fun x -> not(p(x)), xs)) in\n\n\
       let List.mem = fun eq, xs, y ->\n\
       List.exists(fun x -> eq(x, y), xs) in\n\n\
       let List.filter_map: ((? -> Option), [?]) -> [?] =\n\
       fun f, xs ->\n\
       List.fold_right(\n\
       fun x, acc ->\n\
       case f(x)\n\
       | None => acc\n\
       | Some(y) => y::acc end,\n\
       xs,\n\
       []) in\n\n\
       let List.concat_map: ((? -> [?]), [?]) -> [?] =\n\
       fun f, xs ->\n\
       List.fold_right(\n\
       fun x, acc -> List.append(f(x), acc),\n\
       xs,\n\
       []) in\n\n\
       let List.for_all2: (((?, ?) -> Bool), [?], [?]) -> Bool =\n\
       fun p, xs, ys ->\n\
       case xs, ys\n\
       | [], [] => true\n\
       | x::xs, y::ys => p(x, y) && List.for_all2(p, xs, ys)\n\
       | _ => false end in\n\n\
       let List.exists2: (((?, ?) -> Bool), [?], [?]) -> Bool =\n\
       fun p, xs, ys ->\n\
       case xs, ys\n\
       | [], [] => false\n\
       | x::xs, y::ys => p(x, y) \\/ List.exists2(p, xs, ys)\n\
       | _ => false end in\n\n\
       let List.find: (? -> Bool, [?]) -> Option =\n\
       fun p, xs ->\n\
       case xs\n\
       | [] => None\n\
       | x::xs => if p(x) then Some(x) else List.find(p, xs) \n\
       end in\n\n\
       let List.partition: (? -> Bool, [?]) -> ([?], [?]) =\n\
       fun p, xs ->\n\
       List.fold_right(\n\
       fun x, (ys, zs) ->\n\
       if p(x) then (x::ys, zs) else (ys, x::zs),\n\
       xs,\n\
       ([], [])) in\n\n\
       let List.split: ([?], ?) -> ([?], [?]) =\n\
       fun xs, n ->\n\
       let go: ([?], ?, [?], [?]) -> ([?], [?]) =\n\
       fun xs, n, ys, zs ->\n\
       case xs, n\n\
       | _, 0 => (List.rev(ys), zs)\n\
       | [], _ => (List.rev(ys), [])\n\
       | x::xs, n => go(xs, n-1, x::ys, zs @ [x])  \n\
       end in\n\
       go(xs, n, [], []) in\n\n\
       let List.combine: ([?], [?]) -> [?] =\n\
       fun xs, ys ->\n\
       case xs, ys\n\
       | [], _ => []\n\
       | _, [] => []\n\
       | x::xs, y::ys => (x, y)::List.combine(xs, ys)  \n\
       end in\n\n\
       let List.rev_append: ([?], [?]) -> [?] = fun xs, ys ->\n\
       let go: ([?], [?]) -> [?] = fun xs, acc ->\n\
       case xs\n\
       | [] => acc\n\
       | x::xs => go(xs, x::acc)  \n\
       end in\n\
       go(xs, ys) in\n\n\
       let List.merge: ((?, ?) -> Int, [?], [?]) -> [?] =\n\
       fun cmp, xs, ys ->\n\
       let go: ([?], [?], [?]) -> [?] =\n\
       fun xs, ys, acc ->\n\
       case xs, ys\n\
       | [], [] => List.rev(acc)\n\
       | [], ys => List.rev_append(acc, ys)\n\
       | xs, [] => List.rev_append(acc, xs)\n\
       | x::xs, y::ys =>\n\
       if cmp(x, y) <= 0  \n\
       then go(xs, y::ys, x::acc)  \n\
       else go(x::xs, ys, y::acc)  \n\
       end in\n\
       go(xs, ys, []) in\n\n\
       let List.sort: ((?, ?) ->Int, [?]) -> [?] =\n\
       fun cmp, xs ->\n\
       let split: [?] -> ([?], [?]) = fun xs ->\n\
       case xs\n\
       | [] => ([], [])\n\
       | [x] => ([x], [])\n\
       | x::y::ys =>\n\
       let (xs, ys) = split(ys) in\n\
       (x::xs, y::ys)  \n\
       end in\n\
       let merge_sort: [?] -> [?] = fun xs ->\n\
       case xs\n\
       | [] => []\n\
       | [x] => [x]\n\
       | _ =>\n\
       let (left, right) = split(xs) in\n\
       List.merge(cmp, merge_sort(left), merge_sort(right))  \n\
       end in\n\
       merge_sort(xs) in\n\
       EXPORT",
};

let z: Zipper.t = PersistentZipper.unpersist(pz);

let ctx_init = Builtins.ctx_init;

let ctx =
  z
  |> Zipper.zip
  |> MakeTerm.go
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, ctx_init)
  |> Id.Map.find_opt(Hyper.export_id)
  |> (
    fun
    | None => ctx_init
    | Some(info) => Info.ctx_of(info)
  );

let env_init = Builtins.env_init;

let env: Environment.t =
  z
  |> Interface.eval_z(~settings=CoreSettings.on, ~env_init, ~ctx_init)
  |> ProgramResult.get_state
  |> EvaluatorState.get_tests
  |> TestMap.lookup(Hyper.export_id)
  |> (
    fun
    | None
    | Some([]) => env_init
    | Some([(_, _, env), ..._]) => env
  );

print_endline("Common context built");
