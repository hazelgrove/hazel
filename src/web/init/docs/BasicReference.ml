let out : string * Haz3lcore.PersistentSegment.t =
  ( "Basic Reference",
    {
      segment =
        "((Secondary((id \
         b56a23ce-ef0f-4897-8bd0-4b22b9388467)(content(Comment\"# Hazel \
         Language Quick Reference #\"))))(Secondary((id \
         b03ec82a-050b-4cd9-bdeb-900fd36efb35)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce78a235-0b0a-4525-87cb-6fa866623589)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f80b8d9-8f65-40d2-9277-db5d959dbdb2)(content(Comment\"# Empty holes \
         stand for missing expressions, patterns, or types \
         #\"))))(Secondary((id \
         404f8be4-e455-4430-91f0-e41c6b0424d6)(content(Whitespace\"\\n\"))))(Tile((id \
         95d7c487-18cd-4be3-9a31-0b9e3694daa2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2006c410-6af9-4f8b-84bb-8987c31a8797)(content(Whitespace\" \
         \"))))(Tile((id \
         42418552-dc98-4a31-bdb0-af92e07827ab)(label(empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7abeafe3-46ba-4af6-9cda-90211d8668da)(content(Whitespace\" \
         \")))))((Grout((id 83883e24-3468-4968-8096-59538b36a540)(shape \
         Convex)))(Secondary((id \
         ccb73c72-e8f8-4123-b12e-648033604c5d)(content(Whitespace\" \
         \"))))(Secondary((id \
         061a9083-1252-489b-aa53-c784fb7ebc87)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9fce97ee-7ed8-4cca-93db-866f11c49568)(content(Whitespace\"\\n\"))))(Secondary((id \
         87fcd9a6-6966-4a6d-9834-3125d93d6294)(content(Whitespace\"\\n\"))))(Secondary((id \
         9eb1bb0b-b800-4413-943b-23bc6b53e76e)(content(Comment\"# Non-empty \
         holes are the red boxes around type errors #\"))))(Secondary((id \
         1d78d02d-cfff-425b-80f9-39103f262626)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f86c912-b8ee-4d63-bdcd-379dcd18944c)(content(Comment\"# (you can \
         still run programs with non-empty holes) #\"))))(Secondary((id \
         cd59f5a3-5d83-4ab8-986f-60f7a151594b)(content(Whitespace\"\\n\"))))(Tile((id \
         b3d144bb-87a1-4d93-b23b-a8a97c12c7a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         71415b3e-637d-4c71-be8e-6073cfebf1e7)(content(Whitespace\" \
         \"))))(Tile((id \
         90b1175b-e28f-403f-8a6d-11c9b0d17fb7)(label(non_empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5d902dfe-49ee-46f1-9fc4-63ae1d2f5558)(content(Whitespace\" \
         \"))))(Tile((id \
         13ae5047-15f9-4b99-9b0b-bdd75d2538c1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6b42337e-653a-4d73-ad0d-ac74e1da5912)(content(Whitespace\" \
         \"))))(Tile((id \
         1811c56c-2805-4b99-a5d9-bc18fd9bfc78)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         02ffab76-3aa8-4252-99a0-da8271216696)(content(Whitespace\" \
         \")))))((Secondary((id \
         dd382001-63af-457e-b897-028d561956f7)(content(Whitespace\" \
         \"))))(Tile((id \
         97a52451-72dd-4487-b3f9-0f246448d237)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e358815b-e40d-4637-8001-fc8f4e36d30c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2b729054-cfb6-4fa8-9e51-f29d08ba8d0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         22736240-887d-470d-a52a-4cd26967e984)(content(Whitespace\"\\n\"))))(Secondary((id \
         95142098-af4f-4a18-a59f-e406ed930e69)(content(Comment\"# Booleans \
         #\"))))(Secondary((id \
         d8b75924-7c09-4932-90e0-918b70696df8)(content(Whitespace\"\\n\"))))(Tile((id \
         afddc8c8-6cbb-40a7-a4d1-2a8347920668)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c3d2c0ad-2868-46c8-9396-55ff62704b15)(content(Whitespace\" \
         \"))))(Tile((id \
         c1199f36-a19c-41ea-99c3-151f1998c17a)(label(bool))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ef7ba888-56b7-46a8-a880-c2a827b52d28)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ff85958f-ceee-4e49-a021-db52954df9f6)(content(Whitespace\" \
         \"))))(Tile((id \
         e3d87160-b461-4d08-b468-814d4d989927)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e8fe2fe3-bc6c-4457-b37e-1d1895273fce)(content(Whitespace\" \
         \")))))((Secondary((id \
         64ed59ca-b1c9-45f2-be36-7cef80e1b6d4)(content(Whitespace\" \
         \"))))(Tile((id \
         d98435f8-5ac4-457a-8390-c98b74ebae94)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0d8ae0a-4179-4ea6-a497-cd86dc3a11ba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af551021-05b5-48ac-b08e-c243822c4c0b)(content(Whitespace\"\\n\"))))(Tile((id \
         ab080043-b37d-421f-b38a-2b0b175cf5c4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         889c44a1-868b-46b8-be35-93b30e8e493e)(content(Whitespace\" \
         \"))))(Tile((id \
         46290d91-58a2-4e47-b21c-4d896e48b1e2)(label(operators))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         022f259b-fb39-40bd-897e-405dd32376ab)(content(Whitespace\" \
         \")))))((Secondary((id \
         7afcc927-a23d-4215-9752-b3eace89993d)(content(Whitespace\" \
         \"))))(Tile((id \
         b2e4acdb-563e-4207-afad-bcf265a7ae03)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3341f728-0a62-4ecc-85ee-5ed0e51b8c73)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ebb39cb-f025-4b30-8690-68866ab855e5)(content(Whitespace\" \
         \"))))(Tile((id \
         e50b4fc4-1cf5-459f-b9b7-f41ca104103b)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88600e38-c0eb-4cc8-8d23-09aeef6d3203)(content(Whitespace\" \
         \"))))(Tile((id \
         9e19b0c3-55e9-4366-ad40-18d5aa6957b9)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7956c8b8-c5bc-43ce-8d24-dd648a49d4df)(content(Whitespace\" \
         \"))))(Tile((id \
         8ffd4742-8137-4a68-b415-fd4fc6e1a95a)(label(||))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 33))(sort Exp))((shape(Concave \
         33))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         415e01d8-1041-4f33-b40c-4b494c94711d)(content(Whitespace\" \
         \"))))(Tile((id \
         eca58afd-7295-4003-a1f2-d6730f9ec98d)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d016c35b-c8fa-474b-a163-07b31a13d8d8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9587596-72b3-4243-ba8e-b28702c7c94c)(content(Whitespace\"\\n\"))))(Secondary((id \
         614809ae-4c97-4b4c-a415-6c70ca5768da)(content(Whitespace\"\\n\"))))(Tile((id \
         09062565-f399-47dd-b29b-a7b041e4ba28)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d259008b-af1e-4e3f-b5b7-acb896c7a2db)(content(Whitespace\" \
         \"))))(Tile((id \
         a8db951e-0121-4e0a-8044-46862a620e94)(label(conditional))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d5a507d1-9bf6-493b-8768-412d8c9f5caf)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca890188-b856-45d5-9cbf-9edc7cb82967)(content(Whitespace\" \
         \"))))(Tile((id ea99db26-8cc1-458b-bf7f-eb570a01b059)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         98f40056-1bd7-43c1-bc42-424e766969b5)(content(Whitespace\" \
         \"))))(Tile((id \
         8db3ce0b-92a5-4283-aab9-e1340bb3b129)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39931d34-7e01-46cf-bb10-fb500adc6230)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         abc27f17-13b1-48f5-87d8-c1974eac7218)(content(Whitespace\" \
         \")))))((Secondary((id \
         b97e8b9d-730c-4aa8-9bf9-4554bbcb30e9)(content(Whitespace\" \
         \"))))(Tile((id \
         df3fc7ad-4cd6-406e-9c84-d0f12fac4e1b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18172b42-e567-4958-b69c-9088136bd2db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f11da9d3-e7fc-459c-9f48-a491b8427138)(content(Whitespace\" \
         \"))))(Tile((id \
         1c73207f-1baf-4ca1-b95a-6a3904bc1f45)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d1b5314b-6c27-4117-bca5-fd298a9871c3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8427993f-c202-407c-9fc4-0112e57a57cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         18a5bd93-327f-48f1-b160-87406965a5d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f252cf3-7360-4733-ba52-645c302ca421)(content(Comment\"# Integers \
         #\"))))(Secondary((id \
         09aef17f-9a84-4a2b-9b2c-4d8b5048faf3)(content(Whitespace\"\\n\"))))(Tile((id \
         c0e060db-6583-451a-b68e-2c3acc63d812)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb3bfa36-397a-4810-820b-9d4f316aedc5)(content(Whitespace\" \
         \"))))(Tile((id \
         1d9477ad-80ca-4d59-96cf-5f7cbddaf343)(label(num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ac289241-64bd-4dec-b116-25c33578a9ba)(content(Whitespace\" \
         \")))))((Secondary((id \
         713001b8-dd9d-425a-bf5d-686ef9814c12)(content(Whitespace\" \
         \"))))(Tile((id \
         afba77a7-3466-4768-b4c5-4d637d74b6f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5096e9ef-ff18-4160-bdcb-a9767a5fbfb0)(content(Whitespace\" \
         \"))))(Tile((id \
         da2bee6c-efe6-4e44-bdaf-627cbba6575d)(label(:))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 24))(sort Exp))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         742967f9-d636-4c80-a0ce-ea3957330cb5)(content(Whitespace\" \
         \"))))(Tile((id \
         e9dde623-6c59-485f-8cd1-9295eafb9e78)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         958b43b2-2691-4be5-ac96-01dde67637c7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b56b0433-2e07-4d41-98e5-bacfe7568236)(content(Whitespace\" \
         \"))))(Secondary((id \
         705e93c8-5740-4b43-9fc0-ad62604a4a98)(content(Whitespace\"\\n\"))))(Tile((id \
         00afed5f-2c9f-45b3-973b-5204fd5a03e0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ad1100f-8c80-43f9-8c80-bef486788aa5)(content(Whitespace\" \
         \"))))(Tile((id \
         f5161c92-4d93-488b-9177-3daaee60806d)(label(arithmetic))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         805da594-4af4-4420-9ac8-dcce869af5dd)(content(Whitespace\" \
         \")))))((Secondary((id \
         76f4a0c3-edcf-43a5-ae57-d56e3ebc346f)(content(Whitespace\" \
         \"))))(Tile((id \
         3b531267-930e-470c-b167-4616004a614e)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b801b22-0d1f-4357-a0cd-615807606bb5)(label(num))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         03b78400-eaca-49bb-9e9a-de252eadba00)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a4f22e58-5c07-4e17-ae0e-3cbb59663d62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a6104d9-e356-47d1-a340-dc30907dbc86)(content(Whitespace\" \
         \"))))(Tile((id \
         0efdc489-e5f5-402b-af23-dfb86bf20588)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd00f014-7772-4fd8-bde2-8707a5a8496f)(content(Whitespace\" \
         \"))))(Tile((id \
         e95eee24-00cf-4e46-adf9-cd10c8100f1b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54845913-1d67-40ad-9f63-90de636a349b)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c564783a-3c00-41fa-ba56-fc8d7db5e15c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8fbaaf52-1db2-42fa-bdf0-ba4680ab01ed)(content(Whitespace\" \
         \"))))(Tile((id \
         ae3fa2d8-4de6-49b8-9774-fdba63d6d449)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16d17c2f-cf83-4401-86f5-acca34887d4a)(content(Whitespace\" \
         \"))))(Tile((id \
         cb793c72-2724-418d-a404-8880393dcc94)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d85cc4c7-d2e6-4c54-86cb-31729416b1a7)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         173207bb-d8fa-42d0-ad36-01604b166b2b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2fa5ab6b-2321-437d-abda-57d63f2aee92)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         105afc13-f709-4c09-aa79-9eeda361b7b5)(content(Whitespace\"\\n\"))))(Tile((id \
         d2dfe886-8732-4001-8a9e-73e58133b7a5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc980cb8-bfac-48d8-8e9b-73328a6e58ee)(content(Whitespace\" \
         \"))))(Tile((id \
         cee8c070-f208-41f6-a105-7a8b1df18c97)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4e64f811-6ab9-45ce-938d-9e7329b808b5)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f048b81-fcd4-4149-a69d-d1881dff6bba)(content(Whitespace\"\\n\"))))(Tile((id \
         86e986b3-9973-4faf-8626-d3ad5063ec3a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a318cf9c-2436-4a6a-8edf-492f3afcb2aa)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17e1120a-5189-44bb-b33e-8b17fba8010b)(content(Whitespace\" \
         \"))))(Tile((id \
         ff9c05d6-95b5-4d3d-81d4-010efa51dda5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e43459f4-8a43-4bfc-91d0-4ccb0637b530)(content(Whitespace\" \
         \"))))(Tile((id \
         76768e9c-f277-4177-9eba-07124a2ce3b2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9402532-3cff-4d8a-b84d-008a15151513)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c34b72f5-e642-4abf-94a1-5689f3c257b1)(content(Whitespace\" \
         \"))))(Tile((id \
         1e51e7c1-c981-4e1e-b2fd-e7fc9c46a4dd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b4f898b8-eb90-4610-83a0-840a33c47bea)(content(Whitespace\" \
         \"))))(Tile((id \
         c2a5a9f5-2458-4682-905e-71c5898e8dfe)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41f2bd4f-7da8-4e0d-9d82-79db9af67428)(content(Whitespace\" \
         \"))))(Tile((id \
         41a953d3-6253-404b-9286-d48fd312a89f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         297b4925-77fe-4fb3-898f-d39367932d36)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79b56b51-fc54-4225-b7be-52851fd6dc59)(content(Whitespace\" \
         \"))))(Tile((id \
         09fd6bdb-4a48-4950-a28f-790c87076e35)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         54eb9175-630b-484b-8222-9eb762a7c6da)(content(Whitespace\" \
         \"))))(Tile((id \
         b698f922-b8fb-407b-a64b-c35c605dd6bb)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6903c5b-9f7b-41fa-b976-66cb9eb5dea3)(content(Whitespace\" \
         \"))))(Tile((id \
         097e9e1f-c511-4559-bbfa-1d7b07659ded)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc085015-2b61-4712-8148-e838efd95e8b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7d4b86c-7b47-4a37-a92a-26dfad48eefe)(content(Whitespace\" \
         \"))))(Tile((id \
         b3c7d783-05d1-4380-b857-2b8d77d1fceb)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d655323a-0880-4e9d-a64b-287a5ceee2b7)(content(Whitespace\" \
         \"))))(Tile((id \
         3a54d3eb-367f-4123-b763-18fed270b7c4)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd557a94-9962-43cb-9a47-dd2fe8847430)(content(Whitespace\" \
         \"))))(Tile((id \
         684add4c-8a7e-4709-9fcd-d2b47f38be37)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92a347a8-8738-47b3-a161-0c05f617e85c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76432f4d-2c74-4d1b-973c-891b6026d0da)(content(Whitespace\" \
         \"))))(Tile((id \
         2253e7f7-3c35-4995-9207-b4aae9c2b0b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         53482a1c-965c-49ed-9b1c-8f972b4d823d)(content(Whitespace\" \
         \"))))(Tile((id \
         1a54f737-432e-4afe-a3da-e801cc1a89e4)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfa26d31-f6de-4cb8-805f-17ecd7c9b984)(content(Whitespace\" \
         \"))))(Tile((id \
         bb8f48a6-ccf2-4935-94c0-5c8889077f13)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         208d8648-d1f1-4dfa-8cce-54e7362fcfe8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6c5107dc-6b28-4c39-8a3e-90aabdc0d409)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cab3c3b3-2619-499a-851d-a2218d32bf98)(content(Whitespace\"\\n\"))))(Secondary((id \
         6fbccafb-4a45-489a-bbb7-59cc0f4d980c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0420673c-d958-43f6-9418-c3118f3b2926)(content(Comment\"# Integers are \
         unlimited by default #\"))))(Secondary((id \
         e48f61be-9963-434c-8716-beab7c8f6b49)(content(Whitespace\"\\n\"))))(Tile((id \
         d62aab18-90b7-45b9-9511-5a285848997e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b726e49f-85f6-4dd4-9a2d-b168dd487de8)(content(Whitespace\" \
         \"))))(Tile((id \
         630a6388-39ca-4f2f-a86d-39dbad34c598)(label(big_num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5ba96ace-095e-4d86-b44d-820af03a1cf2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         40aaa610-2ad9-46b1-a110-5836b5772f69)(content(Whitespace\" \
         \"))))(Tile((id \
         470d230b-15af-4f5d-80f7-8ec7f713847e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aebdbe10-f3f0-4d23-b0b6-ac00b81a8b78)(content(Whitespace\" \
         \")))))((Secondary((id \
         1d445a8f-edff-4649-8b35-900806115148)(content(Whitespace\" \
         \"))))(Tile((id \
         27e922f8-1ae5-467a-8fdc-a5aee0a15c2d)(label(10000000000000000000000000))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4bd74f8c-f5a8-4600-9a75-39f1c67649ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1955e2d8-1780-4974-b739-db71105d9266)(content(Whitespace\"\\n\"))))(Secondary((id \
         7bb46ae2-2b4c-43cc-ab1a-1e886606e3c8)(content(Comment\"# Use SInt for \
         fixed-with system integers #\"))))(Secondary((id \
         ffcc7e74-5880-46c2-b6f3-48d9468270af)(content(Whitespace\"\\n\"))))(Tile((id \
         34226e2f-36f5-4405-9619-be217b297d40)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0f2475c3-b76b-42d9-b277-39f687d81b19)(content(Whitespace\" \
         \"))))(Tile((id \
         5f08615c-9f59-4341-ae3e-ed20d8cd359d)(label(bad_num))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8884b314-e300-4a39-875e-bcbada03b581)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9ccd596c-24a4-49b6-a719-f4743f4dc6da)(content(Whitespace\" \
         \"))))(Tile((id \
         6b445a2c-9fa4-4081-a9b4-94eb721a2691)(label(SInt))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1307723f-70cc-406e-9b7f-3406af058898)(content(Whitespace\" \
         \")))))((Secondary((id \
         866114da-bd00-46f6-9d36-00ceda74a749)(content(Whitespace\" \
         \"))))(Tile((id \
         fb70538f-62eb-462d-94c1-f11aa1e27c04)(label(1000000000000000000000000))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         90716499-71a5-4e18-be64-fee06b39a834)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c4b3d164-829a-4e32-927e-e96b2fe2bafa)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ad7cfe1-9c49-48df-9f11-9a48ecbf37c9)(content(Comment\"# Use Nat for \
         non-negative integers #\"))))(Secondary((id \
         f8a20e3f-dc2f-4db4-9583-d850fa2fd2f0)(content(Whitespace\"\\n\"))))(Tile((id \
         5ba909a6-51a7-47c1-be72-9f906dec3540)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea15a56c-50a6-40f8-9fb4-99480f80081a)(content(Whitespace\" \
         \"))))(Tile((id \
         0cb245a5-1de6-4cae-aad9-a3200eed3ed0)(label(nat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ae82208e-1f20-436c-8d54-d002f2ffecc4)(content(Whitespace\" \
         \"))))(Tile((id \
         402b9d6c-c031-4532-8108-72be6a4f9f7d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23ef9b1a-ef64-46ba-9a05-6c8cdfe6db86)(content(Whitespace\" \
         \"))))(Tile((id \
         7ec0a2d8-9ac8-40e1-876c-742bb140183b)(label(Nat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3d0d385d-001b-4bed-924e-77149f95d9e6)(content(Whitespace\" \
         \")))))((Secondary((id \
         7de72b4f-34e9-45fc-99d9-2af4e72f6bb0)(content(Whitespace\" \
         \"))))(Tile((id \
         417160d1-6480-4d17-9578-a0c23ccc87dc)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9386bd16-5460-41e3-99e5-67edd44139ac)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c6710ffd-82f1-42ae-ba41-2cc94b51ada4)(content(Whitespace\"\\n\"))))(Secondary((id \
         b23b04df-cf7b-4227-a113-1f0a72095326)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2d31389-4fd7-4dde-931a-cd5af837b42d)(content(Comment\"# Floating \
         Point Numbers #\"))))(Secondary((id \
         1bd190aa-9dcc-4555-baca-9dfa1060fa31)(content(Whitespace\"\\n\"))))(Tile((id \
         d5f39ac4-aa13-4c0f-bc3c-538ef13f4ae1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         88246f44-0163-420d-b833-65a30c3c30d5)(content(Whitespace\" \
         \"))))(Tile((id \
         23906372-3713-4b2f-8101-f6aa6037e22e)(label(float))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6eca6b4e-adbc-4c76-9b71-60db5a49ca0e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73d026b5-beec-4ab3-bd26-f9057f761026)(content(Whitespace\" \
         \"))))(Tile((id \
         f7de30d9-1fcc-4162-a781-ea1f74e2d0e7)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6a08c747-b5ce-48ca-9c50-0a87e44e1a58)(content(Whitespace\" \
         \")))))((Secondary((id \
         a50f3d87-6663-4c25-b0f0-07d95a953b82)(content(Whitespace\" \
         \"))))(Tile((id \
         a54e8493-0529-4781-82b2-2a1fd573727d)(label(0.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8549b6dc-8942-44ce-92da-f2fb572dd4fb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         449f87e9-33fc-4cbd-bb69-4650583ff718)(content(Whitespace\"\\n\"))))(Tile((id \
         59f16b8f-1308-4d1e-b502-a9599388c410)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3cb291a8-9635-474e-924a-db1f49aa8620)(content(Whitespace\" \
         \"))))(Tile((id \
         2e6a80e4-c91b-4d29-983a-311151dbcbc4)(label(arithmetic))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8af57a6f-b1dd-474a-8831-c26f6271e51f)(content(Whitespace\" \
         \")))))((Secondary((id \
         92be73de-08c0-4fbe-bfc4-fb1a6cc21907)(content(Whitespace\" \
         \"))))(Tile((id \
         21106a0f-8818-47ea-8ba3-f552fdeba64f)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0a95d6cd-34df-44c1-9a2b-70fc90dabe6d)(content(Whitespace\" \
         \"))))(Tile((id \
         a8329801-aec9-425d-9be1-c7c4b3160130)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ad5ca50-ebfa-4867-a347-fff976e27952)(content(Whitespace\" \
         \"))))(Tile((id \
         df3b8933-aae7-4490-83de-59af64ad02fb)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26bc7729-2e48-4441-a8fb-555bc65fd5b4)(content(Whitespace\" \
         \"))))(Tile((id \
         1c7d079b-e21c-4698-aca3-96706a1caaa7)(label(+.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1923dc6e-d3df-4a83-a922-ef4ef114087a)(content(Whitespace\" \
         \"))))(Tile((id \
         d3f80712-fd78-4b3c-93b7-837e7def99d2)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb2db746-c97b-4f1b-908e-d5d406d2a241)(content(Whitespace\" \
         \"))))(Tile((id \
         0f6163a1-dc5f-4ab5-b824-7f69c0542898)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b776534-2471-454a-bebd-fdee0037684a)(content(Whitespace\" \
         \"))))(Tile((id \
         d38a7a1f-5e7d-414d-b33b-6520929cc67c)(label(3.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0d37c11d-3c00-4da0-9c5f-0d9894309416)(content(Whitespace\" \
         \"))))(Tile((id \
         d4e34fbd-70d2-4b48-9bb1-9c8e8fadd687)(label(-.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd0e2df0-84a7-478d-9bdc-38aaf7582710)(content(Whitespace\" \
         \"))))(Tile((id \
         e18d8908-7010-4480-bd7a-a227d73f832c)(label(4.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e5d4391-0ac6-4251-88b1-5a89c0335359)(content(Whitespace\" \
         \"))))(Tile((id \
         8816cb43-2447-4310-b6cf-a566b2975dc0)(label(**.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e9881b1-b53f-415e-b737-9dd2e0d546bd)(content(Whitespace\" \
         \"))))(Tile((id \
         d20a3241-88bc-4ed9-8dce-25c37efa9f8e)(label(5.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5f7e57ce-4d7c-4bcc-bf63-d0116f0e8459)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e8a2e332-45db-437f-86cf-a58a89eb8dae)(content(Whitespace\"\\n\"))))(Tile((id \
         703785e1-f209-404e-b9ef-1896c8766b89)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         589b303d-d559-4c1f-bdfd-4ba5ba747347)(content(Whitespace\" \
         \"))))(Tile((id \
         645cde16-667a-4500-abc9-0dc1b0c21fbc)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a484aa99-58bb-4259-88b8-cf79456a0504)(content(Whitespace\" \
         \")))))((Secondary((id \
         1ac02e77-4622-47d1-92bc-166aa45b44a6)(content(Whitespace\"\\n\"))))(Tile((id \
         3d9ae5e6-cfac-4f92-bce3-4dc04b2c3443)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9fcca074-a672-4098-a501-9c69cbc076a0)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3109203f-923d-4e6f-ad2b-cd7911553d5f)(content(Whitespace\" \
         \"))))(Tile((id \
         1bca3731-9b50-4b25-9709-c4825cdc22e9)(label(==.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ceccaf99-5297-4918-a24d-50bbb1b3b114)(content(Whitespace\" \
         \"))))(Tile((id \
         d57bd064-9822-4490-ae6d-7064105a353b)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         122c1027-aeb4-4618-8449-451f89ea0801)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fdbacaa8-be06-4818-b33f-ec0de8e0e165)(content(Whitespace\" \
         \"))))(Tile((id \
         00afd2b2-6161-40bc-9809-82db67c80057)(label(0.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         766d5ae6-65d9-43c4-bd2d-4a1e2c13ee41)(content(Whitespace\" \
         \"))))(Tile((id \
         6aa21aa5-664c-4803-8f22-b6564642fbd9)(label(<.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d11c7b5a-6387-42bd-aecd-53b683a5a3a9)(content(Whitespace\" \
         \"))))(Tile((id \
         7acba465-eda3-4ec5-aa5b-5f5e6c27a017)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0df7792-4545-42ca-acff-ce3581278039)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fac35b7-2e12-4d38-a21a-41159874f491)(content(Whitespace\" \
         \"))))(Tile((id \
         d68923af-7bd2-48dd-b500-0a32348bc4e2)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de74a8e1-18aa-46f9-9f34-cacc061672bb)(content(Whitespace\" \
         \"))))(Tile((id \
         a8d1282e-ac81-4d95-bce9-42a91b51db80)(label(<=.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7027794-354d-4024-9403-30616dd460a0)(content(Whitespace\" \
         \"))))(Tile((id \
         8cc304ee-4b74-4776-8e31-5823370b426a)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80ea9eae-0ed2-4ae6-8175-eee49a19c7ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         143f406a-e92a-4ad3-807b-d47f38a70ba6)(content(Whitespace\" \
         \"))))(Tile((id \
         92c58411-6c26-4021-a824-9538a2caaa8b)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b4d68448-cd4d-4668-8a00-0a7e224f888f)(content(Whitespace\" \
         \"))))(Tile((id \
         1ca7952a-c403-484f-bcb1-eed64ff8f5dd)(label(>.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         446f3631-392e-46f5-9e0e-7a680a8802bf)(content(Whitespace\" \
         \"))))(Tile((id \
         cf11d459-108f-43e9-8fd4-5f18c5e53ee7)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18ecede4-46ec-441a-93c9-4db0758b8925)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a63cbc8-a3d0-43a4-a783-5ad4f5d3b367)(content(Whitespace\" \
         \"))))(Tile((id \
         8fa3ebd9-deb2-4b76-976d-db435db12f57)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91624c6f-842e-439f-8eb2-b60c14fb25e4)(content(Whitespace\" \
         \"))))(Tile((id \
         d9269857-1a12-4c65-9a49-f9a66c1a4af8)(label(>=.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c57861f8-9d11-46ef-a030-61f88b21b12a)(content(Whitespace\" \
         \"))))(Tile((id \
         01fa3f74-eb64-46ea-b3a7-e40576f8b369)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70ce74e4-3e74-4c6a-a475-fcea2007e80d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7d47dfad-fe6d-454c-a993-ec8ace4cef6a)(content(Whitespace\"\\n\"))))(Secondary((id \
         99772c12-c401-4cd1-a610-4abec96598b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         02845a7d-443d-4b22-aa2c-8dd068e5121a)(content(Comment\"# \\\"use\\\" \
         lets you set the default number format #\"))))(Secondary((id \
         2470225b-f9c2-4f6e-9fe3-83439a348e51)(content(Whitespace\"\\n\"))))(Secondary((id \
         004770ca-54a9-4177-a168-8277a45ac4be)(content(Comment\"# for literals \
         and operators. #\"))))(Secondary((id \
         6e76bbe5-c65f-47c0-a03f-11ae7efead98)(content(Whitespace\"\\n\"))))(Tile((id \
         7870c523-0f79-44fc-bdd3-cab89afe80d8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         49c43488-63c7-4aa6-8988-f457d88794c5)(content(Whitespace\" \
         \"))))(Tile((id \
         9c60d1e3-117b-4de5-8984-a50dcae001a6)(label(natural))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6012d365-7cec-499b-8558-edeb38511759)(content(Whitespace\" \
         \")))))((Secondary((id \
         7baa10e7-ea45-4d40-ba75-6e26ecdab10b)(content(Whitespace\" \
         \"))))(Secondary((id \
         78502e00-9d86-4920-b945-870311f3a744)(content(Whitespace\"\\n\"))))(Tile((id \
         d08db4d0-7442-4dc9-aced-b006a554e3c2)(label(use in))(mold((out \
         Exp)(in_(Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7dae5dba-2083-400e-883e-1970583ec0ac)(content(Whitespace\" \
         \"))))(Tile((id \
         dabe95d7-0ffa-4e2b-a6fa-1aea7d76cc24)(label(Nat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c96678be-181b-43d2-a659-583b8a90ef30)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         750bd72b-8a19-49d5-86a4-e452b8b8832f)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe83a573-f0e6-42f6-988c-59e5e7b9a05a)(content(Whitespace\"\\n\"))))(Tile((id \
         2bd0d749-1ad9-4236-a571-010cd73c8a74)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4bfcabb3-3428-4480-b826-4cea611e1a23)(content(Whitespace\" \
         \"))))(Tile((id \
         2b44f6b7-be19-435f-afe7-89058c87826b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5ad97b1-1b44-4cf5-b818-32463c6b00b7)(content(Whitespace\" \
         \"))))(Tile((id \
         74d4dcd4-4576-455c-8770-e0657ee58fce)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         495e6a53-962b-4a65-8a02-2eabeac02028)(content(Whitespace\" \
         \"))))(Tile((id \
         ea9cd221-82c1-452a-ba33-99fb6824f941)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3ee68ac-b41c-47d7-ba2a-a649a7c4edd3)(content(Whitespace\" \
         \"))))(Tile((id \
         29c59389-5333-4144-9901-292b6e592ce1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9682b7f0-8400-4fed-9af7-0e689d82de38)(content(Whitespace\" \
         \"))))(Secondary((id \
         ffe1c0b7-7228-4b9c-9b09-734651173b80)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9b9fc054-eae1-4aaa-8f6d-e33dd209a5a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d71163c-37f4-47c4-a49e-bd02efd5ce99)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eb71221-489d-4ee0-9a07-a91e60c5fdd7)(content(Comment\"# Strings \
         #\"))))(Secondary((id \
         500b1f3b-f11b-4bcd-8c4f-73954742fe2f)(content(Whitespace\"\\n\"))))(Tile((id \
         0dcf3659-0b1f-41ab-8902-2863fcb33dd1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         67dc1ab9-add9-48b7-b714-61e275d9c8ae)(content(Whitespace\" \
         \"))))(Tile((id \
         e425bf94-64bf-4504-939b-8844e6fe7f99)(label(string))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         45c1e470-9aa7-4de5-91c4-eb0a517fe57b)(content(Whitespace\" \
         \")))))((Secondary((id \
         452bb8bd-6d49-43f1-9007-26e49d29ae84)(content(Whitespace\" \
         \"))))(Tile((id \
         12a01645-24e0-411f-9be2-6c3c0c1ff5f7)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a10717c6-856e-4c06-b04f-eb373955bff8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fe8834dd-a84a-4eef-9950-b5919d61ae23)(content(Whitespace\" \
         \"))))(Secondary((id \
         84f50e64-920b-454c-b56c-57ddc25545e6)(content(Whitespace\"\\n\"))))(Tile((id \
         2be3070b-5c22-4ba4-9b2b-a681cdc8f4e7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9676358d-0129-48e0-a53a-a3c7c8d41714)(content(Whitespace\" \
         \"))))(Tile((id \
         be7bb9a3-fb8b-47d0-98d9-48e08a610388)(label(concatenation))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3b601af3-0335-4149-852c-c88d595b56df)(content(Whitespace\" \
         \"))))(Secondary((id \
         9dc13a79-7ad7-45b2-9a34-e46831011902)(content(Whitespace\" \
         \")))))((Secondary((id \
         977b5db2-469d-484a-8e7b-57f0aee4f686)(content(Whitespace\" \
         \"))))(Tile((id \
         6d1c2d44-01c7-4274-ab0c-f771ccd3022f)(label(string))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         139deebf-e898-4031-b13a-f7736b518802)(content(Whitespace\" \
         \"))))(Tile((id \
         e2207454-4847-4b82-8b9e-92ad60a3df97)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4fd2a71-d916-442e-9552-6f8dc3f5d35a)(content(Whitespace\" \
         \"))))(Tile((id fa194f57-4a4f-4731-a2ea-7f0fededbb49)(label(\"\\\" \
         Goodbye.\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fcd44e40-e2c2-49a1-b30a-4c967124e456)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f58268cd-1cd1-42d9-9079-e98ce34b3232)(content(Whitespace\"\\n\"))))(Tile((id \
         931c19a5-76bb-4e4f-b0d1-43b1c5430246)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6eaf4f5d-2406-4722-8236-2a84e7e18194)(content(Whitespace\" \
         \"))))(Tile((id \
         bf77f423-0c01-4180-a17f-f1c07507c26e)(label(comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4bb3c821-7d83-401d-8a67-cf27ce93fdce)(content(Whitespace\" \
         \")))))((Secondary((id \
         78aae566-d8de-498d-99bb-b3320bb62581)(content(Whitespace\" \
         \"))))(Tile((id \
         4ee57c3c-3434-42a3-bf70-64de1540bd28)(label(string))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba2ee14e-ce55-4a95-862d-28a8509c5b9c)(content(Whitespace\" \
         \"))))(Tile((id \
         ca344c2f-8765-4eb1-8814-e5cef909e4b3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d1e1b20-777f-47d6-9412-8577267a8b7b)(content(Whitespace\" \
         \"))))(Tile((id \
         53e0bebb-74b9-4479-b0db-13fa981fc85a)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd892998-7f91-4ed1-a979-10c8c2c2b78c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6314e118-bfaf-4334-ac17-f426dfa9a2ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         78756120-a9b6-47f2-8cd4-a1a0ce5da49f)(content(Whitespace\"\\n\"))))(Secondary((id \
         40ced349-ffb7-44ac-8396-f04f047183eb)(content(Comment\"# Tuples \
         (Destructured with let expressions) #\"))))(Secondary((id \
         3e3c593e-6069-48b4-8f70-be56d43ea776)(content(Whitespace\"\\n\"))))(Tile((id \
         93a240e5-3e5d-439a-a58e-a68257b87d8f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         606fd9d4-6e88-44b7-846e-9c6c5b660089)(content(Whitespace\" \
         \"))))(Tile((id \
         00af43a2-b04f-4306-bb79-832c3ff6ecc0)(label(tuple))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8358bc1f-db55-4f2f-9938-c533634ff64f)(content(Whitespace\" \
         \"))))(Tile((id \
         bf4705a6-634b-4477-9539-5f6dff7870ff)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         756d86e6-ffa7-4e1c-b182-cdcebda50aff)(content(Whitespace\" \
         \"))))(Tile((id \
         6fb04fad-c350-471c-b8cb-6ab893f8e0b0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         d6b669e7-1a56-4e56-aaef-496a217ec2e9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ae880ff5-382a-4f61-b0ef-fbfdd2f1c36a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         408e3384-7e5b-415d-bac6-d45ba3e4962c)(content(Whitespace\" \
         \"))))(Tile((id \
         eca64a76-f855-4c11-9d27-547814e5f51b)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         af56ce33-f0ee-4e83-a644-b2fd42914110)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7b414fb7-1e52-4ad0-bbec-97900a32a9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         9ceac9b2-120e-4d88-8808-c0ec8fb1f6ef)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         39aba13b-f809-4473-989b-b4fa6dbdd4ce)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9b52bf25-3476-4ebf-9745-0d0a6f81c273)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b9d67cb1-fd8d-479b-8b84-e67e5d6dc4ac)(content(Whitespace\" \
         \"))))(Tile((id \
         d1e619d3-b0ee-4eff-a527-93d150a755d0)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         e5744d22-32d3-46d8-8f8e-2598ce8db94a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6cb21c14-18d0-4aed-a6fc-99b9c10f5107)(content(Whitespace\"\\n\"))))(Tile((id \
         91e0ce7a-b0fd-464c-9965-a52a8f52ae25)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c734cc38-a9eb-4937-9e1c-8e657d9427a1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         369472fb-ee96-4103-8a20-c078c4215fcd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5d0c581-12aa-428a-8b65-ae165407cb73)(content(Whitespace\" \
         \"))))(Tile((id \
         141c5f50-658f-4203-8de8-f6bf5d4ebfe6)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         050b69c2-a657-4d8d-83fd-cbe8b7ed1a2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e9536c7-521d-4849-a9d4-ae10d908143b)(content(Whitespace\" \
         \"))))(Tile((id \
         eb2e0790-61b0-4ad6-a26f-ec074dcd66fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0acb9931-5e39-4dc4-8c5b-e577ecfabc88)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e22fb2bc-acba-4c9a-bd69-b19e3cc6fa2c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1c2d3e3-50e0-4929-9f74-c00c7c254692)(content(Whitespace\" \
         \"))))(Tile((id \
         7e0a8d61-bffb-4393-9e1f-0572084664f9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         764d6a4d-500e-4f2e-bf66-a2fdd91678d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b7da38c5-32f6-44b2-b221-25b734b58c7c)(content(Whitespace\"\\n\"))))(Tile((id \
         21f5b507-ca86-4eb8-b02f-e58f2d6c0601)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae75890d-c444-4b61-922a-535000e0683a)(content(Whitespace\" \
         \"))))(Tile((id \
         af6cc9ea-97d2-4b67-9cd6-c1282ae39106)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2cbba52a-9d03-46fb-8b6b-6d516bdf7bea)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4ba7ab76-8534-4313-a5ff-b30faee5b76a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e7159913-3a06-4793-a12c-0df504723044)(content(Whitespace\" \
         \"))))(Tile((id \
         a03ca7c9-b4cc-4fd0-87c0-c3d10982f693)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         14d9a3e8-3dc6-49c5-9f8c-0104068d546c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         98d371f8-ee73-490d-bad3-9afa6e880331)(content(Whitespace\" \
         \"))))(Tile((id \
         925c58c2-1f8d-484d-9ce8-a3e799f5ea27)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7b9403e0-f167-40d7-bb2c-c54c7a0dc4db)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e40259a8-a6f1-4798-877e-e169496614c5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         05d41713-0233-47d8-aed6-b42903967435)(content(Whitespace\" \
         \"))))(Tile((id \
         2959292a-082f-4180-b24a-582b863585d9)(label(d))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
         ee07a80b-40f1-48d5-aa04-16593b3de690)(content(Whitespace\" \
         \")))))((Secondary((id \
         bf35b19f-0766-44df-b737-d76433a93b12)(content(Whitespace\" \
         \"))))(Tile((id \
         5bd859ad-23b8-4274-8f5a-4e35b6df0e03)(label(tuple))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d13605e9-3108-4672-9ca2-e508b8f0f4f3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         372c0f26-2d2d-401c-8adf-50f9cef81bbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         23a02257-a298-48a7-ad8b-ba05451ef968)(content(Whitespace\"\\n\"))))(Secondary((id \
         1af4be43-9f25-4bb2-9a9f-fcca0a24e03d)(content(Comment\"# Functions \
         (Take a single argument which can be a tuple) #\"))))(Secondary((id \
         d95fa62e-2d7e-475a-8289-d42c88b1df4a)(content(Whitespace\"\\n\"))))(Tile((id \
         58a24180-8f47-4e2f-9b63-68f9f7008999)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         88799eee-bdc2-4335-921f-60b8cc6a6229)(content(Whitespace\" \
         \"))))(Tile((id \
         ec75f334-ac32-47e6-b7de-27f091c72787)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2132fa8c-2939-464d-ac52-b0ff3d428cc4)(content(Whitespace\" \
         \"))))(Tile((id \
         894a9092-d709-4090-8bb6-7284641f4429)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a546bdac-e517-42b8-97ea-72f8e1435b55)(content(Whitespace\" \
         \"))))(Tile((id \
         843ba668-d8a6-4434-a884-002adb3c4523)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f638c77c-0b1e-4ac9-af38-075215c75d63)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7cbb9a8f-d362-4fbc-a9aa-18a14262259a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1b6fba55-4917-4817-b690-ee9bb453bbe7)(content(Whitespace\" \
         \"))))(Tile((id \
         dd0dfa43-3dee-4996-818c-2e001fdd2d96)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3a4d2e73-a17f-4800-b29d-9b3363d9acc6)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d989cbbc-3b72-4d50-9b3e-66a2ab5e81eb)(content(Whitespace\" \
         \"))))(Tile((id \
         482c0f62-52a2-4bd5-b4b0-11b327350a0f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ce2045fc-0a74-4f24-9709-19a368f9ad68)(content(Whitespace\" \
         \"))))(Tile((id \
         49d01548-33ac-4d4e-a353-ddd525b7dabf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bbb4da25-106c-438c-ad64-8a2de9b12c96)(content(Whitespace\" \
         \"))))(Tile((id \
         aeb39e8d-f4c6-466b-acf2-a6304aad1adf)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4895eee8-658d-4f11-890e-4ff44caa04a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         48122840-fec1-4dd6-a300-0ddf37bbd5b2)(content(Whitespace\"\\n\"))))(Tile((id \
         c59f38e9-3f73-420a-9d12-de577710f767)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         66598685-3dcf-4fcb-a7c9-b36f627b40bd)(content(Whitespace\" \
         \"))))(Tile((id \
         7de92e4e-9d76-4ffd-8cb9-f5187b5421d0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9c162573-8781-406f-a295-5b371bd5f213)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         566d5ccd-0f77-4eea-9268-79c260d0c378)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9cd3dc87-260a-4333-91eb-c5d3b97ca05c)(content(Whitespace\" \
         \"))))(Tile((id \
         e3715222-6f0b-4e98-a6b4-17bc89cbccca)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         57cdb88f-1395-46c6-906d-1879f7e4a0da)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1098e902-3217-4d1a-9a31-cf97c618f61d)(content(Whitespace\" \
         \"))))(Tile((id \
         bea5f5fe-7e90-4b94-88c8-bf6317cb3dea)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5b4cec47-62f5-4819-a00e-30a4b4c9e382)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         17be0bfb-d7c7-4cc0-8ee3-04cfd7f37de2)(content(Whitespace\" \
         \"))))(Tile((id \
         08cdfc76-936b-4c70-a900-f4bed9e11f28)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b424cc72-13bb-405a-bad8-977ddbf2da48)(content(Whitespace\" \
         \"))))(Tile((id \
         6e5b6e1a-85a3-4367-b6ba-443e1675fac9)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e09b2a9-fc31-4ac7-858c-3182f0525613)(content(Whitespace\" \
         \"))))(Tile((id \
         48003034-8b4e-4df0-a260-4d625aa210da)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89f48818-485a-47c7-a40a-94edc74ebdee)(content(Whitespace\" \
         \"))))(Tile((id \
         9df3e536-723b-4b1c-9797-c4de6f4b9679)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5b9cfde-041f-481d-a3f1-212e5db4c37d)(content(Whitespace\" \
         \"))))(Tile((id \
         19532c79-ff68-48c7-80f1-16ecec71f869)(label(b))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         98f4c8ce-d12a-4643-b959-f0b2a800a8d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1050fc6f-5bd5-4e30-bba5-6b3622bde4d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         011791e4-5fa8-4ff3-a4ae-539d4a65e028)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8b8583f-63c4-48d0-8d4f-9ee839b72af6)(content(Comment\"# Recursive \
         Functions (Arrow type annotation required) #\"))))(Secondary((id \
         f31043c2-cbfc-4084-856c-12eb80ce9f92)(content(Whitespace\"\\n\"))))(Tile((id \
         1ab652a6-ac1c-4976-a157-d7511dc53c0a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6987669f-83f2-42e7-bf26-c000ae168759)(content(Whitespace\" \
         \"))))(Tile((id \
         c5ab3179-98de-4500-b4cc-1f1e040209f9)(label(double_recursively))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a7eb475f-9032-4447-bfa1-24b24fade97e)(content(Whitespace\" \
         \"))))(Tile((id \
         c202f061-937a-4cf4-926d-f4191f8f61fb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         001efb0f-6533-48d6-ba59-80c2a1ce45e3)(content(Whitespace\" \
         \"))))(Tile((id \
         6d8ebfb2-d9a9-467d-8897-b22389fd2a33)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2f129cbc-3feb-4e6e-97b1-19f4e7377d62)(content(Whitespace\" \
         \"))))(Tile((id \
         ca9e1999-3a7f-4429-b751-1392472f7eb8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0ee070c1-7229-4321-a013-ec33785997bb)(content(Whitespace\" \
         \"))))(Tile((id \
         554973bb-676c-4759-9ba1-016414bfc758)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b0415724-ec25-4bc6-875b-20067a47f69a)(content(Whitespace\" \
         \")))))((Secondary((id \
         5dad22d1-5260-496d-aedd-cfe100a7fa95)(content(Whitespace\"\\n\"))))(Tile((id \
         16f75b4b-d383-4e4b-b9c9-79c7fa295a37)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e7d94d04-211d-479d-b290-f1f0b00a8ee9)(content(Whitespace\" \
         \"))))(Tile((id \
         fd9adb23-8a2e-4bb7-aaa0-6a7add16bd85)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         89c08b5e-4407-44d5-9e25-8267f16e3a95)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         31a54eda-e255-4614-a35d-7b0ed3a95c71)(content(Whitespace\"\\n\"))))(Tile((id \
         2a2f38ea-e855-4d5c-bbbb-7018e6a565fa)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eacb8344-3819-4a6a-9e01-2d39e0bc768e)(content(Whitespace\" \
         \"))))(Tile((id \
         bca8ae82-caa8-4df6-aad8-da8c48f817cb)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc9b5e1e-864e-47af-8d73-8890fa3e3871)(content(Whitespace\" \
         \"))))(Tile((id \
         f33fb3d8-e948-42ef-b913-b483188e88af)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c4db808-39c3-4874-9977-465bf3342a5e)(content(Whitespace\" \
         \"))))(Tile((id \
         afa08494-9e41-421f-abf3-d39a98446c06)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f66768a-80d4-48b3-9070-d642625d978b)(content(Whitespace\"\\n\")))))((Secondary((id \
         e97288a8-9434-4c03-bac8-2b6398828aaa)(content(Whitespace\" \
         \"))))(Tile((id \
         80d5f530-3b29-4f77-ae94-aafc71b163d8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f7ad0e1-7064-41dd-afef-2d5f6d913412)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9ae7ff08-df85-4018-ba8b-62cabcdf6cfe)(content(Whitespace\" \
         \"))))(Tile((id \
         7e20f2e1-4845-4fd9-a425-0446724154ca)(label(double_recursively))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09332df7-1a00-496a-bf6f-48d43db25104)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f9449c80-8cd7-44c5-b178-f61d7ccafaf8)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         64814885-523b-49d2-b3ee-64dcf9aa4331)(content(Whitespace\" \
         \"))))(Tile((id \
         e22d0cf3-72a2-4512-b922-286fdf12af83)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b7e03c9-2012-438b-b385-3406d04f0d37)(content(Whitespace\" \
         \"))))(Tile((id \
         fc893cda-8ef3-4c84-88b2-501a98d82d10)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         35608240-1bb2-4929-a9c9-f0fbb356afa6)(content(Whitespace\" \
         \"))))(Tile((id \
         a1cc2b0d-a824-4f8f-a250-5a389d9205ec)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5546fe36-d4ee-48fc-be32-52d65ea224bb)(content(Whitespace\" \
         \"))))(Tile((id \
         4965cb17-8e87-4e9a-8843-53700c3b17b8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ab3962e-3f38-470b-8a7e-545769afeb69)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03f410eb-7ae8-4a36-9568-4eb453ca81fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5880cba-580f-4883-b5c5-9db28b31c2dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         48c61a92-591c-44b3-b06c-a0009bd3010c)(content(Comment\"# Mutual \
         Recursion (bind tuples of functions) #\"))))(Secondary((id \
         3bc956ee-4c85-4d41-b7b3-e133a8371230)(content(Whitespace\"\\n\"))))(Tile((id \
         3ba72d5f-53e1-4617-82ac-c74351f9eca0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc17a032-982d-483f-be92-1f7ac5e0d18f)(content(Whitespace\" \
         \"))))(Tile((id \
         27889ca0-ce5a-4e54-9fe9-e36832c56c33)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9adc4752-6ffe-4785-b8c0-cb964cc62ffa)(label(even))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e83a3d33-241d-4357-ba8a-38c53c3f31fb)(content(Whitespace\" \
         \"))))(Tile((id \
         7aebe72f-0b3a-4c20-af74-5267afad5bdf)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1fc1be58-4dc4-4984-85b1-67463394c3fa)(content(Whitespace\" \
         \"))))(Tile((id \
         bc74344e-46d5-44b3-91b8-6c4a2319d49c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9fd58a3b-ae38-4a30-94dc-a791341c5b86)(content(Whitespace\" \
         \"))))(Tile((id \
         9d2d2f65-026b-4438-a739-50991bc1e749)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d4ee5a33-4ff9-46bd-99f8-888ce245f1e9)(content(Whitespace\" \
         \"))))(Tile((id \
         f8e5171b-8a9f-4a14-86c0-0b8ddfbc8ddf)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         86514867-e791-48a5-a869-655370981993)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         52533440-2255-44df-b372-8b177479ca0a)(content(Whitespace\" \
         \"))))(Tile((id \
         0d70a9ac-2dc9-4c43-8a62-c295ed941153)(label(odd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         641fe1d1-ac59-46b5-8012-e5e4a2c0b94c)(content(Whitespace\" \
         \"))))(Tile((id \
         20da8641-77f4-438b-ada7-6781ea65ea6b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         12b14c6b-6d43-4620-9934-dc6e10dea126)(content(Whitespace\" \
         \"))))(Tile((id \
         3c237b3e-c27a-4641-bb93-4c00bee56937)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2b5d7971-bb8b-4069-b8aa-7b649ea133af)(content(Whitespace\" \
         \"))))(Tile((id \
         c5744fa1-f82b-461d-a859-5163d09b46a5)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea87ea4a-6e7d-4831-a711-f7ffccfcf81f)(content(Whitespace\" \
         \"))))(Tile((id \
         bf2ba963-dddb-4f60-a571-8c522f16b8d7)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d39fee3c-469a-4a83-9e66-195abb450041)(content(Whitespace\" \
         \")))))((Secondary((id \
         deba44a5-7865-497c-b500-f7f74b553041)(content(Whitespace\" \
         \"))))(Tile((id \
         5aae2d8a-511a-4103-8a76-34a2b338747d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         152d2f9f-f91c-46f1-b753-a6fef09adca2)(content(Whitespace\"\\n\"))))(Tile((id \
         0703f5bb-da7e-41b4-8661-157ab303fe6b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         42c27b1e-b132-456a-90dc-e9d9436c7266)(content(Whitespace\" \
         \"))))(Tile((id \
         b66b03b9-323d-46cd-aabb-6a0c74bcb5aa)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d7e56c7c-7d6f-47fb-931d-e1029e569832)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6c6cbe8d-e34c-4303-ab38-9318a893d4fa)(content(Whitespace\" \
         \"))))(Tile((id af192c08-bc1f-458e-861d-9008490bb926)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         20879231-b156-43d8-8c2d-f61fa4fd3d4f)(content(Whitespace\" \
         \"))))(Tile((id \
         0f0d209e-2d5e-4d03-8943-aa07190e2355)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8f3d2c1-7c8c-4e18-a99b-709d13a4600f)(content(Whitespace\" \
         \"))))(Tile((id \
         87e160cc-8d8b-4335-b9a3-b50fd308615d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         172a9180-10fa-4244-80bc-b4f40e9ad388)(content(Whitespace\" \
         \"))))(Tile((id \
         2d624526-8d8e-430e-9088-ba8e65271df6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         027a011d-d130-4808-8b2b-ccd9a8c8f9f6)(content(Whitespace\" \
         \")))))((Secondary((id \
         a4f578a5-e3de-4be4-b878-08eee22e24e7)(content(Whitespace\" \
         \"))))(Tile((id \
         7c2c81ee-9c9b-46fb-b078-70ab8f3a9cb1)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be59c9d0-9087-49a9-8a20-2e770241ce71)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b927870-be18-401c-ac40-77ff0ff48d10)(content(Whitespace\" \
         \"))))(Tile((id \
         79189d5c-d1b1-4164-a059-2bd48964e398)(label(odd))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f106e59-1835-4520-978e-dcacb3431eee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ffb6dff7-a2df-4b79-92d3-c4a0f3f1885f)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52742e90-0fd0-4f2d-b3c4-e216900c6619)(content(Whitespace\" \
         \"))))(Tile((id \
         2028ab83-0d3b-42ae-a31d-929a6b223ca7)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31f98250-16b8-4a5c-b67b-9ceab2dbbc60)(content(Whitespace\" \
         \"))))(Tile((id \
         a0005614-67fc-45be-9ad1-735942694e69)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bef37ec0-00e9-436e-b7cb-92a3b6c738fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ed32a2f-7ebd-4ab7-9616-d31bb076f537)(content(Whitespace\"\\n\"))))(Tile((id \
         652cc6eb-220a-4571-baa4-04294815aa71)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4164183e-88e9-4231-abf2-06d68e49d1cc)(content(Whitespace\" \
         \"))))(Tile((id \
         1816e2c3-6679-431c-baa2-d24f6fca2fb5)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         75614fe5-9b8a-4d55-a115-c2298719c503)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46b65362-f9ce-4d6e-8eca-26741f9fc685)(content(Whitespace\" \
         \"))))(Tile((id a07b2eca-3e7a-49eb-ad53-94142d850ca2)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         b18a2b6b-e82d-441e-b88d-27f086170003)(content(Whitespace\" \
         \"))))(Tile((id \
         c3af87a8-cf9f-41dc-a629-d57f2ec00687)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdfdf9f7-e151-4fa7-8f7b-75202db0f8e2)(content(Whitespace\" \
         \"))))(Tile((id \
         8047cc43-844d-4916-b83e-9af57c1e9536)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba6c22e6-cb7a-4f75-a746-21513ef5227a)(content(Whitespace\" \
         \"))))(Tile((id \
         560c67f6-1652-4213-8c5a-3ec91755eb84)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb1fa87f-9602-4eec-bbe8-e4d8da3b7be3)(content(Whitespace\" \
         \")))))((Secondary((id \
         501057af-1eef-4d9b-a88d-d14c432e01c0)(content(Whitespace\" \
         \"))))(Tile((id \
         6ecb6f02-f118-455d-9917-2958b513128c)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         688186d0-6026-44be-a412-eac35ff228f4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e9d624cc-cf5b-4b40-bd64-19d399a93b7c)(content(Whitespace\" \
         \"))))(Tile((id \
         39a1f0bd-6399-46e1-adea-ae3f86c93e15)(label(even))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e7ddd2b-fed1-4b45-a534-3cd4660d0011)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         43117674-4221-4111-bfcb-40c162c7882a)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         182cff53-1074-4c0d-adf1-599bdd6001ad)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb7f2ec-73a9-4296-af0d-6ad1ebf4bc21)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         abfd63f4-a8eb-4b26-b935-075144c1ceb4)(content(Whitespace\" \
         \"))))(Tile((id \
         c6063854-80f3-4a29-996a-a6359f3e56b3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         30b7db53-3564-458f-b756-1be2df6fe3bb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d82a17ce-f4d1-4f33-836d-d578f3313a28)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e2c2afe2-283f-4779-9ec5-acd659bbf54d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b45dac5-a191-4a68-8753-6245908dbcc9)(content(Whitespace\"\\n\"))))(Secondary((id \
         202c67f2-98e3-4482-954b-188b4bb71636)(content(Comment\"# Lists \
         #\"))))(Secondary((id \
         411e0fd9-e63e-44be-8039-fe2117a48c8b)(content(Whitespace\"\\n\"))))(Tile((id \
         bfd9cf5d-dd99-46c0-91ac-e395647754fc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8f9729e3-a98e-41f2-8f6e-88febed9029e)(content(Whitespace\" \
         \"))))(Tile((id \
         a0f258ed-0127-4bd4-afb7-8dd6b0392dbb)(label(empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1921a5db-b4d8-4126-8f7b-b243b8df99d3)(content(Whitespace\" \
         \"))))(Tile((id \
         f6225ca0-f1f6-4c96-bc4e-f3fe6d896718)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b39ef55e-b3ff-4c34-85da-6f1c3cb344b5)(content(Whitespace\" \
         \"))))(Tile((id df478586-faec-49bb-acbc-b53cbcbc7106)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f2a71a97-3478-4ad7-9841-039b1c2b50ba)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3840633b-2f30-4705-ba4f-1222b990efe0)(content(Whitespace\" \
         \")))))((Secondary((id \
         ab06e0eb-76b9-48aa-be54-553181ca930e)(content(Whitespace\" \
         \"))))(Tile((id \
         114f4e20-2207-4023-8edb-d0684a312d34)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         58b21a69-f17d-433b-8150-545f001122db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be3c226d-f15d-4c10-b125-7840de9d6412)(content(Whitespace\"\\n\"))))(Tile((id \
         57c00e18-dcfe-4b01-9ba8-2c2356e2b9ba)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a268352c-5050-43fc-89b9-0b13c4f73a80)(content(Whitespace\" \
         \"))))(Tile((id \
         e6ac22f4-01c0-45e9-8e17-21172e1b283b)(label(non_empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b80ac797-25ea-4fa8-bb42-a5ad5fa567a6)(content(Whitespace\" \
         \"))))(Tile((id \
         447ce4c2-43e1-4269-b00d-73edf31ca17c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c4ac118-244e-4c4e-a750-faf6191e6118)(content(Whitespace\" \
         \"))))(Tile((id 5bb9c2c7-55cf-4ec6-a69d-df1e4721c4fe)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6fa62da2-3b39-4b78-832b-8927efa9e4c0)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         98a4348a-2d59-4c02-bbe7-62c0696517d1)(content(Whitespace\" \
         \")))))((Secondary((id \
         1bd455df-c165-4c5c-9a0c-7bab9abd1932)(content(Whitespace\" \
         \"))))(Tile((id \
         238eb85f-59a4-463b-8c67-801639b7d6ed)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c899b14-1138-49a4-a2ad-bda700bbf599)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5ba4eca8-b55c-4361-834c-7abcbb89e542)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad52af16-049d-4e72-a2b4-c601eec11c47)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0d49ad36-81bf-4436-858e-7b0c3ad4658a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a755b522-4ae0-461e-9ca6-f506dfacd736)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b8c072a2-760b-449e-812d-375b30b31764)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd74934c-fc73-46d5-8713-ac8ae11bb689)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f69b3039-2a00-44f9-b478-f2b60685ef69)(content(Whitespace\"\\n\"))))(Tile((id \
         2b891327-e94e-49cf-a16a-2840604e549b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         392da23f-6d80-4835-bcfe-b7afb5e58d27)(content(Whitespace\" \
         \"))))(Tile((id \
         b584a1d4-f322-4eef-b0e5-7e06755907d4)(label(list_literals))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         11874fbb-ddfd-4004-b35d-bf62b51b0c67)(content(Whitespace\" \
         \"))))(Tile((id \
         d3d857ca-f27c-4593-9872-6a478eb6bb69)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3d77faed-7e79-44a8-ad9c-026ff2ebffac)(content(Whitespace\" \
         \"))))(Tile((id 8a16fb29-6aae-4f3f-bfbb-81cadd10486e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         affb20a3-93b8-4e69-9ca6-ffbfb8856383)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         71ddff4e-91b6-42fa-babc-8949452786b1)(content(Whitespace\" \
         \")))))((Secondary((id \
         26576b4d-e5ba-4f27-af1a-4a2297112c67)(content(Whitespace\" \
         \"))))(Tile((id a94e2229-36d0-42e1-b9cb-c920d5303f7e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8af3a1a-0151-4031-8477-b2b4867ca113)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a1eeccb-f795-417c-a3a2-8f201e9196f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea13a231-5040-4669-b667-c4e715025dc2)(content(Whitespace\" \
         \"))))(Tile((id \
         e0649d13-604a-4699-a277-6f3361eca614)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6b105bb-bcd7-4c3f-a4e8-be2385c1b9e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d7a0b8e-d099-4715-86aa-5529670805d1)(content(Whitespace\" \
         \"))))(Tile((id \
         5d121ee9-fa1b-4d1a-9b57-371a6991226c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9ce0c793-bded-4ec2-9896-facb11e8e81a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca24b9d0-2ad4-42ab-a83a-71259f6f8a26)(content(Whitespace\"\\n\"))))(Tile((id \
         70e4a5d8-0426-401e-bebd-bbeb0fbf966d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         17baef4f-738d-45b9-947b-247e0896d44b)(content(Whitespace\" \
         \"))))(Tile((id \
         6e4e6762-ee7c-4fb5-a2cb-a0dee0b0b6a5)(label(length))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         913bddc5-1fa4-4606-a596-45939656c756)(content(Whitespace\" \
         \"))))(Tile((id \
         8781ee09-7269-4f58-8ec6-ef944ac28a79)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5e1c4f45-990e-4087-81e2-9e567b94a4b4)(content(Whitespace\" \
         \"))))(Tile((id bc430d5f-d90c-4c85-94a0-41925329abda)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         08e37ad3-3f75-4d5d-9d2a-a2857e573572)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2c1a77f1-575b-4833-95af-8b0c09139db8)(content(Whitespace\" \
         \"))))(Tile((id \
         087cc006-1dce-4746-80a7-2ebdce4b8c27)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28292e83-272f-40cf-a0af-7771991beb05)(content(Whitespace\" \
         \"))))(Tile((id \
         6bbbb150-8057-4994-a4c8-b10df2a22032)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c9c519d4-5178-457f-bfae-ca572eb6d666)(content(Whitespace\" \
         \")))))((Secondary((id \
         bf9e7a8d-f6aa-4125-aaac-4453a1df67e2)(content(Whitespace\"\\n\"))))(Tile((id \
         d364dd1c-695e-4a25-86ef-e7181380e10f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         20a6627f-06c6-4395-ad69-857e76a677c7)(content(Whitespace\" \
         \"))))(Tile((id \
         ed4219f6-6f79-46b1-8d66-dcba1f0d01f6)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         14463318-508d-469c-9a39-62201c0a27f8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5df5176f-bae0-47aa-b14f-07a55ac57937)(content(Whitespace\"\\n\"))))(Tile((id \
         4077fa57-c44e-4aee-90ff-78b0d140da5e)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c8de77dc-ac6e-494a-ae67-aeedf38fd1fd)(content(Whitespace\" \
         \"))))(Tile((id \
         d38e58f7-febd-4919-8038-ecb7f1cb1119)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         06eddaa8-9a77-42ae-ad2d-0913eab812af)(content(Whitespace\"\\n\"))))(Tile((id \
         f0f13eaf-ee42-4157-addf-9fc4450cabbc)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         34f2199f-430b-42e4-93e5-a07acc1fa957)(content(Whitespace\" \
         \"))))(Tile((id \
         9d895eba-9780-4fd2-9de6-341c2a28daf2)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fde446e1-97eb-419e-a09a-53bee3c9b6e1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4cedc374-4dc5-46c9-b3fc-a80359528df2)(content(Whitespace\" \
         \"))))(Tile((id \
         167adbbe-92e0-4ff7-a915-82313c5cd83c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         adae0595-2ec5-48b4-8657-b39e68bcabca)(content(Whitespace\"\\n\"))))(Tile((id \
         27aa20a0-16ce-4537-acb0-f673172bdebe)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         805a8bb0-ccaf-4077-b512-e278e58b48d9)(content(Whitespace\" \
         \"))))(Tile((id \
         6030667a-8445-444b-83cc-87c7445f20e8)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ab4975d0-1cfd-4213-a168-f38bfa83a86e)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         af43e856-f8b9-4045-9496-41655fce87a3)(label(tl))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd7c2c11-39c5-4e0f-aa1b-65f5702b9df0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33e093f6-d579-4535-a262-0e72686e6fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         b7990377-a75d-48f9-bec3-c68acb817cba)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         639f026a-af1f-492e-b082-f26b4fc12542)(content(Whitespace\" \
         \"))))(Tile((id \
         01cc2b70-b2bf-4b95-bf8c-a3d5a32b6c5a)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82866b7b-6319-480e-84dd-1d1d0d59b2d3)(content(Whitespace\" \
         \"))))(Tile((id \
         bd125710-8785-4e26-9328-1d67db210067)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c62c7559-41a0-43e6-b777-e3d5b0d9867d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87cffcf4-fec4-4b82-964a-795cc179dafc)(label(tl))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         61f498a8-ef2f-4e43-8717-fda47c6a43b0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2d3ebcb6-26f4-4415-bf01-73036552508c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         730b9513-02cf-4739-886e-71884753e0b1)(content(Whitespace\"\\n\"))))(Tile((id \
         e10ca85d-1dc8-4885-b173-adafc7f44fcf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         57a65597-5514-426b-bc0c-b7ad0acc6680)(content(Whitespace\" \
         \"))))(Tile((id \
         fc333e70-886c-4d50-8572-dd61320acc38)(label(has_at_least_two_elements))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3f56d3d1-f459-4350-9841-b0642dc819e7)(content(Whitespace\" \
         \"))))(Tile((id \
         f8b164af-4f28-46a0-a4db-4f871d3ea0d8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b29f93c5-e4bb-4a7f-bb81-b1bca0b38659)(content(Whitespace\" \
         \"))))(Tile((id c9e6f8be-d727-49e0-98ca-43e1ccd22d3f)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         587444ef-80f1-48fd-aaf4-bfe2175ca1fc)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a06ebbb1-30d5-4334-9d13-c3440f4e5e3c)(content(Whitespace\" \
         \"))))(Tile((id \
         9ca0ed34-8995-4f30-8330-773bf2438d1d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         27ecd4a5-432a-4dca-b70e-a9107b9cd4b2)(content(Whitespace\" \
         \"))))(Tile((id \
         9dab212c-764a-4683-b183-ed74aa998c22)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         069f326a-53be-4356-b382-cc22bfcaeaad)(content(Whitespace\" \
         \")))))((Secondary((id \
         bd04b5ae-78ab-4d8d-8a24-a95b49a69fa6)(content(Whitespace\"\\n\"))))(Tile((id \
         bdda2ca8-6c34-4d0c-a902-438fe1aab7b9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         68ef37e0-f87f-4569-bc8c-7779f646efa9)(content(Whitespace\" \
         \"))))(Tile((id \
         29d80b08-fe4d-46ea-891a-881fca7f8896)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         94d60246-6b08-4506-900e-01a0519d572c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ddab56a8-6365-49cb-b75c-8facf513aec2)(content(Whitespace\"\\n\"))))(Tile((id \
         9c729bb7-f5bb-49f3-8430-271daf32c03f)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e8068af2-544d-4605-9283-e908d67c0be7)(content(Whitespace\" \
         \"))))(Tile((id \
         1720976b-665f-4b72-994b-c614d73f2632)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44e4c1b9-6a2d-4d05-9369-e0bacfc730d8)(content(Whitespace\"\\n\"))))(Tile((id \
         9ed66718-204c-4643-929d-a877670ac477)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2f793642-6764-4ce4-9b3b-899f23c64193)(content(Whitespace\" \
         \"))))(Tile((id \
         8045e58c-a6c9-412e-bc04-acb1ab8e82f9)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f796488b-6634-4ccf-bde7-75e282b9f048)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a596b478-1d87-4fc7-8062-b73a14aae6c8)(content(Whitespace\" \
         \"))))(Tile((id \
         85f4b207-0da9-42c7-8436-5f37b6f45ee3)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         56fba53f-6841-4c71-bef6-d53283cee573)(content(Whitespace\"\\n\"))))(Tile((id \
         ae177376-6295-4d66-a2bc-976272861b42)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         99d28e0a-6922-4247-b9dd-a014f10f7e26)(content(Whitespace\" \
         \"))))(Tile((id \
         596ed288-a22f-4028-94c5-65b52998eb07)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3b7c1f11-9246-4f1e-9b11-729eca8d078f)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         f96303a8-22b1-4213-bc43-544482a87771)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cddbcd5e-4f40-4f23-bac0-b5b888509d8b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aabe0c51-99fc-42a1-b619-7feed6901f0e)(content(Whitespace\" \
         \"))))(Tile((id \
         776bd0d0-4dfe-4664-bea4-4e6b2a672fe5)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77b1ac62-198c-40aa-9c20-d7aad8db4316)(content(Whitespace\"\\n\"))))(Tile((id \
         77582b1f-7608-4dc2-a420-b0e05c9a4552)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d6812d53-d63b-40c0-a554-76c0381a2ff3)(content(Whitespace\" \
         \"))))(Tile((id \
         4d626cfd-8843-4c70-90f1-7ddaa659989c)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d9fd04f8-b9b6-4cad-b618-d73ad3289f99)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         6c43b6c5-2017-4ba9-a5ff-e33ededa961b)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e9931be3-4bdf-4916-be35-e0ebcf604929)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         594c5c44-ad1a-4861-a964-66d813a5d5cb)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         41bd984c-070b-4ef1-aba4-5631996612e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ec990ce-acf1-48e3-b592-2dd73e45d4db)(content(Whitespace\" \
         \"))))(Tile((id \
         52c3e8a1-fffc-4e12-afcd-e754113382ce)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         930a7e60-6876-4866-96ba-ea907cf37b80)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         af01b786-8e9a-4aea-ab75-d6000a611fd9)(content(Whitespace\" \
         \"))))(Secondary((id \
         f023661e-c6ef-4820-ada2-95d88d7d2237)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf244542-35aa-4f89-9636-0101f35d6595)(content(Whitespace\"\\n\"))))(Secondary((id \
         42998389-c984-4dd1-b5e7-ba15a55240fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         381c62c3-5356-4946-ad06-ffa58a756eef)(content(Comment\"# Algebraic \
         Data Types #\"))))(Secondary((id \
         c7d642f4-2620-4f9f-bb5f-01b7153153b3)(content(Whitespace\"\\n\"))))(Tile((id \
         9c0ea466-7652-40a8-be3b-470262e88f19)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c19a5383-6e08-4c75-a551-6ba8b3e85d00)(content(Whitespace\" \
         \"))))(Tile((id \
         c4f12e41-1e51-43fa-93b2-cfa3bd5cc0b4)(label(Exp))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5974565c-3566-4090-a98f-52de073d995e)(content(Whitespace\" \
         \")))))((Secondary((id \
         7c624bc1-f80b-4e13-9b39-a2d828fcb7fe)(content(Whitespace\"\\n\"))))(Tile((id \
         dcc862bf-e39d-4401-8774-abf17280113b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4c730e75-64eb-414a-9a27-f31bdb346c52)(content(Whitespace\" \
         \"))))(Tile((id \
         fa8641e5-6194-4ddb-895e-94dfcefb95a7)(label(Var))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e91c0f2a-37f4-4653-9ecb-bd2c8992c38f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d32331df-1809-4eab-a560-e5e13e3d066e)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8e049b32-da37-4c81-8ef5-6cec5bfcbccc)(content(Whitespace\"\\n\"))))(Tile((id \
         df7093e1-277d-4541-ac76-9ebb3e4837a3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e71df6e9-e470-429a-86b2-2400a6611a9d)(content(Whitespace\" \
         \"))))(Tile((id \
         e9ea2287-fa8d-4510-8a26-2ea10b6335b6)(label(Lam))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         68a35f35-43f8-4316-ac91-5ca889eb9e7c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9f6d087a-ec47-4b82-8fa9-c55d48a18972)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         989c6ba5-bf6f-431c-b115-129bff9aadba)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         696e3560-7ce8-48c9-8b91-732876b74c08)(content(Whitespace\" \
         \"))))(Tile((id \
         7a6f0419-8ceb-48e0-b486-41f0ef2d62cd)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         34d85a81-4b30-40cd-8ec3-c9ced01f17ad)(content(Whitespace\"\\n\"))))(Tile((id \
         05d35475-4170-4f51-a7da-4be612621f03)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         61ba93cc-14e7-44d6-aa63-291749230acf)(content(Whitespace\" \
         \"))))(Tile((id \
         f817fb0e-6d32-4bd2-bfd3-1ed07ad2f464)(label(Ap))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b0453180-ddf5-48e8-9f95-61ba99ec8c90)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4e6f2c25-3a1a-458e-9ee3-814a13a2e6cf)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         54ac6f85-d0e2-4463-835d-a71eaf3ec1df)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f1e02b54-8f64-42ba-9cbb-064adc6843d2)(content(Whitespace\" \
         \"))))(Tile((id \
         ffebb5a4-e656-4360-9ed9-d124d84c6ee4)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f5479c83-9b8f-4503-a68d-499b5691c133)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15e4df62-f7cb-4328-9081-d82936cc9da7)(content(Whitespace\"\\n\"))))(Tile((id \
         628f052e-28ce-40f1-9bfa-3300b42b1d78)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5b11397-348f-4a6c-93c5-0fe27dfe074f)(content(Whitespace\" \
         \"))))(Tile((id \
         bd6002f4-0777-4699-b01e-160ef3561b63)(label(exp_equal))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d81b2bb3-57d3-4aa6-8f35-44ae6311a2f0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0ff1237a-1a58-4363-b78d-c539ee49d141)(content(Whitespace\" \
         \"))))(Tile((id \
         5ac7ceed-7666-43d5-9c98-b0f4e8098e08)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         bb7acbcd-0dcf-4475-a6e3-87c50d2a4497)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         222070fe-ec83-4e3f-bda8-492b6c965266)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eb83d051-d362-4d77-a20f-13c3bcce75fe)(content(Whitespace\" \
         \"))))(Tile((id \
         bf2ec373-0d9a-48d6-afef-31536a65d793)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4efc9050-6807-4f7e-92e6-6bf562990073)(content(Whitespace\" \
         \"))))(Tile((id \
         1517b1c6-e0be-4920-82f0-20a0540743da)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bbf0e87c-6e13-4f96-84ae-339b302186a0)(content(Whitespace\" \
         \"))))(Tile((id \
         5e05915a-c58f-485d-8071-0504fd007491)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7f4d5ae2-9e88-4055-8a74-03bb306256a1)(content(Whitespace\" \
         \")))))((Secondary((id \
         5d3de8f1-e5a2-45a9-87cd-a29e4368d47f)(content(Whitespace\"\\n\"))))(Tile((id \
         3845a537-13eb-4e4b-9cff-d0f451b54a6d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6625f44a-a935-4139-a4dc-b5ad86e7a3f7)(content(Whitespace\" \
         \"))))(Tile((id \
         6b056805-0555-4f8f-8323-ca2b7160195b)(label(es))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         372be305-9e9c-484d-b767-3ac448c83f36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0b6d35da-f669-4b47-aea0-d414844d023c)(content(Whitespace\"\\n\"))))(Tile((id \
         a598a82e-5298-4f86-87f5-d9c4c74f8085)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e9f7775b-8b80-474d-b9ba-810e27f62009)(content(Whitespace\" \
         \"))))(Tile((id \
         22fc306b-883a-4a56-afae-eba24bcca65f)(label(es))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b8379a71-c79b-4e5e-8af9-11f0cd455ca3)(content(Whitespace\"\\n\"))))(Tile((id \
         500941d6-75d3-4b07-ae4c-bf704b808d60)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a303d636-9733-47eb-a5ea-9422eaa18a75)(content(Whitespace\" \
         \"))))(Tile((id \
         2f348d9d-953a-4d6b-b458-f198010bd486)(label(Var))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0d798ecd-d1b0-47c2-a8e0-bba85a782702)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5132a8ef-0d06-4674-9236-133f0e577c95)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         c6098889-b33c-43e0-a191-a82a22923659)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9b38c6de-2167-4073-8f55-93f18a6134a5)(content(Whitespace\" \
         \"))))(Tile((id \
         a92d818d-7d7d-4423-8afc-30521e0ae286)(label(Var))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1ee6e7c7-d7df-420e-a45f-2fcbbed103b8)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a18034c9-a9bf-4907-bdea-1becb2857374)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         961db41a-e67d-4972-aff0-7fff7930202a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b41c9c9-7790-47a3-b249-857ee4f8c307)(content(Whitespace\" \
         \"))))(Tile((id \
         638e7c54-61e1-426b-8c1b-1386bbfb8af2)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         034eb069-86e0-4c4f-94c2-638c9b127aa1)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5127dd5-89fa-48ef-ade8-4300341c9abb)(content(Whitespace\" \
         \"))))(Tile((id \
         69ea88a7-79b2-4261-b781-788089104448)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33ffdde0-6a00-45c8-80df-526e4b6908ce)(content(Whitespace\"\\n\"))))(Tile((id \
         def9d4b1-daa7-4c55-948f-0c1f44afda79)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6589e049-89bc-4853-b670-3735c8c9152a)(content(Whitespace\" \
         \"))))(Tile((id \
         613a285f-6366-45a3-ace5-50836e725c09)(label(Lam))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7d149791-352f-4a40-a30c-435e41f7c178)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         faac0713-88f0-48b4-b71a-a232038fea7a)(label(x1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5268e960-b136-4371-a891-da5a9dc7eb72)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4563ee75-dc2c-4686-b742-0f7921444f96)(content(Whitespace\" \
         \"))))(Tile((id \
         e2b42f04-d582-4000-b287-0738f9d4bd42)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         6b1a97ee-10bc-4127-946b-d9755e35d84a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         44717d73-165e-4b69-a902-e0c46f24a424)(content(Whitespace\" \
         \"))))(Tile((id \
         56d1e53f-aa14-4e76-893b-ab6d4362810d)(label(Lam))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f8ffe83-c545-458d-9bbe-08b913b876c2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         ab1c0ec5-33a8-499e-9531-7275995dfa28)(label(x2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e35d7e23-c92f-4aba-b953-d49a21b91f45)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         53a0449e-f15c-4fbf-bdbe-7a68207a0b16)(content(Whitespace\" \
         \"))))(Tile((id \
         e6fdf5a6-933c-4425-8a31-4aac6a149e3a)(label(e2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         38fa5f07-d508-4da4-8e80-8a7a05282b46)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5900d179-d0df-47f1-b7c4-386887b59bce)(content(Whitespace\"\\n\"))))(Tile((id \
         d1f4e073-151e-4d4d-9597-ac629913878e)(label(x1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64c1d34b-526d-44a1-aa69-5a9696085991)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6a49613-8a73-4449-b279-edb74db77802)(content(Whitespace\" \
         \"))))(Tile((id \
         df7adba0-b7e8-4650-9311-8da43d2b4273)(label(x2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7320f794-edb2-4555-92f7-77afd43681c0)(content(Whitespace\" \
         \"))))(Tile((id \
         a9d3b976-8bbd-4603-800a-57f9a47cfd68)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82d44b16-c9f1-4171-ad1f-43a3b2c8c9b9)(content(Whitespace\" \
         \"))))(Tile((id \
         d36b380e-21ce-48ab-9c41-c69255f45edd)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aede4113-685c-4402-a6d0-dd39a0701322)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ab4f776-e44a-4b85-bbe4-635df4dace4e)(label(e1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62eae239-5078-4795-9352-26ef1f590455)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92c8659d-8349-45eb-af42-dd3ac920ad94)(content(Whitespace\" \
         \"))))(Tile((id \
         cf373889-0681-48d4-9432-f226466ecc81)(label(e2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c0a40da6-1c32-420d-af74-6d1a27aaead8)(content(Whitespace\"\\n\"))))(Tile((id \
         49b4fc21-ba99-4feb-aed1-e2bf3ae2abf2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5cc76c48-3b26-45f0-abb9-fcbcdd33ddce)(content(Whitespace\" \
         \"))))(Tile((id \
         92074097-c54a-4b3e-bad7-d549dac14389)(label(Ap))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9ad90171-8c49-4176-bc71-5b2cebd65edc)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b5aeda45-b931-493d-b081-ae6c1f3b999f)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a21ae254-4b7b-4371-b407-3db804d98ff4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5372eb7d-a073-47e4-b3aa-08aadeeb6ccd)(content(Whitespace\" \
         \"))))(Tile((id \
         cfdebcfc-572a-46ae-95c1-559bc7b63d83)(label(e2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         fd0a83b3-1082-4232-ab64-5132b3780c20)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b39142ba-912c-4180-beb0-7da111b6fe6b)(content(Whitespace\" \
         \"))))(Tile((id \
         febe7496-9ef4-4a14-aa71-77eb2bf151fb)(label(Ap))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7c33020d-c07e-4256-9e78-bfeff1fbced4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         94c5095c-d021-4bfc-9a0c-0d2c2b9368d8)(label(e3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6d4cb5ee-1e0f-4838-bdc7-b71160b40772)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         818510f1-c9c3-4941-bd95-802d72edc936)(content(Whitespace\" \
         \"))))(Tile((id \
         205e805d-7c4c-43c4-88fa-105b66eb1c09)(label(e4))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b25c04d9-ae20-40e9-9ff1-1f7e72fbc919)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c04badcf-c7ae-41b9-8918-cd8cf278f6bc)(content(Whitespace\"\\n\"))))(Tile((id \
         6e180263-cf1f-4fc2-97f1-2470129bcb98)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9183706a-658c-4241-8a3d-4505ecd6e90c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         62dc09cb-38a8-459c-8370-24191360de60)(label(e1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73c0edab-68a5-4e3f-850d-3c1b985679f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         312e5837-b0ef-4ea1-b8d8-cd1a617cb457)(content(Whitespace\" \
         \"))))(Tile((id \
         02a03771-5cf3-4129-a465-1b193bdadace)(label(e3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45f87b8f-94e4-4bbb-88ff-3ea12ed4a3cc)(content(Whitespace\" \
         \"))))(Tile((id \
         04c35121-9f74-453a-a12c-ef28af0a433c)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2dbc615-59e2-4745-ba78-2ba63e270f95)(content(Whitespace\" \
         \"))))(Tile((id \
         9dfd9152-afab-4ff6-8b7f-8b65f5325192)(label(exp_equal))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44fee64c-2e0b-4097-8220-a40ab0b022f6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42bb9a2f-449b-4f5e-8dd9-ec9396fb9f55)(label(e2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3daa0651-beb8-4eaa-8ea1-e0cb556ea10d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9607d69-8ec7-44b8-8df4-b3a5c75ef91d)(content(Whitespace\" \
         \"))))(Tile((id \
         89883d6d-c9a1-438a-83ff-8a1caf247ba2)(label(e4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45db3acb-6b58-424a-b215-6b1510ec67e4)(content(Whitespace\"\\n\"))))(Tile((id \
         1a456ef9-396f-452e-8f88-54f74ee941dd)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         49c67bb0-02d3-4b73-9785-65063c9f04a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b0ee7f5c-8730-46e6-a489-7d0f1b9a0649)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         653c3063-1f1d-42a4-a772-dfc300e0b55b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0cbbcb14-06d9-4272-8f84-e0b47f63619c)(content(Whitespace\" \
         \"))))(Tile((id \
         ff0bf69a-06f5-47f3-9425-b0076334a7d6)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1536587c-1195-44ad-8df5-9057ea6c4626)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8fcec3a5-c47c-40f2-8445-a2f3c9d30ca2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         28eed6b9-a99b-4786-ba5b-799b00722926)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b7918ff-d6e9-46d4-ba8e-691618cc8cbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         af983880-e37c-49e7-aa8c-99f359e72277)(content(Comment\"# Polymorphic \
         Functions #\"))))(Secondary((id \
         dd104e64-ff11-42ef-a09c-3d15e07b502a)(content(Whitespace\"\\n\"))))(Tile((id \
         fdf371e2-6f24-406f-8cda-d2afd5913bee)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b39bce2-17a6-4007-bed1-80ce340550dc)(content(Whitespace\" \
         \"))))(Tile((id \
         b4edea59-3f41-4785-9df9-f9577e0cb3c8)(label(poly_id))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         949504e9-f926-4b4f-8a91-0d9dec928161)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         47cb3801-e19f-4c89-854f-a4cf1b3cf864)(content(Whitespace\" \
         \"))))(Tile((id 68268039-08ce-4f3d-b9cb-bac0374f34ea)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         c858db10-4100-42c2-93dd-639d9a789537)(content(Whitespace\" \
         \"))))(Tile((id \
         d51dc924-4880-4e0a-930e-04ccdeeaa7a2)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6eb01bee-5821-4249-a100-90e76360184f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dc65e5d4-6550-4fca-84a7-1a0e9715b00a)(content(Whitespace\" \
         \"))))(Tile((id \
         2a575a58-f611-4182-a31f-708c725a4c9c)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d24c06e0-4789-4fb9-9590-a91350d5dd9d)(content(Whitespace\" \
         \"))))(Tile((id \
         e23e98be-d4bd-4399-a158-7fc4c119c1f4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cdf53e78-bc3c-4152-ad97-8e6b99e5e51c)(content(Whitespace\" \
         \"))))(Tile((id \
         fc51b9e3-6dea-4eac-9d33-849e6935ac22)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2e6ede15-034d-4da1-9bd9-9b15498d5460)(content(Whitespace\" \
         \")))))((Secondary((id \
         35b3b59f-92c5-4612-906e-9e1ca5dcbaf3)(content(Whitespace\"\\n\"))))(Tile((id \
         5e085250-f6d5-4c23-ac11-c8c7bd5ebe2f)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         05aa9bae-cbbf-497e-be42-5e1ae823d119)(content(Whitespace\" \
         \"))))(Tile((id \
         ac99c6dc-b664-43a7-8498-c218894317f6)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d44b27b6-3c1a-4a68-a67d-54e41b7d5002)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b30956a4-5a92-4e47-891d-fb0e695799bd)(content(Whitespace\" \
         \"))))(Tile((id 23480239-334f-4836-bfb6-99fbb39a3c3a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bb9542ed-0503-4fcc-9c99-194af981933c)(content(Whitespace\" \
         \"))))(Tile((id \
         ee856e65-d73a-49ae-b357-460bc05bfc18)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8e8e8616-f9a2-472a-9422-b665dd12d0ca)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         318352f2-598a-42b3-9584-4a83cc362fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         581cfa8e-6946-4e0b-80a9-0c9497aaf78e)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2c3b66a5-9fda-42b9-8b82-7664a8b14ddc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         82d8a9de-6c23-48d3-b858-20ae8164777b)(content(Whitespace\" \
         \"))))(Tile((id \
         054c66e3-81b0-46d5-a919-c9a1d440ecce)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95a8971e-abf9-4f55-b127-959c867df09b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6e246144-d3d8-4b02-a7a1-95445b39a29a)(content(Whitespace\"\\n\"))))(Tile((id \
         3dd9ad1a-b2f7-4154-ab9c-9c0e6f83466d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         09da3026-a7c5-4bce-b948-8387cb715440)(content(Whitespace\"\\n\"))))(Tile((id \
         57059a0b-6dda-41af-90a0-7d1ffe9e8e74)(label(apply_both))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ae28fd42-55f7-4c47-bca1-daf2a2fcf366)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ef3e0805-f5d7-4fa2-b677-3112aba069cf)(content(Whitespace\"\\n\"))))(Tile((id \
         9738df84-9384-471e-aaab-67cc2589b1f7)(label(forall ->))(mold((out \
         Typ)(in_(TPat))(nibs(((shape Convex)(sort Typ))((shape(Concave \
         36))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         7c5895f1-f045-4e31-bcba-14ddd6ced1cf)(content(Whitespace\" \
         \"))))(Tile((id \
         b8c7652c-0269-4e8f-a02e-45a3671d315c)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d7e252f1-9e92-49f2-b9a8-e8c17abd512a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         922f0816-7e82-422f-9b58-3bb32a31cabc)(content(Whitespace\" \
         \"))))(Tile((id a440fa2a-77f9-432c-ac23-031e4e28d488)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         83e84f36-3b45-4051-b957-e27dee61ef50)(content(Whitespace\" \
         \"))))(Tile((id \
         71de32a9-d84f-4fd9-94b1-32d67478de0b)(label(b))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4df5d774-7293-4934-b7ad-170b6892d397)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ab385d4a-0a89-4971-a6e0-f6531901035f)(content(Whitespace\" \
         \"))))(Tile((id \
         2c80f85a-1037-44a6-81f2-4571c25d0679)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         702464cd-0532-4b34-b060-54225a323567)(label(forall ->))(mold((out \
         Typ)(in_(TPat))(nibs(((shape Convex)(sort Typ))((shape(Concave \
         36))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         3526eb4b-c483-4682-b3a1-d274b97a4314)(content(Whitespace\" \
         \"))))(Tile((id \
         44f0f3a0-1505-4c46-b91d-637cbdcb9492)(label(c))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5a14c81d-8b2f-4da1-8bcc-2bf01cd3d67a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         48a7746f-ec32-449f-8ddf-ece1124183ec)(content(Whitespace\" \
         \"))))(Tile((id \
         e5e85c32-8fdb-4960-b82a-f234dc90263e)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7ad8063b-0189-4735-aacd-48c3d3727dc4)(content(Whitespace\" \
         \"))))(Tile((id \
         65d082dd-7e85-4193-8e26-8bdf4fcaecf2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         70939466-0ecf-4389-b938-144703a84072)(content(Whitespace\" \
         \"))))(Tile((id \
         6b42fc80-6c9d-47c6-8e11-c65428c1f6f2)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         be99a0d9-ee26-4d6e-8fca-0218b8c3979b)(content(Whitespace\" \
         \"))))(Tile((id \
         36b3906f-3d4f-45a0-9493-4898f4fc6146)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c05326a-dd97-436a-bbcf-91e448c8b970)(content(Whitespace\" \
         \"))))(Tile((id \
         624d2a95-99bf-48b1-b286-f53f4f258526)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         d60d6037-3aca-42ca-ab06-5252247aa40e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f82cfbdd-27fd-4ea0-9628-8e8eed1e6b5b)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1c3c95e2-8ab0-422d-b7a4-97b482dd4dee)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         548c0040-7e0b-4f29-9f0f-c1110b545349)(content(Whitespace\" \
         \"))))(Tile((id \
         17e8d85b-fba6-4ac2-aaaa-7756584e4643)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         310d0592-4cac-48d7-9a79-1f657f6dea5d)(content(Whitespace\" \
         \"))))(Tile((id \
         d85ecc72-bc37-4567-a1ef-cc90d5eae0a9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         01da4a43-8227-46ab-8841-c90f3cd644de)(content(Whitespace\" \
         \"))))(Tile((id \
         d7a1727b-55ea-41b5-9dd1-28e5376506cc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         80fe1195-596a-4dd9-ad9d-7126137a605f)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         94229301-c6a1-4b8c-a6c7-fcce295702f5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         685c5d04-d44c-4b3a-8385-3e2dbe0a7145)(content(Whitespace\" \
         \"))))(Tile((id \
         a9dc7315-045a-43aa-95bf-c16f829f12d8)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         bb0e98f7-58b2-4beb-a7a6-79bca69c61b8)(content(Whitespace\"\\n\")))))((Secondary((id \
         88756e36-d67c-4414-940b-c13fb0f36bde)(content(Whitespace\"\\n\"))))(Tile((id \
         0cac01a2-198d-4f08-8637-133b6cc985f3)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         82fc4b9e-0725-4dba-a161-9e69ddae7cc2)(content(Whitespace\" \
         \"))))(Tile((id \
         66b244de-6647-4333-8c70-a5337a255271)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         38e0bd97-a06e-4aa0-a05b-3d22bd06e8ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         25946f24-0fd4-400e-aee6-05add162692a)(content(Whitespace\" \
         \"))))(Tile((id 5f197e61-f255-4c07-8a6a-b8af92248d78)(label(typfun \
         ->))(mold((out Exp)(in_(TPat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         34ebb729-a6e8-48d9-a23a-e566e1b212b0)(content(Whitespace\" \
         \"))))(Tile((id \
         cf4671b6-5f05-4cdf-948d-d73967a1262b)(label(b))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d79b0c29-5665-4110-be2b-6a575b41be0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f93a87f-4bea-4989-8776-e03bc8449ce2)(content(Whitespace\"\\n\"))))(Tile((id \
         5eea5261-8144-449b-a61b-4f2e955e5788)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         020f210b-0323-4814-8401-ee84b2fddceb)(content(Whitespace\" \
         \"))))(Tile((id \
         d766a435-dcf8-4114-adbb-dc81a783dad7)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         21288f29-03ac-47b7-bf14-616544ceab79)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         337394ca-e03f-44d0-944f-ffd8ab508c51)(content(Whitespace\" \
         \"))))(Tile((id 963d6cf1-95b5-4781-bfb9-78a60bced7f0)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         9cd7103b-6bf8-4887-985f-b52a518c628e)(content(Whitespace\" \
         \"))))(Tile((id \
         f702542e-44a7-4eaf-92ab-ad2eaf792c10)(label(c))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         22586202-7ada-4f49-b824-09366d8dd2aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d1a3a3a4-776d-4db5-8bab-e11c9da60fed)(content(Whitespace\" \
         \"))))(Tile((id \
         ff03b0cf-fc5e-4b06-a828-d69458b91b13)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b4cf0bcc-094f-456e-b86f-6656e02f73bd)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         90fcfdfe-7935-439e-9638-c9f3d5175304)(content(Whitespace\" \
         \"))))(Tile((id \
         335aa3bd-ea0c-4ad7-81a0-8405fa6c9b36)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6e23bd8f-8de5-4b78-a7f8-bc067163fa9b)(content(Whitespace\" \
         \"))))(Tile((id \
         f5409d45-3b0c-4b7a-a339-9680df2ca695)(label(c))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         372dd134-4151-405f-813c-e5744d127ed3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5fc3e24e-0753-420b-9f02-599c36b799df)(content(Whitespace\"\\n\"))))(Tile((id \
         2679d5a0-7ca8-4899-847f-8d8ece3db0e6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         81a0cbd4-3f2d-44ab-b48c-4f6bba7dd53c)(content(Whitespace\" \
         \"))))(Tile((id \
         e0daa00a-183d-4cf4-94fd-f1ad0e7c4506)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         66201a3c-a4ed-4898-b65a-0ad5712dfd78)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8bc5f4bf-a84a-4ad1-973c-7d5b6653b62e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eb447b9b-a404-4cc8-beff-6ece6b91b38e)(content(Whitespace\" \
         \"))))(Tile((id \
         e45dfe08-c992-4f38-9850-508707addbbf)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         ac5ad3d9-b131-4d4b-9fd5-374512580d30)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7db15742-32d8-4d29-a086-7027cc73e247)(content(Whitespace\" \
         \"))))(Tile((id \
         c7eb07da-c87b-4785-84bf-0a4a195c74d4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         6f0f9a7b-5224-4a8a-9d23-214eddba3137)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0ee65d77-ab86-428c-8487-8e196920954e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c4d3c627-98b8-435c-8f1f-8dcc7636faea)(content(Whitespace\" \
         \"))))(Tile((id \
         eaf42d20-fcaf-4fa6-82fd-ff9909f87521)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         77df72d5-89ee-4c2f-9a19-a93b5f101b88)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         52669557-ec15-46eb-aca3-99c4c4d78382)(content(Whitespace\" \
         \"))))(Tile((id \
         0541913b-22ab-4564-9109-bfa318d88039)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         38aabcfd-fe1f-4680-91e1-ef1afc58fd85)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         988f0d5a-8dd0-489f-9c15-900de65b9e61)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4641871d-1679-4006-b792-741796a9f182)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         1d84a727-7a5d-4849-9675-cdeb3a4ea976)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5667508-0bfd-4f3f-99db-d88e4642f7d8)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         901b66a5-1529-466b-a420-7c496472a0d8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98b3c303-2785-4356-ab44-b8c5ab1dfdd9)(content(Whitespace\" \
         \"))))(Tile((id \
         4068bd3c-2397-4dab-a400-123012e0f5c3)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         427a54c8-92bd-4d69-b2b0-231af0c91b26)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         06ec079b-0bf7-481e-84f8-6b93a9dbe943)(label(b))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         7d9157e5-5155-47fd-ade8-3698c10351d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         398c69ab-b933-4a61-b835-ff8eff46df81)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8506eeec-efc3-46ed-b821-3eae46fbef10)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e41e47a4-a70d-4bbe-863f-f1fe932fe2d3)(content(Whitespace\"\\n\"))))(Tile((id \
         939f903c-f3a7-491f-9b05-0fbbe23137c0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1c68df88-d119-47a9-bb33-eba911a063ec)(content(Whitespace\" \
         \"))))(Tile((id \
         ac53f99e-9892-434e-aa41-94db84897205)(label(list_length))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         35334b80-8bd8-4ce6-93f8-d1998f7ced27)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9a651dc0-3df4-482f-9923-d34c5409ff15)(content(Whitespace\" \
         \"))))(Tile((id 9ecd5154-536f-405a-9970-060b016faad3)(label(forall \
         ->))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 36))(sort Typ))))))(shards(0 \
         1))(children(((Secondary((id \
         22cb77bd-c0c3-4283-a668-6f9b5f749923)(content(Whitespace\" \
         \"))))(Tile((id \
         0839bd1d-e4ae-4e19-9a99-8c91b1bc51a9)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         53e3582f-b94e-48db-9021-4c84080ecb88)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f73d50cc-8a56-4be4-8f2e-5e433d597767)(content(Whitespace\" \
         \"))))(Tile((id 3eb98f32-8dfe-4422-963d-86dac9ec5555)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b2ac6d98-9f89-4974-b870-e0666c334ac6)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a70899a4-61eb-4f63-95f4-58141b8ec8f3)(content(Whitespace\" \
         \"))))(Tile((id \
         b419099a-9c2e-45ca-9931-1e03b951b4bf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e988bbfd-5565-4fd3-b9b7-328758c66793)(content(Whitespace\" \
         \"))))(Tile((id \
         0380f537-3042-4cfe-b947-d6414c24b99a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ce9fa988-138b-4a7a-b19b-09079624559c)(content(Whitespace\" \
         \")))))((Secondary((id \
         ab52705d-33f5-441f-b58f-1536b705e48b)(content(Whitespace\"\\n\"))))(Tile((id \
         45e9196d-806e-40b6-8db7-6c163d9c2ca8)(label(typfun ->))(mold((out \
         Exp)(in_(TPat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6f6eefcf-a9eb-48dd-9d08-f05369446afa)(content(Whitespace\" \
         \"))))(Tile((id \
         bcda57eb-0b28-4ae0-a2a9-22a0193b75a5)(label(a))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         53ccba5a-5ba0-4de4-aa73-b6906327d3df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3f2610ad-1bd5-4512-b16b-f363b37a4e2f)(content(Whitespace\" \
         \"))))(Tile((id b62c26d9-896b-423d-be8e-53ea1f3646e7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         405a3025-e1a3-4ec8-bf85-2e1b363e4356)(content(Whitespace\" \
         \"))))(Tile((id \
         a1a78e53-6430-4b3b-a92f-04228f5a11a0)(label(l))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c7a680c5-fd8e-44eb-995e-1d05af327c14)(content(Whitespace\" \
         \"))))(Tile((id \
         868b2244-17fa-4cc0-aa0a-d94485609ead)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28ea670b-4614-4ad7-9cf2-8d27fecacff6)(content(Whitespace\" \
         \"))))(Tile((id 8d682b6e-c1e3-4f9c-81c1-d89877077087)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         69a0b2c8-f8af-4dd8-ba8a-7d84d8bf2a05)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         44123f7e-6d8c-42c5-a7da-66ecbe7af220)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bf0809f-6fd3-43ec-9fa0-00fda529dc91)(content(Whitespace\"\\n\"))))(Tile((id \
         fbfa364e-1898-475a-bb30-0e022aac491d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         289648a8-d082-4684-bea7-a552ca0d86e5)(content(Whitespace\" \
         \"))))(Tile((id \
         69287e32-828d-4133-ac91-fd2dcdadf6ee)(label(l))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d94b3dc-b2b6-469a-8e58-cd97a7e89ff5)(content(Whitespace\"\\n\"))))(Tile((id \
         064d0806-a287-47ed-8721-ebd77cb6f1a0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8b289320-abfe-4d17-b258-e9f2bc6eb62d)(content(Whitespace\" \
         \"))))(Tile((id \
         1fe78578-36a7-496b-855f-1ef94d53301a)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b8c9741c-8967-4110-869f-fd4eb704321d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3af7dcf8-e7c1-4f38-a99c-243757faf867)(content(Whitespace\" \
         \"))))(Tile((id \
         324a62e7-bf62-4055-97c8-aac6dcaa128c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e72607e0-facc-4ce2-8335-a546b6d7d3a9)(content(Whitespace\"\\n\"))))(Tile((id \
         f9f9c624-ef8a-46c9-95f9-93f57cf6297b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f21cc7be-1d10-4a6c-96dc-a1a1e079cd77)(content(Whitespace\" \
         \"))))(Tile((id \
         3b4ed9f1-8fa8-42ea-a81f-2c4100125c6f)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         831bb1af-2369-4692-9723-d4406cfa7eb1)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         b04c7d92-0e1d-48b4-aade-8e7191d35219)(label(tl))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4ea48742-483f-41d4-9345-f5452efacba5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2f521a31-071a-47ce-9c87-b10315383baf)(content(Whitespace\" \
         \"))))(Tile((id \
         68cca69b-f721-4a52-8a2a-ce6337f5d942)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         754c4000-0195-45b5-b581-142f786af07a)(content(Whitespace\" \
         \"))))(Tile((id \
         5a40e18e-7673-4fd6-bc1b-7344164d9f51)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e12ce5b1-a5aa-434b-80ad-8661411b914b)(content(Whitespace\" \
         \"))))(Tile((id \
         5d6fef39-1821-4ce9-8c66-25cd63b3f0e5)(label(list_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb5030f7-d1f6-4240-b12a-0a57d6336e7d)(label(@< >))(mold((out \
         Exp)(in_(Typ))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7503ecaf-875f-4476-afb4-6fb0e0d9192f)(label(a))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         90a8d5ff-63d7-44e3-b992-de8a6c21cbb7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5b0d085-ceb5-4683-b973-9826873422d4)(label(tl))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         56558fd5-ff8a-40fe-9990-abc0f1fc6ee4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9813e619-996e-42e1-9969-52bb23ad48a6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         29ec156d-fef0-4cd6-97e1-092c16142822)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddc6ff57-12c6-483d-8215-3535e71eb1f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         26fb1f0e-a4ae-4b4d-8f2f-efeeb46bf8d8)(content(Comment\"# Tests, \
         separated by semicolons #\"))))(Secondary((id \
         e28ea3a1-d185-4657-af65-115d7087974b)(content(Whitespace\"\\n\"))))(Tile((id \
         3721aa20-19ab-4f2e-9335-97cab709e14e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9d138753-8627-4779-8bd6-512c0030708d)(content(Whitespace\" \
         \"))))(Tile((id \
         c04d0c71-e3eb-4a79-95dc-9477054db75c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c0d3801-e5ab-42f7-b8fa-cf53e39aef89)(content(Whitespace\" \
         \"))))(Tile((id \
         01eccce0-da56-4673-bcb5-6f4c3e4174de)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d46d4f5-faf4-404f-8718-f4057760c2b7)(content(Whitespace\" \
         \"))))(Tile((id \
         8bb2f2c7-abce-4113-ba3a-d728654d5f95)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2cb928fc-015d-4716-9662-a6e757181c28)(content(Whitespace\" \
         \"))))(Tile((id \
         1b3a78d0-b569-493b-bb62-7da09f529fed)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88e8d74d-b6ab-4a34-9b0d-a005bc3b8b7a)(content(Whitespace\" \
         \"))))(Tile((id \
         cf9211bb-ab3c-41e9-b016-266a3c9886d8)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         296d2df6-9227-4ca1-b3a2-dee8fede90df)(content(Whitespace\" \
         \")))))))))(Tile((id \
         f446a8c7-5807-4e93-a473-698de18d8f0e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b5c8921-c4ef-4e55-bfe1-82b01eac95a9)(content(Whitespace\"\\n\"))))(Tile((id \
         4e37f3bb-9e76-4427-b225-b70fa26ab9fa)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6e161f9c-b395-4f66-aa6a-33843b9b2729)(content(Whitespace\" \
         \"))))(Tile((id \
         a8edb27a-fe47-4502-bd73-8df541e82698)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         09a72602-667a-4acf-8d2e-0c639091ac63)(content(Whitespace\" \
         \"))))(Tile((id \
         70aee090-d145-46e5-86d8-43ccfb6be395)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f011c3af-a77f-44fa-b473-2326a8574d5a)(content(Whitespace\" \
         \"))))(Tile((id \
         9b8a7a98-2da0-4e04-b77e-1f371fcdb707)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1aecf7b1-9850-4d82-8ab2-663d1c88d167)(content(Whitespace\" \
         \"))))(Tile((id \
         9868464e-06b2-42d4-8648-ed953ed670c7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac606106-d025-41b4-9b49-b48a7d41abe9)(content(Whitespace\" \
         \"))))(Tile((id \
         368b5b5d-3c97-47fb-8983-2ce082a283c4)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         59e820a8-d876-4575-8c10-90d97535eb31)(content(Whitespace\" \
         \")))))))))(Tile((id \
         846dc2c1-25de-48ac-9a59-2b51e890a012)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c857a76e-ae8a-4d6c-90ef-120e0f432dc4)(content(Whitespace\"\\n\"))))(Tile((id \
         77d51e7f-409f-4517-9377-2b697c102f4b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4b90f094-613d-47f9-8c39-c0726662008e)(content(Whitespace\" \
         \"))))(Tile((id \
         88b9117f-046d-45c9-a44c-4c28b90414d7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e11fa738-3c80-4654-9b6b-ffe71f9b4b88)(content(Whitespace\" \
         \"))))(Tile((id \
         3d667db4-b807-461f-bf90-0ad76abb11fd)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61184572-1275-4688-ae10-a4591358d7dc)(content(Whitespace\" \
         \"))))(Tile((id \
         282ffc76-89f8-4383-99f7-5e2bc478bca5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         090245b4-0694-405f-bb44-c844f029917b)(content(Whitespace\" \
         \"))))(Tile((id \
         7de76f26-746d-493e-915a-bffa1e02b730)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c8ef3db-b6c4-438e-b701-b9d0d36e8898)(content(Whitespace\" \
         \"))))(Tile((id \
         19956f06-ce18-4369-99ff-38cc5cc08509)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aeda74d9-2cbe-4f3a-bb03-6767bfacacfa)(content(Whitespace\" \
         \")))))))))(Tile((id \
         b4be4699-9128-4823-805d-6012b6b16696)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74538664-7532-4932-81e3-dcc3514cfdc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         44f60380-8bae-4c4a-8e0a-4a8bc2f71a61)(content(Whitespace\"\\n\"))))(Secondary((id \
         18faa495-5e0a-4646-b3e4-6ccd5d8832d2)(content(Comment\"# The value of \
         the program is shown at the bottom #\"))))(Secondary((id \
         a5055a65-399f-40c2-864f-e49d03e495e1)(content(Whitespace\" \
         \"))))(Secondary((id \
         4bf9d97c-8f26-4be0-8c6c-187a7b7a0458)(content(Whitespace\" \
         \"))))(Secondary((id \
         c58fca1c-3d54-4542-9649-91b93756dd3b)(content(Whitespace\"\\n\"))))(Tile((id \
         68a4a8af-7a92-49e4-bac0-29ef1d35a647)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9bac017-6888-4607-bfc6-2fcf9c35ff0c)(content(Whitespace\" \
         \"))))(Tile((id \
         8ecbc9e6-eac9-4bd6-b502-18c023728aa9)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d01740ea-4908-4f07-8804-e019af765925)(content(Whitespace\" \
         \"))))(Tile((id \
         8176b986-f481-449e-a408-b375182f44a6)(label(2))(mold((out \
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
      refractors = "()";
    } )
