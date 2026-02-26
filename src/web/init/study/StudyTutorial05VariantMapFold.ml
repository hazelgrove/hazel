let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-variant-map-fold",
    {
      segment =
        "((Secondary((id \
         ebc4307d-fdfc-4a7c-b574-f3fd6078b2ab)(content(Comment\"# PART 5 \
         VARIANT: STEP INTO WITH MAP + FOLD #\"))))(Secondary((id \
         b03ed008-89de-478e-a872-2f2fc5d98f9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb406e2c-5371-4eee-b679-1a876bc56266)(content(Whitespace\"\\n\"))))(Secondary((id \
         86c72acc-144d-4756-8d29-f3f49f63a0ad)(content(Comment\"# This \
         function has a two-stage pipeline: map transforms \
         #\"))))(Secondary((id \
         9373b12c-cede-4f60-aaff-35b23c81dc79)(content(Whitespace\"\\n\"))))(Secondary((id \
         cdf14bc9-0204-4196-bd27-9622ec2acedb)(content(Comment\"# the data, \
         then fold aggregates it. From outside you see #\"))))(Secondary((id \
         d08f832e-982b-4d12-8417-f847579bbea2)(content(Whitespace\"\\n\"))))(Secondary((id \
         97438b26-cbee-46d7-af05-688a8d206c03)(content(Comment\"# one number. \
         Step Into reveals the whole pipeline. #\"))))(Secondary((id \
         2ac9f825-7196-4f31-a123-e17f166a2ab5)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f9d8b98-d334-482e-86c7-be2395dad8da)(content(Whitespace\"\\n\"))))(Secondary((id \
         034921ee-f47a-4a83-b4d2-bb6facbb277b)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         e913c56d-1e5d-4683-8b77-1ac99186f883)(content(Whitespace\"\\n\"))))(Secondary((id \
         695008a4-7a77-4067-95ac-dd0ec6a22ece)(content(Whitespace\"\\n\"))))(Tile((id \
         43eb1f59-20a8-4715-9c18-a705fb617e8a)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81901257-0be9-4b94-8bf7-b2b82a8e1ee4)(content(Whitespace\" \
         \"))))(Tile((id \
         0065eb13-4bd3-4ba6-b803-6f1845d2c2a8)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         42456ecb-42dc-44ab-b2c8-ca312cf6d000)(content(Whitespace\" \
         \")))))((Secondary((id \
         a2b3843a-74f1-41a4-92e3-9f4c8c4c0c93)(content(Whitespace\" \
         \"))))(Tile((id \
         7dff0866-1366-4609-aae3-0ab1005ac74d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         5c50997d-1f2c-4627-9475-97b951a8f217)(content(Whitespace\"\\n\"))))(Tile((id \
         838c8758-9a94-4231-9086-9ed770ba8868)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5aecd4f3-b202-49a7-887e-a1ed9bf6cc48)(content(Whitespace\" \
         \"))))(Tile((id \
         f3343178-2819-4a03-ac9e-7e059fdc89a4)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         afd1ba77-2b35-49cb-9599-c7859dcead8a)(content(Whitespace\" \
         \"))))(Tile((id \
         9a24c75d-1943-4177-b2c9-ee95854610b5)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4a922aea-1f60-44d9-82e5-65497c484a5e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5af337c2-d9f1-40af-b0a3-e3aa38fcd461)(content(Whitespace\"\\n\"))))(Tile((id \
         f01ad0be-f182-46c1-97cb-616348d2948f)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         58a9e24a-1bdc-449d-a49e-4818cd6485c5)(content(Whitespace\" \
         \"))))(Tile((id \
         1bc2a4de-1823-4c57-a25f-5390d6fa4ce6)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c5a9d48-77a5-42e4-a855-510437164e94)(content(Whitespace\" \
         \"))))(Tile((id \
         dadd737e-3425-478d-9589-cd929ee8ba4b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3137aa78-d900-4241-ae25-3616fc3b696d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         51d74f21-0ff0-4e3a-a69d-541acb34f1ce)(content(Whitespace\"\\n\"))))(Tile((id \
         74a8c049-1ac6-4195-8f63-c1c70a808f25)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0efedf76-c601-48ac-9bef-74f4cabf2673)(content(Whitespace\" \
         \"))))(Tile((id \
         5a396206-5f8f-4173-bd25-182eaecc94ac)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a5554ab-c4f9-4437-801c-5f93cdf9fe6d)(content(Whitespace\" \
         \"))))(Tile((id \
         baf353ab-3acb-424e-8fdf-52ef0cb4da50)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ae32655c-a511-4600-be2b-dfa5e8f56c9d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9b55cc34-f8c2-44f1-9c33-c96c1ee16f33)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d2f09918-2370-497f-916b-ad75ab0e82e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d0b50ae-7fd9-4116-96f6-7358990a7cf8)(content(Whitespace\"\\n\"))))(Tile((id \
         70a9fec2-fa41-4877-a3b6-993c821fa418)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ba1648cc-efa7-4ee8-a561-c6f6241e7d6b)(content(Whitespace\" \
         \"))))(Tile((id \
         b3ba4482-a8f1-4216-9134-43e907b1d394)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9f4da08a-3fd8-4e23-abf2-0d3e1a8e3e4b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         98e6bbb7-5d95-46c3-87ec-ba76405cad28)(content(Whitespace\" \
         \"))))(Tile((id \
         20c5ba1e-2982-447c-beea-0105f41c1cae)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8aafb4ff-cf3e-4fcc-bffc-498f2751cd77)(content(Whitespace\" \
         \")))))((Secondary((id \
         0300098c-d14f-42d7-903a-0e3dcc257942)(content(Whitespace\" \
         \"))))(Tile((id \
         4ec3fbfe-2095-416f-9198-e8cda4126be7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         81afe87a-3c68-4add-a4bf-835018d94af1)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         689d8a17-5438-4492-a026-d8146e35e400)(content(Whitespace\" \
         \"))))(Tile((id \
         abc2b04f-0e78-4db8-bbd2-0f0eb1b81a39)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4eaca22-0b1f-47a4-8570-0d2de473ffd4)(content(Whitespace\" \
         \"))))(Tile((id \
         0fdf90cc-e394-4767-b3b5-eeb77960e80f)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b964cf4-bd4f-45c2-9b1a-1cb7c0de60b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a1d3e92-5439-4080-9296-a8bd5f01f374)(content(Whitespace\" \
         \"))))(Tile((id \
         ea5c4c24-99ba-42d3-a73d-7f63f456b304)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7eddbbd2-896b-484d-9a97-3a836228e98b)(content(Whitespace\" \
         \"))))(Tile((id \
         255a1f1d-df25-4cdd-af84-015527ad9473)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2e3ff1d-6e4e-4864-b685-b038b26b5bdf)(content(Whitespace\" \
         \"))))(Tile((id \
         ee6b0220-2a7c-4c76-84e3-9854c0a1e2b8)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bae38ba-30ca-4cd3-96fc-2042a3d043d5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37987f91-7bba-41b7-a3fe-b88f059efd51)(content(Whitespace\" \
         \"))))(Tile((id \
         df94996b-8110-483a-b38c-63749cc24d98)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6a40133f-4941-46c6-894d-2195e6a2af61)(content(Whitespace\" \
         \"))))(Tile((id \
         87688cb8-f8a3-4359-8f2f-a68f5017e839)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59e7c6af-f745-4966-9736-d9a3ec164bf6)(content(Whitespace\" \
         \"))))(Tile((id \
         c2933d4d-a5f6-49eb-b7a8-f7acbedf26c7)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6025397f-c9b2-4b43-9c45-dc73d0e34fc9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         52d25a00-a747-44f0-b30a-301098975e56)(content(Whitespace\"\\n\"))))(Tile((id \
         1c3c00d5-790b-4dde-bb8d-3ecac58dc886)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cf72250f-7312-4e93-a840-2d432332852d)(content(Whitespace\" \
         \"))))(Tile((id \
         f321f617-3b90-4961-aa4e-ea9862b96441)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f4214413-985d-4069-9561-8a9f306de196)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea6a135e-3376-4a90-985a-4e54093b655f)(content(Whitespace\" \
         \"))))(Tile((id \
         a12fe70a-e32b-4601-a526-3a59be0776dd)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3198777a-ae0d-4a1d-b1be-70fb3887dcc3)(content(Whitespace\" \
         \")))))((Secondary((id \
         bedcf703-6246-43ff-b3e5-e41bf55bc70e)(content(Whitespace\" \
         \"))))(Tile((id \
         5a7b6b71-bf51-479c-8425-a06cd3d8765d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8333c481-466f-49cc-8e23-e890701ee2e0)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df0ee399-7bb5-4016-989a-679f1f481884)(content(Whitespace\" \
         \"))))(Tile((id \
         84c5347d-6f86-47da-b413-4de5d3444114)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f80a5765-f3c3-4357-87df-e5e3b8940a8b)(content(Whitespace\" \
         \"))))(Tile((id \
         e5defcbd-be5d-4ba5-b9aa-ae22b23a68b8)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         320cefe9-b1fb-4674-8b6e-69b8ca920a92)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5a00ac8-afae-4fe7-9f10-e699662dfeb0)(content(Whitespace\" \
         \"))))(Tile((id \
         91fc3a5c-1da2-4560-b845-9d2eadc955d4)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1bdd126-86ab-41ed-a82c-fb0bb1befd9a)(content(Whitespace\" \
         \"))))(Tile((id \
         421d6be8-3639-41a4-afb9-77f7140ee19a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e49d62a-2a32-4b1a-95b4-9a7d260e0dc5)(content(Whitespace\" \
         \"))))(Tile((id \
         da777f29-5dd3-447c-b236-dbf9974407e3)(label(\"\\\"\\240\\159\\140\\183\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a273626-950b-4418-a1e9-9f487c4ac942)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3343dd4a-ba81-4fcb-9ac2-feda484103a1)(content(Whitespace\" \
         \"))))(Tile((id \
         b0b06474-11f4-42f8-801f-4401968a7937)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba521a35-177c-4b9b-8590-d2638b251760)(content(Whitespace\" \
         \"))))(Tile((id \
         e53fb148-e464-433c-9e14-270c705d28ee)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13cf0f2b-11eb-4c36-b472-44a80b53e669)(content(Whitespace\" \
         \"))))(Tile((id \
         c6a4afa6-e40c-4760-a82e-342749d8ac85)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f20c716e-def0-489b-8216-e3b9347420a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         581920c3-a63d-4e78-b8ab-2b3099de944e)(content(Whitespace\"\\n\"))))(Tile((id \
         56bd286a-696b-4e33-8f84-6acb3e7bd7e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         49a309e7-7be8-4b54-93b1-6c56a8367eb1)(content(Whitespace\" \
         \"))))(Tile((id \
         78320e0d-c3dc-468c-b68f-dae03a081345)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3dfcd300-ef96-4940-ae15-e4a15c224dee)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d245b64c-50aa-47ec-88d4-3e201caabacf)(content(Whitespace\" \
         \"))))(Tile((id \
         c6bb641b-4f70-465c-a46d-571ea3b52d00)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         efa3ce94-cf1a-4aad-a7f4-3545be9adfef)(content(Whitespace\" \
         \")))))((Secondary((id \
         f16ed104-c9a9-4ef4-a66b-29729905c181)(content(Whitespace\" \
         \"))))(Tile((id \
         ebee162a-caa9-449c-b118-eaf5dfb8bdc4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0ce0e17d-7ad1-47d5-bfa0-80067b324e14)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         721ec661-3c56-47b9-84d7-d1a170b78696)(content(Whitespace\" \
         \"))))(Tile((id \
         5ae83105-10e1-40d0-b955-2fa687d1456c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97be86e2-bd7c-4898-b0f9-9be994b193ed)(content(Whitespace\" \
         \"))))(Tile((id \
         7f581ba8-b2af-4592-8b90-696a86fbd216)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8abc9e55-2d0c-4eae-819a-757ea1322252)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a47fafb-e0b6-4df1-86d2-9cbf0a4e5a99)(content(Whitespace\" \
         \"))))(Tile((id \
         97d1a283-bf40-4c2a-9937-6374838fb0e4)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8b7a942-8625-4b53-ab89-ce482bf4dd8a)(content(Whitespace\" \
         \"))))(Tile((id \
         6d84c9e7-d6a5-419d-8684-02972c1c7ec7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4de29a4-3866-40b2-a874-222d352da08b)(content(Whitespace\" \
         \"))))(Tile((id \
         252c4305-7869-4564-a02a-5aa649227bbb)(label(\"\\\"\\240\\159\\140\\181\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f671c8a-060e-4ba7-b26f-73dd2c21b5c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99f39522-73ea-4d87-83ee-e3523f0d56b3)(content(Whitespace\" \
         \"))))(Tile((id \
         6bdeac3e-338b-4da6-86fc-f3ec3bb97bd2)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         87b0c309-594e-44e4-ada5-6764db6ac430)(content(Whitespace\" \
         \"))))(Tile((id \
         62302a1c-cf10-4012-a0ab-401bd0c4abce)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3af397c-74b8-42f0-b59d-371e8f0f5d6f)(content(Whitespace\" \
         \"))))(Tile((id \
         7a4ea876-32ee-417c-bd54-23d1fbc9fb7e)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         89c7e33b-017e-4bbc-8e31-66583448006e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         230dc5dd-d7e2-4bd4-a630-116adb366292)(content(Whitespace\"\\n\"))))(Tile((id \
         6bb86ef8-125b-474f-b9a4-1a5eac7d6bdf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         97927d97-a886-4f99-bc0e-7481bbd79b9f)(content(Whitespace\" \
         \"))))(Tile((id \
         89ae4037-0351-4e3a-ba57-6addc5d9ea64)(label(lily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7104bc35-7102-4c96-bd12-134d3e08080e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cab69fb0-83da-47f0-a94d-fc0a789ef3a8)(content(Whitespace\" \
         \"))))(Tile((id \
         02c13ffc-0839-4353-94ca-ca23f37e5974)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ad90af71-c1ac-43a6-a8c0-bf7e52aa634d)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5cbf946-246e-4727-bca6-b12c4c2fe9e4)(content(Whitespace\" \
         \"))))(Tile((id \
         45d2587b-aef9-447f-b23f-e4ea0153e363)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c48f4d4-ca5b-48e8-ad5b-6830da564b46)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         309d8b5d-b565-49ae-8249-042ed52195d7)(content(Whitespace\" \
         \"))))(Tile((id \
         b3bd5e24-5b8e-4b63-9ce7-45a8c331ca24)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b2db868-836d-46c6-80bb-bd35c377fc1f)(content(Whitespace\" \
         \"))))(Tile((id \
         73ff971b-f4ae-47ee-9c60-f3197225d49f)(label(\"\\\"Lily\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ffabbd7-260d-44fd-affe-d14b583cde10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e4cb9d93-776e-4446-afe2-81d5da48a21b)(content(Whitespace\" \
         \"))))(Tile((id \
         46c47bcf-3543-4af4-9e75-d80f90123cfd)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f29ce12-597d-4941-90af-230357e510e9)(content(Whitespace\" \
         \"))))(Tile((id \
         9908c977-8a01-4ab9-b12b-3e3b929c83a9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41ca77d8-ced2-465c-b52e-2bdc3ad151ad)(content(Whitespace\" \
         \"))))(Tile((id \
         cdfa063d-023b-45b3-8544-1b01f2e12741)(label(\"\\\"\\240\\159\\170\\183\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b510da54-6dcf-4bff-89b1-4e2336657b30)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1a7cd65-01a7-41d5-b2d6-f404e90c235f)(content(Whitespace\" \
         \"))))(Tile((id \
         1a3dbf3b-9669-4d9f-8087-3c3b7e7467a8)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         125dd911-485c-46b3-a11b-e246f2f3bf23)(content(Whitespace\" \
         \"))))(Tile((id \
         3f101064-6a9d-4e34-b8a4-ef2c98fb1e7f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df5089a7-135b-4c3b-8b36-087bb52d7df7)(content(Whitespace\" \
         \"))))(Tile((id \
         5598499e-edd7-4f21-8cd8-6800ddfd07d1)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         07c0314c-3aad-4098-92bb-da291dc4a912)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         50b963fb-0c4c-415b-93c2-ce4cab9cf18e)(content(Whitespace\"\\n\"))))(Tile((id \
         2c83c96b-f9af-4a6c-a0f2-8d64dab4c07f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         35cf595c-5d18-4d82-980a-da02f7536bec)(content(Whitespace\" \
         \"))))(Tile((id \
         db5a4d82-4164-40da-87cb-2ae397fb43ce)(label(daisy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5618268b-8168-4d55-8578-0c186c419617)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3020cfc-a379-4e3e-95cb-34339b86ba62)(content(Whitespace\" \
         \"))))(Tile((id \
         f095a0cc-e401-4790-89c0-91863c26bf9a)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5b4d9674-169d-4382-89a5-608abb4a1736)(content(Whitespace\" \
         \")))))((Secondary((id \
         b0b59dd0-6c9e-4cab-9eff-ef575da80171)(content(Whitespace\" \
         \"))))(Tile((id \
         459c7403-5e5d-4985-b09c-8d946296bf05)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         392e73e7-7953-4900-84ab-25f4660902c4)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         66ef8c70-042f-4bf9-876a-e716db57c3d3)(content(Whitespace\" \
         \"))))(Tile((id \
         8ca2b0eb-925a-4d13-9bea-fcee86238ad2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         463d1370-6efe-41e6-a3c5-277bbff2382b)(content(Whitespace\" \
         \"))))(Tile((id \
         ccc3d3ac-2064-41e0-a0d5-4fb3c8f62f5e)(label(\"\\\"Daisy\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6a04a76-428d-4cef-8684-7133babdbed9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         638760e3-5332-4e0f-9bd8-b5b54fe84388)(content(Whitespace\" \
         \"))))(Tile((id \
         ba0df8b6-a13b-4594-9d5a-faad6e0aba8c)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47962973-c263-4e0d-b90f-8e7209ec919c)(content(Whitespace\" \
         \"))))(Tile((id \
         7bed31d1-0656-4f89-b3cb-2f491f665723)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e338771-561e-47a3-88e4-1e1b30c3c9cd)(content(Whitespace\" \
         \"))))(Tile((id \
         a6f2a9aa-2854-4be5-8753-24aa6cad698e)(label(\"\\\"\\240\\159\\140\\188\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bd3056b-6982-499d-a0e9-7c4ba47a86be)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddf66325-61c4-44ea-9406-c0d39be6b41f)(content(Whitespace\" \
         \"))))(Tile((id \
         3feeca19-ada3-49d5-b237-86c7d64dfab1)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a92f6505-7e9a-474a-9af5-b029a36302d0)(content(Whitespace\" \
         \"))))(Tile((id \
         c7f7bf28-89f8-49c5-9aa9-a9e9e78fda1e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bddd1685-3524-43d2-b88c-43ca9b3289aa)(content(Whitespace\" \
         \"))))(Tile((id \
         99316862-d2ea-46e8-a5bb-011655f906a4)(label(160))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0ab71b55-787d-4b4e-9cfb-8fcdc4cf2ca2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ec63b37-2b71-4a8e-a6d0-1506d4bc425c)(content(Whitespace\"\\n\"))))(Secondary((id \
         82ad7be1-d3f7-4ba0-b39d-26a4d5a11d14)(content(Whitespace\"\\n\"))))(Secondary((id \
         69671587-94f3-401b-a9a0-90ee1e490a8a)(content(Comment\"# weekly_total \
         computes the total weekly water for a garden. #\"))))(Secondary((id \
         09d19464-c26a-44c4-8b84-3fc4e1eefc9f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e87481d1-cacb-4902-a466-550c00a9ce8a)(content(Comment\"# First it \
         maps each plant's daily water to weekly (x7), #\"))))(Secondary((id \
         69580478-ae2c-4512-8777-2f70d21ecd89)(content(Whitespace\"\\n\"))))(Secondary((id \
         42aead88-053e-44e4-9f05-2182f666e924)(content(Comment\"# then folds \
         to sum everything up. #\"))))(Secondary((id \
         c5524330-3aa0-4c60-9999-2be5a8445895)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1ea5e68-135d-4a73-aaa8-e16349e4d20c)(content(Whitespace\"\\n\"))))(Tile((id \
         e6e6100f-ce94-4a3c-bf8e-8cfa419f95d1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d91a4c4e-2932-48ca-80c1-37313505d1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         2c0006ce-cbd2-4abe-b010-4d65d7008374)(label(weekly_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6b014fcd-dcdd-4151-98f2-af038c965bda)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23708646-d798-412f-828d-742b7b127d12)(content(Whitespace\" \
         \"))))(Tile((id 13272b79-a085-4e84-a7a4-da71fab97c09)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         aca47e9d-2483-4b84-a0b2-7af534f41bd9)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         37ded888-d857-4daf-8317-dc2038855597)(content(Whitespace\" \
         \"))))(Tile((id \
         c5e03d01-91d9-4ca2-8d85-91cf19946208)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bd441dc6-689c-4338-8ecf-15be474ea74f)(content(Whitespace\" \
         \"))))(Tile((id \
         94fce62a-0fc5-4b49-ba33-2941c7eadce5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         de9c4416-1a37-4678-b262-adbc7b861415)(content(Whitespace\" \
         \")))))((Secondary((id \
         c0722b2a-bef5-4e4a-b7cd-8376a158299e)(content(Whitespace\"\\n\"))))(Tile((id \
         13e52d94-a9a9-4dcd-ae2a-f0f28bb31777)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         20620256-914c-434b-9e40-77c1341c425b)(content(Whitespace\" \
         \"))))(Tile((id \
         bf15de98-d045-41f5-9eb0-fdebe533bde1)(label(plants))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         434cbded-8e19-49c1-9451-dac42a4e5e5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8832a6be-874e-41d4-bcb7-737737ecb558)(content(Whitespace\"\\n\"))))(Tile((id \
         dc58af98-251e-4676-9a61-4e5af617e2bb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9da83a05-b113-44d0-b4a4-7f87b5be85ca)(content(Whitespace\" \
         \"))))(Tile((id \
         88164222-2e3d-48e8-9f53-2a711adb9683)(label(weekly_amounts))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         47131aa4-5322-4f5f-a66b-d0e89ba20ab6)(content(Whitespace\" \
         \")))))((Secondary((id \
         0cb73bd6-de79-4f57-a5de-d3e6e575ecab)(content(Whitespace\" \
         \"))))(Tile((id \
         63a47275-a33c-49a8-ba0b-08deeb4cc646)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01c375ed-bf5f-43e3-9b38-056574927a10)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f76229f0-b341-4a25-a9c6-a119fe60cb6a)(label(plants))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c61be691-69ac-4aa6-bc31-661a7af272a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98058e37-6e5e-48bc-aa08-16b8b5e032e4)(content(Whitespace\" \
         \"))))(Tile((id 0d14137b-8e65-4f12-8666-241ea5c1adaf)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ce51f484-6825-4368-9137-f97d30858986)(content(Whitespace\" \
         \"))))(Tile((id \
         bfe64dfe-70d7-40b4-9dfa-866899d96eb5)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c65cc5bd-7de0-4576-ae01-f410cf0681f0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f3341aac-0f71-4f50-96e6-df1f438fd775)(content(Whitespace\"\\n\"))))(Tile((id \
         6099a696-eb69-41bd-ac34-f70f1603e277)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3d89fa9a-ee8c-4eaa-a079-caadcf32b965)(content(Whitespace\" \
         \"))))(Tile((id \
         e042d5d6-4f41-4a3f-ac14-b406de4cac04)(label(daily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6017e958-8cb2-4fd6-a4ac-9db9b52867ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         4cfe31b8-94d2-42bc-9c4b-d837d1d41fe8)(content(Whitespace\" \
         \"))))(Tile((id \
         b18b605c-d608-4ff1-bd52-5d721e6eb44a)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33b823a2-ce0f-485c-8396-777ba7678de8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5f759d93-9dea-4182-96d1-c5b734f0fce1)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1cca6dec-c4d9-43b0-81a1-94108c605c7a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59011a0c-3c0b-4a19-b8de-d462db2faef4)(content(Whitespace\"\\n\"))))(Tile((id \
         38a5d5f1-cd8a-4aeb-9184-81af156d5e78)(label(daily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25ac68cb-5cdb-409c-90c5-eca6c2a90f9c)(content(Whitespace\" \
         \"))))(Tile((id \
         7b8744d5-b0d6-4009-a5ca-7ab392cabff8)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01d79af8-2599-4926-aa20-8dc455bac1ea)(content(Whitespace\" \
         \"))))(Tile((id \
         bc395534-f78b-4b96-bf95-4cfb1969350d)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         233ffb68-890d-4d45-8c22-6e5e8420fa88)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b64f9e05-426a-4862-b402-4d8b0c31fa20)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e0522e01-f037-4603-b2f0-6a8a99df7411)(content(Whitespace\"\\n\"))))(Tile((id \
         71f21ff4-2b57-4f93-bc2b-c04d92db6aba)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         87079e21-ff5b-4234-adf5-fc43324b7412)(content(Whitespace\" \
         \"))))(Tile((id \
         083d541e-601f-4f7f-a0b4-cfd5494833ea)(label(sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82f11080-64ce-4e62-88b3-5e8adc365fcf)(content(Whitespace\" \
         \")))))((Secondary((id \
         e5025be7-ea15-49db-bfa1-8c3e6692b9c7)(content(Whitespace\" \
         \"))))(Tile((id 31b3f9e5-25ee-4d1d-b53d-119fec2a1e05)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         44c89d51-8967-4c67-94a4-8495a4caa02f)(content(Whitespace\" \
         \"))))(Tile((id \
         2a587864-bd44-4c9c-b638-f2cd999b3c4c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         641d5e13-841c-4eff-964b-58220e56b6d4)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         883faff7-1d02-42be-b5c9-2e6b209e1594)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cc88bb84-8866-483d-9d67-3dd18148d87e)(content(Whitespace\" \
         \"))))(Tile((id \
         7cf276ee-ed49-4194-8731-41d384fcf86b)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         eb244572-7216-43ab-abc1-4f1c73354367)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         97e97363-99ac-40cf-9883-38721209fe09)(content(Whitespace\"\\n\"))))(Tile((id \
         6be33e94-fa82-4ac7-a43d-18e57e3fe023)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         19d4d342-33cd-4a17-8efb-e70deda20485)(content(Whitespace\" \
         \"))))(Tile((id \
         ff501d03-ffc3-421e-84b7-66f17caedae7)(label(running))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d50921c0-e13d-488e-8263-e56df561cf40)(content(Whitespace\" \
         \")))))((Secondary((id \
         61e61423-a94c-4355-afdc-c80aa64a14cf)(content(Whitespace\" \
         \"))))(Tile((id \
         2996293c-5f01-4072-a9fd-7398d9e6b5a7)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34fc24b9-64d5-43b6-b56f-e447f8cfd6ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         baebd595-c93d-4375-b31d-fba98b0be287)(content(Whitespace\"\\n\"))))(Tile((id \
         4d9c969b-99ca-4126-b882-a1c97c00c8a0)(label(running))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8bfa91b8-df5b-45bf-b090-cbc8b853d923)(content(Whitespace\" \
         \"))))(Tile((id \
         c12d08f7-3bcb-463a-84c5-8b68ce2ff5c4)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85231ec9-1a11-4acf-b9f6-cc2a70d4a0df)(content(Whitespace\" \
         \"))))(Tile((id \
         ab90c8f3-6b7a-460e-ab10-8614f1c7c3de)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5f151f5-48b1-4c19-8c33-d1a6ee445c4d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f7830b53-c325-4aa0-905e-ccde74613061)(content(Whitespace\"\\n\"))))(Tile((id \
         33f7bff1-e481-4639-8743-8fc31abde8b5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d34f46e6-3ad5-4698-bd1f-976f939fcc92)(content(Whitespace\" \
         \"))))(Tile((id \
         f97ed379-9920-402c-a386-ecfdef8c1560)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f4e3862a-10d7-42c5-b9cd-d38328e3abee)(content(Whitespace\" \
         \")))))((Secondary((id \
         64f58b08-bf77-48ff-a475-f924e30b704d)(content(Whitespace\" \
         \"))))(Tile((id \
         76495479-892b-404b-84ff-232c23305b37)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c40a427f-c1da-4f4a-b0de-3afce843cabb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4101c8c-7ad7-45cc-8b6a-f3a5b459dd93)(label(weekly_amounts))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b93844c4-c230-4e92-a75e-9fac2a1c9ecd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be369c43-0887-47c0-bed9-d0d3e2ba0bbc)(content(Whitespace\" \
         \"))))(Tile((id \
         351d2310-6da9-48f5-9252-80f7e256373b)(label(sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38a1ea3e-ba31-49ba-804f-c68f13b8007b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b04b4dbe-3801-47e7-b3df-ddad5c559048)(content(Whitespace\" \
         \"))))(Tile((id \
         2f5478a0-d2d2-44a6-81b1-5cc5c6a44584)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4a39ba96-c35f-4578-bed3-ff2c71000764)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0fcbc2e6-24f5-42f9-ab1c-bf5d4f83f537)(content(Whitespace\"\\n\"))))(Tile((id \
         58de4d5f-342f-4b39-ae3f-1f9435332ffc)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e497eb7-2f8f-4632-8ddb-96526c6e246a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1b772570-ae78-44fc-b18d-b231f34c6006)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7f17a16-ffc6-46bc-aabc-8b39a58c8e9a)(content(Whitespace\"\\n\"))))(Secondary((id \
         697c2f97-924d-4d12-a557-4efb2395bb10)(content(Comment\"# EXERCISE 1: \
         Step into the map #\"))))(Secondary((id \
         bcf59e49-bac0-401d-b20d-afa2567fe921)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f60165e-4b87-4fe9-bbe3-35f7faaa9d74)(content(Comment\"# 1. Add a \
         probe to `weekly_total(shade)` below. #\"))))(Secondary((id \
         07cfb0f7-8fff-4472-9bd3-93e653994b56)(content(Whitespace\"\\n\"))))(Secondary((id \
         e169701c-4d7f-4768-894f-e24f5c312f3f)(content(Comment\"#    It \
         returns 4270. How does it get there? #\"))))(Secondary((id \
         eae0f24c-1a95-4352-8b94-d80b36065916)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a576091-b962-457e-930c-50013a8fd7f4)(content(Comment\"# 2. Click the \
         sample and Step Into (Enter). #\"))))(Secondary((id \
         20ddf61b-8977-438c-a5b1-f1b1f428b36e)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a0f1a20-80b4-46fa-ab32-2bcd47f54741)(content(Comment\"# 3. Turn on \
         auto-probe inside `weekly_total`. #\"))))(Secondary((id \
         56e95c18-b78e-491d-af76-c8e06ff78bb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f2231a9-79b3-46c2-a91a-274627eb7083)(content(Comment\"# 4. The map \
         callback shows each plant's `daily` water #\"))))(Secondary((id \
         58f42679-f3bc-4270-8b76-430a7cbf09f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb387cc8-7c47-4255-8f90-1ed2ae39877e)(content(Comment\"#    and the \
         `daily * 7` result. In Many mode you see #\"))))(Secondary((id \
         3d14b71f-5276-4265-8bb5-fd0eb275424b)(content(Whitespace\"\\n\"))))(Secondary((id \
         84f2b5a1-d2be-44f2-ad84-8597ca169fa2)(content(Comment\"#    all 3 \
         transformations side by side: #\"))))(Secondary((id \
         33d885e7-507a-4086-acb8-ac553f3144ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         84c8b046-3dfb-4cf0-a79a-8cc4d0ba0e4e)(content(Comment\"#    daily: \
         [250, 200, 160] and daily*7: [1750, 1400, 1120] #\"))))(Secondary((id \
         bf47a7aa-81c5-4b25-9f8d-d0537cebd6f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b212a670-bb63-4e5a-aec2-5c8afabeb447)(content(Whitespace\"\\n\"))))(Secondary((id \
         687fe618-86dd-40f2-b35b-473c6545e363)(content(Comment\"# EXERCISE 2: \
         Now look at the fold #\"))))(Secondary((id \
         dda36f0a-32af-4acd-8938-f05b8f3f7c09)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0b4d508-74c0-45b7-a5df-4899080a8787)(content(Comment\"# 5. Still \
         inside `weekly_total`, look at the fold #\"))))(Secondary((id \
         f9a81ed8-bb08-4aff-a2e1-afffd94fae0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ec732e2-9c44-416e-94ef-5d9299daf5e0)(content(Comment\"#    \
         callback's samples. In Many mode, `running` shows \
         #\"))))(Secondary((id \
         b898e953-d91d-48ef-9390-43c0e34b1dec)(content(Whitespace\"\\n\"))))(Secondary((id \
         80244b05-124a-46b0-b53d-29237b5a96c8)(content(Comment\"#    the \
         accumulator: [0, 1750, 3150] and `running + w` #\"))))(Secondary((id \
         c201bf4a-7d59-4b8a-80e3-39a94c077173)(content(Whitespace\"\\n\"))))(Secondary((id \
         71a59c69-eb06-40af-88e7-4d004681cfb7)(content(Comment\"#    shows it \
         growing: [1750, 3150, 4270]. #\"))))(Secondary((id \
         aec01066-8908-44ec-a6de-76abf0639a2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c066718-9d49-4d78-919b-f92e6557f4cd)(content(Comment\"# 6. Use the \
         dynamic cursor bar at the top to navigate #\"))))(Secondary((id \
         02bedd4b-15b2-49ae-b024-9d8a56b1b0a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         29888404-3f78-464d-b09d-7e92c2fac929)(content(Comment\"#    back out. \
         Try stepping into `weekly_total(all)` \\226\\128\\148 \
         #\"))))(Secondary((id \
         9b752985-3aa7-4d30-a313-19cf3de2fcc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         725e7e3b-5af5-48fb-a8c6-25becfec0995)(content(Comment\"#    now there \
         are 5 iterations each. #\"))))(Secondary((id \
         06e70033-7d1f-4a66-983a-883c9caf2767)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea1e838d-e768-412e-b4c7-cfb6a1727115)(content(Whitespace\"\\n\"))))(Tile((id \
         cd08fc9b-bcc6-4539-9e49-9932da0eb8b4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0faecd28-03fc-4fee-9c49-a3474624c52e)(content(Whitespace\" \
         \"))))(Tile((id \
         b3e51e8e-ff67-4a15-abc9-3eda61b38116)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8dc29cb3-6079-4058-b6b3-54541bfb0210)(content(Whitespace\" \
         \")))))((Secondary((id \
         4d3b3108-8ddd-4f09-9fe2-9ce84f76dae9)(content(Whitespace\" \
         \"))))(Tile((id 9e814486-0002-49bc-bfbc-158fbe1f8f88)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f471f199-aa1f-4154-a91c-55ea1f28ba44)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41f8335e-f805-40cd-b967-67cef46289e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd2b89af-60f9-4997-8671-0e9283b5c2f3)(content(Whitespace\" \
         \"))))(Tile((id \
         55289d49-8c32-4b7d-907c-783adcda1e07)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8f95117-b8b9-46bc-ab4a-e1fc077269e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         295874ab-1c74-467e-b660-f3b2519d7ea2)(content(Whitespace\" \
         \"))))(Tile((id \
         4e8c5b29-77c1-4146-a0f5-09fde382a7cb)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         820bae07-997d-4b89-a1f2-bab6f13a09e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9808ac9a-2ec5-4f2f-ad45-ef482d10593a)(content(Whitespace\"\\n\"))))(Tile((id \
         e4f82e9e-6866-4381-917b-61bede20e2a2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2103d00f-f500-416e-9e01-598afb9fd4bd)(content(Whitespace\" \
         \"))))(Tile((id \
         2d443c62-46b1-4c34-b2e2-9f2e1a8c48c0)(label(sun))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8cad5d24-bd14-4056-96a6-1c9e0519d30a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6327bc3a-ec6a-47ef-a6bd-6bb24711ab81)(content(Whitespace\" \
         \"))))(Tile((id 259214c6-05ae-40ea-8853-99889fa76a99)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7dcdc655-8fed-49fa-ac83-e74040d0144f)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14f9cc58-ac1e-4ed2-aaf5-13f4a364c6da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53095404-4007-4178-9ad2-a15a8b544f29)(content(Whitespace\" \
         \"))))(Tile((id \
         8d406e6c-ba65-4cb3-9062-868c3af86eef)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7fa7962f-b3f9-488f-81c1-ddab6cdef700)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         53df9680-6a57-4198-81c0-e603746de86c)(content(Whitespace\"\\n\"))))(Tile((id \
         2cb751f7-0ede-4ad7-8d2f-a66df88f5937)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aff9ad19-1191-4a7b-959c-4514c8e95310)(content(Whitespace\" \
         \"))))(Tile((id \
         2256223c-2cc2-4639-8991-9feebac0533c)(label(all))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db01ef92-1fe4-44be-8591-1342c4512b55)(content(Whitespace\" \
         \")))))((Secondary((id \
         38569d1e-aafb-4dff-bc00-c43b3012316c)(content(Whitespace\" \
         \"))))(Tile((id e7c4207d-bc4e-4eb3-b145-4a2931dbc3bf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         10d02e5f-bf89-4e5f-98fc-cb16a2dc4ae1)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b724b6a0-fab5-4398-89a2-d4a13c95f658)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1955ee72-6446-458b-8db0-d0d35cdc92f2)(content(Whitespace\" \
         \"))))(Tile((id \
         dffce951-c526-47fa-ab0f-8c21f8940a63)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f521571-8f20-4d59-9454-7f8b4f30e580)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12bb3e02-73b2-41c2-b128-117f9130629d)(content(Whitespace\" \
         \"))))(Tile((id \
         103a2fc5-cfeb-4b1f-b3a7-9eb2c0069ca2)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ce3f324-995d-41f6-ac5d-007a3932f4e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a2d7d90-3eda-4873-8752-4c5be8762886)(content(Whitespace\" \
         \"))))(Tile((id \
         8e15b940-4b25-49f5-9865-b1567ff71bfb)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1959249-25cb-4e30-b021-ff9efb41d83e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c34593f9-79da-49e7-a984-d633eba4e8f0)(content(Whitespace\" \
         \"))))(Tile((id \
         df8e8ca4-338c-4a3c-a78d-578b0352ecb3)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5b4ca29-5ee7-46cc-bb8d-dbbfd70aebb5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d0b5d41d-8164-4c6c-b6c8-18cfdccf8933)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3190f17-cc1a-4982-9930-254078fdae8a)(content(Whitespace\"\\n\"))))(Tile((id \
         b646a69d-db89-44c4-b9ab-3db29752cb42)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fddf805-396b-4e3e-bf3d-fb8cce05d98f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         548deb7c-1312-45e6-8651-8cec83063359)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6d49e598-2915-4ec2-8141-5fc0d84e2b98)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1dc85ef3-fad7-457e-a8ff-4e2ac038d767)(content(Whitespace\"\\n\"))))(Tile((id \
         ad35787a-cb19-4986-b783-23df368dd72c)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         786b1498-e73c-4379-9a95-3eb5a9a47af2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5bf7ead7-cbb2-43a6-b4f5-8402fbc92547)(label(sun))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e5341d87-11f3-4a1d-b6ed-674f39913d70)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c75a6b9-0060-4fa2-9309-9a1a6b526c71)(content(Whitespace\"\\n\"))))(Tile((id \
         c8cf6f12-5988-4b5f-be3f-4b13febc8326)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a6909bf-122b-478d-b5fc-37daf552c00a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e075f6a1-4416-443b-9808-21533de20b41)(label(all))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         66404c04-cd69-497a-94a7-1c4689a8c40f)(content(Whitespace\"\\n\"))))(Secondary((id \
         08944550-88d6-4473-bb76-b30be074f891)(content(Whitespace\"\\n\"))))(Secondary((id \
         42e48fb2-11a5-4c3d-bc40-bd24916953bb)(content(Comment\"# END \
         #\"))))(Secondary((id \
         418cb39b-adfe-432a-981f-b1f393e12c29)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PART 5 VARIANT: STEP INTO WITH MAP + FOLD #\n\n\
         # This function has a two-stage pipeline: map transforms #\n\
         # the data, then fold aggregates it. From outside you see #\n\
         # one number. Step Into reveals the whole pipeline. #\n\n\
         # ============================================================ #\n\n\
         type Plant = (\n\
         name = String,\n\
         icon = String,\n\
         water = Int\n\
         ) in\n\n\
         let fern: Plant = (name = \"Fern\", icon = \"\240\159\140\191\", \
         water = 250) in\n\
         let orchid: Plant = (name = \"Orchid\", icon = \"\240\159\140\183\", \
         water = 180) in\n\
         let cactus: Plant = (name = \"Cactus\", icon = \"\240\159\140\181\", \
         water = 50) in\n\
         let lily: Plant = (name = \"Lily\", icon = \"\240\159\170\183\", \
         water = 200) in\n\
         let daisy: Plant = (name = \"Daisy\", icon = \"\240\159\140\188\", \
         water = 160) in\n\n\
         # weekly_total computes the total weekly water for a garden. #\n\
         # First it maps each plant's daily water to weekly (x7), #\n\
         # then folds to sum everything up. #\n\n\
         let weekly_total: [Plant] -> Int =\n\
         fun plants ->\n\
         let weekly_amounts = map(plants, fun plant ->\n\
         let daily = plant.water in\n\
         daily * 7\n\
         ) in\n\
         let sum = fun (acc, w) ->\n\
         let running = acc in\n\
         running + w\n\
         in\n\
         let total = fold_left(weekly_amounts, sum, 0) in\n\
         total\n\
         in\n\n\
         # EXERCISE 1: Step into the map #\n\
         # 1. Add a probe to `weekly_total(shade)` below. #\n\
         #    It returns 4270. How does it get there? #\n\
         # 2. Click the sample and Step Into (Enter). #\n\
         # 3. Turn on auto-probe inside `weekly_total`. #\n\
         # 4. The map callback shows each plant's `daily` water #\n\
         #    and the `daily * 7` result. In Many mode you see #\n\
         #    all 3 transformations side by side: #\n\
         #    daily: [250, 200, 160] and daily*7: [1750, 1400, 1120] #\n\n\
         # EXERCISE 2: Now look at the fold #\n\
         # 5. Still inside `weekly_total`, look at the fold #\n\
         #    callback's samples. In Many mode, `running` shows #\n\
         #    the accumulator: [0, 1750, 3150] and `running + w` #\n\
         #    shows it growing: [1750, 3150, 4270]. #\n\
         # 6. Use the dynamic cursor bar at the top to navigate #\n\
         #    back out. Try stepping into `weekly_total(all)` \226\128\148 #\n\
         #    now there are 5 iterations each. #\n\n\
         let shade = [fern, lily, daisy] in\n\
         let sun = [orchid, cactus] in\n\
         let all = [fern, orchid, cactus, lily, daisy] in\n\n\
         weekly_total(shade);\n\
         weekly_total(sun);\n\
         weekly_total(all)\n\n\
         # END #\n";
      refractors = "()";
    } )
