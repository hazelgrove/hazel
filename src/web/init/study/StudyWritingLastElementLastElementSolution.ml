let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-solution",
    {
      segment =
        "((Secondary((id \
         274aa554-6861-46d0-aede-b9bbdb82129e)(content(Comment\"# LAST ELEMENT \
         - SOLUTION #\"))))(Secondary((id \
         0f68d355-2b86-47dd-8d6d-83b6295bf338)(content(Whitespace\"\\n\"))))(Secondary((id \
         41fcd846-3c64-4cc9-ab51-1a9c38b7ffc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c85f7f9-faed-4c75-b59c-460bf7126845)(content(Comment\"# Each step of \
         the fold replaces the accumulator   #\"))))(Secondary((id \
         0404692e-bd29-4426-887e-2a5528328875)(content(Whitespace\"\\n\"))))(Secondary((id \
         670d2894-df99-4621-b542-e1286f3e7b36)(content(Comment\"# with the \
         current element. The final result is    #\"))))(Secondary((id \
         f4c215d6-22b6-43bc-b4f8-167ace572639)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ea2ac6e-a5d3-4b27-8a2d-9e23ccc87ea9)(content(Comment\"# the last \
         element seen. For empty list, returns   #\"))))(Secondary((id \
         ef5e0889-e215-415d-b2fb-cba368609db2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3f8a829-e84c-4ee3-91dc-739fe46898c5)(content(Comment\"# the initial \
         value (default).                     #\"))))(Secondary((id \
         6527ee1e-94d7-4179-ba9a-660ba2abe36a)(content(Whitespace\"\\n\"))))(Secondary((id \
         90e6b964-2552-4ba1-8c3d-6848f024dcb0)(content(Whitespace\"\\n\"))))(Tile((id \
         343b90c0-1a20-4e46-88f0-26ffbbf040a5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fae27567-bccb-4541-aa35-95c7c1adc0f5)(content(Whitespace\" \
         \"))))(Tile((id \
         da7dd442-26b8-4387-b2fa-a09588239a29)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b5f73bc9-1e39-4849-a5a6-afd595192a61)(content(Whitespace\" \
         \")))))((Secondary((id \
         77ec1dc6-0027-4736-8d63-7248fb7d8f66)(content(Whitespace\" \
         \"))))(Tile((id 5a987f97-4d4a-4ce1-907f-bb515219d6e0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9ea646b2-aab2-41ad-b905-6f4da717cb4a)(content(Whitespace\" \
         \"))))(Tile((id \
         fab5372b-0340-42d3-93bf-5a6018dbf5b4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         47ab426b-20d8-4587-82ed-e8f341a47255)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         576c4520-666f-4797-9046-75069956315a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         526da6f0-ca09-458d-b35e-cd7fda091419)(content(Whitespace\" \
         \"))))(Tile((id \
         39c376a0-4ef6-45e9-8f2c-8fd2283d765f)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7db6ae72-260c-4d4f-ad9b-cfef3296f33d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be4d7abf-6c30-4787-80e0-ec66cbc75b2a)(content(Whitespace\"\\n\"))))(Tile((id \
         ef045234-628e-4809-b60f-9941da45be50)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63b1c894-fa11-4e85-ad06-3804a0d11627)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ada7958-4a1f-471e-bbf9-79ea4d9c09da)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22dc4966-0522-43ee-a174-027d4cc9042b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f30daf0-1fa0-4dab-814a-73b112e7e55d)(content(Whitespace\" \
         \"))))(Tile((id afb6f8e2-3cc5-43d1-86a2-d479599113ac)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f5fd2886-2712-4fc0-93eb-a115b879e21d)(content(Whitespace\" \
         \"))))(Tile((id \
         ec6a020e-8750-4a90-9947-df7fd7925575)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         425f5157-97c6-4414-9785-007ea7ff61e9)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24874e5c-d611-4971-b727-f7e73ec8e7f2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         93ebcc13-976a-4d8d-b173-99bfe970fd6e)(content(Whitespace\" \
         \"))))(Tile((id \
         6429d7d8-f729-4316-bd1f-b2425de6ca6e)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f4ba6d5b-098d-4e2a-9a60-9147b853e669)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b75f37fa-178e-4033-a96b-4c99cc5e7eef)(content(Whitespace\" \
         \"))))(Tile((id \
         cdec6c9d-e41a-472c-8faf-32ec19ae2a17)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c051340c-6717-46ac-887a-6963abe69450)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85dd830d-e315-4ada-a2a4-c1ab2508de9d)(content(Whitespace\" \
         \"))))(Tile((id \
         cdcc08e0-f848-4e0f-9424-887e3356516d)(label(default))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e6f14624-ccf4-4060-be05-110eca9e13ab)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3cc1d266-d9e3-4f95-a7b0-ec23ba776f0c)(content(Whitespace\"\\n\"))))(Secondary((id \
         40756c3f-bd32-454e-bcc8-90fd13955cf9)(content(Whitespace\"\\n\"))))(Tile((id \
         d30a94df-e24e-4a90-9ab6-5f3d9ea90f51)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         684bc0da-6f31-46db-85dc-01644228eb43)(content(Whitespace\"\\n\"))))(Tile((id \
         4a48a950-7fa9-4bd7-897f-11c56391c454)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23325509-1fb7-4ce4-9c5d-a4a6b3eaf486)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9a7d4a8-5b12-4cc1-870c-637497d08775)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0610a056-e827-4785-98f3-b2ffcfee6260)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1bfbb6d-1138-4d40-8865-dcbb9d7d1f18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c32a0ef-73f6-448a-a76d-0aa769c7bb09)(content(Whitespace\" \
         \"))))(Tile((id \
         287a5a81-9e4e-4279-8f12-4d3e99c65b49)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8d61291-1b86-409f-9110-7832340ac0ad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a15510fc-112c-48dd-a266-00e819e94d8b)(content(Whitespace\" \
         \"))))(Tile((id \
         9ae026ef-08fd-4929-a0f8-024e71eb543f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4376f6f8-7089-4526-8919-b46bcc7f2373)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22bd5f11-da25-44d3-b0bf-107adbb67453)(content(Whitespace\" \
         \"))))(Tile((id \
         22859c35-9dc4-41e4-ae24-2d4fbe2c43bf)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         743fbaad-16dd-4f93-9d52-05375599c14b)(content(Whitespace\"\\n\"))))(Tile((id \
         b257e050-d241-48e1-8bb6-c8e62e2f1b67)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6fbfde77-f0fb-4e98-af51-3f586b910843)(content(Whitespace\" \
         \"))))(Tile((id \
         e891e6ef-3bfa-42d6-9848-f6495381cf38)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9743c726-5f70-4737-8adb-935a9e7cdd2a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         458f91c5-ddee-4e98-8249-0d99386ce6ae)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c6532de-dd69-42de-891f-312b3aa8db85)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1e07e5b-e2b8-4be7-af77-d6814c07f676)(content(Whitespace\"\\n\"))))(Tile((id \
         f7415c96-3b5c-437c-931d-49347a779a0f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ab5eecf-b246-4564-ba95-d5d49d20c4d0)(content(Whitespace\"\\n\"))))(Tile((id \
         a9d291ce-78b7-4658-a7b6-5c401b49c70b)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4ec115e5-40c0-40a1-a5ff-40d71dbc5a6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c9360b5a-86f1-4376-a2b6-2081fa301ebc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         471f2e9e-3683-4205-a566-716a26acd4ad)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         aa91adb9-65bc-4529-8c4a-3aa8affd70ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e38817f7-7195-4205-b241-3f1ae82a361a)(content(Whitespace\" \
         \"))))(Tile((id \
         08ba596a-8554-4d48-ac9f-ab68855dbae9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63e08c5c-15bd-4016-8a60-01fb18923566)(content(Whitespace\"\\n\"))))(Tile((id \
         7ce4b363-85db-4841-8044-1f3cf2cd2947)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98a57f2b-b6b6-470e-a0ef-6e34f726ac44)(content(Whitespace\" \
         \"))))(Tile((id \
         cd451429-dd9a-4b1b-8f52-8d2786cd27e0)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         019c4ff1-6519-485d-ad8b-7a5a54adf830)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f8161760-59ea-4ec6-84dc-fc89ec611c05)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e90d38c0-5737-424b-b3aa-20feae196cfc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea20593f-ea3d-4761-b4a1-e6567cb54f8f)(content(Whitespace\"\\n\"))))(Tile((id \
         68a06c1c-59f0-43e5-875f-ec48a61a423e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         084814bc-9f9d-4e22-9533-212899aae7ff)(content(Whitespace\"\\n\"))))(Tile((id \
         50ef0466-c7b0-42b8-aa97-a0812349af5d)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b53ff54d-6c02-40f2-a370-33392c50ce28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf895bbf-d134-4f07-b459-51bfe0635b63)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         72c05464-835e-4c8a-99cd-7ac123806258)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cec44cae-f7a6-497f-9592-dfd556651476)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63653891-4438-4020-a14d-1afd2cb85beb)(content(Whitespace\" \
         \"))))(Tile((id \
         107ab2e0-d922-46a9-9d33-704366a075c4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         13a0122a-0095-48c4-bc3b-39fe6c56654c)(content(Whitespace\"\\n\"))))(Tile((id \
         775e3ad6-0dfa-456b-b4dd-6a8aaba58d2f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8529978-ffe0-4ae9-b5b8-a90d584e4a8f)(content(Whitespace\" \
         \"))))(Tile((id \
         2ee109e8-31d2-4a0e-9b51-04260ff8eca3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ebb5a773-554d-4222-b761-0b332db394ed)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bef36920-78ef-41e5-9086-c49fe203af69)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7539c8e-fcb5-46e1-bc2e-f8ceee0a8ebd)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3088664-9d2f-42d4-9d67-54dbdba57aed)(content(Whitespace\"\\n\"))))(Tile((id \
         0e5dd7a8-f457-4d7d-bdd1-ec8cb91e88a8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         240c2cc3-d411-4d2f-93dc-adce0d6f1922)(content(Whitespace\"\\n\"))))(Tile((id \
         eea7a228-9cda-4a79-9998-6f0e1b127402)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8fe1855-0047-4ee4-be01-9406516becf3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4affd776-118f-4540-a0ca-386b90b249b7)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77b5cf22-3af2-499c-b79d-b25e4499c970)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd1656be-c2ef-411f-8a29-a040a29faa90)(content(Whitespace\" \
         \"))))(Tile((id \
         ba31dcba-64fd-4c41-8232-4c203ff97fc2)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f7995fc0-3e88-4dd3-b65b-f603b2900bc8)(content(Whitespace\"\\n\"))))(Tile((id \
         edbb738c-37eb-4579-b66f-0870c202dd46)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66431fcd-bdf9-4951-b2cd-65ea88cde6a7)(content(Whitespace\" \
         \"))))(Tile((id \
         88af39e8-cf63-4743-8ad9-743bb3392bdd)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb922fdf-9652-46fa-9d71-d0a0f68fbb5a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0ade019e-bd97-4738-834d-80156e840d63)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6913b3e-641f-4af9-9a2d-0f70bbb44ff2)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd5f331b-d20b-4960-bdbe-928f3581e4a8)(content(Whitespace\"\\n\"))))(Tile((id \
         08d45902-a9c0-41e5-85da-3fd52e82f9a8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9bb06192-84f6-463e-ac0e-1ab7af86cd80)(content(Whitespace\"\\n\"))))(Tile((id \
         54d09b20-7a5e-4772-af72-b02d45124336)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a99288a0-6d57-482f-90c6-79ff292299bd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         76c4258c-5215-4148-a5f3-69622b1527df)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fced1733-10f9-4bb1-8128-da4fd37f5d01)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a89344d-81d3-4125-bd4b-2eee365eb7f7)(content(Whitespace\" \
         \"))))(Tile((id \
         8e932352-e780-4141-ab74-22d4d9931c4a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         08cfd395-ca24-4f8b-8a0a-7dfa36ee9154)(content(Whitespace\"\\n\"))))(Tile((id \
         b62f9dac-2af7-49bf-b41c-08adf51d05bc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79afe428-4c5b-4f09-b615-79654a70b075)(content(Whitespace\" \
         \"))))(Tile((id \
         d6942787-5daf-4016-b138-22efb0deeb29)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         102bc79b-2304-4ad7-ac57-2d750cee39d3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         222c6004-be39-4b3a-a3f2-643b0f12b9a9)(content(Whitespace\"\\n\")))))";
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
