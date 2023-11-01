let startup : PersistentData.t =
  {
    settings =
      {
        captions = true;
        secondary_icons = false;
        statics = true;
        dynamics = true;
        async_evaluation = false;
        context_inspector = false;
        instructor_mode = true;
        benchmark = false;
        mode = Examples;
        breadcrumb_bars = List.init 10 (fun _ -> false);
      };
    scratch =
      ( 0,
        [
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               dad126b8-40da-4a33-8f94-471139ad060a)(content(Whitespace\" \
               \")))))((Grout((id 97978c5f-95bc-444f-a113-ab1d7d1f59b3)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               69b91914-da52-4cc2-982d-889c4a60f0ff)(content(Whitespace\" \
               \")))))((Grout((id e1a78ea5-bf3e-4eed-985a-e787fe48ccf8)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               a598ab14-b09a-4182-aa41-a94870ca2d5b)(content(Whitespace\" \
               \")))))((Grout((id bdd6780e-29eb-4fc9-8328-67ff12531096)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               841466a7-648c-4fb9-aa70-77a6a3d0a1b3)(content(Whitespace\" \
               \")))))((Grout((id 9a04f2f1-e20d-40c6-b4fa-67ef9ec44b62)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               0c8fc5b3-7e2a-4c85-b889-08999fa380e1)(content(Whitespace\" \
               \")))))((Grout((id f90ce54d-e232-46f3-9c2c-29fb1a6516eb)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               46d6bc93-38d6-4ff7-9c62-154bbe8882cc)(content(Whitespace\" \
               \")))))((Grout((id d5bb7a70-88de-4de4-8bf7-8969d337ea0d)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               936b2101-bbca-402e-9f8f-726b906c0fe4)(content(Whitespace\" \
               \")))))((Grout((id 2e75a573-b1ef-4910-b507-b3d6cd3db6c0)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               bb419304-c8c5-4f39-aafe-ee1014d15dae)(content(Whitespace\" \
               \"))))(Secondary((id \
               8faaa9f4-08b6-4d9c-9fc7-970734d48bde)(content(Whitespace\" \
               \"))))(Secondary((id \
               f103b1ee-1441-4d18-840c-6363f3d6b9b1)(content(Whitespace\" \
               \")))))((Grout((id baef35da-c761-48bb-bcd2-89074bc550cf)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "    ";
          };
        ] );
    examples =
      ( "Introduction",
        [
          ( "Basic Reference",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                 75dd9138-fceb-4d64-8b19-14d2be8a2941)(content(Comment\"# \
                 Hazel Language Quick Reference #\"))))(Secondary((id \
                 10679d65-4a59-408a-89ea-07105fc4c258)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c465da11-4653-40e5-a77a-a25f9249a6fa)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7ec51aa2-1ce8-44f9-b809-8f0799000c6b)(content(Comment\"# \
                 Empty holes stand for missing expressions, patterns, or types \
                 #\"))))(Secondary((id \
                 40ecbff8-4964-4e20-9092-04b6f3f1e128)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 665c3a26-a9c2-445b-9279-d4ab39010f9b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1c81ab1d-d563-4deb-b5d4-f8e47a72d304)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a68243fd-a14e-4aae-9193-a8d2c3ef32f4)(label(empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b81cad56-068f-41a9-8c38-adae9142581e)(content(Whitespace\" \
                 \")))))((Grout((id \
                 4f290450-68ec-443b-8841-1cca96a6bf81)(shape \
                 Convex)))(Secondary((id \
                 c70da7f0-d876-4b40-804f-b0638236bf01)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6f0f8e81-d390-470a-abe3-f19df38ef0a1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 70bdbb5a-a3a6-4b3c-bc18-51e757e19f14)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4f394cf9-12a3-42b2-a478-46c1aed14379)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6ccf73bb-d6c0-4dc0-a189-cdf008120d01)(content(Comment\"# \
                 Integers #\"))))(Secondary((id \
                 f78e40c3-8fed-4df1-962d-dbd3d9ad7210)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3a4375f9-84aa-4eef-ba24-f9d0c2e74639)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7a06abcf-2b39-4634-8f23-d51acc419dc7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50dab883-b900-45cc-bba1-b2f4f520289b)(label(int_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6c3ef2f0-177d-4de6-8c42-46b1862526a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3852de45-243e-48f6-ac9c-dc4bc38f714d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 191162b1-fd40-4950-a9ba-915100ba561e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 149e7aa7-51a1-4243-a7b9-fcf9b932f888)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ab61b68f-a2e8-4fe3-806f-a5c9c2cc2677)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 41605932-f395-4803-85a2-6f9433d9351f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13498e21-2324-4f0a-9e8f-e0a99308be10)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1cf982f3-edf9-433d-af05-95e95aa8754d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 97887b46-e2b5-4cff-9670-ac1c248d2b53)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e5a9c551-b3fb-4111-bc3f-b4c93c509618)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f8da0003-1f2f-4de8-8efe-5a9b9217671e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e82167a5-ecec-4ea9-990b-a845c571b8b8)(label(negation))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ff0e9926-f2f6-42a8-a5f2-4d8f956e1a34)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ebb49a6d-a822-487b-865f-0c07815ec143)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18b4e4d1-af24-4a58-ab7a-f9df77cc3138)(label(-1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0b3e27ea-60b6-4bb4-9ed5-93f9f1760516)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48fdbe41-27a3-444e-98ca-68d243cb2c05)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c265725c-7392-423a-8fdd-b3d5a92356c8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b0f36434-c8ba-43e2-a3c7-3f16ce5acef4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 03ceb0ed-fd00-4c1d-8d0c-2163d19b9817)(label(arithmetic))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9a0feeff-24d9-4ed5-abbc-94fe149254e5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3de5fccf-59b8-4288-a32b-026eb20e53f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb07ebcd-bc39-492e-ac90-76b72266356d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6472d144-ee67-404d-9756-aae929549450)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 40408b38-bdf1-4982-a161-203c79813713)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7bc99940-d1af-4fef-a975-da6a73167206)(content(Whitespace\" \
                 \"))))(Tile((id \
                 78589cc3-aabb-43a5-b6e7-fc786c3d6d8a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1ec4e7c7-82b0-4c5d-86de-7237828e0f33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2843ae8a-5f0d-4125-84e1-883c8df9f298)(label(8))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4ea132e8-2a79-44f4-9aa0-4ab33635ccdb)(label(/))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1344266b-1d38-4a42-a510-080618c0cfaf)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ea1881c5-b8fd-4179-a618-176df0b4aed9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 50d3fd8b-4a86-4dc5-a6c8-f310fe681a6f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8e9c5fd0-9822-4c40-9459-f56972271d19)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f5d25e4c-f7f4-4a4e-bee2-b574c74788f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a80f8102-0d0e-4c30-be49-be0501054908)(label(int_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 bed641ee-8d33-4c19-b0c3-eafbf0619e23)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 39246038-695a-492a-8fc1-d9f90d16b63f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9121e3d8-0f0f-4f9d-b3ff-0620114c3d6d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 74d0bd51-cdf3-445c-90c3-fd1bacbde1da)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b438f233-315b-4496-ac50-52c46d65bacd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f7c94fa-2c1d-4ff0-a8ff-c0abcf7ac469)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 721d314b-75e8-4aa1-bd56-eab7bdd7176a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3f7fd71-d1b4-4e37-8e4c-6ad69281d404)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 952e43aa-ed5a-4d69-94ef-88efe0665310)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9271fb65-0e93-41cb-9d82-52104287d8d9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a6534d7-200c-46a1-8188-7fc83fd901bb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 01ce854d-60a1-4de0-b1f1-ab075bbe150b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab3db9e0-58d1-408d-8da2-ee2a64945706)(label(<))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22b491d1-395e-4cde-b88b-a440c957e17b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11f47d0e-49b7-49ff-ab17-59d2476e64d3)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8df16a6e-eb54-4509-8ff7-b5055598fcff)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 58ca9655-9cc7-4f76-b446-87f619d85582)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4acec9f9-2ca0-4265-88f5-62d3918317e3)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 593b8502-361b-4052-83ad-92e98f892379)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5d61b14-aae2-4ce4-abc1-da9529480fbd)(label(<=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 081904ca-5e4b-45d7-9dc4-93b011ed946b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dac502e4-a4bb-43a4-bc02-dcf0eb674a18)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ba2c6ab1-0e0b-4b0f-82f0-93d1ff3c0834)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 92bd6476-5279-4504-bd7e-a45d8b864523)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b634091-6f8a-4746-a610-25487c3c0f59)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0660411f-9c3f-428f-b56f-e3a8c405c424)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a82c3150-c664-406e-89c8-b9e7414747ec)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3dfb0a12-75d7-4602-816c-0c414df67964)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1970cb6-c7c0-489c-a816-ca80282cbe72)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 aa08bc7b-44b2-4e6c-a4aa-f4af337da50d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3066cf74-efb9-42a2-a47f-3e61d7264f1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bcf445fa-c02e-463c-951d-147cbb967196)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1e6138f3-1a16-4c82-96e2-f0e68114b622)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14c2bd9d-cc9a-405b-a957-624876976641)(label(>=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7d3e6040-0356-4015-bab7-8cd893cc1069)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b742293-b6fb-4ba1-9575-774461b706ed)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 24bf7ea2-ceda-43d9-b4cb-74b4d0b11e94)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f29e4b11-53f9-45c9-ac55-5188b6a21005)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2b0487ce-ce5b-4431-8ae2-c1645a0cb6d4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 631287e1-8827-49b4-895d-481e1fb37838)(content(Comment\"# \
                 Floating Point Numbers #\"))))(Secondary((id \
                 48d20e0c-c268-4d42-906b-def5a8841fdf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0ed3cd15-be8b-408c-b8d8-b30da4d585fe)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 708d2c4e-8838-4f25-a2d4-a929fdd8dce8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6a8073a4-ae6d-4c46-bfad-70130089f462)(label(float_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c7962738-8d04-4c17-a912-5b35a5e62be6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f750a8be-fdd8-4c3c-9816-140a919970a3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 af096488-df56-4711-8894-7c79f57c3e81)(content(Whitespace\" \
                 \"))))(Tile((id \
                 69558467-f773-41bb-b6cd-6a5573d1bfbf)(label(Float))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c107f2f2-aa79-47a2-880f-ca3f146b65ef)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5ec71037-6e8f-480d-a75b-a500e7c75485)(content(Whitespace\" \
                 \"))))(Tile((id \
                 704de116-68ca-43a4-a707-fab827c84268)(label(1.5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 db2c9b67-ea12-40b2-9484-978b9553874e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2187f824-a65d-4d24-986e-5ed86a297e00)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1b9ccdbc-fd9e-42d7-b78d-327b386433de)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 66ca210c-a665-4089-833a-ec75d4fa9f6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f66ec07-bc7f-49b9-8672-2d57e97dd6f2)(label(float_artih))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 83c182d4-f941-4ab3-b654-47331499afa8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 519e006e-6045-4c47-a05b-d41f87d8246d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45b567b3-bc14-4510-bfd7-3397ae3880ad)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 12f44745-034b-4f08-ab6a-81e7efe1297f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e325a33-ab37-4521-9166-4021ee739016)(label(*.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ad1b5a73-fc23-4c34-ad80-3e4c4a5ecefd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a2b9421f-5e3a-43d5-b66b-4401407fa52c)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8f5f27e8-08ca-4d27-bf82-3197f160a8b3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7930c18-49e9-4a82-9c9e-26f64b07bdd9)(label(+.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 64a1a5aa-9393-443c-8b0f-5d9cf62b3e6a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e97bcb6-a65c-4b5c-b6fa-318c76d4492e)(label(8.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 39658970-368d-4a49-b58b-b306ef5267e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79e974c4-7e74-4207-bb52-6f4076dd6609)(label(/.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 63b92516-14f9-4a1c-bede-01803840913d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0d9c123-e7f3-4d7d-a828-e9647da1a359)(label(4.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 59a2c50c-392b-4bf0-abe3-4b0fc257e8e8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9394429c-132a-4af4-af56-30648a7f88a1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf118a02-075d-4d40-a542-6ad0c648908e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3fa76cd9-0625-49f1-ba45-8b27fb403cbb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 699fbc12-c695-42dc-ac38-855a2b51a154)(label(float_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 8a8ad144-ec07-4490-891b-d43629ec6162)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 24ce98f1-2340-4198-b01b-f126e39b828a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b3ab730c-654b-401e-b675-ae6ab22c19d9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 421cc81c-899f-4de9-9754-37c25eea3920)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e202192b-8b02-4fde-8e1e-23f65522cfe0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc5cd516-353e-4458-8daa-097965aab855)(label(==.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 31bf29e8-9152-4d94-8acf-c0acb69e8c0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 068a691c-cc8f-480e-84fe-7f49ee9e8340)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9e8a9e2e-dd1b-4efd-b2f9-c78c848e92df)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 dac49313-df86-45de-983a-a6feae4777d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1041e4c1-a3ce-4d4d-b908-c48ebb3ff9c5)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 273652a0-0737-4046-9a7f-ba8dd8c24873)(content(Whitespace\" \
                 \"))))(Tile((id \
                 760c18a1-fbd2-4788-a563-3e00414a42e1)(label(<.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f4c6a1ee-6920-4b04-b51c-7bc3bb7f6b17)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d4682adc-bc80-48ac-863e-6b04037560fa)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d49a4883-e432-4924-b9c3-f8dfac22293b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c7cdb4f7-9e9e-4eda-bdbb-594b23e2ff4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00c63c48-463e-49db-abd9-8db5bbbd5615)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 61a6b391-3716-464d-a4f4-4e2c78942ef5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5ddf8a8-ed7b-43c0-9e05-7697d7cc9718)(label(<=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0c9ca608-4d49-417a-ad1c-ca209e9b9381)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f288176-672c-4aae-a0fa-329748be9b83)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 57ed2ad8-5256-497f-b76d-b5af31a96c40)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 44bb169e-2272-49be-876e-e20d9df410fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd26ad56-c7fe-437b-999c-81f1bd562f98)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c4233a34-eaa2-4b09-9816-f88a30a2bb95)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8f736d4d-fd61-4dc4-a0bb-d6f1255208cb)(label(>.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 54f63437-584c-4266-a3a3-690de677190d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f097c286-99cd-4fb7-8e8b-2b59428b665a)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d9d4229f-1b4a-4786-85cd-2fc2b1579d1e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bb280599-2e0c-4bf6-b4b9-1ec61871de1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fe7b104-b586-497b-87f8-7782f0af5ff6)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 10b030ca-307b-4d8d-9296-ed8f334683b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e217744e-d5a7-4d21-90df-cb57eb0eedc2)(label(>=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d88ca67a-483a-4f3e-9c82-7d64fd45b2a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5785fd0-0a7a-49e6-9b17-38e8cf495cf6)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 0e97711b-f503-442e-bf8a-9b822f449cc7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7e9607c1-0cac-4a78-a375-7dbf017cd1ab)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5fd4ae18-bd07-4358-a625-7ad8741759a6)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7fa59caf-fff6-47ba-868a-1ac5172c5973)(content(Comment\"# \
                 Booleans #\"))))(Secondary((id \
                 d48c09fa-f12b-485b-b97e-3ee8e03d6b68)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e530034d-b0fd-42fc-866f-7596a2c2a953)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 eef76040-345e-4b08-8219-3e0fc1964ac1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 96f7aaa1-ea1f-4abf-9437-df66950113d7)(label(booleans))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d21debaa-e76e-4d1f-9200-80f485cad502)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72f09dc3-f48e-4cc0-b6de-24e9f8679769)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d71f8cb0-2a07-4a71-8486-a5deb1394e58)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a30940a2-e64b-447d-bde6-ee3a9b89bbf4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2c98f6ef-6cd5-4545-8acd-95d065bdf7eb)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 859f7d1b-a9d1-488a-825c-c5babb9b3a4c)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 75589554-0369-48ab-9c84-c356bffb9b11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0293b0ae-18a4-4245-87f0-56023ef3feb5)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5e12e046-286d-4e8d-9549-6c7d37e82dc4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b5479479-4932-4d47-a1a0-4a4a7ebc5d33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bbe3d7d4-0e5a-4df8-a295-cdcc784e10ef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0eb90020-c65f-4585-81fe-98babd54ba86)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4322e29a-4d81-40d3-bdc0-5a7a3a11d7b9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2acd2f32-d328-40a7-af4a-9a8be41a53cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 132bb8b0-fab5-4dc4-9866-a282efae3187)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 fe4a4dde-30b2-421a-a947-c50263bc7ba4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6d28090a-5edf-445c-bf1c-d1a2e1fd45e5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f39aaa6e-ac87-424e-af72-8853508d37ac)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 288aca39-43bf-46ee-ab16-bbbdbb606730)(content(Whitespace\" \
                 \"))))(Tile((id \
                 206a6060-ba72-442f-95cd-6d1daec1f9b5)(label(conditionals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 15636bea-440e-4f96-99d6-7c05c35fb1ff)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 96d7f2de-cbc0-49ce-8a2c-ba07d42b141f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3f64e77a-8262-4809-82d0-331a24830459)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e4fd84ee-912e-4195-a091-976cc9f92625)(content(Whitespace\" \
                 \"))))(Tile((id \
                 539702c1-68e3-460a-a10b-317afa44bc2e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4f3b8ce6-f640-4514-b90a-986376b55c8d)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5cad3ad6-471b-4537-814e-ad084503895f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1ef16647-15b9-499f-bf11-f670df4deb3f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 91a14585-571d-49e6-a425-24096620608e)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 2ee2d7e8-f0ae-4087-b006-ff011e6d4b43)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a0eaee57-2e23-46be-af5a-640bc59e9309)(content(Whitespace\" \
                 \"))))(Tile((id \
                 01816af4-8efd-4831-9708-d0a575f25a96)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d718fc19-ceb5-4d54-93b2-1ad325ddd747)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5a9b9c1d-7a85-40dd-8de9-9f44fdd6abca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc7f95d7-5d95-452f-bce9-88b9627faf00)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 43b3210b-c113-4075-9bd4-3a906fdc0575)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d13d930-f6b4-48ab-8347-b02b43b35546)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2a7f413e-ad9d-4d85-a243-204b6c1b10e6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7ce878d0-b715-435d-a049-79375f13a88a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55c1b03c-e7ef-49d7-9a4d-ab6d4ddbce10)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b7812a56-a494-44fb-b403-153762410e5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62b4ac54-5200-4793-b2aa-87479850579c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b8065914-3029-4f9a-ae60-acc712c82f56)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f05b93c-b395-4c48-a47a-d02d5596cb9e)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 676dd3bb-32d8-4453-8089-9f86483c472f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7fb66c81-5198-41d7-b096-03236018fd32)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fe3e79bb-9ab7-478c-bbfc-b7d398c05474)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2a8c1b50-0f2a-4a4c-8ba2-39a88d945a34)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d830ccd1-00aa-4bac-ae37-690556e34339)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ea357648-6127-43d4-8462-f8a66f63cf1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02918c12-855f-4afa-af79-42c469dba158)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e82c8cb3-3596-4e01-9b0c-5f677107d792)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49a59ae8-7dc2-4e51-b7e5-ce6123c8694c)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 66cc9194-fa00-486f-a3d3-3bafbf1443dc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5dd5dabe-bbb8-44e2-a8f3-8e9b3a8c46c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e4cdf60-d422-4361-91e5-5752288c9b93)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fabee49a-7097-4aeb-ac9e-e0e04ef03d75)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bdcde690-20f5-42c6-a6bc-3fca9154f8ce)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 169c2903-445d-46f5-aa1a-b70e389ee7a1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 01530b34-b5d3-4aca-807e-6dfcb4f93597)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a9a4092b-ffcc-4b6b-9c0f-342ea180bb13)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 6f66fc7f-76f5-46de-ad9e-3b9b78bcd882)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc1dc39b-897a-44c2-8e03-918032e703b7)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 697cfbdd-2e39-453a-9705-92ab5814297a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7ada5c2e-f3d6-412e-906e-ef1c5cddb95a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ff25e27a-e734-4a9d-98cd-e363d0176c9a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7f2ae316-e616-4ad3-8abd-87a0b4a357a4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a6d66ca7-3388-410d-8605-7bbc4e3e14c8)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 7be828b6-5f4f-4842-83d5-c493d7b10f4c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 834da74d-bbbf-4cd1-8fec-8b052bbd5c67)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 9f089f84-1fd0-4bd5-9236-7dd62cf6393d)(content(Comment\"# \
                 Tuples #\"))))(Secondary((id \
                 b153342a-dfea-4402-a6d9-17990ad5c996)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e4ebd9c-94dd-43f2-ae41-706b1db879c6)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4c3543f9-d75d-440e-b6e1-60cdd32fb225)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72df2a03-ac74-414b-89b8-96abcd384858)(label(tuples))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dc9cd918-5175-407a-8932-e9bad93a8ec1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 865658f5-b53a-4929-84e4-7499d1aa9484)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 46a68fb8-9c7b-4344-a9c1-c8a3d14cb0be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ae79d79-6396-4c79-9fc3-20f41fd7ce4b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 c7e63b3b-e01c-458d-9214-5c2f52079352)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0f2e352a-d95a-4530-8887-f8a889cbfaff)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a6c31b66-a309-4d8a-a7a3-a316c9c3912f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84a96c5e-a7bd-45de-94f1-2d5eba3985e7)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7305e719-4877-4275-944d-ce357ab95b03)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bc4eb8b9-917c-425e-8dfa-b3437454f0b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dcf888ef-a985-4914-a73c-3b547f8ea351)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 08c1630d-9176-4e85-aee1-b8b38c3699af)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7ec49ae0-dd45-40de-a9cd-8b38338218e2)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5e817455-f019-444f-bc77-c6ac27a4838e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 43c38950-fd2a-4c81-893e-9c8b276eda36)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 ea26ddd4-b321-412b-adc3-aa75b04bb305)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 508b82a6-f93c-4a53-a4fd-6faec85448bc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eaa1057d-ac50-4424-b57c-e8bfa7e3c3ea)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d45346cd-c685-4f01-a204-e690e78ac9fa)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b3651b98-4aee-4cb8-949b-31bbfb6d9e5a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f88f796a-0512-46d9-823e-2a5af1d29247)(content(Whitespace\" \
                 \"))))(Tile((id \
                 594f34c7-96c3-405f-a180-da5b1d2ba994)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6732602d-a2e5-44ac-bcce-e9c3aa5ba3b6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8ffd5924-7aa4-47a5-a944-386d5edad902)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec13deba-2cc5-4511-bb70-99019b9ad3d8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c93b572e-0b18-44e2-ae6d-4245ad679cb0)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4badbc43-5fab-4936-adcd-aca1c9ffab1d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d8283139-a32a-4cdb-85c4-465526548fad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1207afdb-334b-413d-b23b-01ddaef4a096)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 8bb43fd9-accb-413d-8651-e2f4c2f40467)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 978d5f49-45d0-4abc-bf83-321600aa9dd3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4187e58d-f3de-46f4-90c9-43e1e46ee69d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 25c81819-10e4-4b54-8436-2fbcc998cbc9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8db4a7d3-6986-457c-9743-3aa14de0afc1)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 fb6151e5-355c-45ed-abda-81215b2b9b12)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5bf324c7-b566-4173-8e8a-5b6c9355310b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c0a8f38c-5098-4a1d-bd4c-8080e498211e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62836b64-f082-4940-9b87-e9ac1075f95b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5efb59f5-c579-47c7-8258-538e9c99bb0d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 7f4ecdc5-669a-4465-b799-54d62d089b4e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5932615e-0be3-49dd-bd41-802913374631)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 18c7e96e-0fea-4156-94bc-f598f6381080)(label(c))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2d21dec4-c1d2-4d23-b89d-377868711739)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3026d640-7735-4c9d-bd92-4fa83b206148)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bb950fb6-d605-4d4c-a40c-f3bdb27b85da)(label(d))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
                 bbb3b2e2-07d7-4a6b-84b0-c20c8dcc899a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bdbf6d18-d1b2-419b-9dc9-233b11df3ccf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de146acf-6a1c-4e78-ae50-dc6c88757dc8)(label(tuples))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0878af1d-d3af-4bd1-8ab2-bc17e4865b7d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 effe6e97-ac78-40fc-9f69-d81696069679)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5fd17515-b792-4fa3-80e5-2777971e3e73)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 83f036d7-b5b7-4f27-a0ae-2989a1cdc889)(content(Comment\"# \
                 Functions #\"))))(Secondary((id \
                 398b5652-a47d-4026-99e5-be025c761a8c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 89d47476-ebf1-45da-b5fb-d0275c01a1ea)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5995d778-aa6d-461f-bb8a-772b08650c0c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f509417-0d43-4414-90ef-9f86b4a57843)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 649a09a5-7984-4663-8a6e-cc24a80a8541)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b5062243-5cf6-40b7-a31d-8231c0505c76)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2d3e287c-a6f1-4a30-be6d-bc8ad7fa0dfa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d6d9cbe-2ada-408f-96a5-83c539a394df)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 ba4e32f5-d543-4436-a465-0c71f98660f6)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 90019d0a-9940-4949-8899-068365eb13ef)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 51865a75-b921-4f3f-988e-0d48bc40a8ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd618bd1-560b-408d-bea4-e365977eb257)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9b48a688-9be6-4292-b645-e3808d160fb0)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 975dec43-316a-4963-ae2e-e0315e34814f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b6e1382-0fc2-4962-8197-74c04cdb136e)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9b4dde38-8b72-4f34-a214-a0d6b028ca94)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88b71573-3206-4fb2-8100-e1afaf8cff7b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e9a67a15-9bfe-4688-adbe-e673b9c34e13)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2bc6a15-d038-4fc0-99be-0a859ede3766)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5f8446fe-faed-4bb5-a70d-08258c73c5c5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 80e137fc-26c5-4776-9723-47e9d55b7e41)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 244a004c-c1af-4c83-90c4-70a2c247692f)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b5321e53-e24f-42dc-81ba-f754f1ec4c08)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7c9e3f23-f839-4c56-a1a6-3f941470ad00)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 46ad6cc2-f331-48d3-b7f3-bcf36a61a1a9)(label(m))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9fe9faa8-ab91-48a4-aa5a-f22b8fd9cd8d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2eeee62c-62f0-4622-908c-fa704d21d477)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31b2581f-4de3-4a03-886b-35faf43325b2)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b3d4ad37-2b9d-43a3-9391-473dab91c8d7)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 311f7757-e2c1-4ed0-8b11-a5f08d38e263)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b490edd-eac8-488c-80bb-b9685b138bf7)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 9c76232a-91f2-4908-a219-67fb82dcf2a3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 76c121dc-9306-4f4d-b03e-8b40c2f82c6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c4b13de5-339a-4ba3-874e-5a6ea7bb3bd6)(label(m))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 70472c06-dca8-4caf-8825-49b54a5e3548)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d85ea2c7-6ee7-4b56-8d11-48cf5f3eabf3)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 732aa4e8-101d-4e92-96c6-fc8d8621b4d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edea8a44-fd90-4263-bb48-7680dfa39c2d)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 809450c4-ab0d-4579-96a1-a435d53296b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5c07927-f945-4bc8-ac18-b13ea8cb6743)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ec1a911a-8f75-4d3a-b973-f08ad48cbeea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98ecae94-769a-4c0b-b2f2-5b9bd3c915d0)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 62dfd641-fc55-4ded-835c-d99dfe8e0c53)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 28656142-5390-4c3c-9f27-2dc6e742149f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e1bdfb47-0ddc-4c0c-93dc-be9cd1cb5404)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d6a6cac7-098c-4908-b8c6-9d3b64dcb2df)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e439ec00-c7a1-4340-902e-e771f5380d2d)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 e0c4eeec-613a-4b9d-a259-811ab7a3a4fb)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d10b36ca-835f-4e46-b363-d95d9f90244e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 16e0d51c-dd71-4fee-9232-0e426a8e8000)(content(Comment\"# \
                 Recursive Functions (arrow type annotation required) \
                 #\"))))(Secondary((id \
                 dc48c4b1-4790-435f-a89c-835fc1446de0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1efe816e-68e2-4729-9cc3-d58576ba7be5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2f38196b-46a1-4198-8a8a-103c1328d8e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 672e6f6f-d9e6-47f4-a8f1-153431522cc7)(label(double_recursively))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4d487ef6-dee8-4b55-a4fc-61b92ffcfefd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bbb04a0a-9ec4-4c14-89e3-8401e2dc0704)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 26eea84f-0816-4bab-90c8-f0af6469b9ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 434ba12e-ae09-4218-beb3-a9d6a0a11412)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2e616524-5658-421e-9e0f-c0e56198e6de)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c69c2cd6-70d1-455d-8179-e734fde5b353)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 42d58ca1-3005-4b97-b6ee-81a8212eae7b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30b27681-c553-4fd5-b371-1b0241a7b00b)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a4538fef-2799-4396-b584-8b271aca24ea)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 68781c42-97c5-49e8-b15b-a9ed6c5ae1e2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 89a33a75-44f4-4bf6-a51b-3692dfb1ab4f)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d1271898-cc57-4e62-ba71-71a3e6ab3e60)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f175d80f-90bb-4953-a973-a96473b74baf)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 37704655-4daa-4d67-bbf0-5310a475d1b9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9ece1589-c737-46b2-81a6-821245412b6a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 90b4d14f-cd4c-4186-8318-e2be3ba3e8ad)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2549f1c4-9ad0-46db-b61e-27fb7ace7cc0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b48dd57-4295-464f-8acc-e2fb4d200678)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3250b9ea-d8ed-4b5e-808e-029c8bad95cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49a51145-6fa5-484c-8d9d-fbf93f81d6bb)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a0f741af-482e-4a54-be4a-e97d19bc0717)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56339d7c-dbe4-4919-8f32-d9638532d6a2)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4bb1496d-470b-40df-bb7c-d79ec2aab760)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 414e6495-8a1e-41e1-b354-74606519c097)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d59b43c8-c6d0-4a53-9631-b282d5129dc1)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c81518bb-8463-49b3-92e6-28bb78a5f97b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9c233477-d89d-4af4-b48b-273f99ef00d7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 de590d79-219f-484a-ad65-bfd2d16e4cc0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 78412748-7b63-4363-966a-00c122b12a45)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d6a52a73-37de-48dd-a825-8ce8b5bac7fe)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 48a35fbf-dcdf-43ce-bd4a-f7d3368de63a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c2a332d3-c015-4b76-b087-eb5b5354dc32)(label(double_recursively))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 754a6c98-f331-49c0-bed6-5e67d1557a72)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 65c66581-c58a-4a69-a23e-08939965637f)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 35efac0b-30a8-4ae3-a2c7-7af01b450946)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a3f0ad9-e6d3-45ca-abc7-acf079b24750)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c3e9b2b7-25fd-495d-bef7-118d06787c71)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f507684-b24d-4ba4-9d3c-b8c36a955b31)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ebc23e55-5709-434d-8942-b875e1befe15)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83b52d46-fa32-4cb1-a2ab-2c21146cb9a2)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 00d34170-499c-4597-afd7-92e7ad54269a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a90076e-8643-4b4a-8456-01bd29481168)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ed545b86-7b41-41b4-aa6f-ea935c7b0bcd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c956c930-7f73-4590-940a-3e16ccfebdcd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ebfe47da-c684-4818-8ca7-2492c5c13738)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7b500a8d-c818-498c-85ac-cccbda5db7f9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a8987a76-e4e9-4de2-a14f-02211643c5b0)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 9930e7bc-2f65-466f-84a4-cb816b58716b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 45283df6-84a9-4032-b340-5ca5b6ab1bb4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8cf808cb-1fb9-4d1a-b40a-78a75eacbde8)(content(Comment\"# \
                 Lists #\"))))(Secondary((id \
                 c954adf2-2c05-49d8-9e5a-1bbc0d7f9642)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 40e3991c-ad55-4e77-b4f4-76bc1749e536)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 88344a86-1dc1-4284-9d1f-07c85108e2c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 546a1bef-bcdd-44ce-a035-a519c4525cc9)(label(empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 8669b80a-0002-43cc-a8e0-dfbea3437fef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed1cd028-40e3-496d-b69a-9ea8d6be1bfc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 eb401d17-6a58-49d0-9080-75e8fabcc255)(content(Whitespace\" \
                 \"))))(Tile((id ae34b831-0f49-4e12-aac6-262825c499b6)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 e6cbf678-937e-4898-998d-d709149fc69e)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 75d6173c-4c90-48ca-8a07-cf532cf70fb0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fa043caf-6a13-4b1d-800f-f882c02dec51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f720ad00-cf5c-4da0-ba90-06a1b1c2d184)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6409a822-033d-48b0-827e-ccab41c6c0e4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e765abca-777d-408a-826d-794e1441bc2f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 76420c4d-75f4-4356-98e5-deca97b10943)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 02665089-a776-40de-9b90-728a154c422a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e3bb7ba-f37e-4e7a-a316-dc9574192558)(label(non_empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5ee5e9ea-f4d2-4b44-874a-49c418d587d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86cd388d-fe0a-4ddb-ac0d-f7b7912af4e7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3dd68bf4-3c6b-4438-8d5f-83a74ff7461d)(content(Whitespace\" \
                 \"))))(Tile((id cb896fb9-27e7-4d7b-8528-4a46024553bc)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 6fd3a774-fef0-466a-869b-f98323987916)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 724007f3-15f0-4f75-a766-9e1c56b805c6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 16658440-87c6-4987-971e-f57d063bb853)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b1f9747-19eb-499d-a90e-5bafa05cb825)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 09c850a2-d322-41b3-b0f5-c7497c14fcd7)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 97de6708-46d5-4894-9e71-9dd354715665)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 789aa390-6c6b-41a7-9070-9d252767498d)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 8eae58a2-31a9-4e14-944e-5cbd8739e47e)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 52c10d14-b280-4118-9cac-1979d5f89c5e)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 58271f2f-2f2a-4e59-bb68-411a5f103291)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 38490643-8a17-4adf-bf75-16951162cc6d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cc47ae1e-55e7-4524-bc1d-8a9ea0e34841)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e967fa2c-473d-4fd1-95fe-843aa8ae20af)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 699ea9fe-6d71-4e5a-9453-f70b8ab1dfe7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f17a8f1b-fe73-4f35-9287-2a83f39917f9)(label(list_literals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a15875fe-bc7c-43d8-84a2-e5ce2214fd36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ca8d1d5-2ef2-4781-b76e-5254cac37e09)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0014e3d4-2af2-4429-94b0-b4768e55826c)(content(Whitespace\" \
                 \"))))(Tile((id b4d20643-4224-40aa-a288-54c20bde6f63)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 3b0744c0-0ae5-4085-a676-df916155e1f4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9f765f23-e801-4a75-8a8d-a2f2d09d83dc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0ee2b074-1ab3-422e-8ff6-12f9d2865999)(content(Whitespace\" \
                 \"))))(Tile((id 7660c0b3-fc07-4355-ab83-7b1b3ee34bce)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 b0f18bc0-869d-49fe-91eb-960a5c708874)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f555bce1-26a3-43ca-91a5-a941e6a6da2d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 51f35d72-2a85-4109-852b-058ef9027b2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12b3e42c-1daf-4584-8736-3e845f56a890)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2e3dd9da-bffc-471b-95e7-75225c4e1761)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 521cc5f0-b8cf-4773-92e6-9d4283dd45ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14bae752-58b4-4b3b-9cfc-fd4d995544ed)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b72eac3b-a1ab-4c65-8180-7a16256153c4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8a82899e-a266-4de9-9a69-14007f7923f1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 35580f29-42a0-4afa-b049-a1884d3837a5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 24e7cea4-8ba9-4384-97e2-42c43ef3dd29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a68409f5-757d-45e6-b0d2-ef2d6b9641ba)(label(length))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3c4f2d81-d2e2-4dbd-88c4-a7c6cd05e4fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a81c412-29d0-4605-b26c-16e2aa57e2f1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 80a7f893-9dfa-41e3-92ed-fe36ee34dc67)(content(Whitespace\" \
                 \"))))(Tile((id b3ce2b1c-7cae-443a-bbb3-4bbe5c49e466)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 0b3d4db2-b523-4cc5-bec8-ba53349b5884)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 48e24008-69f0-418b-bd58-1fc41fa802ed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e37762f-a3ec-4540-907d-26d57bca1be6)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f0f1dbb5-22a2-407c-a788-f4eac18b9947)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7edc9877-3004-4f33-a17f-aec59059799c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ba5829e7-bdde-4553-a4cc-ecd75067cab1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e95e13de-16db-436e-a7cc-c67738625056)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3a925fcb-a203-4f58-b6b6-d9b65dd7be21)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7f41584d-aa62-4cbe-9f0f-b2f710072c6e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48455983-6191-403d-8c08-81a5b1d936c0)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 562ddd45-5fe9-403c-8486-2d0d8c571781)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 97562bf9-c73e-48ab-981a-de73d51e9221)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a3c7b1ff-1097-48ac-b363-92fc00a9dc08)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 84ef479b-8891-4594-9ed0-ff9f9f8980ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e38eae39-ec1e-4529-a5af-069fe8d43264)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bd7bd1e2-a196-4b9a-8d1c-8ca862d66c2c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 979a34fd-6f0a-4888-af16-12c6cd4d49d7)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ce59b96e-65bb-40ae-aa5c-efada8117c84)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d3a1bb9-d3e1-4830-8d08-f19f957b729c)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 993ab2ae-e78d-4296-b5e7-4efd096939b1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 68b05fe2-8400-4ea9-8b1c-734b1778835b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 973f8fb7-41b1-4b0e-93a6-d4c2c98f5605)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f49b6bf0-b41d-4436-bc02-2c0df8343865)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 729b0aa7-c749-4c31-bb8b-785e1ee4a29a)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 208f1784-d0d5-4fd1-b3ce-d30da0c11ea0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cdb422b6-cd0e-49de-966e-fe81780dd3d7)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9d4f2c78-0b32-4f97-aa44-6bf5cd0649c8)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 46d1c30e-0d1d-4a84-b9d7-dfb5bc3829a2)(label(tl))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 02fc9338-13c2-4da2-9140-fc3e7a946862)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5f7679b1-4e80-4c39-b191-4a7a71f87385)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19bfc395-4683-4400-9cac-2fc1eafea9c3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fe3f5c31-b77f-46f5-9796-a1a4d521f44a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62e64256-1f6f-4531-961a-6684021bff0d)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a4326b2e-d7c2-413f-8104-4c4b864584b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5ad9982-28b3-4213-84e0-7847df7eae88)(label(length))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 535da9bc-f3ee-4878-ab10-adbd1abcec6d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 be4dcebf-7b43-498a-a8b5-5f6aadc9270f)(label(tl))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 87a5eace-70d1-4b0b-a042-91b5ce9fe0d7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fec6bfa6-3aaa-4349-86d7-af0335f5b6ad)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 07739b9d-3919-4c03-b354-12c9356bbe7d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 900de1f5-348e-4aa3-a6bf-e1283ff412fd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 dc87373f-d19e-445d-bfab-5d1e08eb7730)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 42aca7fa-1d2e-4f27-a379-9ee5384fa7b4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cb9a190f-ac79-409b-aa7a-6cf4b84e8c9d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f1b46c07-a5cf-47d3-bd34-5f77b8c1ea94)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b7fb525e-5ba8-4a36-a217-07b2c4a5eaef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cf5cc514-896a-448d-aea7-2a06be04daad)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 60d753be-8146-4aea-bf13-d73cb384be86)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 368edd87-447a-45be-ae38-14c8dfefda7c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f74e9abb-995f-43ca-a668-4649165d83a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b38b07c9-5f1b-4dbc-942a-d49a51367561)(label(has_at_least_two_elements))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6eca9626-d4ce-45f3-952d-fc4820ea2650)(content(Whitespace\" \
                 \"))))(Tile((id \
                 96c638f9-75d8-4ade-a152-f55d677c264b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 65298787-a76d-497c-8c6f-23346cff8bde)(content(Whitespace\" \
                 \"))))(Tile((id 77567126-f7c1-4dc8-aa7f-0fd95b342eaf)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 25803e50-9eb6-439e-b28a-2e46b53aa249)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 28c37088-94e6-4bf4-96ce-93db3b3af527)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b04f9a0b-2e34-4049-8aba-dccbed65329c)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 70ee5924-43af-44b7-8643-330ff2dd7a7e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 103ad0fc-ab6c-4506-a2a1-b3fdd76761bb)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 95fd10d5-bf79-43eb-87b7-82befbddda6d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 001cb1e9-d83a-4d56-b391-abfa36ea3956)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf492bd6-36f2-4385-a99e-bc41be67e5f0)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 04ef6713-53ff-4dc1-8364-2942ffff6a63)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd2e5728-0303-4582-aeee-7714cbc370d0)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2866c93e-62ae-48ef-a6e1-1010f086cebc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c33f601f-df41-458d-bd59-aee619ebf075)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a15e2862-fdcc-48a0-922a-5304ef76bbe4)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 69d95103-c257-4d08-ab46-fbdbbf1aacc0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a7e5263-c1a1-4201-9dde-b224d088ed89)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 150d9d8d-0a8c-40ae-babd-1bcf726ddfaa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1a93be8d-2a8b-4f8b-a584-36fe4951f686)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 345d48fb-9dfa-4abc-9718-c9fd4f9b329e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13990549-10dc-406e-ba28-3f592b714334)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7b0c268c-e43d-4539-aef6-68f5c24b15bb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5766b7f1-72e4-484a-8179-0feaffe2d1a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b51ce270-6009-48e4-804c-5b8e93ee84fd)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3a4b9172-0204-4509-ac3d-db534db73efa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1c8c862a-d4a3-4e95-ab74-1877c59b0e4a)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9ef91bd7-a028-440b-a13f-54decbfc2379)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b9ca649e-5f94-4236-a559-c518a6e27300)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cc6ee3b5-e049-47db-904b-a2330565ab9a)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 f4f0340a-4912-49c0-b38b-35745246b987)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e929fbe1-2fa1-4e09-8039-20065ce631a2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 09ee0000-0c41-46df-aea2-587015b19602)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7efec01-16dc-4623-99d4-3671e105b6d6)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 950862af-3413-440c-8ded-34a7448d3284)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ed802c6d-af32-4da6-91b7-ad05034312be)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 341704fd-617e-488a-8680-ff288de3e021)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f590a2e9-bd18-40b5-94d2-eca934d3a576)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1ce74f48-afa5-418b-a119-6fecb8b6e854)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 ecf92a1f-f645-4f8f-9f6e-fb2267279b7c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b1d28b74-08b8-49dc-8c14-03d8793cee25)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 6f1ec43c-3bd9-4f92-8f0a-52569bc2c4e7)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b8a73023-58c2-4a7b-a94b-96eccb6cebf1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9606f1e5-dddf-403e-aef3-5ebda30ff283)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b125b11b-9d32-4de9-bebd-21bf70bc0cdd)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1dcd5954-c94a-4010-8a1a-889c904538f9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a7cd6c47-c9b9-4d01-95bb-f50809e89acb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 72320ae5-70b4-4892-80a3-94c59ced2d93)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 97fb33cb-bf0d-47d4-b862-623d8df58cdc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7c1db986-c042-40f8-a075-21a0508d8585)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 921cbfea-fb87-4946-bc44-46b7a0b0df42)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8ec199ec-c596-4c80-9f49-f173a95c44c9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5366e9eb-3a2b-4328-bf73-26f4c7a46abd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5296820b-3814-4958-9cdf-9642426132fc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2dc3f577-506b-47e4-ab88-3d4f21aa1969)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 58983993-390a-4c46-a2a8-61c400decb0b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1908c75a-2ca1-4f7b-91b0-bd21fbc627ff)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a8dcd534-909d-4669-a4f9-a777f7d6794c)(content(Comment\"# \
                 Strings #\"))))(Secondary((id \
                 da143017-da5b-4fa0-b263-34c0b6006999)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1699fdef-2d68-4c48-bc2a-eed0138c42a9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7d870a8c-3f58-4da2-8870-78b2fd0a2557)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e852ddc-6089-4ad2-ba9b-e090c3b036bc)(label(string_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 20579bfb-0f5e-4e6c-bf70-3a1947d8e53a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 86e73902-49be-48ce-a4c0-80d0be16cb42)(content(Whitespace\" \
                 \"))))(Tile((id \
                 27a162d9-c603-4dc1-ba7e-3da08f795c15)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a3d28c21-3d02-4501-8c85-7dcaa011956a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 efdb6579-f4d2-469a-b5bb-152cb876111b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e59852c1-a319-479e-9fdd-df763c2cfd11)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 59ad428e-f4f7-46e1-8042-c3df8656ab2a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6d04f78b-9549-481b-8d2f-cacad94f3744)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6dde9b4a-71e6-47a4-b6a1-f8af208d6c19)(label(string_equality))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 46b6f7be-9255-488b-8233-954209fa2e2c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 06aa253d-03d1-48b1-9016-4a3fe2672cfc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46f7c74a-a5f8-43e0-af27-6b9b76a20661)(label(string_lits))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dcef2478-9e00-41a3-b272-8d775fc15b70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2219dbfc-8042-4ed3-a3ae-b98cfe80d9c1)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 56bd8e77-f069-4e68-bd47-56061a68410f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ead4a14b-7781-4ff1-8dcd-1e3349bc3270)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e140eb26-3a8c-40f9-8ecf-0c17ab2b9204)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a9bdf255-f627-43cd-a4a3-9aad4dbd45be)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cd2b1fd8-77c0-4d26-aa0f-99d0cf229cb8)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 576d0435-2d33-4ba5-9c60-261c52ec69ab)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c597062f-1631-40bb-bc47-1e665b911bf4)(content(Comment\"# \
                 Non-empty holes are the red dotted boxes around errors \
                 #\"))))(Secondary((id \
                 420b55de-7bdb-46b7-a079-65a9772f3458)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5bfb2176-5a08-45c0-ab76-cc6f62a0c409)(content(Comment\"# (you \
                 can still run programs with non-empty holes) \
                 #\"))))(Secondary((id \
                 1abee569-2d68-4bf7-b7d2-b4746a281a70)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a75773bf-286c-4a09-9f55-342b7fb69bdf)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1ceb0443-9ab1-4f3e-83e7-6710d2866bb6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bff41fcc-2e4c-4046-8718-7452d3feb5fb)(label(non_empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d52da5b0-542e-4fe1-931b-f8c6e6ef03da)(content(Whitespace\" \
                 \"))))(Tile((id \
                 823fcbde-2c11-4049-a641-a5d0d65cb0f1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3d8ba801-62b1-41d8-bebb-a52a1a56f59e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94ec560c-bca0-4aa7-9ccb-0cf062e73537)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5616bf32-a896-47a2-926a-0624a7e8c4df)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d3de26f6-a3d5-42b7-a037-c8d48c35d1ef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1cbf364e-8ff7-45b4-ade4-3fe3d7895980)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f98cfab4-75a6-4dd8-a22b-338fc918fd7a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 26cb0bf4-c884-4443-aa05-abb8fd188658)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3ccc4f91-fcad-48dd-9f91-e30fc596dca2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a3fe0476-6b31-4ec4-ad86-1870a5314952)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e57205c-3bbe-4950-a701-e6c3040573fc)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 102d6587-69cb-4a11-a888-460cfb5f6ec2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d23903cb-cdd5-4887-84b8-6faa67507862)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1605ccda-54f7-488d-bb1c-0e37e3d66c21)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba8daea4-089d-4839-ac12-ee411fa53728)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 033a4a8b-547f-4d58-bac9-d3181075335f)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# Hazel Language Quick Reference #\n\n\
                 # Empty holes stand for missing expressions, patterns, or \
                 types #\n\
                 let empty_hole =   in\n\n\
                 # Integers #\n\
                 let int_lits : Int = 1 in\n\
                 let negation = -1 in\n\
                 let arithmetic = 1*2 + 8/4 in\n\
                 let int_comparison = (10 == 10, 1 < 2, 2 <= 3, 3 > 2, 2 >= 1) \
                 in\n\n\
                 # Floating Point Numbers #\n\
                 let float_lits : Float = 1.5 in\n\
                 let float_artih = 1. *. 2. +. 8. /. 4. in\n\
                 let float_comparison = (10. ==. 10., 1. <. 2., 2. <=. 3., 3. \
                 >. 2., 2. >=. 1.) in\n\n\
                 # Booleans #\n\
                 let booleans : (Bool, Bool) = (true, false) in\n\
                 let conditionals =\n\
                 let (x, y) = (2 + 2, 3 + 3) in\n\
                 if y > x then 1    \n\
                 else 2    \n\
                 in\n\n\
                 # Tuples #\n\
                 let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, 3)) \
                 in\n\
                 let (a, b, (c, d)) = tuples in\n\n\
                 # Functions #\n\
                 let y : (Int, Int, Int) -> Int =\n\
                 fun (m, x, b) -> m * x + b    \n\
                 in\n\n\
                 # Recursive Functions (arrow type annotation required) #\n\
                 let double_recursively : Int -> Int =\n\
                 fun n ->\n\
                 if n == 0 then 0    \n\
                 else double_recursively(n - 1) + 2    \n\
                 in\n\n\
                 # Lists #\n\
                 let empty_list : [Int] = [] in\n\
                 let non_empty_list : [Int] = 1::2::3::[] in\n\
                 let list_literals : [Int] = [1, 2, 3] in\n\
                 let length : [Int] -> Int =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => 0\n\
                 | hd::tl => 1 + length(tl)    \n\
                 end    \n\
                 in\n\
                 let has_at_least_two_elements : [Int] -> Bool =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => false\n\
                 | hd::[] => false\n\
                 | a::b::[] => true    \n\
                 end    \n\
                 in\n\n\
                 # Strings #\n\
                 let string_lits = \"Hello, world!\" in \n\
                 let string_equality = string_lits $== \"Hello, world!\" in \n\n\
                 # Non-empty holes are the red dotted boxes around errors #\n\
                 # (you can still run programs with non-empty holes) #\n\
                 let non_empty_hole : Int = true in \n\n\
                 2 + 2\n";
            } );
          ( "ADT Statics",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                 0a2cfbd0-ceae-4a60-9249-a1bd32d27745)(content(Comment\"# \
                 Internal Regression Tests: ADT Statics #\"))))(Secondary((id \
                 1a027874-d464-49f7-beb1-a5522b9dbf6d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 14faa5f3-7d7d-4edd-a7d5-c4dda68d697e)(content(Comment\"# All \
                 commented lines should show errors as described \
                 #\"))))(Secondary((id \
                 b7499e4f-8dd9-4560-9a49-4a8c9f0f9a00)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6a64fb29-be89-4b28-aa7e-c9c045244f50)(content(Comment\"# No \
                 other lines should show errors #\"))))(Secondary((id \
                 18862cdc-67a1-4c97-820c-8621cb3ebc3b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 643d17e9-1771-4b08-9d6f-1192728b64d9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dbd3b3c9-13cb-453c-b801-800a574800c2)(content(Comment\"#type \
                 definitions: no errors#\"))))(Secondary((id \
                 f158f488-cb07-430a-b851-20c8d2435c3b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3817184d-4d07-43cc-b9f6-4590d575c445)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 86bc7b51-10a1-4f92-b128-2c2302e4e4c8)(shape \
                 Convex)))(Secondary((id \
                 bc213dac-2206-4841-9e77-87fd40ea074c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ded4fdb4-63dc-4f9c-aca9-031f310b755e)(content(Whitespace\" \
                 \")))))((Grout((id \
                 2836f91f-54a5-4d36-8b39-1e709d65949c)(shape \
                 Convex)))(Secondary((id \
                 5b28f92d-2a2e-489a-bfa1-3219a9c2d284)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 46a40c9a-257a-4cc5-a723-61e4460f9a1a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 172c1064-382e-4697-ae86-8503a1973b96)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c9d7d313-6c78-48c6-b0bc-f099aca348e7)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 732f0794-2859-4ee4-b4a7-efc38c3df804)(content(Whitespace\" \
                 \"))))(Tile((id \
                 528c1933-3f41-4412-b7c6-84acc733156d)(label(SingleNull))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 f8d65b39-b4e6-4674-9386-cb4ff3080701)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 163daaca-5b29-4117-9e6a-de6572e55dce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42c2b11c-7567-483a-94ca-b2fc202d6bc9)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 30df380b-d10c-4e07-ae9e-4218f8e837fb)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7c82dec6-0e86-476f-a65c-3521a459f005)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5f1b4d6e-7437-49bf-babc-ce20f3558790)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dfd490ce-5fd5-48df-b726-29db6843bb14)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 18c00b59-0a17-4f83-ae03-9efb234e508f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12b0421f-3d47-402b-a658-f42a51002246)(label(Single))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 7debfdc8-a2b1-4f71-846b-3312a9663cdd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5e81d3b6-c5fa-4c37-aabc-89e9b6b3ea0e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 099fcad4-d6b3-4d2f-b7cc-26d5e0570879)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 72588348-4d56-475d-bec2-82ce7215ef00)(label(F))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0140fd13-fb6e-4e3b-8586-bc39525dedde)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 ae890d18-ef23-4511-b14e-3cd32d22cb69)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 55693684-554f-41b8-8ebe-c3c07264097b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 97910d1e-ca5f-469d-9e26-a0131e426a7f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e04cde16-16b1-4a08-a1f3-f33c65594ff0)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 505511d2-233b-4663-9ecb-20949b4817a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d53813d3-6dd2-42f9-a66a-3b43402b03be)(label(GoodSum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 dcd1f82f-5245-4186-bd6b-85de734f92ec)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d2b951dd-f648-44ab-b556-e045bbd653a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62ab30e0-83a7-4a81-bb17-05baa331edeb)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 77cdae5c-b13c-430f-84b7-1c28da089bef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 902d2bb4-14a6-4bd5-9424-9cf9ceecdc68)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 65ddf64d-1bc6-4e34-bc1a-5707f541e22e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0875d4ca-8953-4033-b110-74b6c87d665d)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 31887ab7-8f42-4958-8324-1d913009c7ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f06b7f7a-2c6b-4868-8a51-69a3af366cec)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7f593ce2-48a3-4041-9235-813469c8958d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c16ba57c-17b7-4e01-97b2-507245f670f8)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 1a17ea87-d03f-4912-bb51-ff72e32bfd64)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 55688115-df0f-4135-a16b-49951d1a81e5)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 f075ed9c-3206-4154-b919-734413c2a41c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d6fec68b-c973-4c1d-95ae-271203732293)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5c2fda60-d713-4f7a-8ed4-7481499a0597)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ba24a226-37be-4baa-97e7-e708d554daec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9da5987-3917-4c21-baa9-be86c5efb76f)(label(Partial))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 5846580c-f9f1-4336-bb20-410070803859)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9dc72e9c-3597-4f33-961d-b8d115eba763)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e3f4ebe5-7b05-4fc8-b13c-8ea175a2c633)(label(Ok))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 24c95889-dd05-4881-94fe-a8ed439292a4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 707954a8-c0c5-4dae-99a7-4638f8121816)(shape \
                 Convex))))))))(Secondary((id \
                 88ab8639-bcc0-4ddc-9aea-b2d14209b56d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd7ef0fd-78e1-4f22-8855-b91730173abd)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 5681b6d8-16e3-4bc8-aa8c-11c5433f6e6c)(shape \
                 Convex)))(Secondary((id \
                 3d2989cb-ffd7-4126-a17f-a0a850ddf97b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bf6c18d8-3049-4bb8-a296-1d18cacbe2b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c3fbea52-aa80-4663-a6b8-a77c851f4069)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3c0c5c28-b887-47cc-905f-2262ff3f627d)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0d4d7e8e-e5a8-4697-ae6e-9b164b414ce7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 670ac639-fe15-4220-acaa-51b4b75e137f)(label(DoubleAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 ca1cc377-120e-4f46-ab95-cdcbf38fe0b9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bbea1963-4514-46f2-8db8-07b737e9dddb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a0ebb12-8058-4057-85ba-dfe0a8956445)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 cf0c3671-b2ec-4746-b578-0099bfd3a1fa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9da1a7b3-282d-4dcf-85d8-134b57cea574)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fd92be3c-52f8-4073-a55d-4e72b36df944)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a635ced3-6f12-4f0c-b7ef-b04e5831f647)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97ba7624-76c1-4540-a334-7e15029583ea)(label(VerticalLeading))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 e7a25a6e-c6cb-45c4-99f9-843b9025774f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ee7e102d-61cf-464e-95fc-1ddccc38154c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 df026f33-d3ff-4a23-a9fb-df85c74123f8)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 925dc165-0ad2-4858-93e7-c0f75440693f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57f8b3e6-d931-4cf5-b174-aed62f8c074f)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ec05a7d8-cd11-444a-bf32-3612fc7181d9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 abcf0047-e988-494d-9868-be8b76c5ff2d)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 27844537-2848-4160-afc2-9e5dec29259f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a9e4e51-019f-418a-89e2-6d61a2cdd6df)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7cfe6076-fee3-44e8-87d4-2ff90b41b42b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 761ee324-5946-4e4f-a6df-de8d48c680a0)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a70d1ac7-1194-4ca9-9099-d505827d93fe)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b0f247b-4a4d-45a1-98e8-71e19d7655f3)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 74f5d6d4-2f17-432b-a3e7-c7331df66e47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d212423-8a97-4a0f-83d1-5bd59dec77d1)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 48d5a3e3-7727-4b00-a7e0-f892bc245101)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e5ecf194-f65f-46c3-a573-19942c493708)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 58642580-a3f3-4914-b360-6f18d96958dc)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 2cadae80-d73a-414e-96f6-b1e396038634)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 19744b38-3dc6-4d76-9128-03b9d237c5c0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d53080fb-8a74-429b-9525-f17cdcc3ac7d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 971fdbec-aa73-4da9-8973-927153bfedf1)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 187155b0-01a8-41ad-97dd-fbefa0a4b9fc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8b3d43cf-9038-4057-bdca-5e11c17d097e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f003836e-6790-4522-8687-70775fcb58e7)(content(Comment\"#incorrect \
                 or incomplete type definitions#\"))))(Secondary((id \
                 f0770405-cea6-4ffc-b2e6-70ab972c4129)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a4da001a-5de9-4b28-8ffc-1fdd0a0d4d9b)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 681a3291-af1d-47f5-8e43-63b9cff17ef2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 541dafa5-33fe-484a-84d2-cd1bd4a4d1c1)(label(badTypeName))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a953fe39-e63a-4afa-9032-2d2c4a76311b)(content(Whitespace\" \
                 \")))))((Grout((id \
                 1da5e6c3-0e12-443a-8429-ecbc966d3bbd)(shape \
                 Convex)))(Secondary((id \
                 cc11f941-395b-4c6d-a78f-287527917673)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4ba42149-80a0-4dc1-83c1-53f713adf0c8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d8812816-f1c6-4aa5-95fc-15bd42b32100)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7392e0cc-9e62-4b73-8feb-b8dbce5df63a)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 be155b82-7b03-45dc-b040-32e68c86d21e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 25237687-4a79-411a-96f1-00aacea6278f)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a9437c7f-8a16-4a03-a5fa-4a1040899947)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5e0483b4-8e42-48a3-9c3d-7e9660bbc036)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 99042e15-6cbc-44b2-8983-3bdb443cfafc)(shape \
                 Convex)))(Tile((id \
                 07037f5e-dfa0-4356-8a9a-ca13ee396fd5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 cfd66f65-899d-4600-bf44-6629eecedd79)(shape \
                 Convex)))(Secondary((id \
                 823ba5ef-b7d1-45bf-896a-00c6eab273ea)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 53878070-b875-4bf4-a45a-09f9efd752a3)(content(Whitespace\" \
                 \")))))((Grout((id \
                 e382a31b-573a-4e8d-9dea-ca394bfb21f6)(shape \
                 Convex)))(Secondary((id \
                 3fef3104-a2a3-441b-b165-7ce5261d8009)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b6fc3a3a-8b05-4236-860c-0b296ec59415)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8358a09a-b703-48b5-9017-f4fe926ce72f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ce4fbad2-92ff-4cc9-8498-148acca711e0)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 8b5ffb70-adfa-4081-a345-039c24096d72)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b8f2ff5a-d482-42d5-84dd-6b573edfa1e7)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 91b76664-3d6e-4b85-bedd-6dbaa4620083)(shape \
                 Convex)))(Secondary((id \
                 6340592b-192f-4705-9bba-82a9ecc7bb0a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 33a6f560-13d9-40fa-859e-19f48f36bf7f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e6ff457a-4df4-494f-8427-69f49c075cd3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf305c4e-a79c-44ab-850c-60b7130d7380)(label(badTypeToken))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0e650ff4-1499-405b-88e9-05b37be7533a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 06a42a0b-5977-4b6e-b14b-9c64cc29af2e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 466c37ee-f1ee-428f-a1dd-f4540f40fe0f)(content(Comment\"#err: \
                 invalid type token#\"))))(Secondary((id \
                 e5a612e7-c153-4a18-834e-2fa2ce2ff710)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cdc08345-78e9-410e-b393-cdead52eaa52)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3a0e3abc-b7e6-4eba-9331-09740ad53d16)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbd7f3fa-4a4f-4b6f-8c1c-c381f66e936a)(label(NotASum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 7ce0a1f4-3ea4-4a26-8e4e-c77cb90032da)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dbc700c2-539b-49a2-99fa-92361ea6de06)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e11ccccf-9d32-474d-9edb-898f778c4732)(label(NotInSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 563e653b-0e11-4a25-9655-051bbac821fc)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3db9f6f7-e1ea-4163-9859-1ea6ba0aaa9f)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1ec765b2-a18a-4b6f-a7bb-7d6e101178b5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7d0ec78a-bddc-4f2a-807c-fface1b617e4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f9beec36-38e2-4a8c-977c-5bc36b94f2ff)(content(Comment\"#err: \
                 cons not in sum#\"))))(Secondary((id \
                 64d657f4-e6d2-4936-abf1-70074cb9210f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 31097c03-65c2-4603-86da-08aae1b07982)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 18bb0a69-07d6-4d82-b621-17f446ba1901)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cedfca6f-9158-4734-bfbf-fb014ad52f3c)(label(Bool))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 b381f0c4-5cb8-43bb-a2c1-dcb528297acd)(content(Whitespace\" \
                 \")))))((Grout((id \
                 bcab01a8-04fb-4e6d-a3eb-74e78cefbc3f)(shape \
                 Convex)))(Secondary((id \
                 2c77cafd-d6a5-469d-8d7e-4115212fe314)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 63cce14f-4f80-467e-a883-aed8ce4c686c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8e59b2e7-dacd-4d45-9a2c-3b8112aeb593)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c68131db-8437-455a-b953-22ff6bc1cb2f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f16fe49f-bb0e-426d-a45e-a871f66f3e78)(content(Comment\"#err: \
                 shadows base type#\"))))(Secondary((id \
                 a39eb4b5-efec-4472-91e4-89667ec932f4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ed3bf294-9b38-4aec-b7f3-fd695c2bae5f)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c844da44-d640-41d7-8907-f588fffc97fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 414d26ef-0811-499a-9f17-924f0369cb95)(label(Dupes))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 ba543e00-cad1-46b0-9031-7e2f2d1be7c8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 379d8150-82e7-4de6-bd83-c1dd6971e60b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 795298e5-c032-4b04-b1b3-510d26bbf09e)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2200c407-21bc-42ad-87ca-49040a428fe7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8f57d72-248d-43c1-a036-84a506481a5c)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0832296c-8220-4c4f-b8ac-0f60d03babad)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 273d3a55-4622-49c4-9454-3f7420c1506b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 476fd237-edd8-4bfe-96c4-a640d60857ee)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 49418f67-59e4-4ae2-b01b-f6388d8a8ec4)(content(Comment\"#no \
                 err#\"))))(Secondary((id \
                 ec4eacf1-9cc7-4ec9-86b3-6b85c8294bbb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4b7105b0-9cfe-4169-a326-1bce1a4e74ee)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 84c637ca-58eb-4254-8123-98f647a19dd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b3d857b-e0d6-4d88-ae55-2cfba4ad533b)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 b531c7ce-42d5-4e1c-b764-23f087f3eb13)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 69649ed8-bed1-48d7-bb8e-04cf63d3a5f0)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 ad554137-e6bc-4915-8947-33f1cb49bd20)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a8035ecf-97f7-47dd-a9ce-b1942a0ac3b5)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 13772556-b17d-463f-a687-325eff007c02)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3a74d7d0-4d66-43a0-9e03-c29508543ab5)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a17210a3-c6ed-4594-b0a2-cea9270b05bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44bacc03-f51f-4dfb-8baf-b03973918357)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f7dacdae-5416-448c-9927-1b3365e1a665)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 452a732d-bbec-41b6-8adf-1ebba51757ae)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 291b30c5-aedc-42c3-9d02-ca92de196aae)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 756de824-4dab-4501-9b92-2396db0f8748)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9357d240-dfa3-4dd4-a79a-672ab2efe0f4)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c4711826-ee30-408d-8d63-c362e624eb92)(content(Whitespace\" \
                 \"))))(Tile((id \
                 78b138f9-1bee-4923-95cd-f5e9f2a7b11f)(label(BadCons))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 93a061f1-3e00-4a62-8c62-d434044344f1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dd8055a9-9f8e-400e-b0c2-8daa8c9390c4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d7e9530d-f251-4009-8c6c-37cd47f4c820)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6c01fc09-bd9f-4a0a-b38f-49e72ce424dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddcab784-b742-4fc9-95f2-578bd941a343)(label(Um))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 de7615b5-e62d-4744-910c-dc6c0313d39f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 ace843ec-6de2-4ff3-891f-c61b7b884ea3)(label(Unbound))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 64dd0c6b-2226-47bc-ae35-b7aac0a8433f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1128e9c4-f5ea-475b-aad3-5e43572b4957)(content(Comment\"#err: \
                 unbound type var#\"))))(Secondary((id \
                 8b81ef9a-d277-4c46-8fa8-a42651a659e9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7ac1487b-7f65-44bc-ba4d-2138943babf0)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 22eb624a-b0a8-49ab-8c36-3b0c9a0425d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2e4c3bb-e4f8-4f4a-98b6-0593c3e2555b)(label(notvalid))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5ee48646-80e7-4ebe-8a45-190fb1eb31b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2401d396-cb16-431f-b411-52e73b64976f)(content(Comment\"#err: \
                 invalid#\"))))(Secondary((id \
                 765ce0ec-9953-40e9-a292-db123363c3e1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 12e4a456-fc12-46bb-b40b-7ac0a4ccbd98)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9b477fe7-d328-4aed-aaeb-0a9d1d4bba62)(content(Whitespace\" \
                 \"))))(Tile((id \
                 196c152d-0575-4914-ad23-b0a16214afa0)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 947fdd9c-9068-40a3-9a86-860f7afd4cf8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a299e101-0c4e-472d-a174-715fcaf3f50d)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 9c8a028d-9595-4050-9b41-1ddcf1e693b8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9d2d4385-d94c-45c7-ae1d-9abee4ec614d)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ec1a5ccf-ce66-4470-b3c1-5a6974e82b71)(content(Whitespace\" \
                 \"))))(Tile((id \
                 996b8c03-b07e-4adc-9ecb-d6826da9c6e4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a9bd78f2-b650-4b0a-b9e4-7cd7a03d6f00)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 c713d593-53c9-4779-99fd-b7a8afc0cfbc)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 73bb4056-9e42-43bf-b870-460340f27c82)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a3510302-d549-4656-bef3-7ffa9983802f)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 7d6451c8-ae9a-4614-9612-5fb3e2f72371)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7c2280ca-9271-41b3-bbc4-92e229c23b5f)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e81e9707-edba-44e9-a700-8af5bb075b6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 03a98856-d2f0-4d03-9d8e-837f2ca17f4e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 43d69658-1af3-429c-a8a0-b4327f22d7d7)(shape \
                 Convex))))))))(Tile((id \
                 ad0f3fe1-db8d-4159-b7fc-59794f970d54)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bcf65794-66be-4c88-af8e-51e2366b7bad)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 830a7e66-8a00-4116-b98d-6a24f7c3afb4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f3509a66-48aa-476f-adc2-dde106b5cc53)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 3a13e30b-f61b-46e7-9466-501ab1e9a10a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9a15bd55-359e-4751-8c73-95a2dabaae21)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a38eb774-2dd7-49da-9471-59a1384a1792)(content(Whitespace\" \
                 \"))))(Tile((id \
                 61811b8b-be76-44c6-8e8e-176c95b3b8fe)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 38b825db-2bdc-440a-8375-05d0391a84eb)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 751c9762-1558-4a63-a619-19b3a32847dc)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 6f310bab-b849-4dba-bb72-b5e9f3e5ba99)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bd125aba-fc06-4836-8963-45e0e8b72d54)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 6768456f-4476-499b-95a9-37909db77edd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0f24760f-eebc-4e4e-8b1b-06f3d1efb50e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4bc9df90-322e-432b-8481-acdc83483ab5)(content(Comment\"#err: \
                 expected cons found app#\"))))(Secondary((id \
                 0dd1bbe8-c332-4537-a3d3-e6160f734006)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c68d97c8-c916-4b57-97e7-514675d48509)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7c74fd72-7251-4808-9a10-f6a1bcde1a1f)(content(Comment\"#sums \
                 in compound aliases dont add ctrs to \
                 scope#\"))))(Secondary((id \
                 cf2db721-5739-4d9b-a353-86e8eb80247c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b24736eb-4f4e-4d3d-9f21-25faa57d5aa4)(content(Comment\"#but \
                 compound alias types should propagate \
                 analytically#\"))))(Secondary((id \
                 2ff5b294-23cf-4957-aca8-a3a75f77e3e0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c928e5fb-87de-43e7-981b-6feaba6be8f6)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 30e98407-9e58-4e2b-8d74-3469d1a89228)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6977dcb-4e22-4407-b2ec-ff7e3802171b)(label(CompoundAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 fe0537e4-19c9-4bd7-bb06-bad8b365dbca)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8ab64af5-f03f-409b-9350-880e41ad1db6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74e7e36a-4630-4252-a6a3-55ba3e5cde94)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 26aed166-cb1e-4a83-b560-e340df8b8f44)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 820fefd0-6921-4c13-80e6-39fbaff7c487)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c2c19d29-904e-4ac1-b2c1-4c6634945bcd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3078289-1dda-44a1-8db3-d8a4a1083b31)(label(Anonymous))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 fe178593-f15d-4c5a-b366-e9e42e8b5ffc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4b692c1-10f9-4841-92e9-b252cb5a17b3)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1a14e88f-3ef9-4671-9e30-4fd4126671f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 369707ed-3122-41fa-b194-54ab47d8fb38)(label(Sum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 aa1578e1-86cc-4391-8300-2fa53c95a019)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ac30d698-768a-447c-a856-1ef559e612b0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e0ea6404-1bab-4eaf-a1b0-4ff230562c83)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 671fac03-a203-43b9-87bb-89737190bebc)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 df05d1e9-ec82-4e89-9324-59d5ddd207f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63ddef1d-2d98-4d75-9c85-b84d96af6e9c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 bf5b2772-7d0b-4c6d-8975-f8ebedfc7c33)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 935e3e41-5ccf-4c0a-9f4f-27879ca056d5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33f9d2b2-3e4d-4953-8862-9ebb391b4cd3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 88f400a4-d504-4568-b983-5823106f2d53)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 04faafcc-155c-4cc5-a96b-a645c98fc483)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cdf8fd3f-5846-4134-aabe-6641a999b810)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb57dbd6-0c92-4220-9f38-9e0a4b01464f)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 e6d6f47e-e930-4e4d-9a1b-c8bedbfe231f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3600c6bc-c14a-4d29-821e-4c399091ec69)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c1a5be33-4280-43ab-a097-b7b7e5b7cfdd)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 d76a29a3-f18c-4b5e-b9ce-d2bf6ee8141e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3ee58e1f-2c5b-449e-b524-540edcdabd26)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b6cd9bed-2c6f-4d30-af89-5f453377e286)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b85fd29e-4ac7-47d1-93f2-75669a6624b5)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e2b093a9-5370-4b94-9b3a-73a22e474c80)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6f2fea32-3555-4df7-8428-da205add1d1a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b53ff04-f58f-4046-bb6c-277574e1f640)(label(CompoundAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d25e0bac-8849-48f2-8691-a8d85a576d01)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a5c4e307-1180-4e16-90c3-db7617923998)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b11c4f9-d09d-4259-a452-f8b675bfc16e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 31cd551d-3702-496c-84b6-e9f387b1f533)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 42aaa81e-02d2-441e-9885-762a6cc5c5b3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 20e387f8-da77-41e6-932f-8fe3fa78f220)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8634b86f-f819-4199-afce-68d39a1c1f5c)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3a7576c3-ee5d-40be-b5be-c0ecbc9fdbec)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6f5dd058-32cc-45b3-997c-cc0696e8df6f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 024d535e-77d5-4d71-98e9-30e310bd72c2)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 861c5638-8d5e-42a0-b5ca-1ad1c56a9052)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 553755c4-6ba2-457c-8709-5d342360b182)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9b61f1e3-9a8f-43aa-9223-11275836a334)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b71b985f-5a15-4c3b-bd66-fb25d1a003d7)(label(Yorp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 2d445250-57f7-4c5b-bed4-d078ded97f4d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a3b62c46-d02e-42ae-b9ed-57a3721565f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 173eeddb-9f4a-4345-b257-83d5dd3b596d)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 80fdfb0a-7689-4f14-af3a-93da645c5964)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ffc0918-4fc4-4396-bc72-8e5df1e40ff1)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 fbff5e35-5ba8-4654-a41f-7d2050a346b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ae971ea-fb56-402d-84d9-299763ad1396)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bf0360e7-53c4-418d-9b2c-757f1cb88c56)(label(Inside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5e779d68-d00d-4f34-a597-cac950bd6cd8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e53c9be4-27a6-4bb3-b5c3-bfca33b5bbe5)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 693ed14c-b695-4fe7-9a6d-2d0b1b80a1fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cc236ed1-0a27-422a-b313-4377eef63f0b)(label(Ouside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 ebb43e7a-bb6d-4716-8a17-3784be38d895)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9ebb33ad-0b98-4aa0-a6dd-9dc3ad09c566)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f37fdc19-a8f7-4ca3-a6ec-5c8bd0957ada)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e1c3a338-2d99-4cfb-8abe-4d883e1d882e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94a73075-41ad-4077-aef0-68b6dee18151)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 85e7787e-1280-4e35-b9e6-d8fa5f83fbfd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 09b2b746-dc75-41f3-a720-4b3853930198)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ee66c39-9c29-467c-86e1-8d27a7aa8a91)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ca8b05b4-f36d-43e9-bad6-a28edbc5c1c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 791df273-15c9-4d3a-b362-4456e9e788cd)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 410bbbea-0d5d-49a2-b60e-e0d0252f2ccc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 11458dd6-7916-42dc-a433-ea40672aca83)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a176d01-427d-4d04-beec-4ff2a1b9c6fa)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7c56186b-f575-4f3d-abc7-a162cac7daf3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dc158eeb-aa2c-4970-a622-3b1a2ab5df73)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3a2f778a-e7ec-40af-a9ff-df68807cf064)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 4b7013f7-e59b-401b-be9e-99fe9efe362b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0a71cb2c-af01-4462-b627-df56a839a254)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fdb8c5ed-586c-4c8a-a2b9-043ac41ea38e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d7928910-2b0b-4ccb-b9a7-0131f4767d69)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3c42bd3c-0363-46f6-9697-2b516d59cde1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 22c5392f-f9d3-45a9-89a9-77eb0c1ea513)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ac6501a-1526-4e6b-b297-8d39b4353998)(label(Yorp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 34fed20b-e588-40ce-a636-ce65284a89b7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5e028145-255b-4b48-b466-55449ed3c1da)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10de9d49-eb8e-4a36-9fc3-3c3552af00eb)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9877a431-d589-4a05-b0a7-48d81e93ba87)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7f04519-15b3-4d18-890d-0a91da825fa6)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5deffd28-0cd2-4d37-83fe-d7e8ff6067ad)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 05d5245e-0424-43b9-9c3a-4fbbe7a41108)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3a7c6be-7e1e-498d-ad49-b7da6ed63257)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e4bd7eec-6b19-4a18-87ba-c4c4769c456e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8d1ba844-89dc-4164-b8f8-4fc6b8b64b04)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5f45f4a4-c7c4-4164-a40a-fb04bde0d209)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 4028987c-2593-40b2-8067-3878039788d4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 552e6279-7e99-4375-b084-5e41b56ddac8)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c30ca713-a8f4-414f-b40a-a72cad8a03be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25ea3bcc-800a-4980-9991-7f67be7fdef8)(label(Gargs))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 6cd16180-d632-483f-99a9-c7c8286791a9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d5baf065-b036-458d-bee9-b1d46c6185c3)(content(Whitespace\" \
                 \"))))(Tile((id 36abf667-ef3f-48b7-ae85-0056ddf25bda)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 67cc6f6c-a471-4b4b-9be4-4bd457718b6b)(label(BigGuy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d8efdf7d-36d6-4a89-ad04-51bd79021653)(content(Whitespace\" \
                 \"))))(Tile((id \
                 90053e87-4ced-4baa-95da-9b3d096b0d8b)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 141db156-e1ed-41d0-a359-c3872a565198)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e40da821-5516-42dc-a743-015bee0dc277)(label(Small))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 553cd467-f259-48e0-8502-87be1a81f5c0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 aba3eedb-400a-46e9-80d3-4b8e173848b3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9ff6e38d-6a7a-4046-83be-f0624751ccba)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e92ccbe6-6851-4a6e-bca3-11f4d00ec004)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7dd503a1-4c6e-40e1-a899-8cd00289f6f6)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1e63e569-e13d-4893-b7b5-8b137da8b360)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6d706a47-82c6-49c0-9c61-1c5dfb87388f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65fbaeb9-f9e8-4ca8-b9c6-6636a231b347)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9c1fa54f-c41f-49f5-bbb9-e1c5e8b8f658)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d06f8a15-822d-4475-b5a3-858ee647df78)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a28c6df2-1246-4459-a1b7-d88f4e3be387)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 e9936938-7c2e-4f1d-bc74-173333179dea)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 653a585b-e872-4567-a682-57252af08ade)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 aeaeaf83-bdf3-4ad4-8c5b-6505963b9ca4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35d5a654-d00b-4eca-a8c1-2426c75fd66c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9a327aaa-0cde-4441-a510-6fa274b4545b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cddd7c76-8f42-433f-95a3-b3d49d7272f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c09ab6b-6962-4f71-a9e1-8d2d3b1c27f8)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f7a34cee-d197-4019-8f14-faa82b23b6df)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 17e3abfa-4faf-424f-86dd-af210be4635d)(content(Whitespace\" \
                 \"))))(Tile((id 2ad70abd-7fd1-44b2-802e-52c468756d55)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 e2c08c77-948d-4caa-965a-2f080b2ba972)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 dbefedc4-af61-44be-9a21-fd071cf772d8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 efc19bb4-19c7-42a7-8031-376c10367974)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 37893357-5875-45b9-b208-1953ad13e2d8)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 5316f971-e07b-4be7-b55e-af9e55a7b9d3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fd466e18-2e98-4dc3-92dd-66f4a5529e3d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 89ae37a2-ac4e-43e0-a5bd-598606803ef1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e7f26be-af14-4d8e-aca0-b6e29edbc1fb)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b71797e8-5198-434b-97c7-2a06a80ce311)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c35f2e08-f36d-4db8-bbae-d51465600696)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f7cc6f75-6195-4c66-b95a-28c482fa7652)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3ef056d7-9981-42c4-8575-96433f583d54)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a29ecfc0-ddeb-4a66-87f2-5f4def128a71)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25a646dc-7013-43f6-9298-7169b4cffc78)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c3933ed3-2bd9-43e0-bda5-8fbc4d443f1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d4bed0db-531b-46ba-80d5-79b56968ba90)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cd3de317-c545-4630-aabe-8ef043ac1b66)(content(Whitespace\" \
                 \"))))(Tile((id ffd94e50-9434-4d18-ae7d-11b7285673b0)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 ad92c23e-a53f-448a-916f-b7279a28f6e5)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 fd624746-9f42-463e-b7f7-7fb38503bb43)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 59ff8a3c-3ab0-4a1d-8773-cc7299e114a1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 227df70d-8101-4616-b502-754bab3eb399)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 cebe11e7-5c5e-4cdc-8c80-07a8bd1722a1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 46c967f8-7df2-46b2-bc45-a39e3dd7a979)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c079c78c-9770-4e3d-817a-ca388eb7c852)(content(Comment\"#unbound \
                 tyvars treated as unknown-typehole#\"))))(Secondary((id \
                 732f3e22-9c9e-4e7f-a3bf-75e0694c7a88)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3a7516d4-e244-473a-a4f0-83b71b935e29)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 06d71d8e-ae3e-44c5-9e1b-11dcdd5d24b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9ab2ab6-6191-4b45-ba89-eb5884f78844)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4ef82b4f-c1bb-415c-a2b1-3837838dab2b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3fbda5f0-a605-421c-b3dd-5a7ad6f04473)(label(Bad))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0edd4a47-1ef3-40f1-917c-209af17da53e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e515b520-9827-45dd-82d5-f2c7756b130c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32392857-fc14-4367-9d82-9a24a449a2d9)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8b633a56-6b2c-44db-963d-bfa2205be561)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 93a2493c-aabb-4e03-b6f2-3fc6de3366a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b78078d3-c972-4bcf-a357-11cd62df45b9)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6f3637b7-6b09-4694-af83-9e9ccd4be529)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df3c006a-c1de-45e7-b45a-24c962dc937e)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4b7f867f-64a1-4a07-86a6-b99c0e36cb78)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c7652a70-7d15-4d4e-bcff-cf1495631692)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0fb8b463-a142-4bcd-bb4f-30dca755da1c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 638429f3-b1d1-4467-b9ec-b844bd8a7afc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d8ea9110-d5bb-4c8e-b424-d68528969228)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 4b3795ba-14c0-44ad-b582-d03033d6e401)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dae5ef06-ac17-4a58-89a7-f0695bcaa00c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 ff04db23-2e48-4af8-bddc-4465deee9a56)(content(Comment\"#non-sum-types \
                 cant be recursive#\"))))(Secondary((id \
                 8874c10a-2d08-4701-98d8-9a0ed5cc7a59)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9ef317bb-502b-47ed-9edb-29c64041cfee)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 24840b8d-4e24-4da5-9d3e-891adb4d9f82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3bb1a772-af17-4283-a465-f150029430df)(label(Lol))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 70b64809-db4e-45c4-ab72-19b29e595ac6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b83153d3-884b-4b8a-bf9f-25fc197f5755)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32903664-5b39-4626-8ed7-c7b46aafcdde)(label(Lol))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 50457b72-5db0-47f9-8584-1d87a130141f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d01885f0-47fd-4956-9ce0-e17673eed65e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 88ddb85f-fcfe-4589-9f09-e12221adba65)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 c0420e67-c9a6-4207-8f03-bd02c3255a1b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1452bdea-67b3-4b14-bdb3-044f29dd5c17)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5a1c7fff-3100-4b40-817b-bcd5ef955899)(content(Comment\"#no \
                 errors: analytic shadowing#\"))))(Secondary((id \
                 2cd89542-122a-4e3c-8f7d-6eade34223fb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 266e452a-dd0a-4615-98c9-b9257e24283c)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5e2921d3-6fbc-488e-aefc-8041d7b43e1c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1e9691e-3e45-40cd-a1a5-271dd5221316)(label(Tork1))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 0bebcae6-b971-4cf9-8af1-e3896235d076)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 00c8c75b-174f-4df8-a828-a11ba8a30e17)(content(Whitespace\" \
                 \"))))(Tile((id \
                 daff4ddd-0848-442c-b390-39be51b20b72)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 739cf5ea-a905-4c67-8bd8-b7f1273a72cb)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b4781a2c-708f-474b-a58a-8d1903174185)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 380e4cb2-1256-4852-b038-5e32670374c1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 77306500-800a-45bb-872d-a3dcf092a40f)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 207a361d-788d-4eb6-8f26-a1cea9e7de66)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66da081d-9c0a-4f90-9e11-5ec5f53f96f9)(label(Tork2))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 e65236de-7b24-4554-96cb-5ab92189d7b1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a546ad8e-bebb-4115-872e-89c0975ace14)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6660b87a-ee7f-4bbf-808f-b57dbe4d8f01)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6c226e82-0253-43c4-888b-8673fe548228)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4d7af473-7b43-4481-8986-82d36f5227cf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 08c72078-337a-4c3d-bd66-4a53c7c93e27)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0340f764-1884-4b95-b208-17dfc959af0d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e9623e56-8015-4566-b117-3a52065bff4f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0d1d0dc2-6b65-4086-9c35-83a2b4f22615)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f93df2bb-a7ff-482c-baa9-cefea8351b70)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a80210e0-bbb9-4f93-9ad7-ec5043391b60)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 e5abe616-54f2-44f9-bcb1-c875d0370de7)(label(Tork1))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1f39c3b7-add3-495a-8aef-577f50f729e6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0fd45328-b845-457e-972a-a80d8c37ce2d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f5be34a0-1ec0-4743-a9e6-778d7a5cad6b)(label(Blob))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 331cacfe-0f08-4985-b0c2-48e66b2b927b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 30ca5894-a158-409a-a193-50b33ded7e9d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 437231e9-d034-4b0b-a65b-1dc6a9e335c5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6ce7859f-1059-46f2-ad49-11a7fd339778)(content(Comment\"#exp \
                 tests: happy#\"))))(Secondary((id \
                 2d264e12-a5b7-45df-9fbf-c16f83654d64)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc2e734f-4d9d-4ee0-9666-88c5b2421ce6)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7ca8bfed-925b-4628-9461-1074dda5fcf5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f23ecf0a-e26c-4b4c-872f-b8bd32beacba)(label(YoDawg))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 a9d3ad68-f80a-4187-bf18-adf0c705792a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 24b36bdf-e8b7-4a6e-935e-3e01a1a1c807)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 71cc9cd4-7719-4f4e-92e3-b0a2254df8e9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d0873c0-2afb-4651-9b55-21ca6504f26e)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 afa0e149-8b9a-49fa-8644-6ed2aa7db9d9)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 789fdd92-c6d0-4856-8307-7259b19154ce)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4bdbc78f-b1b3-447f-8152-0cd241a29230)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c2ad977-765c-460c-9e22-f4d32992fb8c)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7894d1a8-df4f-414a-9b55-856ce71f5c75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63abe3af-8b21-42a4-a3c1-c32ffb06a8a2)(label(Bo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7ac6e388-3f19-4149-b0ca-a95a168ecb85)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 0478a4e1-77c9-47be-b64a-042ae2831ae2)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 95307a1f-5852-44f4-a99a-810a8a88518b)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9e31ef0a-da64-4e77-80b3-58a5bfa016bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d938f3d6-4f5f-4d2a-acf4-bea6053245d7)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 37b569ce-ae83-47c2-8c01-f790e5ce709c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 c5b0deb0-375b-4cac-b247-c53ff80fcf53)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 b6544f32-e9c9-4809-b834-c1741158cbd4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 994bef13-0405-4dcb-a159-e62f164cf284)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5eb0902a-16b2-4eca-843c-60eb2d7f74ed)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 347d2aec-2c92-44ee-8fbd-dcdf2e3ec0e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3eb840df-b394-47dc-a185-14390bfb25c2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 71a26861-bfdf-43c8-aa33-181ba8bf0da7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 13d4ae1b-f718-41cf-b95c-3c31c8b48aa0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a812cb4f-3d3b-4135-afc1-1231cd943fcc)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e5dd8345-0af4-43f5-a4f7-bddcc2840bce)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f65e5753-74ee-4d3c-b374-089e32fa30d6)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 bd198db2-80d9-451f-a455-511f8ca17f5a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 02b9ae24-f536-43dc-87d6-32af86b56f86)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 158a70cc-904b-4281-bd7d-abbf6b0312cf)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5a00a6b4-a93d-4d77-a9a1-fb4e26c62496)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eeceaacc-97bb-48b7-bda4-f5ee30262079)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 91dbd4b5-2a1d-41f9-97f1-bcfcd7ca7f45)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d3e00f5-8c5f-492e-9639-114f6483b17d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 62751280-5756-4cd6-8d07-dcfa7356d922)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48d29df3-dd46-42ea-a1cf-3dd13106f258)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 db143bca-9f97-4b5e-92b8-9b7a78fad3a4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 02bf6ec9-c08f-4cb1-bf9d-73f382ae49a7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afa99da3-dcf5-4525-a472-76dc8decc082)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0912f991-d0b9-4fb3-ae90-ce4bbbec23bf)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9a942d20-8cb9-426f-bce5-0efc86ffcc47)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3fba3cf8-ffc5-48f5-93e2-41be0b05183d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 197dbe98-214b-4711-9c9c-d97412742a1b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 223847dd-4cf3-4d5a-8d56-ee7eef404bb2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 77024873-8411-427b-9958-aa7e4c727bf8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 add29a84-dac9-4236-9e36-94190884a40a)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 05906666-f159-4493-8d15-8f98fa211cdc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 91818a4f-95af-410d-9328-a7348a7439d2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c8ee7093-d636-4ea1-8eb3-fb8c97d4257a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7811f7d9-65e2-4241-8378-6868bc79b1da)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 42a439f5-8c31-49ae-93ca-47dfa9e82952)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 966bb706-c87e-40cf-a19b-62aa2ff74f42)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3cb61970-a14c-4cae-b04b-50d77131dc4e)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 47fb0cfc-a8df-4450-8419-c16dadbb5954)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fa2bbcd8-3d77-4005-bf53-50809323c8d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02b3eda8-5904-43b6-96d1-cb0e913a9a0f)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bf44e657-a665-43f2-ada5-6f36311f82ee)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cc7f0495-1598-4cd2-986e-472d53034641)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 82fd21a3-6a36-4b93-835f-81763f01362e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 54bea9d1-c71a-407c-bbe1-f4cf5af49757)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f0bcfb14-f9d0-44ac-98b4-7000fa169777)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a719e852-76e2-4781-b267-6239a3f8a406)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cf8c1ed-0845-4296-a365-4d4798332ee8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a11f85a5-52af-4f23-897e-3dc9fdac4f0f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 69674df8-1347-4184-b971-17bc7cede2fe)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7f0b4403-5d0c-4e4e-aa48-fb8bf5661b6a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8fbbfe77-bbae-44e4-a31a-47e7a68ee4fd)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 957abdb8-b618-4ac1-98e2-516b07b54ad6)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0aeb0ffc-211f-4ad1-b730-5b5328627493)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b93a4126-8a46-41e7-a933-4ebdcc429b41)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 36666820-ba96-4829-889b-3081d7f07b64)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0d32ab6-d2af-43e3-9c30-5c5c830a9bcf)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 015e1952-4eb6-4474-b97a-372847cccdde)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 00b9edd2-ce98-48d7-9883-e53cc60604d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f9750be-1952-491c-a7c0-2a1656ac515c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 63498fe1-5c1b-4c9a-abeb-9d7aeaf3f457)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 186c304d-cfc7-43cd-980c-54f54b74b95d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d75099e8-fe99-4220-a8db-5f22e2cc28d1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8490788f-da19-4041-8e1c-556b0fbe96e6)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f47e3e86-5428-4456-a585-f5c1495d7e3c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 e6c7007f-a922-4e8b-9dff-7055a63d2696)(label(5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9873050e-e3a9-41b3-8147-9faee6d10061)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 47effe1d-9619-4837-b32f-61756303789e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f42e0b52-3f9c-4648-ae0f-36eb8d9a995b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 89e19d85-1015-4fee-84cf-f880f9fe1511)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdf1df0c-2337-4d88-9f05-921e06dffd1f)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 35b3787b-8156-463d-bd9d-4d8296b98b7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 08141579-69a2-4d1d-a98b-7595640bae09)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6b946c0d-f75a-4a3f-bd85-c047b0768df7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60795c47-891a-49d5-8bc2-be126fe8f5d9)(label(DoubleAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f480a787-9c85-467b-b778-2d4d7e84464b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 13b15893-2ac4-4f23-b7bd-f7a3f64e6ee6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3ebb8fe-c384-4739-a14c-5fbf949cc5a6)(label(C))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 80fe8e50-5424-4db9-b1c5-126b6f0ed13f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a95d6f2c-60d4-417f-8f56-cbe29c77833b)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 24cde1c9-c78a-4f2d-a80e-73f6a144833f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 12926454-2a10-4c5f-a18a-d45b8d2352cb)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 9b5f9cfc-251f-4344-8453-5cfe446f6d41)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 fbc5a993-907b-4cb9-bb4d-133d941c7b9d)(content(Comment\"#exp \
                 tests: errors#\"))))(Secondary((id \
                 9a9ea58b-d6db-42d6-a79c-3badcc90554d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 141d4ba8-a253-48c1-bda0-4879324c464f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 854c1f6b-fa0f-428c-a68f-610463b5ac6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c26f276e-e008-4fc3-9d6c-653b8ff53f61)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1cf7d106-3949-4c3f-a44a-236792d87e03)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 50974139-ffe4-4ad9-bbeb-c43fb612db7b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8392a44b-7d6b-4963-a473-66caffa0d520)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4c033879-28ab-4d83-be8f-f34a262fba5d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0045364a-ad65-437a-a155-b494b86c12a1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 67e01d9b-fe0e-4d49-a1ee-cc4d85a86f2f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ab0863d2-1847-4f6b-81f9-b073c8da96e6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e50acb75-15b5-4d08-9d9b-d0656e58dca4)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 d7fbc4cd-1dfb-4948-86ae-1d34425ec05a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a3991a0c-051b-4718-9149-ed9d21bcd107)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7122cee5-d397-4482-a865-f7b661c8b93c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60b55916-97b7-4f04-8bff-a26e30f23629)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4b37827e-8aee-416b-98a0-7fd515a4e542)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d86fa410-a0ca-4f53-837d-b4293780fabe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0425570a-9758-41eb-94f3-09314748af7a)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 803597ed-226a-4157-b307-6aa53f81e320)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 60eb0b5b-f404-4a22-93bc-de016cb58ac9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 86e22ebe-3648-4d36-af25-69cf9f0759dd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5a571f8a-0e4b-40ef-99b2-c718e546328b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a3c4cbda-764e-4e41-8bb5-3fa5ce9c0371)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 6ce5a949-33ae-41c3-878f-7a34b8560494)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7e386dd5-d9ec-48b1-8902-8b7900943582)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 18a682ff-d9c3-43da-9e92-7a84f30c250c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b48f9af4-4057-4e50-8a49-f35bd3929d57)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 06ae6ceb-bb2d-40b7-a5c4-e1927c1c9eb9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1e4d6db0-1c56-43f5-af3a-94997c0d2e5c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e27bfc9b-77d9-4ff1-8460-83375d2d7442)(label(B))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 171a962b-29ad-45fc-83ec-bb9b365abd61)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4ccf6dbf-eec4-4e31-8dcf-4fc91fee1f47)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8b4bf5c6-ca26-42bd-af89-b334f5f166e8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4cc63189-75e8-41e4-ba6d-8d7c64747efe)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b3be12b5-d5bb-416b-a249-9859a2f12077)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 860f6ce1-17af-4cd2-95fc-cf556d49f098)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0ef234f2-ab0b-46c1-aa8f-30e3523d0bd8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 304ecd37-bf36-4362-87ab-3e12e002e833)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f23da5b2-10eb-4100-832f-b01be02b0f40)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0e8db152-b534-42ca-87ea-7717b6e74ea8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9830f550-481c-4d86-90ec-4c9c9edb7476)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8a46d398-a9b3-475c-9b93-567338adba60)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0cbca6e4-f30b-4cf1-95c1-cb8ebb447a44)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 cebf40ba-3da2-4532-af1c-368949f4d036)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 3939bdc3-2825-4c59-8ca2-573a286cfaff)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 6b1d7802-a9ab-4d47-aeee-9f749dcfe6c0)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1bd2220e-1b04-49eb-adb0-0f991a17cfee)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 927ea284-8614-4ab3-a891-b6448fd88418)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a177e2f-a6c4-4f3a-9c7b-5b5a0b757642)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aa0a2816-e3a6-4a67-9496-31a873d0c3b5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 80404bd6-12b0-4fa6-9748-93f6a192ae3b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5911cb88-5b95-4463-a1fb-f9dee51493a4)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 98502f1b-bde9-4403-a5e8-357e0f83a180)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a9e6cda2-0d9f-4cf5-bde1-5168bb892487)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d3e16feb-9299-41d5-b432-0d187e552bbb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce5e64e0-42f5-4cb8-a7cf-ee4fc4cdfe21)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9fbe0b8d-fef6-4d20-b268-277d8334841b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7d0c7c2-ecf9-426b-b364-4607eb7abf85)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0e547d99-183f-4a59-bb86-4d9a6686aa33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57466b04-6049-4138-b613-b529f8cf91b6)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a2ed0901-e7b7-4ad2-87b0-a766dd70d32c)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 69b78e90-7dca-4ad1-823a-34c672e27f8f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a0765ec8-e101-4b61-8951-d76f1d761aaa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e48a61f-e476-4a9f-898c-6c8e1feccdf3)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cf37c591-e34a-4e38-a87d-94f5bcf42af7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2d4e1e43-774d-42ac-923f-1de419373482)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 f032e33b-1a54-4e89-8127-219a8f9e870c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 702c5d3c-f888-47c0-b93e-36ce5d2731eb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0126df11-0d93-4826-bf83-bab315e2f71a)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 7fe558b5-0714-4b6a-bc48-4c4453b88d65)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d2c0dea2-946e-40e5-bf24-06a19460fc7c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1585d858-02df-4078-a1d1-075d3c75c028)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6505ff6f-ddda-406a-a368-dfd45da04b97)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 63d85785-fc1f-4b17-8250-b0141b3ef236)(content(Whitespace\" \
                 \"))))(Tile((id \
                 043c1513-5b44-411a-afbe-861330a3a184)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 145e0f9c-7502-40ed-88d3-a65e8d536261)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c0598d32-5733-41d3-ae1c-93b1b2b8851d)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7ba3b5ce-6e1d-4d26-9e74-38f3c2f9df0c)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 970b46a7-ab13-4a19-9afb-6ba65e7234fe)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a8333854-a65f-4a55-9a54-9f2ee93c3829)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09fd4161-3500-4afa-8468-989cb6bf6c97)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 32c340c0-f928-41da-b241-06a15ddda7cd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1a84d190-f27e-44c2-b30b-baaa2c47c46d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c28b3c0f-912c-4180-aa44-cbc359ed679c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 153f8c7e-d158-4380-8818-1ef86787736c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ec9b1dae-dc92-4569-8a0d-fa88a296f449)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 1fc0e8f0-475f-4706-858a-d46cf439cba3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4f6bc0a8-279d-43a2-9afa-08d5c745ab9f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0b32113a-79c2-4530-8091-1958aaa62d00)(content(Comment\"#pat \
                 tests: happy (but refutable patterns so \
                 weird)#\"))))(Secondary((id \
                 29ab0d29-f8e3-498d-a163-6fee54c0cbd7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6bf3811b-8e4b-4465-92da-e2a9d624d703)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 db0aa97e-e5ae-48c4-b8e7-5d0fee703d3a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e858ca34-3e6c-426a-a6c9-1a3eabc7441c)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 fd7625fc-3aba-4a2e-a07d-2569b9453252)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3ec162cf-ae58-4d34-838e-636cde5e084e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f06b9517-104b-4a48-8ebb-7cc9d778e636)(label(Bo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7bdcd90c-d084-4b00-b628-1088498d1e7f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cb04e2e9-8609-437f-adc5-49c2a21506c2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 93f85a6d-69d0-4e66-b8ea-06f6c345fa62)(content(Comment\"#kind \
                 of a weird edge#\"))))(Secondary((id \
                 f3eec09f-4ba9-4273-a50f-c29904f4c465)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f4be92ec-ec87-4b90-948d-5ca5d5672243)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 42d3c552-ab89-4f0d-9a61-9c4f2f03cf63)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b0bb1e9-5dea-4c00-bbd0-80520ec3c1ce)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3d45a1b8-dd73-4d5d-8bed-62fd2f3f1443)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 f793993e-76c7-4a7b-909c-63d7f743eb4c)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 436c74ab-d404-424e-9982-ee1941614da3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 12f572fc-2430-4409-bc31-29fa380adc67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0deddd7a-2bd0-4013-b7c4-c6a952c701f2)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bc174caf-048b-4f27-85f9-b65809d20fc9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0a98a94c-fef1-49d2-ad1c-6f6da4584603)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 83dd4c50-f7b1-44ba-b877-44beb9868930)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 66a53539-73be-4a34-b5c9-ccafe360aab7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 46f7302c-7d28-488c-83cb-5c40ab51cf14)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 646697f1-7a18-4b6c-902d-74faf9d043c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 892653cd-ea74-460f-a3c5-a7b235e7be1b)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7f027128-79bf-437e-b83f-aed8d711ff39)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ce0e1955-de13-47f5-a7d7-f8a9917c58d1)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 aa51c83b-27c5-4861-8991-b1cebb39de19)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b71bad55-a05b-4c01-8e33-79fc39beaf12)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7957f74c-8387-49d7-8fc7-ee432df76807)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 06d954ba-dce7-436b-b350-8dc4d6297cd4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 21ef12bb-3f4e-4aec-b682-1c5d78375898)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cfe77e78-9db9-42a9-8ebd-4051b5891630)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d5453959-4d8e-4d09-aa48-7cfc6f367a21)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 32510064-1bdd-4fae-bcc3-50e4b8a7a6a1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 862e34bb-5d7c-416c-b373-5a79f5510a80)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f1a2fdec-1404-4969-9750-2f8316970a82)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 da2ee048-49f6-401e-aa76-77459007e774)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8a6920fd-eb3f-4b33-ac23-55a39daf4013)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e400ae9c-816d-4898-aa72-3cfb290be291)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f864cfbf-3baa-48fe-8c68-bac463475839)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 9388742b-ea66-4bf4-a842-592dcfb5ca85)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 d665d167-35fa-4874-8cb1-50f75aea8ee5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 368dd3d0-32d7-4a61-a322-a8bab4aaa3cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14409b18-5a8d-4d3a-9e1e-d5ca4ca7bb91)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 cdddbaa8-c6d2-439e-af43-e7dbcc985c80)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 4f729def-f614-47d7-86d6-3657b5cc1ff3)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 41f62ca6-70d4-4a45-8b0a-7cb82b1a0cf0)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 46280ea5-e6b1-406d-aada-51850297c0d0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1c7dd359-f50c-4f40-b1e1-eaaa2caf808e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fdbe7283-c129-4b6e-b073-01df403893d7)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ea1eabfb-6ced-4b63-b5c8-6ce95f745d09)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c16255ce-6a6a-4e1d-8c36-f742e0708056)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1339295b-0f66-46b0-9705-42432ad33ab8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 255040b2-aa16-4780-8d30-5963f26070fd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ebc32910-3c55-42c8-9243-1c0cc96faaee)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1d92bcf8-97a2-4d40-95ef-6b19493b2157)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 40c6c1c7-8840-42ca-a87a-9a07a4b834bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1291473e-55c8-4b37-a243-1cd508c0ca80)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6912b2ea-d3d7-4bdf-a3b8-221d0f1e6594)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0c42c147-8c36-4f8c-a0fc-2e8e0c907148)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6fa66a16-fe6e-475a-9a96-e3bd3a9fac08)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e5e5165c-282c-4487-bc7c-4b64579a86b5)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d2765aad-0e6e-495a-a378-5918b0c62813)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 85be24b1-f5af-487f-a1b5-fb471d921b41)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18b57d43-b169-4d7e-a5aa-d16c888489b2)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4c838400-c6eb-4c8b-97f1-75abb6a2d247)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 712d59f4-72ab-443e-b7d0-073b057c03ee)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5df04b12-a3c8-4c3e-8982-c92505710829)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3d0137fb-0739-40d5-bdeb-60b34cae03ea)(content(Comment\"#pat \
                 tests: errors#\"))))(Secondary((id \
                 0bb26450-f50b-423c-a275-984e470e55e1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 84a26841-54a4-4d82-9198-0b32478d7100)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 734f9f1a-0008-4ae8-beaa-5a0af6644cec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0aa4dcd-6a59-4ac4-bfe3-b1476e8da21f)(label(2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bfb728ce-e0d7-4ff1-9e78-6998c8c4b4b1)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ea8b79d2-8674-459c-b144-4b3b07bea52e)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 6db97309-aa6f-40e9-8a00-aabd4e76038a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 83ecf43d-6427-4391-8b81-747545cb4262)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c9798a2-8887-4bb8-9d8f-1dc2f761d782)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 874582a6-b73a-4925-9fdf-38934d4ea101)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dfd3cca4-68a1-4079-a680-4f3e29b5a91e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a09e26c2-c899-400b-9d6d-45e442381db6)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 36aefaa3-4dcc-4f22-9793-a3d9f17ae0fc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fa60cfd0-76f9-4fd6-8f8a-099d27982f97)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4349edc1-d200-42be-8b7a-b0d81ad04eb4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38fb6e2b-ed75-451c-8c6d-9d3c1dcb668e)(label(NotDefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8dd048b5-5303-481e-a4c6-85cec5fd9494)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 d185f6f5-d52b-4851-8b31-ceba9274d4a5)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 dd7b2086-4d07-481a-a1f1-93de11dd666a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f555e348-2bda-4326-befc-5d45cee368e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 833e19cc-de46-4e38-b9af-eb404a07ba5c)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c55b8729-2af3-4f53-9bc2-ae114cc4ca91)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ac39a33d-6e55-4797-b40f-1b8e56f78d2e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 194d2421-3f14-4b87-b4b7-505c000defc0)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 ff345447-992f-4caa-a757-7ff40fd029de)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c00be00f-045e-4987-8a7c-2fa904a0b681)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 008ad912-355c-42ec-a693-d53e9bc27fc5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec0a7379-b777-447c-80c2-64ce1a7a6255)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a1f4c2f2-fc98-401e-b61b-a1b69c1f4430)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1e39f842-8643-4e6c-bdef-a86490c2def9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ecaa89af-aabb-402f-b365-a225071b50e4)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c84c2d20-d701-4e75-9c55-bcd85c6fe6fc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9e0fdeb9-9b91-44b4-9e76-7e9a706ec5e6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0df3e72c-b73f-467e-909a-6c2dc061315d)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 420ecb4e-d1dc-4a8f-9f17-6de24ff324b7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8c886c78-a1c8-47cf-b5fe-7d6cc4b54fde)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2f8ad952-184f-4814-9df5-00a830dbf52e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e3953e4-1730-4caa-ad84-c8e9ec02b623)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9e40b22d-e381-4a47-b620-4c8c2a0cd883)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 fc4046b0-0b17-4f94-b4b2-1b95e32f610b)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 9c66cfcd-2618-496f-a318-31715ea26781)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 17407c8a-332c-49b4-9fb3-deeec8f31948)(content(Whitespace\" \
                 \"))))(Tile((id \
                 05dd7623-7a73-4fb7-9391-5e89aea17e54)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c2c79ecc-9642-4453-9391-9fa5b780183d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4a8309b0-d49f-48af-a93f-25b7501fabe7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 165c14ed-a258-49c0-828a-91971a541c7e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2315db83-cb36-4e9b-a43f-120c74c088b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3dfe2917-387c-40ae-b45f-e1e0b4cc2a67)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 3e243755-7ace-4c83-80b1-ec47d5ce7408)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3e57e3b8-b2eb-4201-aafa-982dff1a3fc9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 800b9486-e1d6-4e16-b718-e71a6000267c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d8dbb41-b167-4c65-9a11-e4379bb8f81a)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1ad0a541-2afc-4bc0-887b-a885e54cd9cf)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7a22b5e2-5d86-456b-a855-048b825857d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2a2c2c1-4d01-43c2-b08c-aa758616a727)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 920ba490-7c5e-47c6-bd6e-a87d8e34087e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b220af8b-d7ea-4b19-a666-691dba80b99c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c4310c60-6d67-453e-adb0-8f16a1f9157d)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b5e4ad15-db17-48f7-84ae-d5d8f077364f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5d577119-b328-4eba-95b4-ff51e0108e32)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 0ad13644-c9b6-40e0-8651-58328902d6e9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48636cdf-ea60-4269-bb9e-6d522fba64e4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f714946e-2aae-4dda-ba33-b90f3f21cef8)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 3075189c-47cb-4e95-889c-f5848188de6e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 49284e0f-e2ea-424a-88f1-62d42bf67857)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 88d3c310-2df8-4b28-8160-b991273e058d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3d91bde4-2ba7-4c88-98fa-37ca4c9227e3)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 10737317-c219-4dc1-b0d4-98e20dc4f31c)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 81dfa3d8-0e52-4687-a716-6b70227ef429)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 78d8415b-f6fa-44a4-b870-04840aa13105)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 97f3cedf-f0fe-4e31-83ac-db25cf90c56c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21f240f1-3c3c-413c-84e4-1879f3c455d2)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 dd982a93-1c83-451f-a25a-44a79fdf4375)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 22b5b600-52c8-44c2-8e42-ec373248c059)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b2091e94-698e-4bed-be91-e90ed284aa26)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fdb6604c-57de-4b80-80b1-ed87395df390)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 677ef41f-1a89-483f-a3da-8552fb32ab71)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 de6c8a03-975b-4960-affc-14b3dd1b5bac)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 20646344-9492-469a-9c46-cc674984e85e)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 9d1f290d-f955-4817-b043-aa2cd08a27f4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bbb25918-0dba-4fe9-aad3-7d35750d4d10)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2be5ea2c-15ee-4d0a-85b2-323d423b234a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ee46eea-f612-4e18-a445-e62dc579fd5e)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 53505972-d652-48ae-94da-f631b3fba678)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 58471fc6-5b00-4ffb-83f2-8725c5f57ca7)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 82aa0223-171c-42df-9798-5a03127c42ec)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b2209e86-2be5-481c-aeb0-bf201d93ea04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0ddf7bf-5ac8-4457-807c-461096222999)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 df17021c-b3cf-42a2-b84a-d6173a963717)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9b643c91-cd71-4deb-afb7-0dd0e052f25f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d9e5358f-66c4-422a-9d8e-117f9c005885)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 de48e378-772b-40da-a9ef-663266f066ba)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 246799d0-1417-4c58-8918-739c9f76d441)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d54a3c3f-275b-4bdc-b8c7-f531890de76a)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 31f87ad9-bba6-4501-b0f3-417f0c6702c8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 eae16bdc-0eac-4a8d-b574-2b9e02e58ce9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 cfbc91cb-4e7f-4eb8-8d3b-c61de7d36f2c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b5ef0b9f-1046-4b74-9b9f-c633c3840eb4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1a4ec4b3-b0f0-4cd9-bbbb-3b2ddebc523c)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 9c6466c4-5997-45f4-9e35-7feba637f44d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a02b5c88-e347-46d3-a844-9fd50a09c2a8)(label(\"\\\"Thats all, \
                 folks\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a199a931-8821-479d-a39c-4ef4562da06d)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# Internal Regression Tests: ADT Statics #\n\
                 # All commented lines should show errors as described #\n\
                 # No other lines should show errors #\n\n\
                 #type definitions: no errors#\n\
                 type   =   in\n\
                 type SingleNull = +One in\n\
                 type Single = +F(Int) in\n\
                 type GoodSum = A + B + C(Int) in\n\
                 type Partial = Ok( ) +   in\n\
                 type DoubleAlias = GoodSum in\n\
                 type VerticalLeading =\n\
                 + A\n\
                 + B(GoodSum)\n\
                 + C(Bool->Bool)  \n\
                 in\n\n\
                 #incorrect or incomplete type definitions#\n\
                 type badTypeName =   in #err: invalid type name#\n\
                 type ( ,  ) =   in #err: invalid type name#\n\
                 type   = badTypeToken in #err: invalid type token#\n\
                 type NotASum = NotInSum(Bool) in #err: cons not in sum#\n\
                 type Bool =    in #err: shadows base type#\n\
                 type Dupes =\n\
                 + Guy(Bool) #no err#\n\
                 + Guy(Int) #err: already used#\n\
                 + Guy in #err: already used#\n\
                 type BadCons =\n\
                 + Um(Unbound) #err: unbound type var#\n\
                 + notvalid #err: invalid#\n\
                 + Bool #err: expected cons found type#\n\
                 + Int(Int) #err: expected cons found type#\n\
                 + ( )(Int) #err: expected cons found type#\n\
                 + A(Bool)(Int) in #err: expected cons found app#\n\n\
                 #sums in compound aliases dont add ctrs to scope#\n\
                 #but compound alias types should propagate analytically#\n\
                 type CompoundAlias = (Int, Anonymous + Sum) in \n\
                 let _ = (1, Sum) in #err: not defined#\n\
                 let _: CompoundAlias = (1, Sum) in #no error#\n\
                 type Yorp = Int -> (Inside + Ouside) in\n\
                 let _ = fun _ -> Inside in #err: not defined#\n\
                 let _: Yorp = fun _ -> Inside in #no error#\n\
                 type Gargs = [BigGuy + Small] in\n\
                 let _ = BigGuy in #err: not defined#\n\
                 let _: Gargs = [BigGuy] in #no error#\n\
                 let _: Gargs = BigGuy :: [BigGuy] in #no error#\n\n\
                 #unbound tyvars treated as unknown-typehole#\n\
                 let a:Bad = 0 in a == 0; #err: not bound#\n\n\
                 #non-sum-types cant be recursive#\n\
                 type Lol = Lol in #err: not bound#\n\n\
                 #no errors: analytic shadowing#\n\
                 type Tork1 = +Blob in\n\
                 type Tork2 = +Blob in \n\
                 let x:Tork1 = Blob in\n\n\
                 #exp tests: happy#\n\
                 type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in\n\
                 let _ = Yo(1) in\n\
                 let _ : YoDawg = Yo(2) in\n\
                 let _ : +Yo(Bool) = Yo(true) in\n\
                 let _ : (Yo + Dawg, Int) = (Dawg,5) in\n\
                 let _ : DoubleAlias = C(4) in\n\n\
                 #exp tests: errors#\n\
                 let _ = 2(1) in #err: incons with arrow#\n\
                 let _ = Undefined(1) in #err: cons undefined#\n\
                 let _ = B(\"lol\") in #err: type incons#\n\
                 let _ : +Yo(Bool) = Yo in #err: type incons#\n\
                 let _ : +Yo = Yo(\"lol\") in #err: type incons#\n\
                 let _ : +One = Yo(1) in #err: type incons#\n\n\
                 #pat tests: happy (but refutable patterns so weird)#\n\
                 let Yo = Bo in #kind of a weird edge#\n\
                 let Yo(1) = Dawg(true) in\n\
                 let Yo(1): YoDawg = Yo(1) in\n\
                 let Yo(1): +Yo(Int) = Yo(1) in \n\
                 let Yo: +Yo = Yo in\n\n\
                 #pat tests: errors#\n\
                 let 2(1) = 3 in #err: incons with arrow#\n\
                 let NotDefined(1) = 3 in #err: cons undefined#\n\
                 let Yo = Dawg in #err: type incons#\n\
                 let Yo(true) = Dawg(true) in #err: type incons#\n\
                 let Yo: YoDawg = Yo(1) in #err: type incons#\n\
                 let Yo(1): +Yo = Yo in #err: type incons#\n\
                 let Yo(1): +Yo(Bool) = Yo(true) in #err: type incons#\n\
                 \"Thats all, folks\"\n";
            } );
          ( "Casting",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                 bb292d19-5d09-468d-9143-4fcb493379cc)(content(Comment\"# \
                 Internal Regression Tests: Function literal casting \
                 #\"))))(Secondary((id \
                 cdacce32-8522-499b-bd68-6d8e6525e33f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5a1ad9d5-92bf-4d29-973e-07870faedcf0)(content(Comment\"# None \
                 of the below should trigger runtime exceptions \
                 #\"))))(Secondary((id \
                 ff0a9ccb-d48f-4e07-bbf7-ef96f3aa8fa4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a3341906-1609-4bd5-a57c-b5ecd1785739)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e5068986-afc8-411f-96bb-203e6bcb3b7d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 97e44d0c-918b-4c26-b35f-4a98efbc0030)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b61ff3f2-ef3a-41fe-bccd-86a8b3aa16c0)(label(g))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 577edb00-38ef-4521-a091-fbb4c016a2e4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e85f2f6c-5164-4df8-88b7-af821ba4f052)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fc76c85b-c416-4eb1-98fe-dc5ec5581e78)(content(Whitespace\" \
                 \"))))(Grout((id 91a172b0-366c-4bd8-8ec7-0537f943c7da)(shape \
                 Convex)))(Tile((id \
                 74d54ead-9ee5-41a5-b41a-25d7c0e27078)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 ca56c0d5-d45e-4dc7-82bb-787aaae133c0)(shape \
                 Convex)))(Secondary((id \
                 db1fd151-49ca-41a0-a81f-27160069da27)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d610dd2a-7738-4778-88b6-9ac32f1e8e60)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 caee7186-e333-4b1b-aa7c-fae2eca92f36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a169c3b4-123b-45e1-9de9-c3e29ae1a972)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 bb1bb91c-5cfa-4392-bc5a-81666a621746)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07a72f19-cd04-435c-838d-7bdf39b37a10)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0ac2abb2-626c-462c-839a-1b3c1d005e7b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 71d93623-0272-444c-8cbe-cd1b9df5e677)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37bc251c-ed4c-41b1-be9a-ccf57a9ccc71)(label(9))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 90d8cf11-d592-407e-a26e-3a91b9a31eef)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e0ab73ad-d884-45e8-a865-fad8a997deaf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f22a8f14-656b-4a2b-9c45-86b27d88bed9)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 2))(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8aa40212-3b91-469c-90dd-c3e1c25fac91)(label(g))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6441dcda-34d7-4b43-8d68-3e78bdfa6029)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ee54b3ee-490d-4007-97d0-f632c767c494)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 15bfe77a-fc53-42e9-bfa7-229778d3ab90)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1c3a01fe-10cf-4b48-a116-7bd1e709e0f5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 234f230e-250f-4235-b5d9-751eb20661af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5b763389-8ca5-4d29-8582-9b48252f514c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a9e7eee6-2f91-45ee-9ffc-31f5fc301e83)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de809ba8-20bd-457c-8b3e-4a22f9350f3b)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 da859b88-c86c-4d1c-b5e8-306556a3a0d2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 789d4603-e5d9-4f63-9e44-ea11406d6679)(content(Whitespace\" \
                 \"))))(Tile((id \
                 119e268b-8695-43f5-b7d3-d84ed47b8152)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b6e8c3ba-46e6-4084-bc06-b1d5e8c4a0aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 619208e9-7bd6-43a2-9829-37270456fb32)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d37319eb-3a9f-4d4f-be64-11098f5d12a8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 83a5163d-aca6-41c6-a284-48bb153a0ae6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ec0d2c1-6566-490a-b1c5-f764baf81a34)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 69e5ff60-ab4f-4178-a74f-e2c35dca9de8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fde260f-b1c7-4c53-9344-6cfcd758cad0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c3c5dd38-e64f-4340-8bc1-73924ea75cd7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3702add7-daab-4aee-8da5-8b9b02764b6f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ceba5f05-3056-41c8-9de8-a2eec9895a6c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b66fcd74-ce5e-4a4e-9aad-d576a4281dbe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 972c739b-02bc-4cb9-a32c-db2ca6652bcf)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8b7d78db-2c22-4240-acd7-e442bf4de1fa)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b11a0d07-4896-4683-9513-c8ff94cca796)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c414dbc0-3440-4edf-a937-eced84f9bb43)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 98530d6b-7030-4658-be1f-c3aefb58fd2e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 08399bba-a866-4c0b-80f9-68dca1df8ef9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f2109631-d24b-4a40-af73-86828a37c855)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02cd1c04-7989-4335-be94-75f4a0b5af37)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dded2019-af52-48af-b116-6a620f0268dd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f26cab71-b504-4b3c-8dc9-120b09da9616)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63a05498-5b70-49f6-bed3-60681da8e57b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 584a4afd-4338-4e28-ba10-227516eaa12a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95d7f4ac-5408-45a2-aafe-58c273da60a9)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 21e31a7d-6349-4f0a-8196-680a8c954efd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 b873e663-6398-4a8a-8324-7b4e9d59b671)(shape \
                 Convex)))(Secondary((id \
                 265f6a60-6972-4baa-b1b0-f4d4c3d6e325)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7467e5dc-8385-4228-b31b-b816efc9e39d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0c51eb74-d3fe-4db2-915f-656b720b6ef4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 43aef67e-4d4e-43d9-a58a-1b355c1aead0)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2558ce13-8d83-4692-8720-14827e5774f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36d2b41d-b8ed-4973-aff0-870c788df2dc)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 abc9076f-9a4e-421d-addb-387048384c43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f920e356-1c2e-4082-8494-36395c2b9ebf)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 30fa3dfc-83bb-43f3-862a-a77df8082ba9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6f787209-39c6-4f20-9d71-73010b8d9619)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca6eaf6b-e78e-42f2-8aac-7acdb84d9138)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 86384ffc-694b-4b17-b794-a753a834e148)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b77e5214-1a73-41ac-b652-b8b66ae4ed2c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c9d90263-b423-4cc8-8ff2-1a584abc2812)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d54c0000-4abd-4db8-af9e-2d4ff425f11f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6b838b5b-8b51-43d1-8b5a-ac037dd2caca)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0143bbf5-75e5-4187-a8d2-1f18ad4ef9b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97e16344-cca1-47e2-a2b2-abf325f1dc2a)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 de4b4372-6942-441d-aa72-574aed50ea2f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7fb6a5a7-61d4-431f-90ea-b0af6d788b0f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a895d9bf-317d-45b3-8b2f-e13c8e776930)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 28379136-d6ca-4fd8-b42e-846cf998200a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f27e4a5-d059-44b0-b7f9-2039c9bec969)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 781308ee-c6a8-417e-a6cf-e00c1f8e6edb)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e1a5217a-3c7d-4553-b952-fb02908c509e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 352fe56f-9539-4392-b276-c8ae660b3a3b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f96110d0-d37a-482a-91ee-31f43678f46b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 17f84bdd-bf47-4860-8876-06030f215c44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d1580a3-9342-4ab8-ab55-43d08f8d80eb)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a4f6f2da-6fe7-4f14-8694-1a6620704bbb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9cda6e7-d416-47aa-be12-57f56376fec4)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 943e0068-322c-469b-a32b-4121da87057b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 488492d8-1092-4a40-85af-9e194a307872)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 31461307-434d-4fc3-93a3-72f3085066b7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0bd1d5e9-1db7-4924-8fa5-940552243013)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4065ba03-8fbf-44d1-969b-a092974ea7f0)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2f800d6f-6e57-4923-bcb5-11f320555db1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a5f04b85-2768-46b3-acd2-e115e8528dce)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 bd816a72-9d43-49c7-8936-3bf8fb6413ad)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e488dc34-ece5-4c0e-af36-1d4748883631)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 027189d5-78aa-4260-9638-a32ed3ee2501)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d0a03384-ed83-462a-aa08-d7e69f9fc3d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18bd540d-c837-4895-867b-c79acd141430)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8dd66df8-52e0-4a6e-9dcb-51c12be30f9e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 0add7770-8a2f-43a3-9051-da8feadd774f)(shape \
                 Convex)))(Secondary((id \
                 f3f3e07d-f60c-4727-9234-a3a989fef252)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cf137685-60d6-4fb6-b7fd-64fcc46ac52e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6bce0b5c-35e4-46c9-89dd-6078aa0a5350)(content(Whitespace\" \
                 \"))))(Tile((id \
                 913bb32c-cf11-47ca-84d0-0099b2a0c4fc)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 679ae0d1-f773-4dc8-9f25-008aa7777271)(content(Whitespace\" \
                 \"))))(Tile((id \
                 58d4db1f-51bf-428b-a42d-1ec722d12f65)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a1f9f6f1-4269-49f9-8853-920be14d7b17)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 234f95ec-29b1-4a63-add2-0aa1ff5e356a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fbe60194-65a1-469e-8f1b-0d890309b957)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 321fff45-38d3-4e38-b93b-ab7511e84371)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a19edce7-511f-4a21-b9b7-4aef9f6aa9e8)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a1d340df-814b-43d1-b441-ac2c7c4f0b54)(content(Whitespace\" \
                 \"))))(Tile((id \
                 28fbc1d3-33e9-4ff7-96b3-4d8d4d391384)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 37eac825-fe84-4539-a922-1783e5bbe6b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b75c0a2a-3490-46fa-b674-a81f0f793e1c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31916a9a-042a-4785-8cc0-9d115b0e914b)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 376a03cb-93b7-49be-bc89-89030003b2d4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3ff1409e-3a72-4e2f-a35f-30b316e1be5d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 654f1b3a-efc2-40dc-8363-8ed9eedecbcb)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7f94db1a-1f1e-4836-bdf9-b6bca6c4b867)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 58c7aa3a-202c-43ce-b96a-6c365f69324c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9349e799-0d74-4b67-b8c0-bfe9d5581026)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba719975-cd78-4922-91a9-336be94f4465)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 410351a4-09dd-4ac6-9d49-6cb36218a8dc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d1b2d957-02e6-41a1-8606-45093f17d661)(shape \
                 Convex)))(Secondary((id \
                 995461c7-ffc0-4ac3-a676-5e1ed7f7244e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 dd5f4e85-3147-4e6f-b181-9e77a618cc45)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5528cd21-a037-48a4-a754-a4d931cb01b6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b5e82431-e3dd-4ed6-b1a5-5e7b425e88e8)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 da8030de-ee03-49b7-bfb5-d1c3400d990f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b3a5ef7d-0eb4-43a6-87f8-c6a0b0a996e9)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 439a71cf-0adb-463b-8750-7900e6935d6e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6da7be42-3df4-4737-8fbc-161d12d40a55)(shape \
                 Convex)))(Secondary((id \
                 b754c59a-0f1b-4f89-ab65-ee6753b5a756)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1b5dc693-4f99-46d2-8e1a-b20ef8c4cf89)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7578ca28-8638-47c9-ba26-e27b36cbd394)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5053df5a-19e0-4dfc-961e-af238733f4db)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 241e3984-345c-4038-bfc2-3fd5195e5d9b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba471f34-c769-4e7e-9216-a1f2ae6ec637)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 da9604ce-3456-439e-bd70-0183f282501e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfdd4013-acb5-4f34-95c2-cea0cc9ac8f4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 462020d8-e331-46ac-90ab-8f3fbb6b5a05)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bf1cf8b7-4561-4b3d-a67e-4bc76a6cef94)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a9ab4c8f-17db-4c83-8d77-ed08e68e3e35)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3eb4351c-6bba-46d6-956d-73a0697816d4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e23ff6e3-133c-44b7-8c1b-8c9c4d414c5b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 dcdadf61-e468-4e10-9602-ecd794f45f34)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cbe6ec40-9a95-406e-8d4f-1af1431e98ee)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d2bec635-b5fd-4463-87e9-40eab839d80d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 623ebca6-f1d8-483c-ae3e-9078e6f8a2fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6149e3b-ec5d-4016-ba3b-ab319f06f932)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 82ac56e4-4ba7-4977-9bc6-f574e5ebbcc4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 084c62b1-39f9-4878-a42b-2195c1634faf)(shape \
                 Convex)))(Secondary((id \
                 7e75cb39-2889-4557-9f07-9f8a9ff43b30)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fc797756-2f28-4dd4-b5ca-f6dd48ffb8b9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5a86c3c2-a15a-4cae-bb40-1bdb299228d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a913012c-abc6-4b28-ba27-c944ab0d08d9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 8972e8e7-db8d-4227-9c20-544cac77b17b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8da46a2d-8c9e-46b3-bcad-885e1455d00e)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 316ed73b-5fd5-4cb9-a9cf-be8294311afd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bd06e102-b678-44b7-8f1c-64fbe5bb20e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 648367ad-4ea0-4147-b219-1683e76611f1)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 da6bfe3c-e808-492a-993c-35af0acda1ba)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d70e4e4f-6b85-4852-875e-38071c7622f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cba91055-b2f9-42d7-ad2e-44f7e21b7a9a)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0ff9a2b4-b5c7-4c1f-b3cb-45ada005b9ed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae79d425-c628-469a-8fa0-00af4fa58bfe)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1a8ccade-c75f-460a-a88b-2877436c02e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef7b85f7-457c-4b4c-be31-b786216aba29)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fc187737-ab5d-4007-8945-8f234d80e270)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 957b68cb-50a3-4df2-a7a1-eb9d9bc92274)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff78b48a-9ed3-4eb1-80a9-ae4ca23ce0f9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 09d11b4d-21cb-4c35-8c76-1a5066301ff5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7e7df3b2-e1c3-4aa8-bc50-98b1d2f8e64c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9452b02a-93c7-4687-bb43-5eefc57463cd)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3429d7d6-292b-484d-988e-71eaebfb2fb9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 44b2b082-b238-422d-b4d9-19c0b4f38e18)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 af802689-f655-44a6-89b1-09df0d0baa97)(content(Whitespace\" \
                 \"))))(Tile((id \
                 96dc104e-8531-4058-b3be-e1c7c8051d95)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ee945094-151b-424c-a0bd-3b4a172931ca)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0e12eb03-a6c7-4c00-9958-1005ae4d26a6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fd34861a-00bf-4823-92a3-f10d8b1583f2)(content(Whitespace\" \
                 \"))))(Grout((id ac0db031-501d-4d56-83ee-50a5d434c59c)(shape \
                 Convex)))(Tile((id \
                 4f6911dd-d773-4570-9c1e-0deddef8209b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4195d1c5-2f9b-4a3a-9b5a-a5c112b9f212)(shape \
                 Convex)))(Secondary((id \
                 c7e8a59d-c825-480e-84cd-c7a88fe72ece)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 00cbecc7-fbbf-416d-aa9a-87467c6b5f72)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9c008754-8e48-40d4-bb24-54ad5f8b631d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d7ee340-9b0b-43b1-9f44-193f257ad43c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ca401273-6b29-4410-a58d-61c730d5e0d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0da818bd-b425-42bc-80b0-6a543e0abbd1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 980eead4-41a1-4eff-aa05-41f388eb0e29)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f6883ac1-6ba8-47c1-b9e3-508b082563c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b82bb661-1738-408d-83d0-7ba24fd40af3)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 afbc2956-b860-4e22-b464-ac10e337d14e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71a7f9bf-2872-49a2-b98a-c6005115cff8)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b25ee0b0-d950-4eab-bc9c-f1750466eedb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e9bf9368-657f-4516-a49e-c4a389fbb1d8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7a862fb7-7257-4037-bfab-324caf8b6ca3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f545141e-bce5-41d6-b531-1662d03a4d46)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8c64e64-feea-4f88-9982-57aec5b1ef54)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2f68ef79-6098-4422-9f00-231c098980f8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8b91ab60-f3bd-4cac-881c-27010317ced8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 f214906b-6330-471a-ba81-b2e61f6be472)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ae150d25-ce9c-480a-8868-23dde5202862)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ce924afd-b2fe-4b25-b036-0ebf258ec488)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bf75a52a-fe06-4621-8a14-45b47f1a925b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5cdc5eb0-c39e-4538-b778-12cabf815a9e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 47ee4fec-7d52-4d0a-be70-b7ae938a681b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 011ad934-a711-4073-a623-ec5dfdf7f99a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 09460845-e2b2-4ce2-a4d8-4061625fbf0c)(content(Whitespace\" \
                 \"))))(Grout((id 82c9e049-cd13-4958-ad1b-c54a0df0c8f7)(shape \
                 Convex)))(Tile((id \
                 f321f10f-fa00-4e44-9a34-88b4429dcbbd)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 8b6afaa6-1500-40cd-ba74-a8afd08c79fc)(shape \
                 Convex)))(Secondary((id \
                 7d96c88d-446d-4558-aa00-813c3d5bc337)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 38f4bc4f-1ac1-4cfb-8050-05594e047cd0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a892d621-9009-4102-9b3b-b68cfa3bc595)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b98c568f-bada-4b6f-9982-96ffdabde1e6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5c52d57c-c86c-4557-9a52-8873118295f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 af2c595a-557d-4f1e-888c-9a5fb8c7cf93)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ada083ab-52f2-42db-a0c3-0cdb265eedd2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 a101f3ba-92ad-4b7e-af2f-923b3416784d)(shape \
                 Convex)))(Secondary((id \
                 a335b5fc-0523-4e34-ae1d-94952dfe2ef5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5efd8968-40f0-4926-b404-0f504fc2a6e1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b7acec53-2aa6-4d1f-9a96-192706995a1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 228cd215-2a1a-404b-8dae-b5d6afd8f786)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 77cf3f44-7dcd-4f37-8e3b-f41359b2469a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a9037aa-e181-4b4e-ae2a-eca044966d81)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 71d4b351-cedf-497a-bd28-e249fdbeb7e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 59b4ad61-7f8e-4c8c-8ccb-74a4aea6cd5c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4e777d02-df3e-43c7-ac96-1fe3e5eaa1bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0ec8ab2d-a946-4a43-9a4e-2a7efeb030fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0f4fa3d-a082-4375-b59c-aaadbe2afa5f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a062e62c-e1e7-479a-9914-8c16260e4a55)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e32eff95-a28c-4e96-99f8-440751e9bdca)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 7ce4f75b-10e2-4ae2-b0a0-caa5e96dd231)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 624967fe-bcad-4daa-b05b-bd1b091b3528)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 075892f3-4f35-48e9-8eb7-38644fc8f4dc)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0ee8d8fa-4c4e-4581-b8db-32cf464018f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff8e18fc-15ad-4099-bd2e-e4b40e18d220)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 eb3648ff-552a-456e-bf1b-dd2fa53429f2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 fdbccb0c-de0e-4fe4-848e-0d72df7d08f8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b2bec1fa-5867-4387-8dfc-8b55270bf057)(content(Whitespace\" \
                 \"))))(Grout((id 1a06611b-a1d1-4ae4-b0be-859c393fb848)(shape \
                 Convex)))(Tile((id \
                 c03c614f-0ef3-419b-902a-d520346db346)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 cd8c3f47-f06e-4457-89d8-a5298f3a2a1c)(shape \
                 Convex)))(Secondary((id \
                 f1e2f686-a649-42e0-a7ef-a3a1b556cd1d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 947e6c69-0e94-4872-ae78-ba9b630cb22d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bcafb072-2a13-41ee-94d3-b52d3d9c197e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d465853-b3c2-49ac-962d-e5a2efc615a9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 43d97665-8dec-45dd-a9cf-eca145826af0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 793ddc01-4796-464e-bc40-284a11e4e082)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4b214cdd-0173-4fac-93eb-c4c219f9d15f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 471b0d71-1791-473e-9f66-b5605ed8238f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef99b36e-1bec-467f-896e-586e4d5f6d52)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 636b8ceb-967e-4a5f-bc19-d5a77bb86d80)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69a0ef78-ba44-46b0-a68b-53cc54a97f20)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5119b56b-ef6f-435d-ba0d-6f13eb991257)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 af2b93a8-0216-47e6-a690-2454edc64e75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3844e451-86ea-44d8-aa0a-dcadb7a19854)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 70c57fe9-21e6-48ed-bd4c-c3ba47dc7150)(content(Whitespace\" \
                 \"))))(Tile((id \
                 451847be-1892-4663-bc6f-344b0decd6e9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 588831b1-7925-484d-a6f0-2d4a46ccc2a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f1dd10ac-70eb-4c15-844b-c662ea61c377)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26252717-b8bb-41d6-b35f-ead48fc20ffd)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d76d5673-b1f2-45f0-ab87-152782536524)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 140e2ead-7322-4ec0-a75a-9523bf9b5ca9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 8adc8334-6d01-408e-baff-eb3d19f92fe9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 08d9cace-0e1a-4e4f-9e41-e8e2093ea242)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d7593963-7183-4c89-9ddd-3f27a9fb0e68)(content(Comment \
                 #ERR#))))(Secondary((id \
                 af47377d-000d-453a-95bd-fa8738d3d02b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7f7e1427-bd04-42ef-8df0-416ef35911cb)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4f7a81c7-2302-4e03-9260-63a6bd3ac258)(content(Whitespace\" \
                 \"))))(Tile((id \
                 884a7648-a7a2-487a-8cc6-036743cb4429)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8cb3ea01-f1d3-4e97-b64c-a7947e7a1f74)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8e568137-0cbb-4603-9519-f3f9c1735d6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 434c995e-921d-4f54-8bb8-04f101e8e4ed)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 61e600c5-30b6-4a86-853d-d2a48a89732f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b450346-88db-4156-974f-94ca0d0fe007)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 bba4c517-2e7a-43c4-ac66-e797d138413b)(shape \
                 Convex)))(Secondary((id \
                 b155b6cb-6f47-4846-85a0-f1b04d928431)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 981f60c9-0f3f-4f53-8e68-23db62c63981)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5a8d2663-a2e9-4443-9cdb-5c1f814c28db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4961d225-d414-4d11-8fa0-22ae59b2a38a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 703885ac-8c9b-4e88-bac6-67144d6e9f07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56e9c0ac-f9b3-4ce2-b12b-bef661fada40)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6c93d2e9-f8ec-401e-9840-e8b38ff7869a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 42011968-535c-4652-bd40-43eeca41fd15)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d3fbf75-1bc5-4cbc-a931-91c08e9b6ec7)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 12a40299-a558-4b0e-bd75-fb7190b57e48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f1486af-5843-4e23-91f4-0431f99df002)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 11dd3a44-1f92-4526-917c-1cb3b06f453c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a285b34-f7f0-4e1c-b834-b4097119a774)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2bebe42c-3df3-4afa-a87b-70f6d827fe2c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4da35bb1-e859-4683-8677-8187b5b54bff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbdd1ac5-a634-475e-a558-8335ec6150fe)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 847e4cb1-bbf4-4b3a-b147-e3f6259ab9f3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cea39563-2afb-460b-9114-7a4221d89ab9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 cc882aa1-f88c-4743-bd25-07776bf638df)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8e4208dd-1b28-4a4b-9262-c27b8f9bb973)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0d3451af-c69c-4adc-b841-7e02abd2699a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1c3031be-bbad-47ba-b376-cc12aa6703fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ee303ea-9d67-4edc-86ed-9f5c233aeb6f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 47fc270e-69ac-4972-8480-61037ca126f6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d95dc273-1d90-4b95-8749-e3f294ccf933)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2113607f-0586-4204-9cee-9f0c15b1d981)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7a3d4551-022f-4ce1-895c-ac39750229ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4dda5490-e655-4eed-bc97-5dfbc6ab2f51)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9ada2dcf-c563-4e37-b639-4b16d4f1b934)(shape \
                 Convex)))(Secondary((id \
                 e3181ddb-8bd5-4f47-ba4e-adab15cdcd42)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e8d2fb26-7614-42ac-a105-f2aa0ffbb01d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3b680ed4-659a-41a4-b1f0-7b442c383c3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c91198fc-d977-47f1-bd1a-1160b97766c2)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4d7b70cc-a2aa-44a3-a804-d77877e957f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36c4b1ba-0315-41e7-9104-9c6f03d8924a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 330bc934-93d0-46bc-8544-2c65e0ff88d3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3f7f9a83-9a2a-4666-a79e-74af7ea62836)(shape \
                 Convex)))(Secondary((id \
                 9d67305b-df8c-4e30-9e36-2031050ef0ae)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 91e51a55-50c9-48c5-9053-b8fbeee55039)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ef156f24-a3b8-481f-88a4-1a78b7d7b49b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 939d1eb0-60a0-4021-a25c-513ddaa07d50)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ffd93ed3-0bb2-4a38-820e-a9790c51fd5b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df521453-9152-49b7-8be6-780e4c26ba44)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22272bd3-5be7-4aff-8474-12298bfec5e1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce0390d0-1b2b-4eca-b4cc-ef87ecfc8a9e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3a3acee7-bfed-4ba0-9205-366112a5a2e3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0fba196a-080b-477c-bf43-e879019e36c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0992fcc0-78dc-4808-848d-774d31d2f41c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f3c4ddce-3908-420e-a3ca-8a9a58a10b95)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9f3ee029-681a-4e10-90dc-c8f170b84b02)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2a1056f8-d4fb-4f79-87e7-c0f7c0da7e59)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 05d54fd1-a886-4d28-a728-6a7092c3f4a4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d96a208f-6314-46a1-acf1-2ff620fd93f6)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5160081f-16ac-42a1-8af4-570a37ee0499)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84303355-ae91-4a72-bfbb-1cebe142cc01)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1e3a6947-1dd4-4ed5-8d88-7a74a69cc5d5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d1c44356-51c6-498f-a082-f5ea53d83239)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f8282500-de0c-4d16-8162-0dc00ae71b83)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5ff008b8-f54b-4b17-85a9-f224ee4a9284)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72df5b7e-de98-4052-a0d8-b61f02123914)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 12581d42-e150-4e33-af25-c914934d9f75)(shape \
                 Convex)))(Secondary((id \
                 593ffb15-294c-4f14-8d26-305893db282c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5338fca8-cf93-4952-969b-9913ff0798ad)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 256e2437-7c23-4900-a81b-3d758c1e8fa7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 461ec3a9-2907-43f4-8685-1bf20b67b7a1)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1663c4c8-0e99-478f-89fb-848c0ab3aa6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e2924d5-9bb1-45c6-9b87-843aba21cb14)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4a0e7d4c-dbba-4ae4-80de-74cba7e2ae7f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a8bcf9d5-607a-49af-9837-a4cce5977cda)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cfd7eb8c-0312-4527-99b2-f41bd01d87bc)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 db26c93a-baed-4c97-b354-06fa3f1b0f53)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eefc0604-ce2c-4b6b-aea1-c69e0aabfc70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51732aba-561c-4036-9c33-55dc8bf66579)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9f3797a6-588a-41c0-93a9-4b762e9a7b73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cff6a05-32b5-4c69-bdd5-f2d5e7b3d7fa)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f8bf7f86-32c0-42d6-99b2-04f5a99c2651)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a001481-b9f4-4174-b246-65e925e8da6b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a6fbfc58-8f95-45ab-bd53-caa69923561f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d45b3cb3-9cc2-4238-8403-4308be801d00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 af32a2db-7607-40c9-b3c5-e37aae83de25)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 96755d49-085e-4862-982e-d9b103a84fef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f0466ad2-08a1-4828-bd14-84976ff5528e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 df877baf-2983-449e-9220-f6ef6e7575e8)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d24e6bb6-2e81-4f1b-97a6-5af5770b6101)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 46f2c487-0640-41e0-8ca8-0a903f212ed0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8d1b126b-f306-418a-b78b-aea8fb40b71a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 78fe46ad-6f83-4179-9265-1971afa912e7)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6ef001d7-079a-4a91-b4bb-ce628e5b65f7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c38c5332-f28e-496c-9059-89a549827b3c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42a89eda-d226-4ef0-8b89-46b231c90ce1)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 20b12853-afad-48f0-bb07-599d6eb78023)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a0af198-941e-4880-8f55-161029cd6b8c)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 917a57bb-e37d-4248-ad01-048593fed320)(content(Whitespace\" \
                 \"))))(Tile((id \
                 add7fa5f-c461-4e92-9885-1edc21b718d5)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 cbf80b1c-949c-4523-9f61-bb5edeabaa37)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2a3cb40d-eaef-4ffc-b4df-cc9d57db8b29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6eb899f0-9336-4654-83e0-f1fce3578f9b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7ae48f33-1e6c-4e0d-b0ab-d22a607b5a8e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 980ccc56-3726-4df8-a32d-52123535458a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 867261a5-26c5-4eae-8096-ab8ba48abf85)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6d0afad0-1aed-48d9-81db-37066707b1e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74283497-3cc0-4b5c-bb06-1b7d4b2faa34)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ef182dcd-c264-4edf-a6a6-70ed7b4e29a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49f0748a-80c1-4bbd-9cf7-3c7fa30d5e09)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c442c045-f114-4ba5-b592-66b2d450ff47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 75dfbaf5-af62-4d56-8477-0e54e50be4b6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6462f5e2-8a36-4604-b363-be407f57b143)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9b199584-cd89-47b6-ac56-e3e93ea2984a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00a7e66d-a2c5-413c-8b33-a76c0795c5ff)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2cbff5ac-aea3-452b-9fbf-2590b3ccff19)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2a4602f4-73d8-4efd-8232-1a11066b13b6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 25f2cdd8-6b21-4e71-8111-26a717f73455)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1808a979-6c2e-49ce-bcfa-358035630b7a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ddc3a0b7-1190-4fe9-ae08-20b2277ad33c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ae4e8ea3-0fc1-4f0f-a8bf-4f6c1063f0c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a802dfad-d164-4f0f-a31a-52273fb323cd)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0ec00ae1-e89b-46e9-b4e8-c581038c6ede)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 24ca4c83-74db-403b-9bc6-90d8a3f9b75a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6844605f-3f30-44c2-a07e-f9c15aae5370)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 42628951-2375-437a-b7a1-abde8f8aa132)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb83ced0-0688-4975-a51d-2ad7f840f2b3)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c2c4efe7-5dd2-41d6-a6eb-46020ceb86e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f421957-bbe7-4442-878d-86a9e2597131)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6b65053a-7952-48d1-a034-a2e815df8b1c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d54f7fc0-c0c9-4ad7-a8f1-ffc350be89c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb6366cd-bbe3-4206-a5c2-bb085608fbec)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 21804b33-7402-472f-9bcd-d437409ed6c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7887db8b-6450-4ffa-bd60-39a79de0e710)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9f6eea77-1d97-47b1-9ea9-145998c196e8)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 413e3bdc-67c2-4a1f-9ccf-7f90f1190554)(shape \
                 Convex)))(Secondary((id \
                 d2d07289-c94a-44e9-a0e2-03c55ebecc08)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 548b6a39-4c16-4572-8122-123a0ea275a9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f834abca-0c2b-4539-ae53-b311585fba1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52b86873-0d8b-4d46-9717-f7c518c68646)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a5e69e20-2b56-40db-a432-0106c5286f47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b31ad018-90eb-4ae2-9866-26c776ca1c93)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eb2c297b-f64d-4301-8542-ca2462ba15ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09741a02-3d6d-4f2d-a430-20411b290427)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bf462d8b-40c5-4dae-a1ed-552ec1282550)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b9c6dc6d-1ed6-4e95-838a-2030b488aa12)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8e774ad7-a15a-4224-b07b-c9af8f85353a)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5f998941-e608-4080-94ba-7127fd944532)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 49e49962-9d0c-46c8-b966-01b0938e06b0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 456a3987-6dcb-445f-b140-993c996c7aa9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b535e828-2057-4eeb-a581-affac153089d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6bff8409-d895-4465-97f4-e0c474f9e0e1)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0ec336be-e639-43d5-9df8-061bcb164abe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71fb9045-9d4c-4bdd-9f9e-9c5d0fbd5770)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c17366cf-140a-4985-8a5c-12f342b5b7e6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 208244d1-f1ce-48d7-b457-d3c59b81d5be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57bd5717-84c2-4175-b882-0ff5e7e5fcd6)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 459d1d11-f701-487b-ab92-2d99da8645e1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51840231-0669-4d26-9641-818ccc6c3fcc)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e24554bc-c723-449d-ab63-8a957833ecac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a26ba4d-4c4b-4957-8630-b9b2208ee90e)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 9abff025-3e64-4dc4-8df5-5fcbf38e1cfe)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b5cdc79f-1e42-408d-9b9e-2e850442748d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 06936335-362d-4060-8785-3e82868f9e6c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 af9b89e9-6ffb-4ae6-b712-8788fe3e9a72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b56ae6c1-5981-4afe-9f00-ffa4735a3b0b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8bad003b-e48f-4346-8e10-51246b71d374)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 21107490-3454-42e6-a26a-d314cdbbe1ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13cf32eb-9773-4eab-990d-24b441853c19)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ed34d283-926a-4fa7-a0ca-36669da62544)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3d517dc1-5dbe-48ef-90e4-3ec372e4d081)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40629be2-8ef6-413a-b460-272d740d9222)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c91efe71-c4bd-43c4-810d-947f019d70c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 08b2bce2-e6b4-4745-ae2f-1500a42e6861)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 771ae1bf-7719-4655-83fe-62915f844b2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc8e78de-6377-4d8c-aa94-5d2b77bc6c4d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1ee60cf4-7d0b-4e70-a799-dd03cc87ecef)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9c07d582-2bb5-4177-898e-07c000eea911)(content(Whitespace\" \
                 \"))))(Tile((id \
                 868485d5-0fe0-4779-8807-7e09a2c9f406)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 27989455-6bcc-468e-95e0-70fe6c713de1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fd38a81d-3298-4b46-8c4a-c0c2b074a36b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d004a438-9a24-4e5c-8439-f2e94d7d25f6)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8e0175f7-b713-4c5a-9c5d-8ce63d62e25c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e0453fb4-1e05-4d39-87b1-f598c5fc3ef6)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 85097ca6-7daa-49e7-a602-a5cf078fad16)(content(Whitespace\" \
                 \"))))(Tile((id \
                 430fd57e-a378-4e0f-87e6-1f7d3a284e76)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 499099ab-b395-4f6a-a157-4d26cf24b03c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4a556a65-7fc5-47e0-b7b3-56f5e97bdb91)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5ac5f116-3d27-4489-9307-020dc75fe17a)(content(Whitespace\" \
                 \"))))(Grout((id 3f536760-ba97-4456-87bb-c9362c454a9d)(shape \
                 Convex)))(Tile((id \
                 b282df34-d46d-4002-bab8-740fb50917bb)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3b2c41d2-7982-47ef-82f8-2eb984236dd7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3eb7040e-833c-4141-91ba-3a8a43457025)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ffb62465-68f7-4094-8a86-5a7d41944708)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 57a22947-244f-404f-8198-98d2cf39319e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44cc12d7-d3b5-45f0-a776-93599bca16c9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 06b4e901-486e-43a1-b34a-265cf33f84cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d63780c-a240-4cc6-9594-cb7850b9b235)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 48ff235f-b168-47a6-a9dd-574d0dfe11bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7d8dcdec-7f81-4c26-b565-ac5027dcaa88)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14057268-a20f-4347-8026-cce65671da4b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a83fdfdf-29f9-4de6-beb0-f710112ceac6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10e622c1-3982-40ec-a764-677b8e33bb66)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3305162b-0bbd-47c7-bbca-a14bfa28f98c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c97e2d96-838e-465c-8476-10baf98ff56b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 089486e5-4734-41a2-9f86-9c51226eaf61)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3032f418-3d00-47e3-8b25-0e55a06c214b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8265ce0-88d0-4d77-8c06-22094f90d417)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 73865cdb-f5b5-4e9a-b980-55417204207f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 62a58bd0-7570-4d1d-ac30-32d631fa741f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 8f227e3a-4252-43ce-b379-63adf5aa6c81)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 73b33750-2f9d-4ee7-a230-abe6c359c7c1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 34b9354f-ea0d-4233-93a1-aeed252f4e82)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 12a336c7-1bb7-4955-bbeb-bf656d8c9714)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e329db60-9699-46ce-a2fc-2f2684f2177f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3d7d6932-f84c-4e6d-9b04-56597d8dcd0f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ee1c00c6-ee96-4f34-9497-d26a76ab08c9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e4543d3d-e241-4e48-855a-a164642a02f2)(content(Whitespace\" \
                 \"))))(Grout((id c19bc314-554c-4c96-9a14-ba78f4a5e996)(shape \
                 Convex)))(Tile((id \
                 486eb615-94d0-47d2-9cc1-cf6d1bbb01c3)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4d0ad19b-4d1a-49d6-a781-80648a391782)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f6ed616-ea89-4e22-8547-17239727fb5f)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 347c842b-639b-41d5-8035-caed0ffa96c4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8b370470-b0f8-46d5-aeb0-30cd6f45c458)(content(Whitespace\" \
                 \"))))(Tile((id \
                 027f4179-2cde-44bf-b275-859944184c9b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 61e07151-1ae8-4b41-8d62-b27324276f70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd2ca79e-6c91-40cc-8e40-e4e0727d3c7b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 09ef9550-6ff5-42bf-b1fe-edfab42d9ad5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f6472044-056c-42db-90cf-c740f054efce)(shape \
                 Convex)))(Secondary((id \
                 902c5677-8d9c-467e-b4c1-ee2e8b3ce9e0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 22a22f44-6ac4-489c-b01b-53ce7c7a9cb1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 599ed7c2-0374-46dc-86bb-4b9de6199a81)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eba2658e-2487-422a-8350-cdfafd80eade)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7c5abc0c-ad0c-499e-952d-0c6306d12b65)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a37b5193-bb63-4828-9ad7-ed8ea3a1e5e1)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fee8582f-5afb-4a2e-a2a4-00a251f35e8f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a26f1a91-f878-4b3f-8057-9b2923e064e6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4195d592-d5d5-4c26-ab01-4099ec329a83)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3471eb6d-f799-4ea8-9ebf-8a5cd40ad349)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f211f9d-5388-4487-b9fd-3f2bff021acc)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f2e89f5c-e1a2-420c-bfb9-35cb823bf606)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 064dd49a-f1e9-46c4-9b1b-1b294b598463)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 ad356e13-77c1-4429-9986-1bd8efa0a00a)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5bad05b8-9e68-43e6-b8d8-df79a58fc7df)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc70fa7a-afb9-40be-ba74-868a18455bce)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 249c295f-e347-401c-bb02-b68d2c8ab111)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6a90a01a-8880-4721-ad06-786cd0b09739)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 73400e56-afb9-4d70-920b-daf9721a244d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b8660bca-9bb0-46ae-b46f-18841f40bc6e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2ba8834d-77fb-474d-aa0e-7d05d99b5219)(content(Whitespace\" \
                 \"))))(Grout((id ac59882f-b962-4678-b31e-23933aa74322)(shape \
                 Convex)))(Tile((id \
                 8e6f24bf-a8b1-4aa2-9a64-b8bda834cd9b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 11c7864c-e40f-40c5-8bca-25bdfa86da9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cc35ff5b-6e3e-40dc-9a84-da9c5d336032)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1a069287-fa17-4418-a50b-8d992a954099)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ab623ae4-78a4-4ce1-818e-c6d6a9a90ed3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02d4ad11-86eb-47bc-a14b-a18b0896aef7)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d9320594-0327-4ce1-8b64-6b3f06011252)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2435824-067b-474c-9865-0a979dd858f4)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a45b883d-fac4-440b-9e01-a0393821278a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 09321b10-5233-4112-8035-e8f211f40bcf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4e4e247b-972e-47f6-b83a-561ada7a5ac3)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 26145776-2e0e-48db-bc1e-a98cab74451e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5cf1816b-fafa-409e-8f55-2c5b5b3b983f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7577d18c-d1c2-49ed-8124-cc417735c3d7)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 85ff2309-e338-4e8e-b35a-279446cb5e5e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b05f3069-ae41-4734-9731-d5dd1bf8637d)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8a73d8d2-2981-42c4-9fa9-e880cd5b2bf7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2b180b90-e7c8-4fc8-9c88-05690372a208)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8e195af6-86a4-44b9-8abb-841a81677254)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5d305cfc-0978-458e-a8de-03779c6aa36d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a11eb46-14df-4e68-b2b0-3d849355d87e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 18011c82-ebec-43de-9927-43f4bd897ff5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 16c0ff20-2a2d-43a2-a8bd-c778effc1f49)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 19951bea-6c3d-46c6-823b-2d77077fab79)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 17af38b3-651f-4b26-85cb-2d42f80736c6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 73229b33-6a70-4a72-b216-08e795582589)(content(Comment \
                 #ERR#))))(Secondary((id \
                 51dd09ba-790b-416e-ab88-95027bdbbbce)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 ad8290de-2571-4bed-95de-73a32c84b05f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3f4a2c5a-f9ca-4a84-b475-be7cea2773bf)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 54c34e72-9682-4d1d-920b-d6bcd5284805)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4de0a9a1-a26e-4cfd-a089-0f97a2abaf5e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0b2673f8-1629-4482-8624-b86957af486c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7fc59c9a-61d0-4b0e-bc06-a84808ae57a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4473bee6-1598-4b70-bb4c-5b55471a1ba5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5c8e9b4c-365e-4dbf-8449-938997465ada)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e27130da-1813-49a5-a481-895f15aa4cde)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9cefc98f-bde4-4986-9880-931bd45d45e7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f01ad02a-515c-4305-bdb1-95879ab2516f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf83f1a9-c046-464e-8762-4b46d75579da)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a4e8752a-3fc4-4e4d-8e86-ab243c9d7623)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b1c90280-1617-44a7-b415-c10b3332ad33)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8b428f9b-3add-4b3d-8daa-610705792c04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95cb3baa-b2c7-41a1-8397-7235ed3e00b9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bda5a4c5-0236-4789-995c-1fa223c3b548)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 42f05850-b8dc-4ae3-9997-9fc88d5c9ace)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12658c68-6647-4253-8194-07747b083442)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 722d75d8-1a40-40cc-89d0-4bb52b81f261)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84d27760-7926-4508-8c6e-18c4ee8a7b5a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 613318c9-e2ca-4183-8583-db1cf4f089d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73a9f4e9-db84-4df4-a350-f3de8f7e6c46)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 be5f658e-8312-4744-8dfa-af215f4fe433)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff79575e-5218-4f2d-b259-141c9c5aaa6f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 99710041-b340-498b-8dc7-6220575286f6)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd90dc75-1287-4b25-953b-9fecf4818f22)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 452bd6d6-cc25-4002-8fac-4c02d6d2504f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 234664a3-e2be-4263-8305-9099fd67a0b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 59b9aaeb-0233-4fd7-a546-2e779e3e1e93)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 87c8a58c-ec79-41f7-b8cf-075d18290194)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a38a12c8-a9a7-40e6-8780-6f069bca13c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd250c19-2c40-44c6-8754-99f214086712)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a0a3d7ac-bb78-4832-9561-47fcb6e93693)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb91cbe7-25b3-46cc-b905-5594016751a7)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e7e48f7d-8daa-4aa7-8f0d-571caff59131)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 0a18de4e-c9d8-4201-b388-f03bf0b52356)(shape \
                 Convex)))(Secondary((id \
                 732e1a2e-3e3a-4f5d-92e8-8afb5f661246)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c4cc9be4-8f81-4310-a083-29ee75cbccb4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 af24eccd-1d1a-47e6-a0f5-4b3c6ec7ee85)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b064c62-80c3-4616-ba2f-88fa68c0a202)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aceb6789-8673-4765-9665-d90786f76a6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30d1d37b-0b3b-42ef-8e53-df6b67d5ea39)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ea9c5301-2c7b-42a9-b5c8-d9ed65e857a7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a462733-acfa-4855-b127-1f6053618cb0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aa4e6b28-e7ee-4752-99e4-3b7a83abe97a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1ce62479-4151-4d04-af00-1151c1301520)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b4c3ff8-5849-41df-ae5e-468da9daa206)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 142f3d4d-92b0-43f4-afbf-bfcf6d7b3021)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d60c42e6-68da-4224-abf1-83b04e2dfbfd)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 7bf7b1b1-0ea3-443b-9c81-50a232e420c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 515ee9eb-405f-4649-961f-e1b182fdd413)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 617a1f2f-84cf-4a82-91e0-acbb6214810d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2bf4844b-7311-4269-8975-f15080f3d24a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 677baead-b2f3-4072-8c6c-20c666f44915)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 efe9aeed-cf12-4bf7-8d1f-f7eb7011f1b3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 017ae7bf-cbf7-4677-979c-51ffc7015eda)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 273c62d3-8456-42c8-bc90-bb12b2a98aee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0bc4de98-3ef7-489c-a6d8-942e695429d8)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 903ad891-5262-405d-b693-7142cea8ab12)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d79ea62f-4d3d-4d4d-ad23-9f9ccf3a828b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8669ddb-fe4c-409d-906c-0d8a653a1f10)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6e7c67de-007c-4577-a3eb-2a2acc37a6e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4c278f5-1c2d-43c7-b4cd-1f504002fa50)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 785b0a9f-77ec-4275-a975-3d9f9e3467f5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 eb860f5d-41ad-457b-a701-7388da39e4fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0f845b1-a074-40bc-84d5-33129d05cd68)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3918c78b-ced7-4ce9-b8c7-264aae1cb58a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 41ad3676-d0fa-48b0-b444-4fe36ae1203c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f6ebdeb-5240-473c-aedd-2f587701769b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d016f960-ca4f-42a7-8235-1473b0450adc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21b9b56d-b17a-4ce9-9182-37c68430287f)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cf41b95f-01ca-4a82-810f-ffaa02dedbdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bb23e017-a6eb-4d1d-b100-a49b6416f16d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 807e0578-92ef-4b95-a767-c43303865549)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 17bf24a4-f8c8-41b0-96cc-2bb920508cf2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e7abd49-5431-492a-8261-db62c1a8a567)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c6df9ba1-2f52-4fde-aef1-c526bb31ae3b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 85f76fd6-585b-4aa4-9c4a-73c276f59aa6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 68b1cdaa-b35e-47e0-982c-ec56f2cc4468)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa51c12d-34c1-470d-9adb-314b649e98dc)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3cdd3151-dac5-473d-b720-5a92fba1a6e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d50216f3-d033-4c23-b91d-d9de72cab2f4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e06cccf2-2759-446f-a994-5c740626d5df)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a9f4f600-dedc-403d-99c9-23dd77aced7a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a4cecbbe-6179-4c57-a7ff-24d13946ed29)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 40a40e91-726b-40d6-8ef5-7ec975701cbc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11f516f5-1b01-45e1-9563-c2d7e3bd7c35)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e2975abb-321e-49b3-8bfe-ccf99ac56a2a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fa595ba1-707e-44e3-b2a5-85516f5343b8)(shape \
                 Convex)))(Secondary((id \
                 8a165a42-4291-49f4-9f54-85e4f83a72f1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 504f1b1f-0e14-453b-bac3-8a7cac80de11)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 aa18d34a-6d1c-4d16-b9d8-6f587b0666e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52e90368-6a71-429b-856f-0a4cf02b84a9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1e6778d4-0c61-455a-a03e-b4773eca3cab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd5ad231-64e8-4e18-a17c-ad8036266aef)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 cd175759-db4e-49e4-94db-654bd45d0abe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5187f67d-40d7-4878-a147-e5d3257d6ac9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d270c6a-0d57-4280-a922-6bad121ff8d7)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 094d7196-9f02-4a36-9a4e-0aae0c0753d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4646107-26f1-43c9-a853-c4885304e4d2)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 26b9a6b8-a286-4fb9-b370-4741e61504ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24e0e9cb-8fd5-4e44-8cd8-48b5d5f903e4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 df0d0184-656b-4778-9489-1966c81d9f31)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 851f7bc6-f2f1-4a21-ac19-263d1e8c61ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ef2c7c2-2fb0-4a2e-a53b-b61e60a8cb06)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3d6608f3-7b7b-4bd0-adc8-766434022cbe)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a6bd87ec-0389-4393-8fcc-b04fb0b721a2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 99061b79-b86a-4e18-8805-9595d4c6405a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 faa774c4-9818-472f-b2fb-62fcb29cfc96)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8b36f8f1-c88a-478e-b3ff-b4c25e90a820)(content(Whitespace\" \
                 \"))))(Tile((id \
                 47b564b6-8ce6-43fb-9c5e-e10371d06587)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e7f08216-0ae6-4dc2-9421-9865c407434a)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f34a9f29-3b7d-463c-b252-d3302ba8734a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dcb1616b-ab23-4fb5-aa30-17716f4cc167)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d9e0a79f-18c0-42bd-a6b7-926780cb5c0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 401850e5-1b73-41f3-b547-35432228fcf0)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 67ece0dd-1f14-467e-8ed5-9c66d3428e3c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 7504da28-9452-411b-9a17-2bb851f644bb)(shape \
                 Convex)))(Secondary((id \
                 98b86941-faa5-496f-a0e0-bbc45ffa0c1d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 33f001d0-70ec-4748-bb35-63d1aba91547)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f8282274-c143-470a-8f46-87651fbabf14)(content(Whitespace\" \
                 \"))))(Tile((id \
                 095e0135-7b46-4e7c-8811-dc1851a69be1)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b6911bce-6351-4bf7-9064-18697d326018)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d3d04a7-b7c3-448d-a270-088900211262)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2c64dcc1-2f1a-477d-b936-dd41ec0c53c4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 24a3a009-b962-4406-90dc-d600b918a90e)(shape \
                 Convex)))(Secondary((id \
                 2a4ed725-999b-4212-aa7b-b3e16ec48871)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cc98c5d8-c11f-49c0-bd30-be2ec67d02c7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 03668c40-db20-4d2f-a7a6-7f70d26acb2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2c1250f-26da-4cb3-a449-91251447ba7e)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 602d5a6c-3e11-4782-a6b3-f0ca6d58039c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdcebcd9-4eaa-4c5d-997d-8a07117779e0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a3b20fdf-0868-4379-aed2-36ac05715cd6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1c10418-4b39-402a-8f84-ec1e29a393a6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b6cee768-7995-4b7e-a327-285b21669208)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 998aaff1-ee52-4655-9611-a219a2bf6533)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45608d46-3748-4feb-adc7-079c2005ba5e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 aefac3ae-f1ea-4988-8a60-c65a4a2a6b5a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fe1b8413-8601-406f-9e93-1575fe0e6dd4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d421dcf0-293f-4a3c-8b2f-4641e1da91a3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b7481fe-5d0d-4ffe-b169-9631b5707fb4)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bdc50b22-1dc8-4def-9fb9-e0c59ee0c980)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4af43792-ee03-4c0f-9db0-3e39e3cdaf1b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 40c3c3ce-acd3-4f8a-a044-e68c77acc46b)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a58c68ef-886e-4900-af15-926fe8c4cc4f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 90ad9f22-502a-431a-b4ab-155908d6043e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f0387718-fe5c-4041-9ae8-3e47dd94a3bd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eded8dcb-51b9-4f6d-b43b-a1538cb9dd23)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5bc7bb0d-8021-461b-a050-47de606ef885)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6d080b70-c970-4f42-b6f9-c333d22ad93d)(shape \
                 Convex)))(Secondary((id \
                 cc2e8a1c-a6fc-4aac-b9f3-6cbda0c67d71)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 45498fe5-1216-46e8-a1a4-3b85b8feb3dd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bd5532e8-25de-4bdd-9a05-05c01fcd7bc9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 031661ca-1335-43a7-9bdc-584a5818e2e7)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d257f73f-75fb-4572-b45c-1d0dd0359ca2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c24289c9-cbf2-489b-a958-28a248d9ffb6)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8a70dbad-3a0c-4cee-ad6c-77d300644dd5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2cfc32e8-0651-4890-9653-d9effa1d401b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b4fba02-82f4-4d33-bcd6-c012fa8bd4da)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2a7f14ac-13d5-4b3a-bb48-535a35aaab9f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 31210854-daee-4c24-8ad1-56a66095634e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7438edf0-cfef-4a7f-bf59-1d9e59ea27d2)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c70b5f4d-ff29-4582-964c-826284c86707)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d97f2e4f-accb-4863-81cf-df0204017cae)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3815fbb5-527d-453e-b797-e3d75b80bdfe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 942e3129-418e-4f6d-8ed1-2f1bb2f73d26)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 33d5236a-93c8-4cf0-926b-e59c8b026874)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bf9037d8-7525-401d-b2aa-b83b574d1a2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f16223e4-fa80-4ade-b0d3-2cfe3d519c59)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 56a4382a-c56d-401d-a512-ca181aeed42b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6366c92c-68dd-43ce-904d-bae1e932d4a3)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c56ec500-251d-4a41-889a-b14c07d56023)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f7cf07c-8cc8-40af-aa89-5aa1f4e03571)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 90ab6414-b38b-46e0-a980-d1ab0046b770)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d9eeda6-9e25-45f0-9209-736c42f39722)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fc0f33fb-dbc2-47da-8894-1f8795edcf08)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d0fd6bc0-5479-4445-8f67-64f0b10726b6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52751d7b-cd2b-47ec-862a-0d39cde1c74e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8edeb649-e6e6-4afe-8ae3-9128f992a879)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a379cd42-5417-405b-8b1d-fbb7bd149359)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 41c0dbdb-8855-48f7-b886-a2f9d0e7efdf)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4570c478-1132-4afd-884a-ac966f90b9ed)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fd2a4fee-2fdf-4691-a4bf-dce879b1d099)(content(Whitespace\" \
                 \"))))(Grout((id 50b58ce6-dcaa-4dcd-b2f4-3bf98b237516)(shape \
                 Convex)))(Tile((id \
                 77d9cfd9-08df-4993-a05b-8e02fb7bc3c5)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 cfd7b574-2177-4e86-b90d-4633da234746)(shape \
                 Convex)))(Secondary((id \
                 df9a6b69-9e17-4895-83ed-a05163e0f43f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1b220dc5-da9c-4b68-9638-793f83f0a0a7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8ed2db37-55f5-422d-a31a-ea327ffc33d5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b437da5-b3c6-4816-b77c-9445934467e9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4653939d-4461-46a3-8339-0f49dacf62eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f6609efa-2a6d-4a19-b480-c00fad20c3c1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0c7c5512-eba0-435c-9d1f-5e680292b254)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dd921232-0d86-4c1f-abcb-6c6d28b0cddd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e91dd902-4d7a-407e-8a5f-431792414a78)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fd7444e2-95f4-4b37-aa52-d6f193b85656)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c71dedf9-6e7b-4794-bdbd-b158569523d6)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e3278e35-7137-4a59-a542-0383f626697b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a128ccc-2d58-4bdc-ab83-6444294b33d6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c7091938-9f47-4b19-9d95-ab7c59bbfa13)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e4c18b25-05c5-46ec-aaae-92f43086c16c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae4d7960-928b-4e61-8ff6-535ac81bcb41)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 089c8f0a-8ab1-422d-99d5-d3f2635028b1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5032e13c-3cfe-441b-a7b6-80ff2989d312)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9ebd768e-8826-48eb-9236-e082e91666c2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa1d1c63-3c80-4cb5-af90-1aca36d4fe55)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ddb49962-3f3b-47e3-8b39-6ddb0f4693fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e03e8e9-7ba4-4b13-a371-e50108507966)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0edeb52a-9a9e-4ee3-bb3b-422e97a6812c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 da6d84c1-b3c7-4c08-8c3e-53cd4ed51217)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 83086448-a768-40b6-b9ad-7688a81c76cc)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 847a44e7-ec6b-4579-b0bd-205afde9f2ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0a876d4-9900-4e8f-8d28-406487e2afde)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c4f93f8e-5f5d-492e-b92a-cf4a9c1aaded)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 28e46220-b8ac-4c80-a852-a70bca8b4595)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6e787ddc-5df2-42ce-add0-75da715d381d)(content(Whitespace\" \
                 \"))))(Grout((id 2548da20-97ea-4924-8549-913022ca650c)(shape \
                 Convex)))(Tile((id \
                 494bdaf7-ef4c-4a29-93ef-9621d40e2566)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 243f1f1e-7634-4e05-b3c2-f8ccfa8d451a)(shape \
                 Convex)))(Secondary((id \
                 67eab96c-1ad5-4118-91ad-058ca002b969)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 18046a27-9d6d-4dab-b8ca-6b654580aa9d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4eb699f7-be6d-40e2-a964-f156d53f75db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7ecae57c-5287-4211-a733-b1bc73d75c3d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 53922ffd-2585-4c1f-b27a-4a093d851957)(content(Whitespace\" \
                 \"))))(Tile((id \
                 276e7d21-2be7-4a03-9c89-9ac85fc92ab8)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9c84b379-be40-489b-8634-bc9117497a7d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 cbb78acc-e1df-4be1-ae3c-a0398f64dad6)(shape \
                 Convex)))(Secondary((id \
                 5d79f1e0-1a96-4ae8-a11b-b266b1e54cbd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b5781aa5-3d97-4c72-bf15-5779c52fb0dd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c5b3557c-57fa-4f73-a942-19b13a807d13)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02816b9e-2070-4831-a62e-0f9e1a0d54cd)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 818c2621-6f25-49bd-a432-0f87ff0a6b3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c08a004-490b-45e2-8ae2-0b95140b3bd6)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f5f99496-0e6d-4551-8f85-3e13f60b3dcb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0c7a3c4-3d44-4d50-9518-b7f44037dc90)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0a60f3f2-f173-4c14-a6c3-902a09dedd77)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 28d8941d-cc8e-42f5-a8a2-be4bd9b239a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72e2d15f-78d4-41ea-b2e9-92c3803389bf)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1abddcc6-ee9a-45df-bdb2-0b6d6f8b2523)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 07b58828-8ae2-47f7-bdeb-3787cf3b829d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b3bf4684-379c-4e4d-a0fa-32723b9aaf7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 28587263-eedc-4ca0-9b48-ff07adbe03e3)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7bc3aa69-c2d5-4a28-93c2-fc8ecaba3baf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 efd7dd65-a671-44a5-9320-99bce4c8a39d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8d757c54-98fc-4557-a822-d02e0a5c6acd)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 17cf85d5-d34f-4ed8-b598-cd75c87c4b48)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf202b9b-fc99-458f-aa0f-d8087d4735b7)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dc702d2a-5916-4b33-b0e1-008680b66ee1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46d339e0-fb59-4107-b6c4-adcfab15feab)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2bfb91e1-6210-46f0-a04e-a10b224b067a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 fdba4b2a-9aad-42de-8800-60e7c3a8325e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 db1729b9-8b9a-4f5b-a09f-555562f420e4)(content(Whitespace\" \
                 \"))))(Grout((id 127e2f31-d500-4a58-8f2f-fd9ada1d9fbc)(shape \
                 Convex)))(Tile((id \
                 b5cd21d3-9ba2-428c-a25a-74b4e903407a)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 db2b6d2f-a2f4-4706-9d67-5280533e3b9e)(shape \
                 Convex)))(Secondary((id \
                 f8200f4c-b249-4365-ae90-6361f6d03a71)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 89e92784-1b1b-44c6-9955-41fc93e89999)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fd26bb46-fb57-4c5f-a029-547a5de74450)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2942398-4d98-4943-a1fa-568677c68dc0)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5f52a517-92e2-4e83-8956-8a1c7ebc030f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86227fc1-6d7b-4f32-a353-6fc919e64878)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 aea1cca5-5b9d-4cd9-a8a7-317f5bf546ef)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bd4c6748-0f85-42ec-adc4-82933a76d602)(content(Whitespace\" \
                 \"))))(Tile((id \
                 446a952e-546d-4238-b0da-072132835c88)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f058d21b-07c7-4f56-94cd-3872332df990)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bbae0b2d-92de-48bb-b669-4743ed0e03c2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a10b9f4-00ad-4488-9cd9-26fd24aa5ba8)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bf9f7569-e697-4687-9d65-ddf1b6d59d48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b590974c-653a-4d43-92f5-355c6da74a90)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 64aebd2b-e4cb-4f45-a453-c7459e2dc036)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b48023dd-7147-4f8d-85ef-e8a0a10c39f8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 949a1415-e70f-4d2c-96c5-83a632ab62a8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8e8481e5-e793-4ad9-ae51-c1358cb4016c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e9e6d3b-85ac-47e1-ba65-babcb084037b)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c11e9532-4a25-427f-8799-6fa70d2ff9d1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7f781205-8d85-49ff-80c4-eee4d6d97c91)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9c66681d-2eeb-4a5a-8e6a-946a39999a1f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b231d77b-b14f-42b5-92e9-2688620625c0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e9d08825-a215-4978-966f-734918f55386)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a18c1a7-3e59-4c7f-a820-a478e5da152c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d6e57151-2cfc-45b3-8c36-480ded899293)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 20a6a318-32e1-49f3-8b99-227ae0fdca73)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 04706e93-f8f7-480b-81bc-20894e6cb9ba)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d0b1c5a2-d1f4-4d97-9d8b-cb8a5eb150dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 58a7db64-9fb4-41c5-8641-c72e7bb1cc97)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4ec148f4-0a90-4443-ad8b-74293488de43)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b7b3cd8e-d3ca-4f30-8d1a-c8d07c2ea0f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6066e25-faea-4208-85e0-7e41ecc47596)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 183bb964-728c-48f2-a785-0547d65966b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8362a309-8274-4c29-bbc5-ab3e9c26d3d3)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4d8dc444-1b54-4e1e-86f8-191fce0a38d9)(shape \
                 Convex)))(Secondary((id \
                 bf575c7a-a522-401a-afbc-d575461a4d6e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4b3c119d-a748-4756-a0d5-85b8b0a1e2d3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2d45a44a-34db-40be-b011-1babe53281b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34cbc875-5266-4c1f-bf1d-b6966b75872d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 53129971-5dd3-4c68-81e6-dee8f85f2bd8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d4ecf92-6741-4118-a0e8-5cabfe6b2509)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c4c34c81-a12b-4c47-bcf4-24c892915aab)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 31bfd215-26f5-4c24-baad-f298e6529e75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c06e239-1af2-4a4e-8f37-2b79bf8126a3)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8bb6a892-7910-426b-b1aa-2743627dd055)(content(Whitespace\" \
                 \"))))(Tile((id \
                 036ef256-a328-4d58-bcfb-d01b7a14557c)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c511563f-3e1e-461b-a3d3-328a3d51ad55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4423ae0-b3b3-4fca-80bc-8b92ef3cd11e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 422edb55-e9aa-4d5f-a52b-f1d695dddaf1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0457a758-8e6f-45f1-be34-ab3668306fec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e13ce4a-bd1b-4165-8f59-943f93ee07fb)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 271d92be-6cb9-4428-9641-cb41f02d7064)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 da402110-6486-4501-b2fa-9e021a28b715)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 bfc9961e-ea69-4c41-b359-b85901fe9276)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a434c43c-79a8-49ba-bf10-e56c3803de95)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 711b194c-ccf2-49e0-be9a-7830b5aefa09)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e74f4cb7-909a-4803-9a74-7310af8468a2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a0566019-4bd5-471a-8cbe-8fd989cc9119)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 acfabc3c-f94a-44d5-8591-2a95915ae48b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 57c1498e-ce1a-4184-b4ba-eb5b402be3c9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 15ac24c1-e74c-4cb7-9dea-320d3462857b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d61f1c55-75b5-4d98-ae8c-30c6d6e986bc)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 675913b6-6d1e-4e4a-8be1-f93d12dd4577)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7d314a9b-6fc4-494e-aa5d-320785250da8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad924ed7-c5c6-4e54-a678-785b8db92cd7)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b2648e7b-8630-4521-aeca-acfd720ee8c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a50980eb-ca7a-42f1-846f-27e1f1887168)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 a52de554-6059-421c-b9b7-c4924b4f7fe7)(shape \
                 Convex)))(Secondary((id \
                 c7d16467-73d1-4067-8cfd-ecddc08b557f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 90b58b63-7c7a-4749-ad20-3f738b517c45)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 644a7947-3ecf-437c-aa4c-775f1c02eb79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bae707b1-98db-4f66-9098-8f28cd0f7f78)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d5030e6c-6f19-4db3-89dd-6caf5598efd1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 27e4ee70-d32d-4346-a670-7fd603780f85)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 716c5c2e-4032-4835-8e79-768e2c15efff)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4057b158-38ae-4551-8151-e7758f0ee395)(shape \
                 Convex)))(Secondary((id \
                 93e04239-cd94-4c7d-92fb-c35963a627f8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 26bcd357-7fb7-4450-a58d-35ce6db7b77a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 377558f0-4005-4125-af90-1500acb1297a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81b18609-1ec1-47bd-9489-30dcfcc72b16)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 688d8b99-ebd7-473e-8a5d-ee1874f2f344)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0cf5c4c3-a30a-4cd6-993c-4af8313c68c8)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1a3f3c3e-8cdf-4119-bd98-74f475c39154)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18cc334b-7979-4fd0-8f3a-d81a446e579f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 67ed4b73-9234-451a-9a49-7b9083e23dc3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3297a8c2-05e0-4bb4-b12c-4fe0b1af87f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e516ab4d-c132-4121-8ca1-fd742929248d)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e7590fe4-9a7b-4e21-85be-f3e9bcfbf1d0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 66578f2d-ec37-4da0-b4ca-77a0c0fff5c1)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 076a5765-4b02-4b97-aa1c-511b645696c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23fd7b84-2946-48d7-a1aa-1c2f11602966)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6e19c268-6e62-4520-a24c-78ccbdfe2a9b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0c2dab4-e08a-4be3-9f46-0fbfa8963631)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 47925fa1-9939-41d7-a39e-8b42d732b6da)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0f22711f-c457-47fc-bf30-f25e2c617840)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6b37a382-7805-446a-be1f-19d376253c59)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bb57f068-cad1-4cbf-815b-f181036a9d9f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 825e30da-c005-407b-9e52-5c55f1f6ad75)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a97474ff-88f2-4aec-bf81-f2ad3407eda9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 81af5caa-bf60-4dfc-b24a-524a0754a7d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdc3ef83-0fbf-46e2-9880-9e6d2fd0cc2c)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 fbd2b206-56ab-4596-bf67-233021e3931d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f5af28bf-7dc5-46fc-bd2b-73ff7e17436d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3dcacc4d-ddd4-4651-9ef0-3daa5eed13ac)(shape \
                 Convex)))(Secondary((id \
                 9c431e22-04eb-4583-a752-e144e9bd85d0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7adf073e-7115-4c16-8491-6b9413c46795)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9496a1f4-5ae2-49d8-b121-c12053dc839e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 286d49b9-d800-4e6f-aa31-fb0764f5414a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 541bbe21-ef08-4761-b452-139c91e88a22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d783a1a-a59b-4751-9ae4-7244e5e28f37)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 92581c54-bdb8-4982-9173-629a17658d14)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2a464f30-2cf6-4e0c-a69c-86e8c103b33d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edf2cc09-fc8f-4ee0-8aef-9aa46f417dfb)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6967983c-85d4-4a43-be9f-a9101bac328e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8b39db40-fcc8-408e-bb0a-b1b5ed11fa10)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29f4d795-2172-42d5-82af-eaac1287d272)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c82331eb-01d5-43e7-8967-13ab2a16e5fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 842fb46a-270b-408c-bb1c-4aeb94fbf0ec)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1e0d0af3-0cc6-4b21-be93-f4c47e83aa93)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f3fa546-f4fe-40b9-953e-42c1c2f8f9a7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c5dd9936-3ec5-4551-ad9e-9c682c4549bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 83a458dd-d28c-4a31-a1df-219baf3c34e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 391ba9c1-fd2e-4e27-9bc8-b721cc51d01c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c720a91a-da2a-4e09-abf1-5a0e25544c4b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1f584669-6975-48f2-8d81-4576ac6050d8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 dfd5232e-5501-457e-baef-11db0000af22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6c94189-9e6a-4c70-8260-44b65bd645e2)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 25894833-09c5-4d77-a23c-29d4f7182c27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 303d5c4b-114f-4eac-a619-2bf251cafd0e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f0462ebf-5537-49e9-a212-614d5328f7f9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c448a18d-1be7-4010-9392-1d5a7bdc315c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4a852fbf-2333-4b54-8798-37922c3bf899)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 61ac8ffb-8d9e-47c7-8c44-92f4bb46e0ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3876da42-c7b9-4ec3-a4be-0cb97ea8686d)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9a947cfb-d070-4a06-8d29-ca7a9c310956)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f0817abd-718c-4441-bf40-eb718e28bc8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef8fd97b-3d37-4aa1-aeb4-de9e11847658)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f3a41c6e-71f7-4af6-85ce-d23f620c2bca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d5c84c5-05e4-4e6f-8788-8421ce2c8554)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0a3b1fd6-e714-4f4b-8cf7-faf8ab16538d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60cefee9-0396-4aa6-821e-bc8d498d3e67)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 597dd544-f8a0-4e42-a1d6-23ef4260e75f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 005fb929-292b-42b3-ab57-d51c1df3ce73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc66a306-bbe1-4f7d-abeb-6a108e5e7988)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 effc87de-1fd2-429f-90e2-890fd1dcf7c7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 da0d4ab9-7904-474c-a3f0-4654de5e7615)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f97727f7-9951-4e31-8cb8-f8dda5b21e71)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d254f3d7-0715-45e9-9c0f-7da4991b6c9b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 69847daa-9050-4db8-8d1c-d2e97ce70a7b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4749ad7b-f709-42b3-8f38-00b7f3f2b99c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53fe7e59-305e-426e-bb63-6ce61a209844)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9795ed66-9b7b-49fe-885b-5c9daae84f7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9472a44a-0e3d-4229-8bd4-28206357caa8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c5b59225-3418-48a2-b0d4-83718749bd43)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0d0794ed-60e1-4413-84bb-29d9402c7ad8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e24a0940-4651-4bc8-8a8c-a580d2e24014)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d367929f-f9cc-47c7-ac02-ab6eb5af81d4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a2f9d65c-620d-44ca-847f-a5ff88a9d893)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 89a2e2da-10c0-4b05-9a50-1e60859ef8bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83801030-4322-4e3b-84a7-7e9e79b83db4)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6b6d8fcb-3362-4b79-bed9-b0435fc79352)(content(Whitespace\" \
                 \"))))(Tile((id \
                 388ee9a9-4f09-4e18-b44a-6430b17d52ae)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 13e40292-2826-4a77-922c-e5099e13871c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bc4c7434-b69f-4334-a1b6-207a81f76c74)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 85e42848-06a6-414e-9fb4-caf8ea2100c5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5b39ceb1-01ed-4952-aa75-991269e44930)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cea3628-8890-40b1-8496-47f1c5b73519)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ce247a90-409d-4828-a49d-1b0f254dd5d7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 928f078e-f789-450e-afd0-0524ed28158c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f77cc55-8d8f-47e9-9efd-ed2706e7301b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0d044994-a6b8-45f7-8e92-defd6628e120)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e6d6788-7cdf-421e-88e6-1e238a6950f6)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e3cb0c0d-0bed-4ed4-a240-09e65b131bbd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df5a57e3-a3b1-46f0-b305-d6008ec4e110)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 03fca5e0-fbca-4de3-8e26-6b42c241f966)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5c01dade-12a2-43e0-96d2-3815777c7f2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6eb0fe43-b8f4-43ab-8d54-fa47ec1ffc0b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c100437d-2117-420e-a082-83663d864173)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b70028cd-a355-450d-b736-3b946b4e6783)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2f327e57-d84d-423e-a492-3d131a5b24f0)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 072bb0be-445c-4b2a-bc73-ccb658498b57)(shape \
                 Convex)))(Secondary((id \
                 b372b24e-dc4d-4ea0-960f-86ae7c8797d5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 74b10252-8c17-44cb-98dd-3ad14901577e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c2bda2a6-503f-4328-bb8b-4101be4a0741)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec937ede-6dcd-4ece-bd63-b2afc7ad73d4)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 600074e0-fecf-4d1b-8f90-dd3ae4b59e5c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1cae07a-d439-4f84-9235-5e26b9e16845)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2a518db5-b2e3-4368-afbd-6dc5d9edfa95)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a84a1c4-f2f5-4a0d-b86a-433f83a21b3f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0fdf56da-8fac-4e8a-a56f-8e797458af71)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 112ea1d6-2077-44ff-86d9-27ccadea830b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f8162bd1-2d7d-4fce-ad99-d569c9ae6607)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6156edad-e514-4572-ba3b-51470f6d42e7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7f60a9bc-06e1-45a2-94a0-ff0101006ff0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1d05e4ca-3987-40ab-a10a-b33f0fae9599)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97a33aae-1b1b-47e1-97db-275aa776a285)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1e82f2fa-6f3a-41cb-b2b2-cdaff178539b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd7df1eb-ce7f-4388-974e-dd82059ebbd8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5a8306cc-6639-4016-a021-9edf9cc271e8)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8742e211-1d21-4f58-96c5-7a54db31eea1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d54295a9-cfc6-4448-b2ba-95ea6a8f2ebd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b3bfecb7-13ac-4305-b3d0-c458d1f7dd3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7516d01-d713-4d6d-b05d-279acb6ff181)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f027ab77-98c8-4d9b-9517-a93da8aeb81d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 13e6c636-4363-472a-a210-65ceed9fddbe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ecee7245-9417-43d0-aea2-f8ea93b4510f)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c4c47f57-db5b-49aa-b430-47c0010fec92)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32978108-e5a6-4ff1-b2d5-6f662d530505)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 335dd127-1a95-4ab9-94da-4ba35ecb40c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9771154b-34ab-4ef0-b8c9-2385f799cefb)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a3f10945-67fe-4429-948c-7d7d1818cfa9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 68167459-7f02-41ee-8547-ef2526e3e857)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a2efd8d1-ff14-40d8-9bf7-a46858b9ef9d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 08205e02-074d-4295-90cd-4f3641c55459)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d1c7fe4-5fcf-4214-8567-01ba1fe514d7)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 63c6195d-e326-4865-828d-d51f52cf307f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 974217a4-9f7a-437e-9693-5a9ec3546c09)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a37829e-3be8-4391-bf68-7bf37ca54e6a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 27d9a13d-3748-490b-a242-1a3dd8445d68)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 43c1864a-2cb2-486a-9a20-3ee2b47a6d22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fbbc2e8f-6c60-4715-b164-b883ec045764)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4a731f1a-19aa-48ff-8214-b9bed79717c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c4d2a6b-5f8f-4542-83d2-a38f0f8e0543)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 24c5a505-e2bf-497d-ab1b-14b61fbe30ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14d7e4e2-a33c-4ac4-a582-05b50ffa47af)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d4011373-7709-4948-b13c-18559cd5fe2a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cdda8822-dd3c-425f-ba09-ab2a07ef7f3a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0ec5f18-de72-441b-aad5-5aee696bd16e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bf8ef14b-6bf3-41c1-9bfb-a221749d3cef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6e8efd59-a8d0-4eef-beb4-a5a96de3dc42)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 bf650250-8459-468b-b85a-2c5e2d0eefae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a27f8a9-dbc0-4016-bfcb-ac51f5061c13)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 14bb4ef8-07a8-4d34-89b2-0626e40bf597)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d17aacf-c401-488c-b307-2c16b6a63da9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1dd4a4cb-9978-4233-a12d-00c699bcb5a7)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 04e930eb-f326-4a90-bb5e-0ac9f40663ed)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a6579b57-7812-4114-b24b-eecf92d4e4cb)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cf829b4b-7be1-40ee-aaae-9596ec64f911)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ec29d19-bf74-4833-9d23-3b273bbe7bd1)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6c925d3f-8bed-42eb-b516-69b7d6090a56)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3a61ea4f-1bf3-451a-92ca-88266c880fda)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a4405184-842f-4460-9bc2-e018101e7f8a)(content(Whitespace\" \
                 \"))))(Grout((id 0bb04c8e-abe6-4124-a929-b2d17ba9e831)(shape \
                 Convex)))(Tile((id \
                 0ed40813-932c-4200-85f4-a0c84dfcf960)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 53cc1e76-d7c8-491b-8101-8579f698871c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4d0ecdb-afd4-4b48-b597-e68911a0cafe)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5dd690fe-d327-46dc-add9-135424b586d5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d480292e-e462-45ee-9073-1f595c39697b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db2f06ff-964e-4409-ad1e-be9d74c80a1c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 195bdcde-79e3-4eed-a5af-d30f681c816d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff015579-7d9e-4fa3-be63-6728f47bd270)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e0dadd11-b288-488e-89a2-b41d6e96240f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a98f71b6-04a5-4db0-a366-886b51ffc37b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d4878bdd-d59e-4721-8e65-bcbf58084d03)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9fba646d-72ea-443d-b95c-d55d097d0dce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48f2be6c-5b7a-47b4-9182-fd8f6cab04eb)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b1fac68d-77dc-4d23-b48c-5a97b21d5ca2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 764e8ecb-0dbf-4db4-9502-7f7221356c04)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aca4ccaf-8740-4496-8c40-30b48a239c06)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 40f3b76f-9f8c-409f-968c-249a47a8fc15)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ad2742e-6e6d-4ed0-8c51-f3d8373921c0)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e7343f9d-7607-4919-8a8f-baed75f87b64)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2e802b78-571b-429d-871a-2ad96d7eceb2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 6c9afa09-1581-465e-a4a8-92a10d81412d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa459aa7-f7f7-484b-95a1-499e051c59da)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 06adc131-fb68-442c-b25d-e295ad3f5b6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3a0ccce-6bd3-4c77-a989-bfcac43841ba)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ddbeaeef-f3d0-4f4c-ad9b-d6f706509ff0)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6849e592-ed94-4f92-a1cb-49064554b023)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4fa3d6f4-aa8f-46e6-8b11-74bf5df131be)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e65b95ad-74a3-455e-bcaa-19c1cdb170a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 303e9a7f-1665-4c69-b552-4e30ef233f34)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2363a191-a41f-433a-9f13-659195abe85c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b9de5db9-73cd-454e-8125-c64da897d8c5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ff608287-4837-4a58-b0f1-e92045125274)(content(Whitespace\" \
                 \"))))(Grout((id 02a42fd8-8ea8-4c1e-88ee-ea5a637c0bb0)(shape \
                 Convex)))(Tile((id \
                 dad83a80-4219-4e15-b4a3-5519bba1fbe1)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0d801b7b-6b24-4553-b125-fa013856775e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f180aa20-2678-4daf-b736-18e4a5c073ad)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c8e6b27d-a2d3-44fb-a90b-5548567aea4f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e16be025-3b1b-4742-8078-f6109ea43624)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce23b6d1-038e-43f9-8c8d-b6abdb2615cb)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 42f34b8a-6b72-4b26-9dad-d0ea28f72c9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3d55f2d-d7e5-4e27-84e2-0e067955dfb1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9b7f991b-5523-4abb-b7e0-5807dc0ffe20)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 46ecc483-0172-4c20-9eca-40501189b590)(shape \
                 Convex)))(Secondary((id \
                 36e5f925-f124-4f39-bc46-6a4eaf2f6ece)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3408a4d9-2102-4aa0-a484-19b84c87f1d6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bd544d95-9d62-40bd-89e2-fda3f9e79bc2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8ff2561f-d919-4ec9-93cd-36f87e76db76)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 eb321c36-85b9-4575-9c5b-14d34b689ee6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c8a854c-d7f1-4e7d-a3c4-bb60d38d6c45)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 12282bc2-c37e-4bb1-9368-3856a2294f61)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f99a0957-d2e7-4a9a-9e31-f329e297a93a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b44a5aa7-08c1-4d6b-a981-36b8dff6a648)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bbf637f2-80af-4dc0-ac5d-e08bd92a6f06)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d599b8b3-f3d1-4cb8-89e5-1b3dbb677538)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 289fff1d-deec-44ec-bceb-80319e391592)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f179e769-edff-4078-9163-ef5487d38020)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 cc35137b-3bf5-477e-847b-e3080d47196d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f642d57-e737-4b92-82ee-4fe9f29bf1ab)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0c0482de-b0f7-471b-a282-e0fce34f967a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e3a3ea5-3269-4069-980d-7ca6ff5b66c2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2dd7ec37-fe55-41c5-91a5-1ae8b3ca5c87)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 847b875c-938c-4a27-931c-c95c8fff35ba)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ad68935d-6586-4104-8d2d-933c92dd6723)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c4a3a112-3593-4c0d-b1ff-a1a5c67bccf8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0747cd77-1194-4526-b3d9-abe15e7ee029)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6fbbdc0d-f918-4a18-affa-f1aa2f8fec25)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 efe0097c-27da-49e6-b4e7-c4272efb9006)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f32f9049-a359-45dd-9b3d-6efd2f639fe7)(content(Whitespace\" \
                 \"))))(Grout((id a2503b91-a6af-4eca-a19b-45e7203ae6a8)(shape \
                 Convex)))(Tile((id \
                 e4c3facb-a250-48e7-9161-958e5296f7b7)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b6025a97-e52c-4683-96e3-b65f71bccb00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dcc9aff0-5eb9-43b9-ba0a-63323c9c9c71)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 24dd5f7f-257b-4394-a338-f1590ba30f56)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7de42aa7-148d-43b0-a5f0-6e7f91b14a33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f48ccba0-a20d-4af0-aac5-35345830560d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fed2c682-f5be-4cf4-a1c8-25db0198eed4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26f04dd4-3c0f-46b7-a27d-a0b177eb67de)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a6b7eb67-730a-481a-8bf7-5bd9f382a26f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 daf18e0f-c04f-42a2-83d3-8b31371aff77)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63f37eb9-6293-4527-bd7e-eb907f710e87)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 94ff4bb9-527d-4ae9-80b2-87c439031dd0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 62bcf342-3113-4100-8fc3-b5e088ee1a8f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 157bb22e-1f5c-45e1-9fdc-d18495daf670)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0675173c-6294-4b92-96a7-c8b0a181935d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13b83683-95ad-4cfa-8903-632ee535e47d)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f5629953-0d0b-49a4-b015-c5e110dde1d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b09db7f0-480f-4e12-a181-162b08bb4241)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fe7578e3-854b-4bd5-af08-f3fbc04d2145)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 53f15312-46d0-45e5-ba28-15c26d62403f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f928a56f-7573-4f8e-88f1-84bab99c00d5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8587a2be-1269-41f4-af74-cc7e4fa9ff09)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f85a6dc4-0e8d-4aad-ae57-8d8436c8e3a8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 56e73c8f-756f-4828-9a1f-3378cf2f90d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 faf8b184-3bf3-4bde-b448-9aaf2993f18d)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bbc6a3f4-e577-43ec-983c-ab782d3ec5c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 070b09b5-4830-44fd-a4ae-1a6377f6af4a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 00daec8a-eb5f-4d7d-9fbf-adfe46a96a69)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 03852bc9-f6cc-48ca-a926-ed0b487741eb)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d82c535b-41a0-4bc1-943d-d82e2ed9a431)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7a656baa-a757-478a-aeaa-ddd31270411d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fce64949-c49d-4b87-b463-60d00171f609)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62d2fa9e-3d6d-4a8f-9554-4875ba149387)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 224b78da-ecad-4ab1-9c69-ea1e11a09fd6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f41521d4-ce7b-42f5-bdf0-3f2d88bcade2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3554165a-cca0-409e-98dc-af8781fc10bd)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6fa8a366-f9fd-4d8d-a40b-c1df7fa51680)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9fdaec51-b3fa-4eb6-b8d2-efc603db6f03)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8a1a8fa3-2824-4517-bfb3-f89d0a8b68c5)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 7bc8e7b6-fc21-447b-867c-ba895a77577e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a3d89e4-9b40-4f05-ac05-2218ccea49da)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 91b66132-3ec6-44b5-852d-fa190c3e0223)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1b282450-82f2-48c9-8b92-e2233795d70a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73f9a55f-ee30-4399-80b6-5112b1dba56c)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5861acd3-7ff0-4c56-a384-b17b07b64925)(content(Whitespace\" \
                 \"))))(Tile((id \
                 876f8a33-37ae-4ef1-a26e-5705beaff45c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 123612a3-7c07-4787-b559-af3cd6427fe6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4011454f-657a-4474-a84d-e6c48bd8d228)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c404e8c9-da4e-466e-ba49-e284c7c520bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3702d74c-17b9-4001-a639-af1ba4303f5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 527ceb4d-fd6f-430c-90cc-3e87888f98fd)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 435899ac-96a7-4d5c-8620-fa37f18f9141)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e21283ff-888e-4736-85fd-4a3a0f9d28da)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7ea69a4d-9db8-45f4-b660-4a72d99d3e2d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e2366647-9856-478d-b720-c6c146d4e2d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e8bd7e3-67cf-4ba5-98a8-d550f8f00ddf)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 7c0e61a4-1b78-4025-805f-95338692c4db)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22636117-64e1-47a2-8e1a-930cdd1ef098)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 43e3bdd7-ed82-44a2-9faa-0846375b9ad7)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a9dcc391-39f9-4edf-a10f-926356e70ba9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10826ebd-351a-4792-9c69-fb19331ba8ac)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ece575f4-8db9-488f-a1dc-7f1f64e6043c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 eb63a5dc-953a-481b-86ce-48edcfc110a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db5e95c9-8c84-45f8-a5e3-7cf4d6d2588b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6ac332c3-2f46-46cf-9594-a3d6917922f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a69b3935-6b78-4f7b-a6ce-518ca756e8be)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5d8ead79-3187-4d30-80fe-ecbfdf2668eb)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 90afcc9e-29ac-4559-8d9f-78c715e74401)(content(Whitespace\" \
                 \"))))(Grout((id 0f946797-96c3-4b0c-b2cd-18b81b96772c)(shape \
                 Convex)))(Tile((id \
                 99ef680a-678c-4568-8cba-d4114062dc7d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0b2531dd-ae06-4494-9a0d-080d6ff78bfe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ddd0f64-0711-4d22-9dfb-d5f103355689)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6b2dc720-550f-4ba5-b011-6cd95a0efb7a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5670c882-98b3-41ac-876a-52946343f23f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6c03e1fc-886a-4c3d-81dc-686c5efd654f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d641108a-fd6e-4455-938f-0935d75eaff4)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 529bed17-9c7d-40f1-94d3-54ecc4676101)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1fc5089-ae9e-44da-8466-3c254c743cd8)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 54bf1d4f-9898-427c-bb37-dc9dc3fce144)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc3cebc8-2550-4a62-887f-7a782f4afc8a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c99bba8c-ac54-4f61-932b-41241b7ea514)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d572a1f9-ef07-41ab-b995-90371825e354)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0814e5b0-dc46-4e8f-9a29-5e1adedd28ab)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ee8cfd81-6692-4caa-acf3-01c5908006d9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c1290bf3-e569-4c6e-a1d6-7bf8d61a32ef)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 50836e31-8aaa-4f79-a5ea-ec43b6b60277)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cf95db7e-0e92-4702-a53a-cf2bf2ea0fa9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95405d20-91b4-4e1b-a92c-b49fb4dc1663)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 13998faa-caa3-48f4-804e-41ba32fa59d6)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e767c31d-4ca5-4d13-afd1-d7084186f135)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9b213aa5-f9dc-4f00-9fa0-f891a5407b71)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 383c9ff5-1364-4b84-9cb3-45b0fdf87072)(content(Whitespace\" \
                 \"))))(Tile((id \
                 80cd7241-cd75-4ebf-8d49-ea450eab5858)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 00e63c95-88da-4f15-9f1c-367cbfa6c1e2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9e560ccc-53ee-48f7-b47d-49a67324e787)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bff8169e-cee4-4fd9-86c4-c05cc1955261)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a8bc1097-3891-489d-ab17-b4518598b91d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa3f7f19-1b18-4ec0-a682-92a9c0d909bc)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9d5f9894-e485-4ef3-bb97-d7f5b26934e4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 212491ed-e210-47cc-a422-1c0ace8472a5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 85168211-5cd4-42c4-9bcd-01d855dff5f7)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f9927cce-0de7-47a8-9862-d0ca18725a4d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 dbdac474-b2af-4de1-99da-7151107f9242)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4cd50d1e-b482-45cf-a306-a1d98b2b0b98)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dc471392-063e-4475-a9c5-716d7fc246c9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 731a2195-dd01-4629-b9b4-df169accda0b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c93b1553-ac7a-4a54-b8a7-e00c5776ced3)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d1c7b0ca-2d17-4eb9-8467-d292c760b254)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd8a9fe3-90ff-4f28-8956-e4b705e856a6)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f81e9b44-6d05-47f4-9754-fc190bd3e0be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc56b538-60cc-4932-9ba8-51378473c4e2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cf654b49-8856-49c2-82c6-ac87a0337603)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 af72b2c1-7e57-4925-a6ad-db1a7ead0172)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dbd12671-8868-4d16-81ff-fda44598be3c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dd3ef622-4f0c-4195-8f29-f52d06e3c2f7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3caca40f-8694-4b64-a635-9486337cd1fb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4d4928d6-1623-4ba3-958f-c8f62aaabcee)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cee09db3-454b-4fca-a487-245afad494be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11d5cb74-57ba-499d-a63a-456b860fdbfa)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4ad8d4bb-cd97-485c-8c9b-b5d2cc40d5b4)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bfe97fa9-bd68-4c1f-926b-113e7d03af30)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 181f903d-b01c-49d7-9cb4-786dd9d0040d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a10e18fa-2003-43cb-abe2-ccf6ba4d652f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50f25ab8-cffa-48c2-8aeb-ed3dc65a787f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b1273e7d-ab97-4dbf-af75-0cbdd84a3584)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d20b58b0-4320-4f77-a223-da81270903ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cda01f4f-6760-4121-8f86-a3a0cd5c6957)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 8019a6b4-1965-4298-bba0-1cf421c9b823)(content(Whitespace\" \
                 \"))))(Tile((id \
                 64e1a9ea-52b8-4549-81d8-5cdc6072aa36)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 7b01f6ef-a4c0-4043-a706-82cd3b3293f8)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7ebd3ea9-3bf1-41f5-8ecb-d515ee60a0e1)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 83bdf1e3-c7d0-466f-9be3-8d2f1ed69da4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 562f220d-7e44-43bd-970d-d6ff4ac5266b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 f8bfa671-1d5a-4f6f-9a83-7b76f02cd870)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0a1de90e-3861-4773-ad9d-a48ce6c00ef4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f74d5cbc-432f-44f8-8ac5-3fbffadb09c6)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 44c4efdb-3dee-4e05-8b06-487d27499881)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f97e0727-352d-4a3f-845f-cc21bad0d6b4)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 35725579-b52b-4215-b23f-79ead179ec77)(shape \
                 Convex)))(Secondary((id \
                 797a6fe4-e516-4c27-baf2-44885f3050ce)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e32a2cc6-0743-4350-91c9-9fa6c788c53f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7088b09d-0044-463c-b1ed-47db6e6732d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6500696d-3da2-41eb-9c17-9969b85a8ed4)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c516dde6-de55-4e2d-aa21-50239c41d44a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 444dded2-f7e8-4d88-8f44-8e32fc79b957)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5cc7db97-4bfc-4302-838d-25a7ed750a8c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbfa704a-8756-4bb1-a835-698b3635ab17)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 03154433-4844-4549-81d1-fdee49b35a2c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 37462ff5-c0f2-4656-a370-5ce074e5757d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f40c908d-bed8-4df4-b370-754efa35c356)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e5f46fba-4574-4cf1-88bc-688c40dd7b81)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4a97b599-932f-4b18-aee4-c0d9c748da3b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cd4f32a7-0512-4020-980c-6c2c5ff8476b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4c5e1327-5c40-4c7d-bf8b-b757c53da6f7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7146f94-b508-4ac9-9b2b-9b6f65096bf8)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 90d840ae-1752-4324-97fa-dfd58ad6a640)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 64dcbb19-c5fd-47f3-aa4e-a75a246ae0c6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c0b29586-c02a-4bf6-946a-f799248fc9bb)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f4b5dd98-d68f-43b0-a2f0-169bf26d53db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1cb1b34-b124-40af-8e1d-f7b6c9b2273c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 53c6f026-f617-4d8b-bc57-88c61084b461)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d5bea74f-da3f-4385-a7b7-7451325817fc)(shape \
                 Convex)))(Secondary((id \
                 bab21d94-76fd-4d19-ba34-155fe5d5e2e2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2ab4c2a4-f3df-41c3-bffe-90741636fbf8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2993d12b-2d49-42b9-81da-fcf8b8699509)(content(Whitespace\" \
                 \"))))(Tile((id \
                 284b6ce7-7c57-4131-82d7-10f932a487dd)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4e264635-d7e1-45cb-a9be-47ee753cdfab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec79f399-e513-4662-a0cc-c716a171b25d)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c7c29a1a-c50e-4e46-a79d-320ee0aa83e7)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 ac57e319-0639-4a3b-b65b-d1fe7d1cb3eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 333e09dd-4268-47fb-b237-4673eb673e6b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4e48fc29-378d-4f6f-88a8-3d8c1c6c0f01)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 28e9054f-ba98-421a-805b-9d3fc52c50f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dea67420-3305-4b48-b379-4b47da1ed076)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7de37473-1729-4215-8a28-0e56dc834597)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf87fb40-e9f9-48bb-b588-46c66a9361af)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2b2a0274-8afe-448f-b5d9-f8c496d86950)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ab5a913-9a4b-4299-ac2a-b5dbff3b86e0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 39fd1af8-a756-4e30-9b16-736924de5b81)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fd01567b-b3b9-45fa-a93a-6109511f2a34)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65fd1351-17b6-40d8-936b-31cdaf35eefb)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d3f06d91-f474-4652-ad05-fbca7fff0c11)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 19ff1316-e757-4bee-8a09-bfe23cd19055)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a18421fc-259f-457c-8485-abae294187e6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3d793f49-ada7-441f-a19f-cbe5307f3b0d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7622ffd7-cffb-4fc4-bf4e-f0d597168351)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 53d20129-84e9-4f0e-9089-9a4baf8b3413)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8c2ec182-b369-45b5-9c0c-e50a704bf6d5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5819a566-f38f-472c-b2c9-c32c31f05167)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 73357cce-cd33-4847-89d4-bc47d298ef9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8f74c685-c302-40f4-b4c4-d3a807b2cb77)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c5c1f936-3466-40cc-9f22-3f14bfb628c3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 178a8f84-802f-4a25-a66b-033e76894491)(shape \
                 Convex)))(Secondary((id \
                 0b87ead9-dd11-4ce1-bc21-a4c83450ed73)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 86e68f31-5541-4f1c-b22b-dd0be7407b7f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 af43b728-9cfd-4195-8a10-523e03bb290a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41248d56-eb25-4d68-ba5a-f6787d3d23ac)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 67cdeedf-9b4a-4908-b7e3-a9acb640a3f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c13f8ea-bd34-4444-94dd-3201533ab447)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5bf62837-2add-4202-8a40-db463ec8c479)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 07940956-0056-45ff-8c2f-a268c57e2509)(content(Whitespace\" \
                 \"))))(Grout((id 2fb38c16-7178-4df3-bc4e-f2897ca54e67)(shape \
                 Convex)))(Tile((id \
                 4f6daa87-f8ff-40e4-a398-af3f39dc765a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 8e7c56e4-5890-4bce-9516-7d4518d47cca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 007451da-1eae-4a3c-8a17-99f56b384031)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3150ef9a-069e-4354-9483-bad95d361560)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c76555f2-f6a9-499c-92eb-f60b00a31989)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 26da9e0f-0c46-4e3e-b5f9-6ae317847878)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23e0cc68-9a36-4aec-ab0e-2a675cb56c10)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ef7f5886-af9a-4140-81d0-ba0ece3bf54e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5aa18625-7bd4-45e3-8a4b-df40e29e6618)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 92fbe7d4-cbca-4001-ba15-6d3a3535319d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f11ff69b-3fc5-42a1-8c8f-b25fb9a43706)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a65cfe6c-0d15-4038-b1d1-0360e72f8543)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 12aef2a2-f782-4005-b913-62036a00811c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d2fc3bb-e147-41d4-92d2-87b8172063cf)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8974616b-c166-43ce-b114-0dd90878e66e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f69ef176-f24e-44cf-87cb-07340caf8d27)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 500c0d04-f97c-4751-96d5-f9a369ce9fce)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 55cfcb7d-1273-4fde-8205-b8e8bf3591f7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c0ad735-6a27-4267-8960-b7465ac13ae0)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 b6be0329-90dc-40ce-b97a-b8e20504782d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 39a44a9d-976c-4151-ae59-72863ceed9d8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 19a21f2a-7f31-4f45-807e-963c0cf24abd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4648a1ec-1578-472f-ade5-04011d2155c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c158a52d-8f4e-498d-9bc5-a512505e829c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c85720f6-8b39-459e-aff4-699f499165e3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 935547df-197c-4361-ad0a-984093511b89)(shape \
                 Convex)))(Secondary((id \
                 f209f25d-4e9f-4b8c-9b10-aee6c396e2f2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a6bef6b2-7d08-45e3-bddb-c38ccdf5ad6b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c272c9a2-3e3d-4fdc-a55a-a85f52a603e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 150ff462-50ec-44d1-9e04-12d668db05e5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 cf492259-ce60-4984-9a4c-fbbbbc44dde6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a14e647a-abef-4ccf-ae4a-d8a66fef64f3)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fa12d8e9-19e4-406c-bcab-b293c1c2f10f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 34aba410-9d35-412f-ab51-ef9436e6e0ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4684adac-826b-4e9d-94cd-2ec9137363f8)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 4fb673e3-b70b-4347-ba95-0cc31cb245ab)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3e6164cc-445c-4c4a-8de5-712bdce95aa2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34d96ddb-54ee-42bf-8de5-6271d2d526c4)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9a3a5495-941a-4a7f-88d0-f856f638764e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1f570e25-8225-4aa8-bebb-84084fb6552f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86767de6-eada-4b05-bb73-13a10d3e8f53)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cfa2780f-c2fa-432e-a06e-0fb95ef8a73f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 99b5b99c-8c96-41d0-8c62-e775c049fd5b)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 223385b9-1768-49ec-aa6f-2f0e8105bc03)(content(Whitespace\" \
                 \"))))(Tile((id \
                 80166d94-0403-405b-a0d3-1b26eaef0c1f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aefcb30d-b0e7-4851-9c55-ec9adf4dfa17)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bd3adc89-6e90-449a-98a3-dc147143de23)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0171237e-1973-45c3-9b33-ac08f2426ef9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1fa1c7c7-6962-40b9-9f28-ba9dde9bee02)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7451dbc8-a21b-4227-b02f-8530b8c3d48e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dbff75e5-3972-4671-b545-8e69b854f30a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 293f8153-5734-47cc-9f61-cb2dd944cfd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 014897f8-da56-426c-b006-8d3ed30b3dae)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 0263ae99-c82b-412f-994d-3d260dd60830)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5ad49e36-849b-4a38-89d9-089901abf330)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 980dbc13-22f9-4822-ae4b-b107ae3b1d06)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 68c19308-e1f1-449a-bd7b-3279cf3fc502)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fbb71b1e-362c-4ccc-938f-812beda3fc41)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4afe387e-fae3-4793-8837-4bdf3dbbbd4d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 cb5bc9b3-ee50-429c-8e97-14626f6e28d7)(shape \
                 Convex)))(Secondary((id \
                 39b0ad5f-0c47-46bf-89d4-286e7f80a63a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fce65e28-9917-47af-ac41-5e171f2d071c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b22386de-7c1a-47d5-b131-daa7ac50833c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dbc1c401-c888-486d-96f5-41a748229bc9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 98230b06-fc1a-4927-bcbf-49138fc2d860)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d982e78-e992-45f7-8b3c-9393f36ebb19)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 808b3a0c-fb7b-4b0e-846e-b4f73bb4556a)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7ccf525d-4993-4474-a76d-78f8e987ef1c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2ae206fc-b51c-4238-9806-5f58a19c3acd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7dccb90-16ff-4db0-bf9d-97cd5f9fec7a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 51a30877-f8fc-4118-a649-c448f30bf719)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8bb87add-f697-4e64-af5e-31ed8d4f4f22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca80fc59-3b10-418c-9a3f-953f91454b9d)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e17965a3-e696-448f-8b17-d3f6c3ed7e6e)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e4650509-36d0-433f-b1d0-ba6b622c00d6)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 2a9adaca-7cf0-4dd7-80ad-f6fed6c51b16)(shape \
                 Convex)))(Secondary((id \
                 2f53482c-b03d-4beb-9ee7-a41e9ea113f3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 368527ae-a317-4afe-b54c-82d73b5542a7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6fb06b85-ee10-4ba7-80d0-d9114ebb9e74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21266f60-f2e8-44aa-83a5-17d7ff08e998)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 603bdb3f-8e4b-4a6c-89ea-374050908d2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 17b7d0f1-788f-483d-9172-15a734412f8d)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7d036e63-cd44-421f-bd1b-6a4c99126e99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62427d79-8e8f-4203-a18e-e0f00551705e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c6495a76-3203-4002-a6c9-1b3c13fe4459)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b564cdfb-125d-4031-a65b-209aab86d4c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19cf7b5f-3c46-4776-9a8b-a270fcc5e456)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8f77c1af-2bf1-42b0-a8b7-6daf5faa928e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d82c537b-b6f0-45fe-8699-51205a8d702d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e929d2e9-317f-4f9f-896a-f82fafb2c8c5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4fb1430b-078b-41a9-8336-37d121b8bfea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd16053e-8405-4916-b698-3d4278450c8d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 7233bbb0-22b8-42de-aea6-955ec2c72fe7)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cfb194ab-8f7e-45e3-9757-bd78c8625890)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ae7795d2-12f4-4771-b8d5-57bfb85e1489)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1fe5af1f-11d5-4cca-bbd7-c9b11c827e86)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec1798b9-3d3f-4ba0-98ce-5337803d13e7)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3f6e7d1a-c18b-40ff-bf80-81b15de30da3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 150a3e23-acce-412f-8ee3-e75234d1d623)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a44afe52-5f95-47b2-be88-555e7566761c)(content(Whitespace\" \
                 \"))))(Grout((id ce933915-6fcb-4805-a800-65c5f11b0a31)(shape \
                 Convex)))(Tile((id \
                 b119c1f9-b893-4fb6-8062-3344e39f72e6)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f577009a-8249-4204-8ac4-ebd35e9310b2)(shape \
                 Convex)))(Secondary((id \
                 684663d9-ac14-4dc0-9b4b-f204a88d3dc1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0edc27c7-7f1a-46fd-b7ce-39e8eb8c2bca)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dff1c15e-8947-484b-b2c1-29a708eb99b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74058b61-0283-4d72-8c9b-8f13022175db)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f9833104-da77-4855-b360-01486b0a357c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 43781c60-6a50-4c90-a2c0-0ed29fbec46a)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e64398d9-3375-490b-acf3-7b45bdaf7eb7)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 d60aa939-ed9e-4769-b222-72d4269de902)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c41fa77c-2cb0-4ef1-9e56-642e99200e16)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 da74c201-f0d4-49d2-916b-991fb9ff7d87)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 043fde92-83d2-43ef-acf3-f86dff3de922)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d333b71-4134-4476-9225-3d86611c2a58)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 eff664de-ac3e-425e-ae5d-8a01948af7d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 abb0e7f3-2a88-4977-aaf8-949d5569f4b0)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5ef9a602-1bbf-4e46-a73e-3f34d4b5880b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94601a4c-981a-4232-b0ad-fbbf3995a1c0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 14702a64-2c5c-4eb3-b7e6-28f96b2bb8b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 871ffc15-4689-43ea-9883-60900f2a0db0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfb6ebf2-d69a-4db6-937f-57a39f4057ed)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 77eca89d-fc03-48ee-a8aa-4fbb06cdb786)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bbdebeaa-b5f0-4065-973c-73d8d88bfa9e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f1024101-c19c-417b-89d1-02d15ebfdb9c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0dc8ab8e-e533-42ad-819c-4d0df06015f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 288ed190-3ef0-45d6-9054-db40439d158f)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 68e31487-29bd-4ab7-a729-3443ff333e45)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9a9f56ea-e72f-49fa-bbd0-1d77bf463520)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3cac3441-2294-4d1b-9364-2c17084f3145)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 01e49da8-c442-40cf-b446-856d97b4aa47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19f05a16-4608-4863-88ae-1425c6a415b6)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 da69f616-2fc7-4bf6-8663-d04cee13b58b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c842d041-c32d-4837-967a-e5a4836f9d63)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a176ae9c-7dc7-4040-ba5d-d293eeccffa2)(content(Whitespace\" \
                 \"))))(Grout((id 8a3d4c7b-3fac-4503-8b34-826d167eb178)(shape \
                 Convex)))(Tile((id \
                 84e55d2c-3233-448e-90f3-91dfa0ad1048)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3ba65a40-89c8-4659-bf4a-8e445d001149)(shape \
                 Convex)))(Secondary((id \
                 d091ed86-25ee-4bc2-b2c5-fce13187ec28)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 840d1d99-94b8-4b99-bd67-16f1f119f5df)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 411bd8ff-5c40-4ccf-9bec-8f68ed57fd02)(content(Whitespace\" \
                 \"))))(Tile((id \
                 318fcbfd-ff10-4765-a2fc-0f82c41214a5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9d288753-b189-4c81-a9a4-44e7abc29789)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb763e5b-1f19-48ac-b0d3-672fc40b89b9)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1623fc9b-e29f-4961-8cd4-c281b0233160)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8149348c-af81-4177-ac29-f7819425fdd3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bc84e0b1-1870-45c4-a5ff-e0e477f9918a)(content(Whitespace\" \
                 \"))))(Grout((id 474b04ec-de81-4239-bf2e-f9ef562d11dc)(shape \
                 Convex)))(Tile((id \
                 5b4cdcce-1c7e-48c6-8081-dd42cb4b0867)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 8e9a3a0b-9c69-40a0-b0df-4f10c1fe04b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44cc51f6-8a8d-4b93-94d5-20797e44ac0b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b01ad23f-49a5-4173-9f91-977eb556f43b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1220669e-c2d0-4eae-ac74-a29adb37e884)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f7c8b9a3-dcf8-4d7c-a523-a5b4da303b72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f7ded23c-66af-4adc-897c-2c9fbe762a03)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fa6acdbe-3379-4d74-85b3-e3b95d58d3ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e832fa17-7a6c-41b3-b445-b2920c04bbd9)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3df9f16e-8cfd-4fc1-8a27-af543affc923)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8510e0a0-1872-495f-8508-0d9a0a733741)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c21f029f-790b-42d9-b922-24a5d3876190)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0d65b7cb-ce0a-4e96-82bc-462ddd41abad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b576b446-1a96-48dd-9fea-e80baf9c15e5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 98cb2fe0-922b-452b-a8aa-4f3b1b5db1cc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 433d77b8-bf50-4390-9e02-e0b27675d279)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 87f38740-432b-4d29-a911-e3de666146c7)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6f0de661-f31c-4275-8438-d9ba60036cdd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12cfe998-e325-4dee-9629-9818cc91b1c7)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4e7632b5-131d-41bb-8d70-3c80fa9a06d9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d0c31ea9-5aee-4a3d-a9f8-3f023e98bd6b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 29847f94-8702-458d-95f5-f4e7e5bdf95a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 953a2d39-defa-4d3a-b110-9d5f216bda75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 643c1a68-077e-42e7-97e6-34837914d93c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9b548642-74c7-4daf-a3ce-9a7d8e8c23d2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 04afbbe0-bffd-4d42-8c6c-157f22b59a18)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d3cc1080-2f2a-4e55-8837-b51a84979dc1)(content(Whitespace\" \
                 \"))))(Grout((id e9ad4a2c-3efb-4953-b3a1-351fbdbdd531)(shape \
                 Convex)))(Tile((id \
                 5235e803-fdea-47f5-8cee-9fcc8dbe3228)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 bd4a4bd4-fc5d-48ba-9070-33b3f3b2050b)(shape \
                 Convex)))(Secondary((id \
                 eebe8207-6912-46d9-8554-702bb61f2c58)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 098473cb-8dda-4e0a-bf3d-f96f8af5d409)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2f3c5124-d6c9-4a63-926d-f3cd0135aebb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e4d0ba5-f693-44e5-81c8-9adbbe4326d5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 68ee837c-c597-4c64-b128-9dd1496fced8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1e79618-c37d-481a-bd67-b4a01cfeb382)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cdf83c14-3f85-4b90-a3c4-e01886501175)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c57e8a99-57db-4c09-87d4-91b8c0c19dc6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d90fed0-2396-441d-b231-ddabb77614b3)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 79d87d84-d9f6-43ce-abc0-746f2c1ecfbe)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 69c4e180-c84b-478e-8b5c-3f2941a298c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2accf732-1275-4a70-83f7-6518929194b7)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2d41e2dc-00a2-4583-8af3-d35b5ea60aa8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 68316005-0a20-4a5d-bf82-71dde183a886)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f00cd3b-6b8a-459d-9aee-8f0f12717887)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 933a5e83-8a6d-4c2f-ad5e-d71d67b5e63f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a863763a-0e02-4cb3-8b98-b397af4ec5cd)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6c048cd4-0c9b-4522-8df9-110dd1eb0de5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8701f4f6-59aa-48d1-b268-3507bbcf9b37)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a276a4a3-fd05-4558-8b38-4894beed84fc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7087f5dc-16cd-425a-84ce-74a9b9a0c2b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38a9de58-dfa1-4ea3-b69e-7a77ce94fd1b)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8e054451-3c0a-4e4e-8eb3-001241a24e8f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cc7799cb-592a-4cac-acd9-28af87d62fa9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 55150c0e-f209-4a73-8bc4-4e81541caa4a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ca5925e4-c033-49b0-a249-eff540848160)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddc87cce-1fdc-4ba6-958f-dde9214608c5)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 33714aa4-7813-4164-8132-79ec6f11c0ac)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c2559d38-bfc3-494a-ad66-606abb4dbce9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7eca2db8-0fea-49ca-846d-94b48bdd4e8c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ec1a6792-1c28-4629-bcff-c1d9187ff31f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 223eae46-0fe8-4558-9a5f-ffa81837ab55)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c4b25eea-4d10-4096-8dc0-6f250f878c38)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 22390632-1e9e-4924-9161-1fa85377c2ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 158b3b47-479a-41c2-8521-0a4c6088a944)(content(Whitespace\" \
                 \"))))(Grout((id 2da86f8d-2fe2-469f-811d-23f8e1713daa)(shape \
                 Convex)))(Tile((id \
                 42912078-acf5-4222-a9af-3662995d5374)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 100a7c98-f433-4f2f-b398-e22ca90b792e)(shape \
                 Convex)))(Secondary((id \
                 2aafb893-2b36-4198-98c2-f910baf9f25d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d10284aa-2b2a-44b1-9a2b-43fd01f7c0cf)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3e2c9bd6-8816-497f-a8b2-c582708d0780)(content(Whitespace\" \
                 \"))))(Tile((id \
                 215dec21-779d-4172-ba5e-e0f0ff051d7e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 48b2bef9-d287-4391-ab8f-ff182453c042)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad4fb70e-74d1-4b2c-b832-738928ecab6d)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 04cad093-5bc3-4e0a-94cf-6cfd28f24c3f)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 52819bc6-f973-44ed-a41a-afaa3da35d1c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 dabb7788-cd3f-41ef-bf1d-c57f3c771910)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de835851-7356-4a12-a228-9582cb9b513e)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 0f47620a-2530-434c-adca-94288a890e3a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7e41cee9-62f7-4c95-abf8-16605432fe5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d716dce6-08c0-4c72-99be-7b70b8ec37e6)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 cd857fff-d597-4103-9969-7aac5ef3fc18)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e8870dd2-0dbb-407d-a7be-87309cc068f7)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f198ef52-deb3-4faf-9b01-5bd4d21cb29a)(shape \
                 Convex)))(Secondary((id \
                 7fefca0a-2e6d-4e4a-b479-9e3da532c433)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d559f4a8-db41-497a-b655-df09e6341114)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 99c51398-3875-4d38-ad3a-68a45640b421)(content(Whitespace\" \
                 \"))))(Tile((id \
                 675f0769-daf6-4bf6-b313-85dad14e573e)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 003778e2-ee69-4075-a8c0-92eae03f9dad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ef1ccf2-007f-428c-bf81-4fabded14403)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f00241cd-b94b-4831-a2c4-077d06755015)(content(Whitespace\" \
                 \"))))(Tile((id \
                 913f9385-1a9e-41b1-bfec-d8d9e83f0817)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bd3c0096-06a4-40e8-a1e1-18c5090e92fe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 39b8bb14-49ad-440e-81b9-c9f02b4e34e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3341ef2d-514e-4370-ab82-e2c6d86c9a52)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e584e831-fefd-4e64-8729-55964ce24aca)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 036f7f9a-fb88-4e8d-a5ce-467c63e69304)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b78f9908-aad9-4340-818b-2bf04a9b2944)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 df3674bc-52c5-4639-b86b-3a5a74a6e965)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7cd3a6c3-7370-4865-afdc-8afb02c9dd2d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 5e61de78-d4b3-499d-acaf-e5bd76685b35)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 68b639e1-d254-4c57-9702-d743f9a5d423)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 127c289e-02af-4a76-922e-3512c0e2987e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3c5fa765-7e0c-4d89-8a08-a23cd9515429)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a0f1c25-1aad-4f7f-a7ab-91263561781e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5a1f038e-1d7f-4658-9842-a04543e5478c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 93d7b0ed-2a55-4189-8818-d7c008b0cdd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84e87d2c-9e13-48c1-8b1d-7f3861fa1d06)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 2e2fb3f7-7c92-453e-a63b-cea5d0ae0a0b)(shape \
                 Convex)))(Tile((id \
                 1c59a6a0-6168-4779-9cba-00a4bda6fa6e)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 57fe6d2e-b17d-412f-b758-1d3d9b836399)(shape \
                 Convex)))(Secondary((id \
                 b6159b41-d8f6-4c09-b9fc-44016e4fc8c3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d5fc67ff-a45c-423f-9d52-a72af59cf66a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a06bc9c-53d1-46f7-9dff-3bf8187b8e30)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d279b5f3-6552-4a0f-9169-9a9264458aef)(shape \
                 Convex)))(Secondary((id \
                 51213634-abdc-4b5b-9c7a-57cc6e0a6fe2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 35d65dc2-2d5b-4f24-9caf-2a122696e7c2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 74e2b5a7-43a0-484e-8e32-04037444ef1a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40142708-39ad-497f-948c-3ca8f61269d2)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0787c162-4342-4426-9d58-e682062f743c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5fa01832-8b25-4b11-a3c1-590f1ec0b134)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 19cb5201-4604-4d43-945e-97aea1ebb507)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 f5542ba0-0e50-4c34-8a6a-9d8cee1b9dc4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 482db7a8-66a9-4a03-94de-e22b6857cbd1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b3b31580-2874-4c82-bd90-af495e837219)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5682bb76-edf4-4297-b860-a5325ef4bb58)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34609a86-c963-4f1a-84c3-2ceae00ede90)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2fff8978-a01f-4c5c-becb-529924c130b4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad47ac93-6c61-4529-8c1e-aa6401776b98)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 91e6e9a7-056f-488f-9b43-442c7eed9009)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8f3d2295-1c83-4d98-a6f2-eb31240310e6)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f163446c-7da3-4fb4-9561-cfb3ffca71d6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 37f4943f-925f-42c2-9e82-56dab916b188)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad497c63-b0ab-40b3-8253-dd575682ce27)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 44bff6c5-c336-4ea2-9b11-fbcb967807f0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ac436125-1dd5-4e9e-afd1-07a3325dfb0b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 21d6e5e7-028e-4696-8548-430dd3f6ac13)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e6bb6fd2-341c-4529-a067-922fde208247)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66d4fd7f-8872-4969-8571-a5f3c8ab60e9)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c8daec77-df9c-4c98-9ff8-07f95e861b46)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 de298be3-f1cd-47f9-bad0-a0a8c1b634d4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f43e1114-3db5-4002-a76f-bdd6a79c5ba9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1ab261ce-a449-40b8-8c20-6d19cd8ff03c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 563dcf6f-9185-4512-9cd1-12d33f26d6d8)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 14e32b5a-688c-4fb7-85a4-98127e9a4b62)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0422e87f-b5ee-4d85-a37e-bb338a3e525a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f4c22ae-22dc-4817-8d50-de853d6278ee)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 4dcd6297-f2a5-45ff-aaf6-0731fbae2d4d)(shape \
                 Convex)))(Tile((id \
                 5d00a41e-1f00-4c1e-ad60-aa3a585c5fd5)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 ac4c80dd-aeb2-4429-99a6-a21ea43248cb)(shape \
                 Convex)))(Secondary((id \
                 152f4b75-f150-4640-a3cc-66763b7d7b1c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 30353107-66c1-4f8f-8219-9a89c0d161d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 282bf6a6-3a3a-4abb-beb2-e2c67bd2f232)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 1c93dd08-7e7f-41b9-a044-f83e2d5cdb4c)(shape \
                 Convex)))(Secondary((id \
                 5c5500ea-78ea-49c2-8cec-953ef8bcb9a6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 af356f21-2191-4522-9cad-50f0d3fea4f4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4b466cdd-ba16-4440-9fb4-4723cddd43b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02a19e01-f038-4f83-81ac-ad813fb52293)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 97ef9aed-7f28-438f-8303-75cca3934696)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30cbccf8-f162-4ed6-877b-ef0e1aad1b83)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d618bcbd-c0d2-4b0d-bd36-ba0bb257143c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 786ef576-81a1-405a-99da-0fde7e71afc6)(content(Whitespace\" \
                 \"))))(Grout((id c27731ed-ca36-4c07-8ebd-2a84eafc874c)(shape \
                 Convex)))(Tile((id \
                 c7731d06-76ca-41c9-850e-8be3bc00c472)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0fdfe20c-8c7f-4cda-875d-910cc45cf378)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa80d4aa-4e90-4b2e-a5ad-bdb95b4a86f6)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f5362d48-ea58-4b2c-8e7f-127900eda334)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69ed9be6-d26e-4c07-afa1-2400d5aee419)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c556ab88-5b56-4b3a-9a69-0b93e93306b2)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bed8bac6-0435-41a0-8749-7be8acaad7f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c065f0fa-f7aa-40d9-9197-05902d155b1c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6cc78171-0fda-485c-a025-698f86f946a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3764b9f-5fa7-46c8-8ec5-010bbd0b833f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 784e072f-a03a-4a84-9f4f-ff5c3f4208d4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fac1bc9e-4723-4cf7-9a7f-7c02eb532115)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e87704ea-d4e3-4ce4-a891-2cc1d8dc4bb9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 85714526-f62a-408c-bfb5-590cceac5438)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5961854f-2c0e-42a7-8bb0-669a201699c1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 24c0ba88-c3d6-441b-9f88-c2ed0b6933b4)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1fe39409-2a00-484e-bda8-c443ce8acc6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d9afd64-78b8-4168-b47a-084988b2b775)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2cc17c2b-570f-43e3-bbfd-911acc33047f)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 88b7c74c-9b82-4583-b361-b97e596d910b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 77d76ef9-1df4-4e40-937e-3dd8835b5270)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 885cb3f9-6737-4760-b4ab-20e174dbc44b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5dde9546-c9ba-4319-96a5-70134a81b545)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a358a025-9d1f-41c0-8c34-166f94785cb9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 36860de1-9d1e-4148-902a-daaba0b4d5a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 788c1317-85e2-4534-b317-6acdc42e2639)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 3778752a-b36c-4025-8c88-d8d6475cdec3)(shape \
                 Convex)))(Tile((id \
                 4e33106d-bfac-49d5-8de6-2e0955809a34)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 199bfabf-7cca-4441-afee-f58012b42883)(shape \
                 Convex)))(Secondary((id \
                 94e16777-7203-4576-8477-cca8b7a63fd4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 23fb4efe-7db6-4d5c-924c-f9404afb8f43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef775fec-52db-4ec2-9278-0d2052027fa5)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 84301b34-de6d-4d1f-b3d8-5d8a77817903)(shape \
                 Convex)))(Secondary((id \
                 9c5664a7-7eef-4417-9f78-87b4df577171)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bd129084-7196-46a3-b2a4-8aeedc808d7f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4f37c75e-5ffa-4f30-b140-1b17607c711a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07c0a58a-dc4a-4c76-991e-296ad53563dd)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1ffc92cf-e0f7-4aed-a12f-c4ac182855cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4f02dae-cec1-4554-aa10-fefc247369b4)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c8909857-c878-4965-9e04-a05401736932)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e2c1889b-ad9f-42e3-81c2-dfd444390b3b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a371d06d-0a23-4379-b0c1-0756fa07d11d)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 19a88392-96f6-408f-950c-6d8bced940a8)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 58e0aab9-0052-48de-9c02-31f2511c81dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66e7dfb5-dfbb-434c-94fc-b8286988b36c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a84c1066-723e-464a-8e9f-88d2acc124dd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8857805d-66b7-4cc0-9889-8f4ce1f2b5ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93234c53-63df-4d93-8090-e419c3cac939)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 06aa548b-10a7-4cbe-8793-e7725fa47403)(content(Whitespace\" \
                 \"))))(Tile((id \
                 252fffa2-c37b-4261-9943-c4666c782e3d)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bdbefc85-7fcf-43d7-afc0-0b43c28fdb59)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fd0754c-bb26-4ad7-b88c-58fe02b58988)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 df168413-06ae-4ce4-83f4-0f36d4ca2f51)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d2cf7abd-609e-490a-93e9-6340cc06d1f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3d6faa39-f443-4ada-883a-8b93d1b61bb8)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ceb6d5e9-9ec3-4423-8680-a93d78e79e02)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9b86efc7-7459-491d-b00f-6586ddddca5a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 34a65cd4-1bb2-47b9-9a86-9e69aa10f816)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d24993ca-21e8-4b4a-80e0-3f3881965a1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93e7dce5-8451-4f1b-94b1-f555444c2c28)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 b468d269-5005-4d7f-aea2-89b6eb1b8449)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 adacf938-2248-4719-a184-1c83a38037ef)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c4ff06b8-54c6-40fc-a1b7-87c6b939ca91)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5d2c160d-4653-4b7d-84ab-5471230b68aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f8c5f8f-343b-45fb-b7fb-ca8ecb56372a)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 01e960e5-6aa0-4854-b194-93401b3b9ac3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 35f3fa0e-8572-4cee-9f30-fa3ed73b3683)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2229f677-3e9f-4531-a058-88e0e898b0ee)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 124206b7-2d62-44cc-aff3-4d5135638686)(shape \
                 Convex)))(Tile((id \
                 723a0077-9309-4fb2-8a96-fb6e3725f263)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 b5de34df-a16d-4484-b8d5-4342a0596030)(shape \
                 Convex)))(Secondary((id \
                 9c7c5b83-7ee2-4972-a58c-37d413cbc78c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cc3a6db7-799d-4425-bdd7-7fabbc93f9c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f36758ec-695f-4be5-9736-9f70464cccfa)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 8a2716f5-5d9f-4b50-b544-f9f2acc3872a)(shape \
                 Convex)))(Secondary((id \
                 c997de53-8d30-4c49-968a-ea2f11f8e120)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3168db0c-ce23-4977-b630-fb8799892b56)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bcc3998c-ca72-4730-ab8c-3c795a63cfa1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a7bc24c-7019-4839-a2f1-8087941cfe5a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 585582ed-adff-4e49-8187-ad312030a518)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8c321cb2-bc14-4ecc-a893-732992b53875)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3c57c923-2b7f-4712-9254-cd6004eff518)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 37d2cb86-60bb-4c5b-a18b-b853a2482411)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c80f87b8-b216-4478-ac22-e24daad7248e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9b6220a-5b6c-45d3-8144-5b7d77bc1aba)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 492f47f4-6bd6-4b26-a6a7-a13c206f5230)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 00bbd5e0-6ad8-49f1-9461-0bccafa0d9bd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb52b42d-f386-4ce4-916d-d0aa97f8a79e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2814e8b6-1973-426b-8a2d-aedaa7b2b62a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 114aefe2-b1d1-4dab-a1d2-b2f3f559f53b)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 c2cee6d0-ea7a-433f-96a5-81a5178e7e3b)(shape \
                 Convex)))(Secondary((id \
                 c6b41555-928f-4e2f-812f-3744a77dc109)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bd308345-667f-4638-88c5-8bfb901d571b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 24d02479-f12f-4cda-a84c-f8935653badb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2b241579-380d-46f2-b50e-c3fc29c385f8)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 95513b39-36a5-4a23-84f3-4e807b3722d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 54cb81a1-c6b8-4acf-9ea3-6b7fc0491191)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d1411cc5-fd9b-4646-935c-2442bc6fe37e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fefb40e3-0472-4021-afe9-88ef08c38f1f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 22a9ec02-a47b-467c-8531-78a6d97653dc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 249c438a-f977-43be-ab0b-e744396a83da)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19c36327-1b5a-4159-9f39-bb442be9d974)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 75e87f6e-5959-4e86-bd23-37d7cc17ea21)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0b5ce4f9-3ef2-4c31-b8c8-fa2a45604397)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 31d5d6a3-accd-49e4-876e-ea8e955b2f95)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bdde1e33-ea1f-4073-af78-0bd3ccaa7f78)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7a5bcbc1-f557-42f7-8046-adf98c8ce230)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 5fc6bb40-8ab0-430a-b6cb-ef4c78238537)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b9123d5a-4da5-4c90-80aa-c5a60c1a4173)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9a8412cd-46d9-4503-a8eb-212b5e792f69)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9a89f0d6-f26a-4844-bbab-ebc840f0eb9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 970122b4-dbbd-4049-9296-412c082a974a)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 74ce1d0e-1106-46ab-8510-21033a8e0e53)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 be766453-5700-4287-90c0-8df47e5a5f36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60b3128e-346e-4290-97b3-b203f6eefc9d)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 b8ab3bc8-d9af-48ae-b37c-863abe93eed4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 78b26ae5-c7c0-4f4f-ae98-55a47b292674)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 458fdedd-b510-46b0-a80c-7b3ca9e66173)(shape \
                 Convex)))(Secondary((id \
                 b1cc94da-5fa4-4c43-b298-621a84c19ebe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 76452f6a-4663-4883-b5c4-6816ad3c7628)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd065999-e2e7-4d16-b87b-8b373418be65)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 2eba830d-7678-44c7-aa2f-86515c522710)(shape \
                 Convex)))(Secondary((id \
                 523cdb2e-9723-4052-b59d-582ebbdaa45d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ffcaff3d-76f6-472c-988a-f7609106c116)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0a558c1b-00f3-4655-9d7a-4b9cc117aa6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8bfcd951-ab2e-4eb0-9550-6536744aaf92)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4569a031-0fe3-456f-9ee1-98fbb1dfe8e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3de92c02-450e-450f-8010-6c48decc5490)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4e28e5b0-1708-4018-aef9-dce2988cc494)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 53e75689-ed2b-4726-8a6f-413b73da1b27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 448addc2-ee3b-42d9-a539-fd38c5d47489)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3891a86e-fad2-4f8f-84bd-7752862e3d6a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 095f45e4-f9cc-4e2b-b5ca-0d322f289f6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c93889ff-48b7-4192-b818-108f9d88dcf2)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d6ef9792-5564-4f8f-80fa-9f9083b6c195)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b7b3779-aec7-4ffd-b1b1-e24ae2dc544e)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 75238bdc-7400-4d1b-b72d-f034e894e8fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f79bd776-7854-4eb4-a7a1-8b7ddeffb143)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f9e15298-46a7-439f-974a-8f6456db8065)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 96a0c812-0ae9-43a0-bacf-109361f330ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fdf0a4cd-4ba3-4e00-b4b3-1b4c077f08f4)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 df91121c-cf10-424d-a415-afeffb81e1a7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ae9589e0-89eb-4e74-b69d-373532f87c6f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 26b66fa1-d257-4b61-b114-2c0d630065da)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3e5fc46d-7cd9-4be0-af01-09fb5eb244ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d52f8f6-7c70-4e67-921c-aed9bb329d28)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a87c75d7-d44a-4862-9cf1-affee9fd4fd3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cbc9ed0d-c93f-47a1-8b9f-3471114ca247)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8e13f7e0-2b1d-467c-a844-8c53466c18cc)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 26356aa0-4086-4afd-8fff-341c027a9038)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d570377-cff1-48c5-83d1-c4db0606efd7)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7205d191-d4b9-4b12-8312-99babf7dcc5d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 aa1d2d9b-61b1-4446-ac8c-6782d574390a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1cc9425f-ff98-4cc9-801e-caef27daefa6)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 cc242d56-1b89-4bf8-ac13-c9dcffbd3b9c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 31587a48-6e80-49ac-84ab-8d59eeb00c6d)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 145499cb-8a98-48ba-99d6-ea2acbe46243)(shape \
                 Convex)))(Secondary((id \
                 f9ee4bd1-da38-4cff-847b-29dac6bce90d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0a84aac4-b211-48e9-8c00-b380c384e1b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 254bd268-4d39-4e75-8b90-69fd8bd9d47b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 13113920-3b1a-49d9-9b43-6310be1c0924)(shape \
                 Convex)))(Secondary((id \
                 47b46ca2-00a2-41c5-9924-e1c4bebc9e4f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3256a551-d41e-441e-a557-f77b70579ea4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b7c98fa4-3fff-4442-8b49-fafdab56129f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c1b30c3-0964-4a8b-970c-500820af7be2)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 11fc882d-d5d1-49e7-bed5-54a2d3f3fc77)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e1cd2e1-8ec4-47e5-8247-6d867025d049)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5f172360-b398-46a0-8132-7f60c41dd29e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d676dc47-c4f5-4fb0-bde0-ce9561cacb90)(content(Whitespace\" \
                 \"))))(Grout((id b50a9310-c344-4b0f-9b54-96bda283bed0)(shape \
                 Convex)))(Tile((id \
                 1e8ae4cc-2857-486c-854f-ebe76603707d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 bc5c97a7-ef34-4534-9375-0122f78f39c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5c71718-911e-4191-a5c7-c6a073467c80)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f7c245e9-4b8b-4e13-a885-70f61fb29b40)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8366c04b-4c08-48d0-b4e4-69c29ae8e22a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1045f18e-9724-4144-bf2e-7172c5d4239f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 caafc9b6-afbc-4784-9904-10c1fac6923c)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c1c244c9-17c4-48d7-8a9d-0bb748f867af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81bc4bc3-b9c3-473d-8c80-21bc68d75b08)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1c0df961-853f-4676-bbc8-59733ff88bf5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 710f3871-0b3f-4fe7-9696-a028a4659142)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f99da300-6f82-44e8-82c7-3d51f9bd16ac)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6d307472-d3de-47d0-afe4-359a0b7af9ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a62d062-6bf7-4793-8ab1-1676d78aa79e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 60eba9b5-c234-48f3-84c3-3c45ad706052)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7813af92-ca29-4ef0-bd9a-f3cb0b0b87ee)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 37f83917-ab82-435e-abfa-d7cc04a85934)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 db38d1a5-78e0-4f5b-b48a-8e142c9673d9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc0b19ae-4741-492f-a351-5899b231c2af)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 6b422b22-fa78-4db1-912a-1d8fa4b837a0)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 932a897e-9f9b-4538-8ed0-4244dbc49893)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8d7a0c1f-ff49-44e9-b798-e502e4e43013)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b3d730fb-7a8c-4d78-9532-d6ceacbc3fb4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 70e9f727-263f-48cf-ac86-3ea65d63dd40)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f99062d0-0d36-4283-b617-e67b6f524fcb)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e437d911-cfe4-4153-8279-a16c57f9ee61)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7743d996-db3c-4f8f-88f2-77cad4c49fee)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 58dbd50a-6a2d-45bb-977d-1f7f9b5789a4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f25a5ed5-88d3-46d7-b550-3ef600fe4a00)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 c6287189-89cc-40c9-a8e1-91fcbc137394)(shape \
                 Convex)))(Secondary((id \
                 2fc0fa73-a57d-48b1-b30f-4a9bd633b996)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0c8c8472-62f6-454e-b017-6589bc585316)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a724276-e59d-4867-b8f8-b80d908f2faa)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6e8ea24d-71ca-4e75-905e-b629fe190db5)(shape \
                 Convex)))(Secondary((id \
                 50c46f3f-9dd7-4edc-8d54-cddfdf87a4cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fe2c6cbe-9c2b-48ce-a790-d73bbed4a903)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c1d56664-6bb2-4d1d-8399-87fca172105d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f263461e-957a-4cbd-9a3c-e5038a1fa8cf)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 75daf9a6-be33-4722-9036-d530dd6c9008)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1968681a-d7fb-4057-b9d0-e625a99c9457)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5ee6ad65-360d-4003-b228-61854df872ca)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 777bc98b-b43c-4e1a-bc7a-c8944dba7c9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdbb227a-cda4-4b5d-a4cc-e9cd3d3231fc)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a1038e94-4fbd-4f87-a539-439dfeefdd7f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 06933bc8-98c8-403f-9bfb-834d2b973666)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4fa35d1-5c69-49d9-a1ce-43508ae42236)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 084e27fc-ae96-41ed-aa68-f45127e10ce1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5c08e3ab-6701-4495-8e2b-727fc12bf2b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62596f39-632e-411d-aacc-7f1cc548ac26)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 30508fec-96b8-4756-a649-146195bb177f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2870253-2d19-4eeb-bb8b-1e2b462aad37)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1cf141ee-cd34-4d48-82a6-068b6ef88446)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46d1e96f-3ae2-429c-a90d-bc94968b956c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3caaf756-780e-4c33-a9cf-f5f6d8825de3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4c0bc101-15d1-46ba-8f8e-e0b1b85a6dbe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c901288b-f8a8-4f4a-a441-627d18c29a7e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 37aa0e48-8deb-484e-bdf5-c36f12a1b5d3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5d9b0604-7af9-44b1-a860-91e7fc8fb18b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 db06f540-b116-455c-82e6-bf25d6134266)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 18e60568-0c9e-469b-8aba-e5b678b51b13)(content(Whitespace\" \
                 \"))))(Tile((id \
                 15b021e1-fdef-409b-8d6d-dc1590ef513e)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 b0f80751-7f18-43eb-8bac-94310826c327)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7316d1bb-381e-4597-9a8d-4a988e6980a1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 be6ab9a7-1ee5-4ec0-87fc-ece8f4ec1d96)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dc4bc6c0-b710-42de-a76c-9f47530e3a10)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8ebf987-b518-4b53-82bc-7df8b5a2fc30)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 759b1ca0-c6ce-41e1-b007-a7872de359b7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8585c531-518c-49d4-9a82-9f4dd5ee7d64)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb1d8df4-1801-4127-afd0-0c731a45b867)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 8d0a8cf8-c485-4f00-802f-8496320cea49)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 016515ed-d4b1-46e4-92cd-915757cb4510)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 71d697e8-e0ec-441b-9f41-79c5f6c213a1)(shape \
                 Convex)))(Secondary((id \
                 06ea0c72-75b5-49df-9bb4-bcd3e413a54d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 484aa37a-bf4c-4ea4-92f3-9205b9992d07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d3b3866d-2038-4d7b-bb7d-c1bfc8e5281d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3d3b69c1-b3c3-4a13-8eb1-0449bba51bcc)(shape \
                 Convex)))(Secondary((id \
                 1370b099-c99d-40e0-933c-e74e4ee7f63b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 49b40c62-3b35-4c81-ae96-4b03d31b337d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e4af494d-cc6f-4ab8-a229-37290d7ef839)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a2e5b67f-c2fd-47c8-9703-3ea7e93557d6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 8c49126c-24ae-4882-addb-345d91ed2cba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a53417d-43fe-49e8-b6aa-2d99737c5702)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 181038c0-6c8b-419a-91b3-c78807ed8b51)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5e7d7957-290e-4eeb-b7da-cd46920d60fa)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a6db712e-8534-4279-9d8a-3a193f534ae8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afab16af-d42c-4427-ac22-70e76c764952)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 1a149c6d-035c-4db0-a034-ba1bac3f73d2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 227b5af1-e2bd-4bdd-8b66-58905ee600a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d736246f-af15-4702-b7ac-f3327949f908)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 148a7d8c-da24-4606-924c-e909c92d11de)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 306c4f11-0343-4ade-977e-b36a52b63482)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 c0e2ae47-3b7a-4bf3-973b-eaaa69eb5db2)(shape \
                 Convex)))(Secondary((id \
                 abbb3ae8-dd8d-4b2a-ac37-53f8321a3bd1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3197ac12-e5bb-4d7d-92be-fbd8bc510fb7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e2a94eff-8c35-4e3a-944e-4d9d38ac0390)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d1b5d1a2-194b-4fb9-8669-d54aeab04ddd)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0989cb96-9065-4dc8-add8-fbf759a6d4c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ba6d626-2598-4dde-961c-84d01c8dae2f)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ff09ecb1-d8d6-4438-93e7-2512c2fc6dd5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cdaa4fe2-c971-4f17-aab0-b40073c7d334)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 07966e96-731c-403a-85cf-bb872925305d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 97346baa-1177-4651-bc3b-dcf9deb95cf0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2529474-696e-415d-9def-95aa58ec6014)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d4ac9da1-09e8-46b7-96e2-11c00b63b669)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 257874dd-37ca-4f48-ab38-29cc72f1785c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 349d1956-c328-421f-a1a7-f9cad80f9aee)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 33e32a15-e62e-461e-a000-cb7edb9296fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 722f2fb1-62ce-49ad-8c48-34f1e209f2ad)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 36dcbed9-e083-4bf0-8f09-9420690c0363)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 783dadd2-0f82-43ba-a8f0-faca055bf3b8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc0cf94e-ac96-437d-929e-fd6f7c8091d1)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fa665e5f-f8c4-4ec8-b700-cff2fd2c6334)(content(Whitespace\" \
                 \"))))(Tile((id \
                 03d830f2-2ff1-48b2-9a2c-3015fe7679db)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5bb436da-596b-47b3-8b33-a22ee8b89192)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d5161dfc-2ec9-4515-9f51-d55c41474284)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81ec6c82-180f-4603-a268-1c5c218c83ef)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bcfdbfe3-2dfc-4154-bb8f-f265a240a132)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 b540b20d-7cfd-432c-b71e-82a4332ccac8)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d41d5899-b79b-45dc-8f14-8659a140cbf2)(shape \
                 Convex)))(Secondary((id \
                 695c86d8-ea85-4e9b-ade4-360246dcbe73)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3a13bdcb-799e-4a5b-9c98-ecad02361bae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2dfd83c2-3ae4-40b4-a07e-3f1b392213fa)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a85b5cd6-29cd-4507-8045-616a32652a8e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f31ef4cb-427b-4de7-adfd-59c643fe299f)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c11e166f-acb8-4516-893a-d2ee050035d9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ea85aac1-05ac-42fa-b62b-cfa34a9869d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20b272f0-c209-4f79-be15-d0b833f0da8a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b2c3f682-e66a-4a9a-9917-73c14c110fc1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 131d9862-7b79-4865-9726-fd7e752f8be1)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8acf721d-7ea7-478e-a4d3-257b9fb2137a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 d2992282-f783-46b1-b206-1460ccd4107d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0a0296a-93c3-4675-bf1a-2c8e9cdab572)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 02408dc0-1289-41d1-90c7-c8a29923cff7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bb333696-ccff-4fff-8c1d-5e22280a4796)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ffa53496-6725-4f8e-841a-d0a3c364abdf)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9a3986cf-8f95-4167-b832-ccbdddf5b6e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97141bcf-2a12-4657-af86-58406aa02325)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4f4a6508-ada9-4a64-88ea-8586914fb430)(content(Whitespace\" \
                 \"))))(Tile((id \
                 77f43d38-3a8c-46b0-8a19-f8db80b883e7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dec4589e-2bab-428d-b9cb-b6ab32004331)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 264a6d42-29b2-468f-8197-a3fcaaae8b2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14794814-cd04-4148-bd11-448ee4108a22)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a62b02d7-1640-4d05-bee7-06b49a2f6678)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 21fa223f-16e2-4e98-b3fb-46c2838d4892)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 23f771f1-4928-4788-95f1-6342989735ba)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d9d84f42-12c1-418b-96d1-110a17486efc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9507ded-3805-41fd-9b0b-71aec9c9bcf8)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 97deeb62-fd08-4a99-a08f-e03c979be941)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f03ef23b-76bd-4ed3-b837-887515b7c764)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0c4992a7-aec0-404a-8986-081f6745f8d3)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a82b4801-3747-43be-9e19-e607be28e459)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13be19e4-167b-42a1-9dff-c764863b9822)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7626d560-8118-476b-b509-b6ed278af377)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 02ef569a-0ed1-4d4a-8f2e-8fa309af0979)(content(Whitespace\" \
                 \"))))(Tile((id \
                 920218cc-3d4c-47a4-a44e-288e859dff5b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 38299298-9150-4dd6-84fe-d3a11283b86c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9ced5603-b93a-4721-b6b6-83c066af07a5)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3988bd1c-6a6b-4eb9-84da-bde5b1957350)(shape \
                 Convex)))(Secondary((id \
                 ceba5a97-1a88-4374-abfb-b83d4b626555)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cd2ccb25-14ed-43e9-97bb-80d8fa9eff29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56606242-ff24-468e-b472-15ddb40993f8)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 71469bb0-ed04-485a-b0a5-862c187e41bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f589f931-f849-49be-9c67-6d9e6c72a59c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1712282c-4eed-434c-ae06-4e9714a8e99d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 373f09c2-7243-4f21-9768-d7a0b96402ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bedba056-40b5-41d7-adff-ddeace8ef17b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 3b6d54b1-c163-4b13-b592-7a87f01e6454)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff8e401b-f2e4-4117-a704-5bb70bb42a5d)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ea747146-75b1-46f1-b2b4-0a9d9ad12c61)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 74bde9ef-9e99-4272-8cd9-e40a45ec6138)(content(Whitespace\" \
                 \"))))(Grout((id 4a0d3ba7-d134-41bf-b6ba-bf8d3b8b0700)(shape \
                 Convex)))(Tile((id \
                 bab51df4-b6c7-491c-b661-c2a6ceef9a6e)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4ffeb15a-f320-4a8f-889c-4026c92d704d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16db1ae4-fdc3-408b-b5d8-d251a8b28a0b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 04e41423-56a2-4db6-8498-02b5644ff8c6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e9a4c173-cd4a-408a-9588-b401eb796e96)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 58174174-6673-4a3a-8b54-24ae1fda91f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d070c757-b79b-4cb5-9e7d-95aedbffa4e0)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cdc98a07-f324-4e34-b82d-d106ce005f16)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94cc0727-d7bc-47d7-91f9-5a830a41b065)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5715e10b-1b00-4501-b52f-402f534725b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2826f024-358b-472f-9361-76f06cdbe2ee)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7cd1bf4d-2feb-488a-8553-ba47e405b02b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e56c54e7-693c-4fff-a935-5441fec8d1e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c440286-cd04-4dba-bb4a-3033151d685f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6648ad4c-13dc-4f9c-a89c-c73e2b96e3e8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 30469aa4-bf53-4d74-84ab-a75830061697)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4617c80c-4fc7-4025-b49e-0b8b2184c38f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5b5613f9-a438-46ad-a76f-016c16c2128b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6d6d878-559c-4efd-8f2b-a5ae2ff8ea91)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9cdb98d9-da98-4695-881d-572e5648ca86)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 816b43c6-f68d-4584-bd71-3355221dd181)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f506b30b-b2a1-410c-900c-48797ab9b496)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 53727097-ad54-4971-b8fc-fd1acea76295)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1140292d-07bb-4382-ba1c-5bc656aa9bb3)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 520ee97b-5b21-4004-9dd0-32fd79cdd26b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 21bd89b5-e0e0-4e4b-b833-81009357e5ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1141a8e3-8eff-499a-a6e4-e0eb11480b5b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f6b61897-185e-42b8-9ecf-fc201d075242)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 38fa41d1-fd2e-42d5-ba5a-a7dc54bd1050)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 902b007c-922d-4e52-9f03-e7c546e863e6)(shape \
                 Convex)))(Secondary((id \
                 dd059c03-2e46-4105-aba0-9af15d9d02e3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1194c30a-41ca-4cab-8b0d-06f0a47d7398)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1212d6d-e8b3-4d71-8423-65001d58d0a9)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 84277d9a-f829-4b7e-9466-75ad0fbb5cc9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff3ec739-fbc0-4f35-8392-a33e12d92b8a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 fa76ddeb-6e3e-4677-8434-e4cd0ac7a3d3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4566044b-89e8-454e-98d1-d37c2c33abd7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92a3968b-0afd-4adb-ab98-e9262802fe46)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0eed6c95-5e1f-49f1-a753-1f88ff959844)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd8a83f6-71c8-4868-bb48-325fc1d2c063)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 29a9c964-cf3a-4075-8f4f-de9b9b6c9fd9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 26523a94-6898-41b7-aa24-6d4768f67d5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41171f78-bcdb-4786-bff6-fe2d27cd3e07)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9bf8b479-ff94-4f44-ae1d-4fba8a27b220)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a6cec460-9aa3-4edc-880e-94e728f185b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d3475938-4eff-4371-8914-70298b1d0b4f)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 36a136f0-8be0-4434-be7c-1d347ede05aa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9fb90096-5d9f-4b5c-8681-2fe6806f9e6e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bac6d1a9-7c2a-4d44-ba13-2631662d066c)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2196a788-576a-4358-bda9-2bb8035a3242)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2379d217-4473-4637-81c0-8dfc50833627)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6e2fac5f-b152-4bd7-aff7-ce17f850a79c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b21a524c-6f17-4108-96f5-05e671260ddb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6788e07c-d142-4aef-8147-80edef4189b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d935218a-537a-4786-8ab9-59b2230ed9ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 459f581c-027c-4ecd-a94f-da04635596fd)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 59e68b34-b739-44be-b706-c78cbda8bfc3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3839d1ac-a188-494e-8a74-3e44d865bcf9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3ecb36d5-c5b8-483b-8f8a-da13f75bf9ea)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ac42c1b1-8ab9-489a-bcbb-c59c19d5b918)(content(Whitespace\" \
                 \"))))(Tile((id \
                 58c261ee-4d25-4b47-9a69-0ff5d51c261c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 86e5c84d-2b6d-47e3-9a49-bbd20b6b6e28)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4d3631ae-a74f-4ba0-9499-06c41339a6fd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 850346fd-32af-45b2-97bf-c6fc01b0a355)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b5db5605-94f3-4e58-991d-a66521b8ffd8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0b3d81f-5ce2-433d-9c6a-d34ca9482c3e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2e6c7a72-c86b-4dd8-8b14-eb1e8a2a9435)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 835c5196-e979-4f18-9f93-af39c5e2d401)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3c41238-8ed2-4aef-ad78-1a10c9deba87)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 98487e4a-c524-4646-9a7f-6fdf67c572b5)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0f118011-c9f3-4ae7-b3c7-c5e3dc7b173b)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 440278de-0152-4f61-8ae1-79d72faca868)(shape \
                 Convex)))(Secondary((id \
                 a48fddb7-f25d-4699-b6dc-525a67767abe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9128fab6-0be6-4413-8084-d8b149b68181)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57dff435-e184-4c13-be69-5d2a9e21121f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 61cb6c8e-d34e-4adb-8cef-02d16acdbb25)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79035ef9-6893-4a69-b748-cc3fddcb521f)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d6d29a6d-9732-4b2a-b1f8-33df607f5fae)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ab32cfbb-5ff3-44f5-b292-8e51c0bf7962)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c352c1f9-b27f-485e-ac72-f670bf596595)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 46a2869c-53ba-4ea0-b4b3-e01a02cfaca1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 82ddf886-ab32-4c00-9ad7-3b9d70f1a911)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 b74cc215-49dd-48b4-8cb7-4e5dbe4651dd)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 793bd082-386b-402a-976d-fe16b9653e84)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 6d79fffe-0470-4147-94cb-c6b6b6d13571)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ea5e6cd7-259b-4f19-816a-1e9e754de16c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 60500213-84a7-4491-8958-8f737467bcb3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 333fbcb6-ff52-47cf-ad45-d3feffcaa671)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df8e9432-46bf-47b1-8c8d-21ff9261d439)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 8f5a7f0a-a34c-4ceb-9751-821f8c15a182)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a456c01f-111e-49ae-942f-329fb517b12d)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 23bede98-84fa-4ced-9b62-60e4fd8d7c44)(shape \
                 Convex)))(Secondary((id \
                 e147349c-f8b3-4929-a117-6358a2681667)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a88f9c84-4ff9-45f7-986e-c688c70e558e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 aee57c30-9670-450c-a327-b1a4b19cbdb4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34a4ab66-730c-459f-8582-3635b4826bad)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 795c70fc-0b85-4241-a90c-ecaa4c0880a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb6b82da-0d17-4765-a4f1-43711c6790b7)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 02c08efa-94d9-4df9-8770-4a3d5e8562dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9749eab-274b-46a5-9759-a1f47d384870)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a6630019-c513-4d15-98ff-264fd48d7b62)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b3c6e035-f058-4e9b-b1a7-3b6ccf50b0f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8e9da54-1e64-43ea-aa6d-3804320bf5f2)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 eeb7a14e-ba90-45be-af1f-6de95bf5dd4b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 98f28120-3b91-4a74-bbb9-37dc371eabb2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cdae3b64-786c-4857-a7b4-77ca6e8ce6d6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6ee2a152-379a-4466-b08c-ec907035613b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1e589f2-bdc4-4c9b-9577-64c8ac889365)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a4601ff0-280a-4967-8e2a-890489320127)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 650da09a-5999-4243-9852-978f92b8389f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5bdd85f1-2c50-4206-bb57-77e037d4c17d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 55343f32-460f-4a32-9797-276dca97e4bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10ee5632-819a-4770-bbe5-2c33af2496d8)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cb8bc233-af3b-44d6-9dc8-fcc7bc83ad00)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 51f9266a-696b-4958-ae39-991e1f62ebc5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a0383d54-3116-47fc-9f93-1013a2e05452)(content(Whitespace\" \
                 \"))))(Grout((id 6dc4461c-c5de-4ae1-8813-9ec5fee77716)(shape \
                 Convex)))(Tile((id \
                 1f89db2c-49ef-4739-9d96-af53660ea8bd)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a7b81f54-bad8-4fc1-9e14-a72422791594)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7522c6c1-e916-41bb-999c-b6f3035bde32)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b5160f6f-4583-487e-ac59-db7a3366e61e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 85a6de4d-3446-43bb-8f86-43b5b0b868fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76c4a4f5-0b69-412c-a476-877e8db34ac9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 18ab56d6-daca-42fb-bec0-67ed667187ef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2d90c38-9303-4850-a29f-369291ba39b8)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 abf1956e-ab57-4862-9c30-f381592340c6)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 830f54e4-31b7-475f-8f87-af71020f23b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66539e68-130b-4828-a3a5-8504ec9f9fbb)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9c11f4e5-1d8b-4a1b-89dd-f43e9fd16f28)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6005c55b-7b0a-4379-a767-e1547d047616)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1252032f-3180-4c87-b5c5-c0f6b1acfdfc)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ba396732-58db-4321-bf91-26234600bfe2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a099d90e-b1f7-41a8-a2a0-3daa935af950)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1e8904d4-c1af-479f-a559-57ef5f4745e3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 01d3a5aa-283b-4012-a55e-ff9c2a439085)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 23839345-5181-4b8b-8e99-801917b0f99d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a39dad93-d466-4090-b2af-7c22d59f25a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8286edd8-c78d-4136-956b-9332578ea259)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b08deecc-09b7-4137-99ec-6af126cec8e5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 02f9075d-1875-416b-8292-225f65d90382)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 16641e0f-b23a-401d-8be5-f523399befc4)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9066be7b-6e3b-4a52-b594-d740b3c42f07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cad908fb-353b-4bbb-8587-bc8c41367591)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 58888ca4-d39b-417e-b463-96bad89692e1)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a856daca-8bfc-4ef8-824a-62ed85bffbd6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 35b682e2-3785-484a-8c88-d0f84a156b68)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b0eb8cc5-1eb6-450c-a714-e03a5af4fc18)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de2cc9f8-df33-4ece-830f-84bfb1e2ab0c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9aaa7b5a-e29e-4826-a134-ffb8c8867f0a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dba8330e-50a5-45e5-8fbc-d706c6bb9ffb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 62175677-a975-43f6-a276-70a0ae448f30)(content(Whitespace\" \
                 \"))))(Grout((id 4d817ed7-f8a7-4044-86b5-28578268bddd)(shape \
                 Convex)))(Tile((id \
                 3f869260-bea2-4cba-b41e-33a2685a63c0)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 640014bc-001c-4292-94fc-5cefd46779eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c7349a8-5ee2-4921-ab1b-b95a04544812)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 280e2974-0427-4423-80d3-567255622cf4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e7049356-1409-4e0c-8474-39d8b7a7e1c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95cecbae-b52a-4a12-afdc-0591bcc6b3a8)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2e8e106e-129f-4611-9b79-2c3e3114e999)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76a7bde7-e702-46a5-ad3e-1d48ea037de9)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 562074e1-b4de-49eb-b613-6f10ffb7b4d4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 504f3ded-e0ae-4355-af5e-1e0f553d4b67)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4851705b-cff6-40b9-8ec3-518db48eb199)(content(Whitespace\" \
                 \"))))(Grout((id 3795a63f-082a-48ee-a2d3-22959913389e)(shape \
                 Convex)))(Tile((id \
                 d74dcb82-c44a-4443-a14b-7497bf38702d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2d82047c-9b61-4c80-b605-ce84d0a55fea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b7f0d06-6e00-4d17-ac17-fe7fb1546673)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 51b6993e-1b44-4f4d-ab2e-1c838dacbd33)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 234a2635-4b7e-4b69-b629-2495a698a702)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ca99030c-5af2-48a0-aece-1b041ecd9a01)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9ef89e0-5efe-42b4-8f23-38b55ccfc3f6)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 44f33168-5d5a-498b-b4e5-5f1e5b41b08e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6510311b-8395-4e8b-a9a1-b01db07e8362)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7433704b-9b2c-4c0e-bf22-774d131c49fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81fabdd8-2daa-4c00-8a2a-52d77bec91f2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 16e10d11-2e25-40f8-849f-9fc5094337f0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e1ec0443-0c03-461c-99f0-caafad2ea0fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ac6b2d51-bd04-4b2a-ba5b-5aeb8f4d6e1a)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d3ee50db-03b5-457a-ac36-616268d22cf8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5be3dcd2-eced-4202-bef0-d69d863d754a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 12b5052b-4db3-47e4-a808-45362b2d3b06)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3f4ba52a-6287-4f08-90e9-1e3bb5eeee2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f9360ad-376d-46d3-93a3-f092823ff8e6)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2aca2b94-e340-4a2e-88f5-18d0c2cc1228)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a8660a13-4938-4cf6-8eb9-5e92b337229a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ef1c255e-21e7-432b-a9f5-dfda62ba6ce5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 63d1bf57-3b50-4d2e-a412-cf6a3643eb71)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56f6bcd8-1489-4f7e-8db1-781fce314f36)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2616ba91-7a6f-4412-8257-446ba093fb1b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 98a3f38a-f6c5-42df-8927-c5b3a78f0adc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 75147bb2-0727-41a0-9f26-9169442706fc)(content(Whitespace\" \
                 \"))))(Grout((id 46a35b8e-694e-4816-ac74-1073b852778d)(shape \
                 Convex)))(Tile((id \
                 3fb3ecdc-eda5-4890-b78b-d3bfead67229)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 647515a7-f723-4317-a1c1-45acd30a6299)(content(Whitespace\" \
                 \"))))(Tile((id \
                 805e1a9d-b732-42a8-949b-b4681b211b7e)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b504eda6-362b-4488-a27d-8c7a7b1f0c04)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cf284f03-d89b-4952-97bc-d1953943d6af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65d8ef39-49a5-4bd6-bdad-f0f3ed187d08)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f91e1362-852c-4719-b5d5-97c222ddce5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38e82d31-f73c-449e-a693-a95e6b9a0af1)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4f0c116b-fc53-41dd-b7f0-9688a31782ca)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8ba8ae3a-581a-4f75-b55d-b213a68f32b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eed295b7-2f53-4604-b2f5-e7cfcff625ed)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2df82325-f0d3-4337-b830-0b7278954f2e)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 48870c97-ae3f-4650-a773-1187178e062a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b5051da8-7f10-4160-86f6-d638b6e41427)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 65c435db-ec8b-4335-9775-57901c714eef)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 77da2fa0-c0a7-4eb9-a354-30a0aee11dbd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb686df5-912e-4ee2-acf5-1c4bdd68dbc0)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8839b82f-7653-4118-bf72-cac11ed200d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d748e68d-b403-4e5a-a3b8-90700c92835a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8511418b-aa35-44c1-945f-b47fcfd9a8e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8cf10ce0-39f7-4380-8d14-0ee468e4563e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 459f988e-9004-4e89-b4b0-d244975f2c22)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a5b06af3-69ba-4bb8-9f55-f8387b126be1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b97c5fe-4736-4afc-8a77-13d258c59099)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3528c524-1185-4f0b-9925-b12c52cf52ac)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 95a20e25-8154-4afe-877b-d711e3eb7e1b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 76cdfbf1-7197-452e-bbff-36cdf7f5b20b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 467a2823-cd08-427b-97e3-fd5fe5ae9b3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6783888-15a3-436f-b442-0b7ab4f00ca6)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 35b8a7ad-bee2-4926-8eac-051b3c286f02)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cadb5042-a22f-41d3-b03e-3412af5a196c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 73a1a1d2-c0be-4f1f-afe6-76dd05d5d0a3)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 87bcf813-9813-4e5c-8c53-5de01ccde2bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7214a413-6379-44aa-8df7-90f985fae1e5)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 acb94629-1480-48e2-a665-af175d4398e0)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 76714bd7-499c-4186-8e5f-2912bb58f241)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9e64ee55-39ec-4b09-b637-261f010cba64)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c24f1f59-cca2-44a9-92d8-874616442f7d)(content(Whitespace\" \
                 \"))))(Grout((id 231d6978-b286-4435-8986-4af65eae25c1)(shape \
                 Convex)))(Tile((id \
                 e019f811-b5f1-4e99-a8c6-c04e87fb811b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9db74309-0642-41b8-9a66-24ccd929772f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 902bf6c0-d2e0-43b5-a7b4-d181a87129e6)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 273b8d38-3a15-48d2-b58c-852027a5e094)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0ac22ee9-b75b-46aa-b1e9-98ed7b6fafbc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a6b9a735-2e5b-40d1-9681-9be47a86d58d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f708d83d-f62d-4f25-ad89-d046454922b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 39159d87-2f1a-4bb7-a2ad-72b3695af8fc)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 c14fda5f-d417-4e55-8946-c1411ad04684)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2fa5697a-5c45-4c76-a2f6-bfa0857525e8)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e1e52a10-42b2-4c41-94ed-ba97fa662e24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3207dfe5-7be1-478e-a8ca-3e5d04876add)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 16a99e19-d6d7-424d-9f6e-bcc0102d3545)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c538a973-35f2-4dfb-8a8c-c7d2277dbcd2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f032007d-a743-481b-88b4-b62621f50bd8)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 98f8115b-bd41-4722-abb6-f1b1592e924c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ffc2d710-e2c3-4ee8-ade5-ba18d6afeea5)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 e722f2bb-ba54-4363-be96-3030a64cb5df)(shape \
                 Convex)))(Secondary((id \
                 e5040b38-6e1f-4c32-a33f-7f6fbb762ea1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b8284f6c-0fd6-4119-b96e-a9bb218f9a4f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c9f0b3b0-8730-41fc-a005-e9f364475285)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1f70526-7bf9-44ce-a1ed-3acc24f684ba)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 65486e57-3cce-4346-9e3e-43c3b1a278fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c922861-9076-4a6e-897b-8bfc690cc85c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e67259a5-8578-4f0d-b294-84a8a6571fe7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66a535b2-2b27-43ed-a44b-1544b4735531)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 54a7ce56-19a2-4b89-ab2b-3c7486b6669d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dc4c49fc-613f-41ae-9f02-4b214d6ca000)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e52102df-c2cf-45de-ba1a-75d5a99d8999)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4a2d8afb-b5c8-4f0a-bed5-c092860e00bb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cc40fb2d-623e-4a1a-955b-c495ae50f001)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a1f268b9-236f-48ef-bec4-93c9e4d1671b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 af778a20-9b3a-4ba4-89ad-67253ffad249)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a9bc6569-6dff-4368-a906-15e382da3c06)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 73ed6fa3-f376-4e51-b632-c56dd14feaed)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9d7b5d8d-03f7-4e68-ae00-c67ae28f207c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 924638a4-6fda-4e48-b5d7-e6145907273c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9571fe20-6261-48d7-a044-f5f6132abfc3)(content(Whitespace\"\\226\\143\\142\"))))(Grout((id \
                 2825fea8-c86b-487f-853a-885069c09e7a)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text =
                "# Internal Regression Tests: Function literal casting #\n\
                 # None of the below should trigger runtime exceptions #\n\n\
                 let g:   ->   = fun _ -> 9 in -g(1);\n\n\
                 let f = fun b -> b && true in f(true);\n\
                 let f = fun b:   -> b && true in f(true);\n\
                 let f = fun b: Bool -> b && true in f(true);\n\
                 let f:   = fun b -> b && true in f(true);\n\
                 let f:   = fun b:   -> b && true in f(true);\n\
                 let f:   = fun b: Bool -> b && true in f(true);\n\
                 let f:   ->   = fun b -> b && true in f(true);\n\
                 let f:   ->   = fun b:   -> b && true in f(true);\n\
                 let f:   ->   = fun b: Bool -> b && true in f(true); #ERR#\n\
                 let f: Bool ->   = fun b -> b && true in f(true);\n\
                 let f: Bool ->   = fun b:   -> b && true in f(true);\n\
                 let f: Bool ->   = fun b: Bool -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b:   -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b: Bool -> b && true in f(true);\n\
                 let f:   -> Bool = fun b -> b && true in f(true);\n\
                 let f:   -> Bool = fun b:   -> b && true in f(true);\n\
                 let f:   -> Bool = fun b: Bool -> b && true in f(true); #ERR#\n\n\
                 let f = fun b -> b && true in f(true) && true;\n\
                 let f = fun b:   -> b && true in f(true) && true;\n\
                 let f = fun b: Bool -> b && true in f(true) && true;\n\
                 let f:   = fun b -> b && true in f(true) && true;\n\
                 let f:   = fun b:   -> b && true in f(true) && true;\n\
                 let f:   = fun b: Bool -> b && true in f(true) && true;\n\
                 let f:   ->   = fun b -> b && true in f(true) && true;\n\
                 let f:   ->   = fun b:   -> b && true in f(true) && true;\n\
                 let f:   ->   = fun b: Bool -> b && true in f(true) && true;\n\
                 let f: Bool ->   = fun b -> b && true in f(true) && true;\n\
                 let f: Bool ->   = fun b:   -> b && true in f(true) && true;\n\
                 let f: Bool ->   = fun b: Bool -> b && true in f(true) && true;\n\
                 let f: Bool -> Bool = fun b -> b && true in f(true) && true;\n\
                 let f: Bool -> Bool = fun b:   -> b && true in f(true) && true;\n\
                 let f: Bool -> Bool = fun b: Bool -> b && true in f(true) && \
                 true;\n\
                 let f:   -> Bool = fun b -> b && true in f(true) && true;\n\
                 let f:   -> Bool = fun b:   -> b && true in f(true) && true;\n\
                 let f:   -> Bool = fun b: Bool -> b && true in f(true) && \
                 true;\n\n\
                 let f = fun a, b -> a + 1 in f(1, 2);\n\
                 let f = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                 let f:   = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:   = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f:   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:   = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                 let f:   ->   = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:   ->   = fun a:   , b  -> a + 1 in f(1, 2);\n\
                 let f:   ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:   ->   = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                 let f: ( ,  ) ->   = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: ( ,  ) ->   = fun a:  , b -> a + 1 in f(1, 2);\n\
                 let f: ( ,  ) ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: ( ,  ) ->   = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) ->   = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) ->   = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) ->   = fun (a, b): (Int,  ) -> a + 1 in f(1, \
                 2);\n\
                 let f: (Int,  ) -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) -> Int = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,  ) -> Int = fun (a, b): (Int,  ) -> a + 1 in \
                 f(1, 2);\n\
                 let f:   -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:   -> Int = fun a:   , b  -> a + 1 in f(1, 2);\n\
                 let f:   -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:    -> Int = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                \ \n\
                \ ";
            } );
          ( "Types & static errors",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                 908acc98-58bf-460e-9f46-ef979a59728a)(content(Comment\"# \
                 Internal Regression Tests: Type errors #\"))))(Secondary((id \
                 dd05a0aa-8237-4aef-9229-d4eef34424ae)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f71e0233-9a47-481c-b7b9-085180530144)(content(Comment\"# Each \
                 line should show errors or not as indicated \
                 #\"))))(Secondary((id \
                 0f293047-4342-42d5-aa9a-06a831775917)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b992eeee-7643-478e-a880-efdfaa5f9acb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0347581d-9952-44fe-8c60-ace1889e17d4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 97516917-a658-4bad-9613-790de0723351)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b523467-5ef7-464f-a3de-64c8a8ec89ab)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f560064d-978f-4290-b269-e6a6382c5146)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 30b2a2c0-3996-4b29-b282-5b61e7799764)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8f372a3d-9815-4286-bbcf-1736c68f0aac)(label(unbound))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cce1d17b-d0fe-45f6-9903-4634d6710b7b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ee3347d2-2a84-47f0-a914-18b072367103)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3b606396-cb18-48cf-bb0a-d7779a347374)(content(Comment \
                 #err#))))(Secondary((id \
                 50f71259-889b-4e8a-b298-479338edfe45)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b39cf141-8d18-4191-aa61-1f9ec9980068)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 751b103e-4891-48e1-bcc0-399baa517bdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca66c116-69e8-4544-8845-95c027473779)(label(Undefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f3906285-8232-4e1c-aa22-3de5f672da53)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b8e724d2-4697-43c0-9034-c59042c3d7d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc95c807-5610-4399-adee-17ca9ae09a11)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3b60653e-dcbd-4e79-b814-33b1e99fa970)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7954ef5a-f12a-47b6-b259-c86be8692731)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a02d069c-0167-4942-9efd-50bb0124e491)(content(Comment\"# 2x \
                 err#\"))))(Secondary((id \
                 293ded0a-3dc6-40aa-92cf-1a9dc44ab84d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fe21d8cc-8b60-46f0-9050-cb43af9509ce)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4c97c514-b40d-49f1-aa2d-f57f2cc59395)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dafd3dd9-436c-475c-a186-8d29da508667)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e5c9bb0-9de5-4afb-8f8d-71c3220021e9)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c26f20b1-f740-4b7a-b5e1-09d228cdfb13)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c2f941f-39b9-407f-9869-408d4733338b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bee0a82a-6d30-479e-9442-dd83eb90e5b5)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5fe80f7e-a103-4aaa-993b-81c900303ea0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6f58a91b-cf0c-4440-94f3-fc03d764d73b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b4dbee63-0a58-4453-85c3-2b8d985845e1)(content(Comment \
                 #err#))))(Secondary((id \
                 eea8e43f-2b84-4f78-a4c5-be8fdd73f885)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8dc8b0b4-e840-4cd9-8368-05d4692200a6)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b4f6ff1f-b2d1-40db-a762-46128727896f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d089db61-96b7-450c-9136-ceb74b014953)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 6ea0021f-4669-4dae-b79d-694d7b390ad1)(shape \
                 Convex)))(Secondary((id \
                 e4783917-cb10-40cc-a790-055132dd63f0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 74d43f59-87c6-4f29-8a77-16213f98df4a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b4a26d16-429a-4989-9d0e-424a040b4f07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2af38a1a-b637-4312-a78c-04a1c7be5ed2)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a0d2e969-d9d2-424b-9ab5-f335089f87c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45118925-cdf6-43a3-b8aa-41717783e7e9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 db1ed299-3b86-4f4f-add7-1faa5f09a564)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dadd2406-d997-4818-9a41-d30fd5c5d22c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d20b947-ad6d-43ed-a8d5-ecb41f816a45)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b91cde15-a69c-446c-a28f-2c7cbc7ceb4e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1a55adf1-36c9-4db3-9324-553c54300e8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 601ef198-4dc2-49f1-b4a0-19ad2bc6027d)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c52bb8c0-f6cf-4336-b496-a1b04bf4405c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f3b72bf0-c428-432d-af35-47843c4c5a81)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bbfacc98-e2c9-46ec-8b25-04705388ce47)(content(Comment \
                 #err#))))(Secondary((id \
                 7803484b-afa7-44e0-b28d-0cadfcb238b3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4925a295-4af8-48a7-beda-c985609b5ea7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 84e05b93-4e52-40c5-9f3e-debac7e4189d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 37c66756-2ebf-4c85-9e96-abda18d2c26c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a0d6430-cf1c-4484-80a3-128336101381)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 933c1a0d-bbf1-4f43-b9df-02bc5c35c2aa)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 07c07854-e581-49f5-b136-24f40990147c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1765e0f2-d4d3-4f80-a9ec-a55228779839)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2268c79f-ec3b-4f92-a636-de58d01c9415)(content(Whitespace\" \
                 \"))))(Tile((id \
                 819a87cb-99c7-41f7-b340-2d100de18499)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cdc7003f-f458-43f1-8d4f-f0f144a720ce)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 46ef85b6-6ab0-4c3f-b028-243d27adb3e9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 04b10af3-0903-46c1-ac33-ebe9da94c9f1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dc53a56f-c603-4d25-9da3-4d2664270009)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4019d9f2-5750-4818-a97b-5322e315eb99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2f8c98e-74fe-4373-a132-e1d244de9725)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1c36413b-b601-481b-9d50-4098af6bd978)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8c2ba160-d4a2-4f6b-80cc-fc9bddedc156)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b8e01dd6-61bd-42fc-a691-4aa2f9a03606)(content(Comment \
                 #err#))))(Secondary((id \
                 3d8a7938-3183-4976-b51e-c2557460497c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2ff5b431-420f-4e5d-96a8-063b64685f6a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2feb87d3-5021-4b98-a0ec-ca0d6686483d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00a71fad-8841-4fbf-a7cf-b9f580a5ca79)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cdec4324-4456-42ba-9ba5-06f58d75e352)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fd34808c-b843-4606-8dd7-a05b8ebc684b)(shape \
                 Convex)))(Secondary((id \
                 8dc68af6-0e45-4634-8e9c-aa4a0d1ca442)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 24ebdbac-429f-4d9d-8d41-48d438074871)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1e518c9b-f637-42de-b071-13cb84277925)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ca8410f-1379-4437-bfe8-ec4318a4a2e6)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7c27047d-3564-453d-a791-9bed177e83a3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1806af76-69d1-47c9-b067-a158807938e8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 478db887-e733-4504-987f-4690eb46f1c0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d61bfc72-feea-4a75-bde5-45de91f5e11b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1b1ad4c-34d8-4145-aaf0-4eb800844db2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f538a8a5-1b93-4aff-9a95-0f05d1673303)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 19d7a368-ddfa-4d1c-8cbe-e7259bd41504)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1398d331-4ce3-40b3-b00d-f2af67af04f5)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2ea6eba5-ede2-4c08-b0da-abaeeae0408a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8bd83ac3-eebc-4155-9e1d-aa393ddf10ec)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 69c3a236-642c-40e4-80a8-a9fd961e8d2c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8ea9e85d-0a7a-459d-8cee-c2765533eef7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d76c9f2-1c24-49d4-9d76-7e8d354caea8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7c2722c1-7c64-452f-b618-0cf75c1c36a5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c6fc0acd-91be-482b-8459-50c63956768d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f261ebd-0b21-4402-85cd-df257044b08c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 41171f28-33a0-4131-9f3f-15ce2db8e81c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 323fa9a1-3cb7-4040-9e8d-e27735c5c47b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a908af97-2150-4dcb-b232-1dde447b54dd)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7805b6f3-f70e-4363-a70c-e8426bcd6009)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42fb799c-928c-4910-b418-b9d15d2aeb07)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6789fcec-9ec8-4493-ab76-fd8f43af4281)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0e523e3b-58ad-4ec1-a1ef-336de7bc5e89)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b013265c-ce1b-4645-afc2-734fde387f60)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3647ce96-1a88-4dcf-947b-93a15ab688cf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3e465e74-5552-40e4-91a3-e8fd8681763f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0515511e-34a2-419f-9815-b89249280b0a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5ada2811-261f-4976-9770-bd823b8a6fef)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b0498de7-aee8-46a5-8176-4f0083de3e87)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 09956bac-704a-4925-92c8-ecd0a2ac0a62)(content(Comment \
                 #err#))))(Secondary((id \
                 8aab2f9d-c869-44ad-a506-9d9aae466303)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 00e1878f-0bb3-459f-a618-5319eb57d75d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f476c57a-b6cf-45cd-a420-6b43d83c6c21)(content(Whitespace\" \
                 \"))))(Tile((id \
                 372d944a-c8b7-4837-bf1e-55b8d0df789d)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4cf5f693-70fb-4bcc-8e7e-b2a05d40f037)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5cd950a3-ef93-4096-91d3-f5daecf6aab5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 746fd68f-0a5d-4e13-a13c-72f3d87eac04)(label(Fake))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 54ac57e5-46c2-4b6b-ad35-f7cdd16735e2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 05793aa9-5536-4892-ad88-bf464052e7ab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cf9e4f1-44ef-4343-8781-86cd450040c0)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bf8fe9ba-e348-4461-a7fa-9db5c4f924dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb63b6c5-3a2a-4fd4-b753-bbbf4291a5a9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a8cabf65-1381-4483-bb01-a15d924b94b8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 59fcdb2c-5802-4aa7-8a42-e12b6f440095)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a615346-07a1-4e15-9b18-af41df5038ca)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ee34e58b-44b9-43b1-83c0-1462377371fe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48cff5b1-408f-4320-9ac7-a8f7d1b07650)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c2f24cc-69df-43b8-bb0c-af746bceaf47)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dc758f07-ec91-48a5-b002-c07f58db7b80)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 20477675-c277-4487-8f5b-519bfb02818b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7f59a31f-7460-4012-b832-fe85b64a4c4f)(content(Comment \
                 #err#))))(Secondary((id \
                 989f5cae-2891-4baf-bf79-7c75406b0cb9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 31f413aa-3ad0-46cd-a9d3-30460528ff19)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b2314048-be95-4e34-8615-b0a318878ab8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46ac7a5b-baf6-4489-a202-3cd3479257ae)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7a8ddb9d-743e-4eb5-9e75-a2c35cf18f1d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a9d85e90-b5e1-4078-b8d8-a0549157a853)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09f52a97-1950-4d0a-9495-1338d4413181)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2908a63c-3eda-40e8-ae1e-ff06bc802cb5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 79ac5bd6-3923-4ced-a692-0b9a7b744b2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 67f15eb9-cf34-47e9-8d6c-0942ffbe5fdf)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ce8ccac9-9ba5-4412-89e3-9fd104ab40d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f6be61f4-8831-4e13-8a62-a374d0ceb02c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a1e069f5-9eef-40ae-9102-07972a12a280)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e3a9bde4-050b-41e4-9c10-391046cb29d9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2e64cf3-6daa-427d-88e8-7975101ef4b9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6d070402-b6e8-4f95-bb8c-6bdac9ad3672)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0e05ff58-e48e-4603-a9df-591aaac252ab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c32705ba-0066-494d-aff7-967cebb0fa3b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ae598555-443c-4876-a2d6-918489dd0b82)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6260b54e-c72e-4121-9637-f86e1ad5b9cb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8549b22b-6d3c-4a7b-bb62-78ca08b868eb)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 a46b1aaa-42b5-45c9-b0e5-8e02b1bae2a7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 074d2663-b610-4457-9986-dc6c989e8494)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ce1f528b-dd3e-4b72-82e4-9cfd3f261028)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b9b7526-7ec6-4530-8732-f76f2902a624)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2d9c3fb7-d997-458b-9c69-403b4a059d80)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0c2e5dab-788e-4d8e-a2a0-ead840435271)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc736538-ad28-4d72-88f4-11e751e05dfb)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d0df19fd-c89d-47c6-b866-0305d64f6090)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 47189112-97f0-4c11-849e-c3a930eabd0d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b881a4e-9d45-4c82-9260-16a1cc32023d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d54277bd-4880-4109-82a3-43a5ffb559d9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 73962e7a-2695-4dbb-9f5b-c8fe28906227)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55133cdb-8fec-478b-a08a-f702dc2790db)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 13491713-9294-48d1-8d57-990361d78c68)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 be1c94de-16a1-4232-8f54-23b31d4a26b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1faab42a-1119-441d-b36d-dd2389545359)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7cd61470-d630-4730-ae93-a8e2a307c12a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b01e1136-02ca-475e-8e4a-c0eed571b309)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8ab3e508-f4d0-43ef-bef8-e54c3a6698aa)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 aa30e1c9-e7ba-4ad7-807f-9c162f3b70f6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 0e256f0a-fee8-4325-91ad-88ec4560e901)(shape \
                 Convex)))(Secondary((id \
                 64a2647c-28f8-48ce-8280-7d8998c8e861)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 464147cd-9a92-40a3-ab6e-5d8ff0e734e3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4a8e263c-93d8-4ef8-8b00-859c0f132f09)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cd2436ba-9b3c-4297-b343-7c410e017eda)(content(Comment \
                 #err#))))(Secondary((id \
                 d045a55a-ae0d-4895-9d2c-8afd882abe46)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 071987b7-bfa5-4970-a60e-785091a5b73f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bf527ea7-5290-4122-9c3c-e17e0f4f4cd6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 daf52b54-5305-47df-ae32-4d6f766465dc)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ad1bfd7f-fe78-488b-9997-8b9f505bd0a2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 61b0809a-dff5-44d3-a85b-04719de26f8c)(content(Whitespace\" \
                 \"))))(Grout((id 4b75c1b1-a9fe-404b-883c-a17b0564c7aa)(shape \
                 Convex)))(Tile((id \
                 83850b65-0200-4975-9289-38371cbbad77)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3ebf6ea5-79eb-40b0-9325-820f24c15bd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee62a854-e7b3-4261-9e4c-f8ec75b0e836)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 eb0ebf5a-e135-4a3d-884c-6adf04130dfa)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 87e20ebf-ad89-4fb3-b254-0dd26879784a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26c3f5c3-ba42-4bd8-b659-62a2b53f552d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9677bda0-905f-425d-a046-6e43b451cf60)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8261e8c6-19f4-4c27-8408-64c0498d93cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 203a3755-a74a-47f5-8897-d9f4d37aeaa6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1cd0349f-1e62-4421-8400-b27d36db3975)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4c835c5e-2d18-43de-b230-29a4e61c1b2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fcb12c8-0c4d-4d64-bb54-25be5800c871)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 40513dc6-bc34-4d5c-9344-0bb35140ee64)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4e9468c1-4ee5-4fb5-b9a3-98b59ab7cd13)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c653ffa-051a-446f-aaf0-eb62932cbdc3)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 008eece2-1a23-41e0-ae38-5947384a0a82)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 67a2783b-6d5d-4b7c-b771-1ab69b302921)(shape \
                 Convex)))(Secondary((id \
                 3dac8831-2320-42b6-88c2-827b386fcb1c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 75617e6a-64c6-418a-8503-f1e39c628449)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e1893f15-acb9-4f18-877e-3548de720753)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cbc5c420-84d2-4178-9dc9-149a5dbbd32b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 18e5d75f-340a-46bd-b1f1-3b127c4e10fe)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 849812fe-577f-4471-aefe-7006caff1ff9)(content(Whitespace\" \
                 \"))))(Tile((id 84502dcc-48ca-4991-9d13-f97ef6727448)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 2849fdfe-cb6d-41c6-8b80-ae7d9482b15b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 3326ef3c-ac3b-4ac9-a849-2d0fc0dca0b0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6579fff1-7545-4baa-b36c-9c9bb5464d6c)(content(Whitespace\" \
                 \"))))(Tile((id 34bb8fcd-43af-45d9-9be7-42c4f2208ca9)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 c9ddcfc1-6d53-4a64-9dbc-b17fd1703509)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 92e9fb97-07e9-4bfe-9dad-840508de98f5)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 73e14451-97ee-4520-be15-05bd0db4702f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00e68301-8de6-4cf0-a324-bb6b0a8b2297)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 eae42c6e-82bd-47f6-bf8d-eac1a4b98b69)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 42d98614-7a40-4daa-a224-42eb3ab2b818)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1d2c945-1118-415f-ba6d-a3148c399f0f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4e2c61f0-1415-4e78-8cde-87b40a465fb4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 79d72e8b-1af4-4344-87e8-3c6c7c3b03de)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0443a79b-83aa-4836-b65a-c52cfd72349e)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2303e895-1702-43ea-b44b-e39ca173e5f5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 26fcb1b8-0d96-4e1c-867e-12e195358781)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 35293cbd-c971-4d95-a51f-e6d4e8b096f3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5fa849eb-66fe-4c51-b29b-b9d893bd757a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7b4fba3c-fd0d-4042-b1fc-2ff0fd9f1d20)(content(Whitespace\" \
                 \"))))(Tile((id a7eac982-5a49-495a-bf81-cb5afba3760d)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 69763d09-1a7d-4fe2-9ace-0d36b80f91e5)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 a920dfdf-6d5f-4041-8dc6-0333aed4755f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1531f2bb-2cb0-448f-9d04-9c457d3fc95e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf0b447e-c479-4392-8a6b-a0ff5f182c9c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fc53c7da-ef7b-4fe5-be5e-ae94b3ad79d9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7c7c87a1-b4aa-44ef-8018-595e0b1e2d86)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a6bc870f-659b-4665-819b-499963681498)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 22407b83-31f4-4abf-a71d-fdca851e408f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1e8c6ef8-9bc9-46a0-ab5e-5fc904cffdb7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a891ea4-892f-43df-87ac-93d1b3c5225d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ebffbabe-6d1e-412b-88b7-015e85161de0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f411fd68-9014-4d34-977c-3a815cb56fd8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0eff4135-be17-48e5-8ac9-d5b8d6dabbb5)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d1e5f6c7-0a8e-40ba-a948-1f2c0e9d36f0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f50e5c3f-0aa9-44b1-b003-16885a911096)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a74270d5-22d8-47ed-a78b-bd23dc49f201)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 3d32a923-86e1-4f8d-be7f-b28e9da6e03d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fb232b21-037e-4396-be78-0f34c2b8eef2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 be0edb63-7a25-4d3c-b253-40acec995c3b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7f62811e-7f93-48b4-ab9b-8db7b1f4b504)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 bedaf5d3-fc9d-4332-bc85-0a959dff11b9)(shape \
                 Convex))))))))(Tile((id \
                 f6c0626e-3f11-4c35-80a1-5f22acb86d42)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 11161b7b-25d6-4750-9a95-5e064ce8301e)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 67e454a8-99e2-4870-b6e1-25db842a690f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7e5e8a3-1f0e-476e-b98b-1ee40e21b75d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 62f634ff-bce0-46e1-92f2-542f904cbaa4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3e98c2ee-fc30-4d57-b95c-865f9292b9ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 08366f3d-4d69-46b4-bec1-6b1d3d013836)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f22b3cb1-cb71-4350-8c17-fc8e9e514491)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3ff05eec-6e2a-4401-9a48-354e23b900f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9af3d709-9ccc-4524-bcfb-23e6e3680913)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 27a56d08-4baf-444d-89fc-e4acf0ad2b21)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f2c35e67-ab54-49c4-943e-29c2916185be)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 69aaf811-1881-4a10-a280-d4d098f97c28)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c6f8421b-b6a2-463a-b0a3-e1393bd2fef6)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 757bfb96-35e8-4f79-8ba0-f1b4dd976429)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 456f6bde-c4df-4c86-b023-e680c747e48b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce62f0bd-f40b-4470-832a-63487e505f33)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bab4da87-09dc-4058-b2f9-32b4bf58fdbb)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e2f3a5ec-9490-4756-ab2f-b4a27c734c20)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d239fcb9-cd2f-4060-a4ae-2a3b9ab61544)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c997b420-b52d-4fca-bef4-d3fb04a6dccf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a36824c1-819e-435b-bbab-8e7c0f7c67c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce130c9a-cd13-470a-9f83-ec9c4fc7d31b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 8889c4a2-ff75-4b11-9a71-3b1d7063edeb)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7b94d17e-6ddf-497b-9569-16f0ab0bb637)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a8e261cf-112f-4bfa-a9d0-d61c4e4a4349)(content(Comment \
                 #err#))))(Secondary((id \
                 ddc92bc7-2b1e-43f7-b275-fc39f111e27e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b352b5e-29c4-4c01-bb8f-8f0e5d16ebc5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fa50e6dd-534f-4a4a-8b1d-95b6b74e025b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4c95d335-1c1b-42c5-9d3b-d62b051e2579)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c61e2a5b-43c8-488e-b0e1-ceb189f1a243)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 df26ec41-201d-4dfb-b021-b2f180359f89)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe3a9ecf-13e0-475a-91ab-a97e3a413646)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 60ed4d96-88c5-4ece-8f69-89c251c0198e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 792c6f35-8946-4bcd-a2ab-2e2202b51a34)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46a5a334-fbf4-40e0-a297-a433988e0677)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 34557f05-a7bc-4027-9d7f-bb6273877aa1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 12fddedc-25c5-4db0-9023-edbd0bcbe05e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c171f94c-1297-4c96-99ce-76d1fc4980ee)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 596a1b2b-92ce-472f-95c2-fa817e9b559c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 016a6dba-cad2-4e70-8f19-49832d7efc84)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6aacad0d-56a2-4dfb-8b38-2c3159fd600a)(content(Comment \
                 #err#))))(Secondary((id \
                 fd748768-72fe-4f70-bd6e-a3e1b5eb0a66)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b57eb7a4-0de6-4b39-9088-0ec5b0e44f75)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1c4ac144-20e4-4438-8b8d-5b0066491207)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Grout((id \
                 df108c50-c0d9-4168-815e-b5757b0ac667)(shape \
                 Convex)))(Secondary((id \
                 f637d6cf-5631-47b1-b0fa-99eaa7a57aea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 08d0986e-c875-4156-961b-9f8597592f99)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 53443295-98a0-499b-966b-49100981169e)(shape \
                 Convex)))(Secondary((id \
                 765eadde-07b2-41c6-ba4f-f2dc772e78b1)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 f41a3917-6a70-41de-9b98-62e6536efc70)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5c08a937-9d25-4c7a-8e7b-27f18edf7c78)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d7014c42-698d-45cb-955b-04291b6c3ddc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8b0ba61-7a7e-4453-9e93-04ac5543f385)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ea4dd79e-9a51-4d8f-861b-b13ad180501a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a2f90c76-6f2a-4248-a719-70b8e94b4fb7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aeb9f94a-6684-4bd5-bc4b-b3cffd245a99)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 15b6b4a4-71e1-4d71-ac03-426d4be6f1a9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 88318f74-c29e-4edc-9bb8-79335b791c08)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2b486b5-315c-407f-b123-12c3b08b7b17)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 6d38d9f8-2177-47a1-b8eb-e2b05fe4ba6b)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 683fab76-2ee6-4f5d-8e2b-a162d7fb2691)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9742475b-bc73-4515-bc8e-50d207c74733)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d445c53e-2431-4fc4-aafa-d74301c4e0ff)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1d9eba02-4570-44eb-8494-a36010d14805)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a7c167a-5584-405b-a8ba-9ddd392c0825)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 86cd2772-5eef-486a-9653-2d15ed1bf549)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 4b02cc46-4a2f-4610-b42d-829459059f1c)(shape \
                 Convex)))(Secondary((id \
                 49cb787a-95f8-4d0f-927f-71f9d4cfe0bd)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 841e429e-85ec-4e05-baf2-f04986aa573b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e7977305-f527-496a-9f27-41e4ec92f432)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 95ba992f-a45c-48fa-aafb-f7c4952c4c52)(content(Whitespace\" \
                 \"))))(Tile((id \
                 669c41ab-1f1a-4d2a-a24d-be83a9a33f02)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a410677b-76b3-46fd-afc3-a96f9720f34c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 424dbe6c-5299-49b8-a785-38696bc905ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 15e7db2d-1531-4921-8346-384b1a238d0a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b8110ea4-7e48-4735-bb3c-7c7e1dedb311)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f6bf3673-c9a4-42b4-8e73-8e30246a25bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5142dc56-23d3-4a2d-bb13-f1b10c8011b5)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 afb4fad7-1c2d-442a-8217-4c2f71341f53)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c384c3dd-0006-47fb-842f-2af015837b3e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d44864be-9054-4c48-9a74-b89f2d6c4fa2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 861e6539-727d-465c-88fc-02698779b6c3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f2e3fc1b-e131-4639-8a3c-6e9282b237e3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0c7f1e0-df50-4e6c-b1f6-fbb5f70e4bd5)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 09c27812-d909-4e3e-ba43-d35e3106a478)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3ad6fcaf-96bd-4fa2-833e-74246f5e0715)(shape \
                 Convex)))(Secondary((id \
                 9566389a-6c6d-4a4a-83f5-39da1a7b3e4b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4f55b36f-0fae-4636-b56d-404ec5c46589)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 a1313df6-cba6-4fff-a46e-23e75adbdc6e)(shape \
                 Convex)))(Secondary((id \
                 8468082b-efbe-48d0-8896-062debe73e51)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 7647dfd2-7118-4f50-837e-e6907e3a4ca4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ed0b2510-6563-48a9-822e-cee2c49c346d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b5927285-11d6-4cf6-9c4a-73030c18f255)(content(Whitespace\" \
                 \"))))(Tile((id \
                 004c0b34-619f-4694-b613-16d390d80f9c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fa044804-771a-4562-ae0c-53c27ded2595)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fab74f6f-d7bd-4348-91ac-05a44b880c98)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a74002d-ea83-4f85-99d2-a70ff4950755)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 345311d4-efb1-4def-afa0-a175c555aa0a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9f620d5c-8263-4daa-a100-a0dac209c2c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62fb1576-e957-4adf-b3b6-0b51a598d809)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 17abaf61-f9c9-4c6d-a153-8c3a797aca8f)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2f43f7a1-88ee-4ecc-9c1c-cd9f10e98ed1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2872d9ad-0100-4f86-8bc3-f3fe03f7f79d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5df66401-05d3-4010-a3f2-9662ce879a1c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5add3a23-d8aa-432d-9319-e5f6b0d27121)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55b410c6-e46c-4723-9f12-f0933ef51fce)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8f08b691-23a9-4233-add0-0f4b6cc9add0)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 416746ef-9cef-4569-aa57-f8b6459b6a32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 077f36b6-16c0-4c6b-bcd5-23c853d65732)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 30dddf50-2d16-4d42-a004-39d27b9275cf)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 12e21880-954e-4926-b6ad-e1dc5a1eede7)(shape \
                 Convex)))(Secondary((id \
                 b51ba1f5-78fc-44f1-b6b6-23dcbdecd001)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 9a2f4a5e-1e3a-450a-a9c4-ad3a57cef329)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c6da446a-7926-4910-8200-555c622cd804)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c3a44a1f-c2b4-491b-bf70-23f57fb17ef3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b419bbb-7dc8-4859-9587-8e2d599d7184)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f543d0de-2407-491c-9e59-2c73b370b072)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 354e6ee4-839e-4575-9482-b94a2c23df33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4eecf63-bb4c-4265-83b4-ca862b725ff3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8a1aa7f2-f91f-40fa-9c10-b570208eea74)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 461d2ed2-3a87-47f4-83d0-3e6edc102ef8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1fc7eee-88a1-4903-acde-2b9d5f26afaa)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 30193d03-3fcc-4ba9-b76d-941453b2cccd)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 90dd09d0-6c8b-4797-98e3-d40249cb6af4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 459ffb01-9a00-4a2f-b4d1-5cce3be09978)(content(Comment \
                 #err#))))(Secondary((id \
                 f2380f89-e9dd-4ca4-883b-eef8ebb09390)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2e86cbf4-6865-4e5a-91c5-5165c00064fa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 95f83dfc-c4f6-46bb-a85c-6f762759884e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3164c0b9-0368-4fa6-8ff7-f6bb29016001)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ebfe7901-8335-44aa-b45b-796c614f0f52)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 58f8ff1e-aeec-4126-babd-aaa1690b7ba3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dd1ceef2-7852-4205-9862-868050f1d843)(content(Whitespace\" \
                 \"))))(Tile((id \
                 140ac2ca-c689-4cee-b445-ba3bed118d17)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 43405613-573c-4f3e-b16f-521c7953643f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff0c9159-ec7e-449b-b4df-91e46a6382e4)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 df703264-43d4-4eeb-b132-fdf5f10dbc54)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dd0343dc-4476-4e61-9558-46f3bca5c257)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a08dc31f-66fa-4ec7-8bb8-18fe3cbbdf4d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1c7f77ed-6707-4f55-9c80-859076ddefc5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c2c72ae-8a4b-41ce-a237-5988274c4683)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 17806f43-bfdb-4ea8-8053-ad3a85db22a1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c318a768-fa90-40ea-8441-69bb9c5e9832)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5e3a01d-a819-4378-88ef-d3112f5a4477)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 004d66fa-6031-486c-89f3-20f8748daa88)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 68c38304-12a8-4578-8c3b-8343499baa26)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6229772d-bdd0-453a-999e-3e006ae33416)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8d36d613-2e08-4ffa-baec-c739ceccaee8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2d3e3c51-a996-4f83-b055-ccfc01861a55)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d04ed606-68a5-48f4-b3bf-4a1af6f64b1a)(content(Comment \
                 #err#))))(Secondary((id \
                 0f1fc25c-13a7-4fd2-8d2d-d94e4ab9c0f7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9fdd0e1d-5d84-4f33-a563-42502a3d5533)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0821753e-6a78-4f98-bd4f-932498163e1f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c2646564-c4c8-4a12-a718-fa9140d20b0c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4a7beeb7-84b4-4515-95e9-441d65891fe6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 8fc9f048-8e01-4286-ac34-0c38b3e7cacd)(shape \
                 Convex)))(Secondary((id \
                 53684ce5-6ebb-4e53-9191-bb7c52e7f74f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 57a037f4-19fd-46d7-a38f-507677eaa160)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b6469fe8-6278-4ab2-bbd6-54a9aba7fb38)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b1d2fa9a-e316-4268-8425-62bf13b586e5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 079cec0b-dfa7-4bde-b3f9-4db61ca9e5f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e17739a1-33e8-4313-9e36-7ac9c6f912ba)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b02628a4-c7bb-44e4-8dbb-5dc4bb9c30d7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cd50a9eb-72b4-4903-b176-39c36b002b80)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f09f9b11-57f7-4848-baeb-117d35e16e8f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 12849781-d5ad-4e97-8b09-2960a982768a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 263ebaad-6efe-4488-908b-fddcb5e94682)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f0af802a-92ea-478d-bc67-7819ef15edee)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6a81c2a3-7fe8-4588-bf3d-84b7aa43dc9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6b805c4-5cfa-4255-8164-c5ba73560d15)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cc9ccda7-03e1-4434-ba17-1f48f8541370)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0602c198-4496-448a-bde3-59140c3da8d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4abb8fb-788f-4b06-8bf8-843977ae3119)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6f812adf-fb9d-4874-b6ac-6ac963364b0a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4ef7fb31-e7f6-4957-95b7-0564c4a8c349)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 eb87ec39-4d66-448f-b49c-7d40eb8f1d83)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c25d8abd-bff3-4350-acb7-4e08facd85c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 660392d9-f207-420e-b510-8227c67c21d4)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 882782bd-01b6-4e09-b6c7-3a82ca20f2e2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7b0f948c-41a7-4b50-bb7d-fb6216d2d082)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7d8b0cf3-b652-4cfd-baa8-4cc6e6dd71b4)(content(Whitespace\" \
                 \"))))(Grout((id 08e3557e-558e-4dd8-a3cc-9fbf7cc209a5)(shape \
                 Convex)))(Tile((id \
                 12282b67-1115-4e78-90aa-34f94e91184b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9fd1ce79-dc65-4474-9412-f080c5537509)(shape \
                 Convex)))(Secondary((id \
                 1890d8dc-2ba6-48ab-88d3-4006ed79e3fd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0fb1f007-a960-452c-8f41-6375196159a1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 084e80e4-b475-454f-a58a-acfb1078f381)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8dfab19-7f96-443f-93fa-7971c113173b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e92f6280-c2f5-4969-810c-327981125f29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a0e0620-6ac0-42e5-80bb-3ebffb8f6a28)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 aa57d48f-6699-4226-906b-805615e67d4d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1327231d-87da-4dbe-84a5-3178fcf04e13)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7abb9f13-cc45-4ffb-94b7-6d37c28d8b0a)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3cd8a69e-4682-45e0-812e-6f5ef9720620)(content(Whitespace\" \
                 \"))))(Tile((id \
                 617e659e-b948-4ff8-a212-4c3f010c19e7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 31b745cf-5e4d-41e9-b01f-11d90067b76c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 092729b2-9d8b-40b0-a242-53ae0cc1b8c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f395313-14d2-4457-8453-26c40832815c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5f54134f-fb77-4fb4-abf0-082458a147d9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4bb485ad-225a-4f20-bca9-ee9661c5bd77)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ac85fe4-1d01-4269-b132-066cd494e95a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 16658f2a-93d8-4e9b-80cc-f7eaf1dddfaa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69725f26-7c32-4d6b-8764-2ba651566a4c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a3a5a3a1-a882-411b-ae14-afb7c78499e9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 64144450-0284-4535-b820-321d9a0da901)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8fae3866-e984-4133-b163-51bc4ae69c43)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 afb6f8a7-72df-4e1e-9f4b-733c883ffecf)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bac71964-46d8-49b8-9ec6-3651997ed884)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3e6d0294-9ced-43e1-93a1-65605c862359)(content(Whitespace\" \
                 \"))))(Grout((id 5d1885e9-e5fd-4abe-b54f-cf6ad66a112a)(shape \
                 Convex)))(Tile((id \
                 d3b8d5e1-dfae-4aa1-a9dc-173950411aec)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f3cb314d-8a35-49c0-96a9-3e0f5bdafafb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f63ff64f-4d8e-4d4c-9049-d4cc9ed35c30)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 55586d4c-97cc-486b-91ea-1d8c405207ae)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9a655ac1-26c1-45f8-a90e-bb3a9fa41bcd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16b28720-f74e-4985-9e6b-d070853b5004)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 bc482024-365f-4b70-bd34-5c9ffce4cb69)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92c99cb7-640a-45dd-8dfe-a757b7c0c722)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0ee3a114-799e-40af-9e6b-b0576f703780)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2815fe68-7c8e-4752-b6a2-14d4a0d48849)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bcf74366-a6d8-4d97-b0ed-a7082038ee83)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ca031dd1-6568-4338-b5ed-8b31dce177aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 411d76e2-b517-4980-8ccd-3c76d20159eb)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e093bb5d-d524-47c2-8d08-503adb8e119e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4d9c1e50-f075-4ee1-aca0-92a046f34f8e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8fbeda53-cd9d-4fae-ac30-b89d54a28e4d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cdb0e0c2-b142-46ba-ab4a-044a520d7283)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3cc81af3-0b1c-478e-92f8-10c8661fea39)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b31f542b-1879-4818-9fc3-14c11bfc68de)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cbf12f09-fe14-454d-ab4e-5671cb5dc1c6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b59f80c2-de42-4ba9-9db3-932671a096ca)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a3abd836-4a24-4447-9f58-d5645076a333)(content(Comment \
                 #err#))))(Secondary((id \
                 fc0f9a72-120b-4bcd-a0dd-98cd163aa0a3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 78d05dd4-1a91-4ce2-a3ac-a7172e973d84)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c280c04f-5ac0-4c12-afb6-79be61e7c944)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2b888942-57c4-4501-ac96-8cd3ad887fd8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 298be344-8ac2-4e70-810f-be99698f575d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0be33ab9-0632-4a9e-a731-867576b7fd8d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6da5a539-54a7-4c9c-b712-04ef1d4973b7)(content(Whitespace\" \
                 \"))))(Grout((id 72622df9-abd1-4e5e-b46a-5ade609646f2)(shape \
                 Convex)))(Tile((id \
                 9767c165-17b8-4c96-80b7-7432d9d7b409)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 78cf0e88-4664-477c-bdd4-ad19dcbd4dfc)(content(Whitespace\" \
                 \"))))(Tile((id b233ec48-8b67-4bb7-a31b-9c0329d140cb)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 ccddee65-92f4-4f1c-add1-bc122bb5f6f2)(shape \
                 Convex))))))))(Secondary((id \
                 ef3dbf85-36fd-4326-9049-21ed0cb7bdbd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6d638893-e07c-4daa-b77d-90887191515e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 af927b80-da5b-4955-a8fe-55f31591f537)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f43813c3-4f84-4b6c-b764-18c6850d08f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4290506c-3c82-42f0-a038-0cd34cce0348)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b778002e-7717-49bf-a1f8-4e58ea0f909b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 046a5adb-25b8-4924-ac59-d308b45a374f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9ab67b7-9e25-4801-8e87-ea3c05f13bc5)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 459137ac-6067-46c3-84b8-088b83a28258)(content(Whitespace\" \
                 \"))))(Tile((id \
                 229bb6ee-7bdc-4ffc-a98b-758d8eac8f8d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0a9d520b-7bc0-4ef8-91a2-bd09db543c26)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 68a6b56b-2481-48d3-81b9-e79067658392)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c8667fe-6175-4396-88ab-c581301e337d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3df4ad43-e643-4313-8097-90cc4d16c73c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3dce021b-72ef-4b7b-9886-28cd85f6b526)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab64d194-4918-440e-a1bf-dbbdf85ec258)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f5890f8b-673f-4ad1-a10d-9220b7b664b6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 acd71dcb-faa8-4b92-8cfc-7dbfcdc7d0f6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 143fbecb-9683-4fd4-b402-db325782712e)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 ba78f151-d3bd-4c53-a04c-a98c225dfbd2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2f137868-9f68-4391-8b0b-26e953860f9e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 192cafdc-c77b-4780-b236-6dfe04361afb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 ba34fd2c-89ea-4a82-81df-59dbd03e1922)(shape \
                 Convex))))))))(Tile((id \
                 aee2770d-9f69-497b-8683-11e590fc769e)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 63356199-a7d9-469d-b899-2e5a3c12dfb6)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6e1c8956-c494-41a7-b19f-a45577fa1046)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 df943b2e-fca6-414d-9958-cbbe004de9e4)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 787fe832-948c-4163-84ca-5652d0287c23)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95fec365-da50-402b-b8c5-0e68f8ee07e2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 00630ffd-c59d-43ae-95e7-d940c00acd74)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bc3969a3-4339-4d37-9ee0-6b76d79536b3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d90e5e06-78b1-48e5-b9be-a9fc31744a6f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 46b27850-1592-43f5-a659-83cfe56f569b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3d2b804c-3602-4354-b740-fd057f32c9b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b8ec729-de5a-4cee-81ec-7b3bc1aa9b54)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 cb6dc1f6-b324-4bf9-8e67-c37851c5b178)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c00f8bba-a41f-433e-a177-232b6bfb26db)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bec9f724-f395-4ae2-a345-efde82343317)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3951f3ce-b986-4cf3-ac5f-fb2432d41984)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 644a6408-98b8-4d1b-b7a1-88d78442045c)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2f01039e-5dae-401b-aef9-4b9f0b86c198)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a4fc8bb8-2dab-469b-8811-66946600fc9c)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0305c094-0a03-40b5-88ce-439455cdae92)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca1b8232-2317-40c7-a6c3-911dacbd3f9e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 94924006-1fcd-4f30-b376-92e69df114bb)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 737e39c3-913e-402e-babd-679a5ed31318)(content(Whitespace\" \
                 \"))))(Tile((id \
                 144c7b98-984d-4fc6-bcb1-50b110c9b48b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2913c19d-0fdc-41ff-a26c-d8521bce7900)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eede5859-e065-4f86-84cd-467b911670b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c36c628f-3dac-4c6e-ae1a-08b25bd6eda8)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 919194d7-4c9e-4855-8830-ae3e6bdb1f0d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a8128d9f-2be9-4445-8325-8d2676cb16ea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7dbfe96f-e215-4a36-ae5e-677a0c0a677f)(content(Comment \
                 #err#))))(Secondary((id \
                 c51d2396-599f-447e-9f37-4d15f556af4d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a63ca2b7-e685-4313-ad7d-2c4a8ad87cd6)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6c05a756-6f4a-4eb8-a2a2-a416b77e8ef8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d6f59b87-e839-4654-9f27-c4ec09541956)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f7401fe8-bf1b-4c47-a5fe-ce5e9f5a04d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12bff302-321f-421b-bcf9-2c22c662a36b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 650672d6-59aa-495f-b78e-6c53bf8b6efa)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 94e262b5-2ac5-47b3-9688-0dc4cf124f98)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7a3e2506-dc6e-4454-b2c4-da23963419c0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fae497c5-4834-4d03-9fa7-bf8b5170ea6d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 35ba026e-1141-46e3-80a2-2ae19e32c3e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa413adb-920a-495f-9fa0-70fd0a81eb11)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f65f921d-c5e1-47f0-bef6-6d7d89494c93)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 56a6adb8-bcda-4755-86ce-dd00c623def2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6582d0cf-c40e-4cac-ae54-f11fe06fefcd)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b19cee70-541f-4b07-ac29-31934f17ae27)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d3b229b3-41ec-42a2-98e1-072e600db541)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51d3c7ae-03ae-4351-810d-5b7cc5b4700a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 60a74bf4-86a7-40a1-87c7-7121900cad59)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 75c6a011-74d3-42e3-8053-0c7425640e0d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 be0dea69-4cb5-40e3-a200-69929dbeba07)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 58af5be5-3306-4bd1-ac60-ab80ee6c6eed)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 9d84a383-2e70-4453-9b10-2f7a19eb184d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a5b4b336-2170-4485-a728-685fa640c231)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 1ff4fe9d-78e0-4299-816b-ffa67b78d59e)(shape \
                 Convex)))(Secondary((id \
                 1b5e4c92-2c99-4a09-a3c4-b8dd557cc6df)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cccb0c03-34e1-49a1-84e3-15eda593b4cd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 34a89c48-61ea-4e10-b0f3-f96f7aad8d91)(content(Whitespace\" \
                 \"))))(Tile((id 0ac0a913-5bd4-4fab-a5d9-0a5daa3caf6f)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 5d6f5f05-f059-45a3-a1af-3a2c15f74184)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 97f109c0-6672-4305-8dff-ea7beb999c69)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8fa3b40b-c45d-4c26-a0e5-fce57bf73546)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66ce5854-9583-42c5-93e9-5c1fa42dc759)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 003c73f2-0ecc-4915-984c-de850edbaeba)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f198a31f-9dfa-49bd-ab27-e700c6636b0c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 478a4f3b-c828-4ad2-b7d1-96aa9e031960)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 aa4b0228-fa76-4811-9c30-8d038c39560d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b6830cb4-d009-46ba-91b6-1a00416e36bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 893d6d04-9e9f-44b2-9427-b8000692a85f)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 5f7e7a02-b5ef-4538-823e-1a0e835afd7c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7170d961-d067-481f-9cba-9e4753336ece)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 00032a5e-a823-4f54-88eb-27b9aeadca66)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1bbfad49-b66f-4c63-b059-8f35f08253f8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ec853ee0-7283-4414-8fa4-e5d93efec63a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0020e6a9-aff6-4705-b26a-8029c754fc6e)(content(Whitespace\" \
                 \"))))(Tile((id a0bb342f-bc4e-4953-9798-a51a8f024c78)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 34950bac-bb0d-48bb-9685-445136ea7073)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e913ab09-16d3-4b37-a04f-4e16de5f659d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22ccdfc3-d8de-4fb5-afbf-0aaf4068e594)(content(Whitespace\" \
                 \"))))(Tile((id \
                 288f9609-3fb8-4ba4-adc7-185a48b92880)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e5fea14c-60d4-4c4d-bef6-c2bc9ea417b7)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8ec00071-faa7-4dee-a1e9-4e4ddabc7824)(content(Whitespace\" \
                 \"))))(Tile((id \
                 05401502-9ed5-4ec3-8528-a9cac3210c68)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d0876cc5-b59f-4e31-8e3b-398f5f8f5c41)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 157cb114-a335-48d3-b2f6-703206ffb5d0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f4f09640-bf62-4c03-8c35-df738fcae6e4)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 4554d0f8-26c2-4634-bc14-40c949f767ac)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 535b9dd3-9a0b-4eb7-8e2a-f48db40ec6da)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bc3f580f-e8b0-4e3c-b201-279988545f8f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 15c2504f-449f-4d85-bfeb-7b217513b4cf)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 889db735-b5b2-46f7-afaa-9f168187e1c7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 89ce7e00-5bb2-4f90-99a6-c39fe15a6aeb)(shape \
                 Convex)))(Secondary((id \
                 739b13f2-b9ea-48d2-9ac9-2013a7297b7b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c1f22ef0-07fd-4452-b929-d4ba8cf6ee98)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fbf360d6-44df-4b44-a69a-2134bfc14d61)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 871f0ca2-34c3-406a-bd46-4e0363ec6360)(content(Whitespace\" \
                 \"))))(Tile((id 8a641915-8ade-4ea2-8857-dc523f25ddc3)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 c5644ce4-d25b-490c-84ce-4c056ba3cb5e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6182467a-48d5-4018-962a-66386e1f11e5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8d2b1ced-9d32-4f92-a248-6bb203f17d0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7a8c7eee-8d0c-4b86-885e-9f8a0e9db17a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 561bbbce-0963-4432-9dc5-49e8941cf174)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 90fbbb1b-5513-4792-86f2-d33749900369)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbeea633-c59b-4472-aab5-b2aaa4260901)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c8804963-9a8b-4e8d-aa29-5efb74bd2b24)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 036556d2-8638-4f5c-9d03-180f8d8053cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 89e1523e-d878-41fc-89bb-7caccae3647d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 aa4d9f79-136a-40b2-96c0-ec8adc09ae5f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fb942de3-857f-4832-90a0-b06b7bf886ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b0332cb-53d1-4bf5-a3f4-516563ec31cc)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9e081315-d691-4452-a27f-d67f3999ead1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 22cf9423-56f8-4dc8-b141-5b7d0469adb8)(content(Whitespace\" \
                 \"))))(Tile((id 9774ce4b-36e9-41f1-9873-5da2146acc22)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 9ebd2203-8ef5-4365-bddf-9a94cad59041)(shape \
                 Convex))))))))(Secondary((id \
                 5a133867-44ff-4a46-87b8-e48b489af29d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e515aa3d-0612-4691-bb34-08e486e6c83f)(content(Whitespace\" \
                 \"))))(Tile((id 5007be46-ef36-4362-ac66-8971afd303c2)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 9eec9018-80af-4928-918d-4755561a5c51)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9c158ab2-b2d0-4504-936e-4e2c1fda2dbf)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8c825c6b-9ba8-44f9-a6e8-834baab1d9b6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 182faedd-cb27-472d-81d9-7750d46cd4a8)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 32ab12f6-bf3e-4317-9500-08ccfb995495)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 779156a1-b416-4762-aacf-bebcdf1269c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dff9da7b-7272-4d46-ad02-8c467445060b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9f84fcc7-6f31-4d4a-84a6-bc80303e948c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 436d818d-9d33-4bb7-93c9-dd562c3e20c4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a13c27ec-68fe-4900-8c5f-23b90f7bf965)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a0ed3306-ca30-401e-adc4-aa308e918f00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae639e79-9dfb-40ee-8972-c817c85e4520)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 38e7e2c2-df65-46ca-b11c-43ef2eadb9d8)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 267e519a-920a-4eb0-ae29-b9bd7ec407f5)(content(Whitespace\" \
                 \"))))(Tile((id d6463c9a-5fa5-456a-bac8-7c46c743c446)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 8e0b360b-be6e-4973-b137-c370f2dad196)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 247f47a7-6b87-4845-84a5-167f9242065d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b1757d2f-0d31-40f7-8997-68bd63208c3a)(content(Whitespace\" \
                 \"))))(Tile((id a16c7a1c-2300-42ec-aab2-36023bf2ba65)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 8747f047-a893-4fba-aaf1-b69e988fdfad)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 49206b7f-984d-4e6b-81fe-d155f9f6b38e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a510a246-1764-4c6b-a0b0-34adf756c747)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d63932e-fa4c-4859-89fe-2cae635f565a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 717dcce3-4359-49b4-a613-096fe9386336)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 92e539d7-5e2c-46cc-b45a-5cabbab5a385)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5aefdf97-5b49-42ce-8d0d-aaec0ff5202b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c7f542c3-3123-4151-bb4f-99f9cf79e3f5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9baa5c13-b59b-4090-8bd4-5143dbe38c45)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d41beb7d-2962-4ea4-a4a6-b36c42272efd)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 860d6b7d-411c-43fe-a339-2f1567f01600)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f8982805-1c09-4bf2-a14a-d2763927aa5e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 46af813f-9bed-46b3-8229-d01c6f9c4726)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f7e86dcf-0387-4912-a032-4cbf8b50ba22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5db7ca6-f59b-4ff2-ac05-127e85d136ca)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f88a0358-1eec-498c-882c-bc627d2d6ec3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 baa3eaa8-90e6-464a-b425-ee209ce8f3d9)(content(Whitespace\" \
                 \"))))(Tile((id 4d5a245c-c9f7-48d8-85ad-1b6328c34023)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 6c10c2f1-510c-445a-bdc2-d1f19c5b7441)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9731dfe4-693e-4814-a8fa-b0a1df373c21)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b96d149e-e7db-4f96-ad0a-f561986ccf51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b95066c-998f-45db-b108-4afd92d61cc8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7238b3f4-fed8-4f9c-b26d-0b2c43913100)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 b63ff23f-acd6-4c2d-9074-9c46f389ec81)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 53d1ba13-7d07-4926-9778-4341a909f216)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1a9f17d3-269b-49c9-91f0-23a1af2b1240)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9eb07dbc-ee02-48b4-86d8-f6e2ed11dff5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 013ffd32-263c-4d8d-b4e5-5f2b030a1f41)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f32130e4-3a01-457b-8d38-9605fd821986)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a9c8a0c9-2faa-4c03-afe6-2dd868e1f7c7)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 96374039-53d4-4f19-8433-52794c9c04a1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 21ccde20-fdc3-4bca-9d69-f57f0e0a0504)(content(Whitespace\" \
                 \"))))(Tile((id 695df653-a25c-481b-88cd-f9eae4c2e6bf)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 3b84f791-4e98-4cbc-9c7f-d3ec6d9b5958)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 53cc0ac6-5b94-4268-890f-73a007f04a9a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c646735-1931-4d6a-84d3-193eca9a78f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7cdb7eb4-abb4-4562-a12a-a4c52f6867de)(label(1.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 17dd125e-757a-4212-9d14-3e03f2e9ce88)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 022ef8b6-a812-4082-bd70-ebbf935e46ed)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bc427f1d-072a-43c9-8f17-824b3afd29a2)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ae9b6f9c-4338-4ef8-90c2-e8b71aea101c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2671d573-93b5-4953-8786-d5c766e7ba98)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ecd21aae-fb3f-46b6-ad49-dea8be0ce3e8)(content(Comment \
                 #err#))))(Secondary((id \
                 e0391d55-8369-4d0d-b766-e4e86f3853c8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3f3969c9-a653-4748-8d12-f85471006925)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 03a43710-3a29-4c19-8d28-ee25fb092b5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b6accb6e-7542-46c6-b525-2b1a8422be0b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9d394a31-241e-4c1f-bac3-5fc9a6bf1a43)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e15ccd3b-5e05-475e-b9c8-577f00f540a1)(content(Whitespace\" \
                 \"))))(Tile((id 674d8de2-6947-4e53-8443-f13851f1e19d)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 f6458641-1beb-4743-aed4-de99f8d69d29)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 0cd36654-a356-4523-9770-e843f7d09007)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bd2f6c0d-2d02-4576-88cb-d372eb85dce3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d55ba522-4b9e-47e3-9350-e16765fef4bd)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e1be4901-4cfc-4d01-ad30-3f977525d440)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 52f626db-d718-4566-a411-f461f8fd2045)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8c5b6c45-b725-44ce-9e6e-9bf6a872e35f)(label(2.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3142a8d0-6209-4851-94bc-7b2c0614ca58)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6228524b-3caa-4e5c-9a78-210a9a49a3fa)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 79d9b290-bb4b-4060-b063-efbf1fb2f997)(content(Comment \
                 #err#))))(Secondary((id \
                 42f23ee7-4e1d-4473-8b6f-cffd1dc37dc0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d4400cd6-3723-4a9c-9b99-b8bebfc938c9)(label(\"\\\"BYE\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# Internal Regression Tests: Type errors #\n\
                 # Each line should show errors or not as indicated #\n\n\
                 let _ = unbound in #err#\n\
                 let Undefined = Undefined in # 2x err# \n\
                 let true = 2 in #err# \n\n\
                 let   = if true then 1 else 1. in #err# \n\
                 let _ = if true then 1 else 1. in #err#\n\
                 let _:   = if true then 1 else 1. in\n\
                 let _: Int = if true then 1 else 1. in #err#\n\
                 let _: Fake = if true then 1 else true in #err#\n\
                 let _, _ = if true then 1 else 1. in #2x err#\n\
                 let _, _ = (if true then 1 else 1.),   in #err#\n\
                 let _:  , _ = (if true then 1 else 1.),   in \n\
                 let [_] = [(if true then 1 else 1.)] in \n\
                 let [_] = (if true then 1 else 1.) in #2x err# \n\n\
                 ( )(if true then 1 else 1.);\n\
                 1(if true then 1 else 1.); #err#\n\
                 (1)(if true then 1 else 1.); #err#\n\
                 (fun   ->  )(if true then 1 else 1.);\n\
                 (fun _ ->  )(if true then 1 else 1.);\n\
                 (fun _:   ->  )(if true then 1 else 1.);\n\
                 (fun _: Int ->  )(if true then 1 else 1.); #err#\n\n\
                 let _ = fun x -> if true then 1 else 1. in #err#\n\
                 let _:   = fun x -> if true then 1 else 1. in\n\
                 let _:   ->   = fun x -> if true then 1 else 1. in\n\
                 let _:   -> Int = fun x -> if true then 1 else 1. in #err#\n\
                 let _:   -> [ ] = fun x -> if true then 1 else 1. in #2x err#\n\n\
                 ( )::[(if true then 1 else 1.)];\n\
                 1::[(if true then 1 else 1.)]; #err#\n\
                 (1, 1)::[(if true then 1 else 1.)]; #2x err#\n\n\
                 let   = [1, 1., true] in #err: inconsistent#\n\
                 let _ = [1, 1., true] in #err: inconsistent#\n\
                 let _:    = [1, 1., true] in \n\
                 let _: [ ] = [1, 1., true] in\n\
                 let _: [Int] = [1, 1., true] in #2x err#\n\n\
                 let _: [Int] = 1::[2] in\n\
                 let _: [Int] = 1.0::[2] in #err#\n\
                 let _: [Int] = 1::[2.0] in #err#\n\
                 \"BYE\"";
            } );
          ( "Introduction",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 3a26a8b3-3d1d-400c-bc1c-9480376d7b17)(content(Comment\"# \
                 Welcome to Hazel! #\"))))(Secondary((id \
                 3b3ad502-9acc-4dc5-9edb-2049ceb1cf27)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c635fd8c-5ef2-426d-9d16-36118ba0a287)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 755c0166-c459-4f12-80b4-76b6d074c869)(content(Comment\"# To \
                 get started, type 2 * 3 + 4 into the hole below, \
                 #\"))))(Secondary((id \
                 81c77003-4533-4d13-89c1-141c72072e17)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 097b5f4f-b626-4c99-86fd-1616d8d2209e)(content(Comment\"# \
                 stopping between each character to observe how holes \
                 #\"))))(Secondary((id \
                 f0a22093-3c1e-4a89-b316-f478f9614327)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 69f17546-40b5-47e8-805c-8be4eea74de8)(content(Comment\"# \
                 appear at each step to ensure that every editor state \
                 #\"))))(Secondary((id \
                 2899a000-8357-4f46-a0e0-2a729a68aa6e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 263b232c-a1eb-46f9-ac30-73e70c4a6dea)(content(Comment\"# is \
                 meaningful. #\"))))(Secondary((id \
                 7a61306b-2b83-4f90-9d41-d8a2623535dc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dc8ecea9-bebb-45f4-a190-089e8e9f3b85)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 778993f1-5bfe-4823-8757-c7e9cf3f0bac)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 568ccd4e-d305-43ee-9b85-c46d0d19f556)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe73c8f9-b0fe-4b5c-af2f-788f02fd9ad9)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 25ae46bc-39bd-4e7d-a307-b6a8f58ec105)(content(Whitespace\" \
                 \")))))((Grout((id \
                 737a6005-6dec-46bc-90fe-2ea6e9e3f6be)(shape \
                 Convex)))(Secondary((id \
                 fc2e3098-b60e-4930-8bbc-7598940f1ab9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 92b51e6a-3382-49aa-a0c3-f3f5b04eefea)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f927955b-abd1-438d-90a9-768fb641de80)(content(Comment\"# Once \
                 you are finished, navigate the menu in the top bar \
                 #\"))))(Secondary((id \
                 77582acd-88f4-4a77-9bb7-a81e015e0a27)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8d6784ca-5045-4ec6-9cc8-3a038471139a)(content(Comment\"# to \
                 see other examples, enter Scratch mode to play with \
                 #\"))))(Secondary((id \
                 5439f0fc-0c5d-46f5-a1dd-8ebac7550340)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 59982b9b-9953-4154-a181-fbc8d22f153a)(content(Comment\"# \
                 Hazel, or enter Exercises mode to do some introductory \
                 #\"))))(Secondary((id \
                 a3830a9f-b8ac-4893-be82-fa468a495222)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a262aaa6-1413-4e76-800c-5dc2d8052530)(content(Comment\"# \
                 exercises. Hazel is a work-in-progress research project, \
                 #\"))))(Secondary((id \
                 e151473b-a329-49a0-97a2-6e5b8ddb5c99)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4a8b1120-098b-4d57-8f49-6daa3ad2e6a4)(content(Comment\"# so \
                 there is not much public educational material yet. \
                 #\"))))(Secondary((id \
                 96c000c4-401e-49f9-9a3b-7f8868a3a3ae)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 08b3a70d-527a-4ed8-9bc2-2f4db7a65ba1)(content(Comment\"# \
                 Check out the research papers at hazel.org for more on \
                 #\"))))(Secondary((id \
                 fd18ad6b-9b16-41a2-9cdc-638627347b10)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 784077a7-57f9-48fe-8d9e-40512d0de554)(content(Comment\"# how \
                 Hazel works. #\")))))))(ancestors())))(caret Outer))";
              backup_text =
                "# Welcome to Hazel! #\n\n\
                 # To get started, type 2 * 3 + 4 into the hole below, #\n\
                 # stopping between each character to observe how holes #\n\
                 # appear at each step to ensure that every editor state #\n\
                 # is meaningful. #\n\n\
                 2 +  \n\n\
                 # Once you are finished, navigate the menu in the top bar #\n\
                 # to see other examples, enter Scratch mode to play with #\n\
                 # Hazel, or enter Exercises mode to do some introductory #\n\
                 # exercises. Hazel is a work-in-progress research project, #\n\
                 # so there is not much public educational material yet. #\n\
                 # Check out the research papers at hazel.org for more on #\n\
                 # how Hazel works. #";
            } );
          ( "ADT Dynamics",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                 a56159ea-8307-4e3c-83dd-5cfc762bd06d)(content(Comment\"# \
                 Lambda Calculus via evaluation by substitution \
                 #\"))))(Secondary((id \
                 654f83c3-0af2-401a-811f-3abab92b9949)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 49311b45-bd98-434c-8210-fe75f107a312)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 74612343-fc46-4c86-8f09-48320116affd)(content(Comment\"# An \
                 Expression is a variable, function, or application \
                 #\"))))(Secondary((id \
                 fa2c06f5-8ffa-46f3-adaa-78daccf77ae0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bbd80f2b-f92e-4992-b844-d0da483eb8e4)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f2675c33-cd4d-4190-b3aa-0f3857bfa093)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02dd75bf-3fc8-49ae-9819-945b2622233b)(label(Exp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 1d5c2ef1-9d09-4d43-b1aa-2740cecdc369)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 741dfa4d-f55d-4ba1-ab5e-3ab43129b0e0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5bb6e7d5-4f7a-483d-9960-c910eb3a38bf)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1e1d3da6-5c91-43bb-aaa7-37e1bd129925)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d23b94b-2f8d-4101-83d9-b7d082bf6591)(label(Var))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 1a48f194-5b5e-44da-869e-48acdd5e31a4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d23d3aa8-a16a-486e-9610-381e53da2603)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 8a547d84-3501-4394-a4f7-84d970ee72fb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 979aa1a5-ad6a-4dc4-bdb3-d7961011bc7a)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 74922e65-fde9-4327-888e-c695fa00951d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d4ee9345-493e-4e59-9b0a-19312b18195e)(label(Lam))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2b0a2b07-ad85-4647-9f02-6333efa9c50b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 ebf73f75-5811-4370-9bff-cb2b8de3029b)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 def61062-b3c6-4c0f-9b01-5e18117ea2f6)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ff810a11-1e5b-4f77-b544-24f77b7ae543)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34ba9071-f095-48fc-ade5-1ab344a44652)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1becff46-3ac0-4d26-ab87-61ac5514a557)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3ed98337-8154-4cd7-9529-edaee0bd031e)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 147084a1-6110-448b-9a38-e95992fc045a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ff2ad3d-89d6-43aa-9b10-da8ae1646bcd)(label(Ap))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 5b7a3b3a-45cc-46e8-8c72-c725424ad5f7)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 fcf231fa-c10a-4927-8e74-3353e43db8d7)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 c0fc96f9-1c6a-450b-8532-73a642a54c6c)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7cad6ca0-711c-4249-84c8-9d2fdc73783f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f7966ca8-61b7-47a9-819a-669a1ab7506a)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 7f0b5a17-a9d0-493e-99cf-b047b4ff3130)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9f96b192-ecd0-4f9f-8b37-5c14f46b0237)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 e6fd293b-fde1-45dc-a6c8-c3e2403dafb0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 39f6f31a-f4cc-4d72-80da-799027ec4056)(content(Comment\"# \
                 Syntatic Equality of Expressions #\"))))(Secondary((id \
                 30ca4690-39c0-4c16-9d0e-4c0f9d71a8f9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 19081cf0-8089-431c-9cc5-39a4480b80f9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ec04a679-8a8a-4928-8ae1-68f2dbc8b25f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7c9ee5fc-9900-4edf-8a78-115ce6a2f46d)(label(exp_equal))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d4e5973a-79ec-4cfd-a734-ab711a3d8b6c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b5bd43c7-022b-4618-9539-8f554982738e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d07ca938-4ccb-4519-956e-c934eb219c6b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 6a721509-0a8e-47c2-b215-f8b86668d1f3)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 59f12136-7db8-4877-869b-ffd28ad6ac12)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 925dfea7-0796-4de8-9fcb-49830631f2c2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34f14f4a-a2ec-4b39-bca3-b367996440a4)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 29fe7a20-5aec-4fde-be99-a6006f2ff8c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a36a5f16-5d0a-4e48-be99-512b4a9f687f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d79e34d3-14bc-4676-a1be-0985ee93b1fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4722e11e-b8a4-459a-9fc9-7f9389087404)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 9b976271-e863-49dc-8552-785b10f362e8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 11476e73-a579-44ac-911f-1d7daecb548c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3a8a4562-6c1a-4834-bf92-37d7f0b138b3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 757fb39c-9c26-4398-9d11-652fd8aaadc0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 58c59dec-4828-4823-a875-5f7ee08a7aef)(label(es))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 876cc780-5487-4d30-93d1-dc039e7eae5d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b1f7d443-0d3a-4aad-9526-fda706186a08)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3cb79cf3-c028-4a9e-992e-7ce912034625)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9e90c7e2-aaa8-428b-927f-abbc1ac678ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c377bf9-0477-4940-a922-f1d1601177e6)(label(es))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b9874736-bae3-42c6-bffe-0e7aeaff06df)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 95a9fdbf-ef6d-4e21-a7b3-5bf7710c8e1a)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a760303d-397c-468f-9928-2d9418dff21d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1d95a1f-f374-45c6-aa12-7b25e0ebf5d2)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2aa36938-123e-48df-b110-43ad2a651ee0)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 82adda39-98ea-4d9e-8f7a-f99dc6876d21)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 d34e1efa-61ce-423f-a59e-cc687ca11a59)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4295c5e9-e795-495d-8566-ba52da389e47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02cec734-348e-44b4-bd15-eb3cbe8d081e)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ccf26ea7-a7e8-4a33-baf0-e85f2241a36e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 efe34b71-b0f8-433c-bdd9-cc66a735604b)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 38a24132-48d9-4f34-99d7-56e7c9d9ffc2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7acae016-f1a5-4c83-8b5c-e4ee6daba5c7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aff20e26-6644-488c-b104-b4fc06fba266)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 11bcbcfa-e45a-4ef1-a67d-0d2bfc009954)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f3c8585-c3ea-447b-b399-7ac567dad6cd)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 435d1078-f314-44ea-9ec3-0c7c9480572e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 387528fb-cd15-471e-816c-6468bcca6742)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 948215d2-8c64-4f23-8d42-cb522eaf4349)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ecb32d74-5922-4cee-afb3-e008217b3673)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f7b41bc8-952d-49bd-9a68-b45784f4dc4b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4579882-a39c-4636-bc83-7e02174dc317)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3a9d7998-bad6-46a8-90f1-1dd4fc0e6dbc)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 f760974f-1f03-4731-a50e-5d04b47561fb)(label(x1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 eb2d9094-3c6b-47ec-b86b-8d13e5987262)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 cc62d481-acb6-4a31-9b5f-a2cd07c2eb73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38ee7dbf-470f-4af0-96ab-d294843b1739)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 0ff6708a-2d66-4600-8ef8-62786e12380f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e9b38fbf-1ceb-4742-a3af-fbfad0fcd7b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 625207ac-3b42-4749-afb8-1c4e7501cca3)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a8c5c1f5-1ec4-4744-ac54-b683b368eb6f)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 16509fb7-d6b8-4685-9309-6dbebb4501b2)(label(x2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b4701ec9-b13c-4939-bbee-56c49cc71f6c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1b392555-eadc-4d27-a27c-e58ba0ee5e76)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a9af2a8-d0a1-4a56-b85e-888a2f40c44f)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 189f1ef5-683a-479c-a9bf-a426907ab56e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dc8922a6-56b4-4204-ab2b-f8b5541b7521)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 567ed770-c129-426a-8cab-41f79e1c2b5f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3f50eaea-f628-4307-895e-3c09b8c7caca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f85b427-3502-4ffc-90e9-b3f7cdcaa745)(label(x1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ecca0248-0fac-4865-b577-1143d4f5ff09)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f0bde18-e86f-4dc4-9b1e-59c553cb037e)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 90130bdc-9740-4a4b-81c5-21568e8d991b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa9793fd-5be6-4a3f-bb37-81ed0d1a8061)(label(x2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3f2ca575-dd66-4862-9089-86ba261b1f74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edb47ed8-64bb-4638-8a65-e9904b169af8)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 08961253-4613-48cb-a8e0-f4f28f3b6d4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d066cf88-728e-4d56-9e2a-1f6a854829e4)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 217d0289-65ba-404a-ae60-79016a29c9b6)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bb5e0f60-b4f2-4ab4-ac95-c28ff91673f4)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 edd8e6d3-335e-4d43-8417-88e18cc82d4d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e6f2a07e-b9aa-4696-a913-1ab797244b0c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c2b76ba-383d-4eb2-97d8-daa7d5ca2025)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 7818f3dc-8d17-4952-b415-f5655973e61d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b5bf12ae-d610-40b1-bcef-99258be84476)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 779acaec-9c36-48f6-bdc9-8282fdb0e97d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f122ec91-ab5f-440b-b7ce-bb9d57b419ea)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 abb20d85-332c-407b-81a4-1032d95f3ef4)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 96b9ae51-43e5-4d70-8dd0-93ae1c143d8a)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2c0a447c-f244-4912-86fb-63382d3d1e21)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 ba447ccd-be0a-4e3b-b4b2-5e7ae18bc49c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9caab581-e4ad-464f-a474-45113376a21e)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 d2998699-d025-4984-b880-8856060bda6b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 82ad202a-dd46-4577-aa85-c85fbef7adfb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d7e9f5f-4d71-4248-894a-7bbe96b1cc92)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c4e0054e-741b-4bb1-ae40-de02e7e16602)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 e0591984-a433-447a-8b75-b97a3065f584)(label(e3))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1cd1aea0-1709-4940-af39-2695cbe49a37)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 83e88859-7ccc-439d-bc74-0785ce148fe2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 950c0dd8-7272-4eb7-84a9-850247fe80c2)(label(e4))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 8d4f20af-6d92-49a2-9bd8-99b8348da103)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 52ad6683-3d1b-47ac-b35f-4853109672a7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dd409f8e-8022-4316-99f5-6378a3e466ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c7a61017-202e-46d2-a575-aba081d12cf8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4721a8bd-def8-44aa-ac56-486cfc4328e1)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 95b2c4b6-a87a-46a6-877b-5a0cb8d454a6)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1e9ffd58-746e-403d-bc77-6d1cb50ce930)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 47fdfb0b-ee1c-41fd-8be1-7575eb1db80b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 556c0cd1-0100-45e6-becf-0b8e6ab9c334)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ea39723-5d7e-40be-9ab7-26ed1b4849d9)(label(e3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8b68aeed-cac6-4bf1-b756-cd9be972d5ed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f542bfc-e9c1-4e53-ac8b-3eea1f7ddfdf)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c3e52ea6-022b-45d4-b719-66ce8b1c3117)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31b7c513-c3a0-4303-8665-418000d52a2f)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 904e3d6e-daa1-4f72-837b-9697b11e76a3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9553bdb3-f3fd-452f-a3b8-ec2c415eb6e8)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9a8b793e-7948-4127-964c-cd8b3dc7139f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 93c2e2e9-79ad-4cff-a5bd-b6a72c881f49)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71dd3936-0382-41ec-97d6-99c23e89e653)(label(e4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b75f5d0f-168b-41f5-b819-090306676cda)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 626a73c2-3a72-4674-a6ca-9514d05b4e94)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f8a64049-5015-4e14-9849-d1a2bb7a7bdc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df904b7e-42d9-4492-a67d-3eb414f1dabd)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0fc23da8-8ba4-4c61-8254-f18fd9c53c53)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8a79070f-01c7-4cbe-82eb-d77a266d6c5c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 646a4ba3-9fe0-4535-9715-73df6425ffcc)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 248f2520-0235-47d9-94e5-ffdcbd52a476)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b48ee382-3a1e-441b-82dd-f5eea1d5e9c9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 337b631e-41a8-4f49-801f-a80688999a8a)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6e8b0925-072e-45c8-9121-96eef063a4c4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 04127f0d-73aa-4776-ae80-0bee6adeca3e)(content(Comment\"# \
                 Substitute Exp v for variable name in Exp e \
                 #\"))))(Secondary((id \
                 ecea53dc-5e86-4fd1-a54b-1a29c07988de)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 492a6182-c9dd-41a8-8e64-6bf3e82f6ee2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e790c0a2-f5ca-4e87-83c6-8d7af0d8e664)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8ede3792-3283-4c1a-9937-82f5b079529b)(label(subst))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4564efc7-e3f2-44fb-9a3d-ce60429501e0)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e266a664-1160-429b-8392-04e98262a809)(content(Whitespace\" \
                 \"))))(Tile((id \
                 647516ea-a416-4c89-a6ed-48b6adad0fd2)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 907e1be6-2bf2-4c5f-b19f-8fc562cc2f15)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6db2da37-c8a7-4f62-bc75-d5a06c881e57)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 936bc544-d802-4c30-b98a-a6e45dec0df7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d95cc0d-dce8-4ccf-b58f-266b2632fcd2)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8f2d8420-a2bd-4cbf-85d9-4f0f4ceabdd9)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 05e5cb68-6c36-4106-9329-ed857f2ecacb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30373829-6898-4231-a890-8cf91428cf23)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a798e5f5-2983-46fc-b3d4-1f91215f31ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e280d39-9c7a-4d87-b5c2-b2230154658c)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5fb09b2a-09b7-4636-8813-748e97357533)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2ba82a74-2fba-46ac-a8c5-cdaa3f2103be)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))((Secondary((id \
                 da2c2d60-869a-43ce-a569-49d4ad78d62f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 962c2d40-2b26-4392-be09-53dfa27a7388)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0a6f6d3f-4e5f-499e-916b-5aed1bb3dffe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2005bc73-4e38-4a17-94ce-5187c9adcb72)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 84303eae-f131-4ea8-bc3b-3ae5fdeb57fb)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 092815a6-008b-4adb-b237-46d493b3e1fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b4706a0-2839-40f8-b036-3550c8921f01)(label(name))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f38687a2-8aa1-4f07-af87-ee2b36b8910c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 ceed5297-d739-4e1c-a074-4e8ee7e569d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 065a90c6-6d84-4875-9f5a-71e03b54fa66)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e145e1b1-5c7a-484b-ab46-98ab4b2b73a4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ef06eb60-29ef-44e2-81e5-8a0224a82a7d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e4d95243-394a-4ed1-a289-ca9664bfcb7f)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a41817a8-3ddb-4389-b471-06d1e95db7ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b525dc46-25f6-4c9c-ab08-6d4a70fc4c32)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 39c14ac4-746b-4e48-ae40-d441e65e332c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d4b51927-7265-4283-8c22-9a4af2875091)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 27b1845c-f545-4d6d-b6cf-593d65e4090c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c5d09976-b0e6-4e54-9161-4f122e98d0e0)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b36689bd-4e71-4a51-8371-0a304d133d94)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 8ae682dd-4fa7-4806-8fe2-5daac30806cc)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 b93bdd38-9df8-4ae2-ad3b-c905c04c4558)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8cf58bb4-0601-4c0e-ad02-5e32cafef6f0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6fe941a1-8dcf-4aff-ab12-ec49335a3af0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8e44114e-2843-4f86-870c-d6b2c16dcbc8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09c4111e-5932-4f05-8096-e8168aa376a8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4c288972-9999-4aa1-890b-cdf89f52e465)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2d1cdad4-b0ea-4d9b-96e5-e60fea31bf3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddab5a47-0f67-4d58-9080-4df0642cb410)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 83d8eec5-0cc6-42c7-a404-10fdede58af1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d01696e3-f7d8-47b3-b423-e554e2caf2ef)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 541e4469-94fd-4e8c-acc7-f0a21d0afb44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dea9dbfb-f9b9-46e4-9b3a-93a371dbae81)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e6d1182e-c588-4a3b-978e-9b798b0212ee)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 30a4e8d5-4af7-42d4-83b0-d904db2e9119)(content(Whitespace\" \
                 \"))))(Tile((id \
                 82b3f283-2b1d-4016-b611-7a12a7be0a51)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f04eb0cd-f042-4836-ac1e-c4b857d03df1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c441a700-9f6a-4802-aa6a-7892b97e2324)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e1d18e7-cb27-4fe8-987c-b6ccc6e94c3f)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3ba9cb64-fbc6-49cd-bf19-fcca2bbc0de7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 691b3381-4b64-49de-9638-b64d50213eda)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ec63a23a-79a3-49f9-bd3e-650a7ff3fd08)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab68f381-eb0f-4bb6-a836-cf1396509ffc)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d7a3ad59-5599-4230-8787-3088b67c7864)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 6b32a1e5-e99b-4ff6-b27b-0a7fefaea251)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6cea3997-6aba-42c2-812f-84befd5503ee)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 719339fd-dc05-4869-95db-8ee26bb18fce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad9bcc89-497c-4c62-8af4-249f86e166d9)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 91216107-2b97-4f29-ae3a-42be0eaf7772)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 26604a50-066f-49e8-890a-aef9776e3266)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f31584e5-67a7-4f42-9458-86ef13ff8a37)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e4f99ffb-7348-48d6-a9fa-0524ae557c20)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b7348be-9fbc-4ad8-8f2f-796ae609bad9)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 305bdd6f-2d60-4daf-93f9-ad507408a038)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cff728a3-5dba-4bf8-a471-639d8d6b6f93)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e73c0efe-9d19-444e-927c-19326eb9a914)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c3e3d1f2-5aed-4a9c-8b9a-6cba37ac5f2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d09a07f1-7c52-4239-b325-ec7cc3ee045a)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6ddb7468-58a8-445d-856c-7a3ddbbf8b08)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6787fee5-7817-43d2-bc3b-744dd90ada81)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 20d7c489-5e54-489c-8024-75df9a888b83)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 d5ad11fc-a071-436c-b284-7a0074d11720)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 befa9462-63e8-4259-8ce9-9ef1095a802e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cdc6c0dc-4bf3-4467-b156-416ce09df009)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3395170-5673-4eba-846f-a81d9d09296a)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 ae0e193a-dc0d-4786-bf2a-73534690dbb5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9ea1506d-981e-41d0-980f-32f2ca61e58d)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e9e0aec1-60e6-43b3-a269-8ce8b3b4bdba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c6d7255-2c6f-4814-bea3-166d81e52818)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 35b713f0-8053-46b8-bac4-b34a0a700bae)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 d78eac92-b58f-481c-81e3-b65ea0fe4e56)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 65851dec-6865-4ee3-b3f1-975a4901cb90)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 9c237884-bcb3-47e9-83aa-ac212ee84a11)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 376a0b4e-b774-4b7c-81db-168983220091)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ae5d4cab-3846-42fb-a718-adb1064adb7c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 cde0da9f-df65-4ee8-8bd8-d10d6c8b6c11)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 49d331f1-aefc-4cf9-8bb7-ef6b7d65ff7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 91b256c5-63ed-467b-905a-d398cf6be69f)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0390d325-6292-46c7-b727-9229564445cb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ce95b938-51a0-4c74-ace8-82082992be12)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bf9022e0-9eb4-46ba-aaae-34928631e84a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 15ebdbcc-0b48-44d8-9326-0e1781ccf059)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d0f4ff59-aad0-4ed2-b5b5-70b6b9ba338f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0fea2672-8df4-4de9-8f7b-0c6f7bb0c4b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3684fe41-0343-44cd-9004-fc58b6dfec43)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3d5ecb76-6739-4f63-a635-20824608b377)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 860d0c43-e848-4913-b5da-de8eef4c5502)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6139a27f-2323-450f-b208-131dc2ddcadd)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 6ca886f5-339f-4410-afd5-bd9b759b9295)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9c82937f-9d35-4714-8b5f-40c63a8f0403)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8cb2f13f-fb8f-4375-8624-b04f2ce0b62f)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ab268103-2a4e-4dc1-b5e6-7e466960b314)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 39428d2c-0224-4adb-a533-53aa0411879a)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e769d486-4048-4f99-a8eb-d8d412568a8f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 13b716b8-f090-4d7c-a387-0fdef5cc6013)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52ac2a34-420f-4dd1-8d82-6ad4240108bf)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7ff0833a-80b9-4792-8ab1-7352ace406fd)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eb8f17e7-f2d5-4e01-bad7-f3e36e80e2fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 399bc287-fd00-49a5-a223-bca45825fb30)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 c50aa5a0-0d73-4871-8b4e-1393ca95e5cb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 56643523-3af0-4a8f-bd0b-30fa380705ed)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b33cc7a3-4fce-4e31-9f5b-3b79e6c2a30d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a78af352-fcdc-4dcf-b857-948be7fd77b9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 e41dab60-ea2c-4ec9-a7f8-99aa65f8d22e)(content(Comment\"# \
                 Evaluation can result in either an Exp or an Error \
                 #\"))))(Secondary((id \
                 a09f0cd5-1241-4ff8-8f6c-55cd9cc1d4b3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 240e107a-6592-4d94-81d2-85ca2a25a55a)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 03331f05-5bf7-4c22-9754-7a4f531a3b51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6462c80c-e40d-49b4-ae1f-f523fe372a81)(label(Result))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 80f60e6a-4c40-4091-8e77-6a257f58d9de)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7cb0d63b-7747-415f-bf8a-cbe10fdb5bbf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 de6c6046-307b-43a2-aff3-de514120e2ee)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1499fc64-3bd1-4200-81f0-ca69f406d528)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee13aff8-913d-4942-a4ae-c406d3fa060d)(label(Error))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 c0af192a-6fb2-401d-b896-2e8949e4a0fe)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f40809d5-2c1b-4d25-bc55-175e23e7ac33)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5583c82d-e22f-4603-a80e-7f695228ac22)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0e4110bf-643c-4fd3-85f8-dba729440cc4)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 39ab7fed-ff8a-43c9-a48a-8d71dbe204fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6cd4f972-1372-4727-8efd-1c00214ddee0)(label(Ok))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 41e4ef65-0f1f-4c30-8f9c-91c43217a2ad)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 39419d94-a678-4d63-a009-8746363e7d09)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 48e084ee-c0e2-4fb7-913c-65fd134d9e0c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 885e48a3-7e06-4bce-900a-1e64b171504e)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 4e1cbd7c-b7da-4d57-8b14-933ebd31d4e2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f9507104-a630-4ad6-ab32-73f19d47d8b3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 47df0303-4391-4202-8465-f1b0edacec35)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 39e9c552-b3eb-4a57-a7d4-769af794e5e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 691a7861-daf0-4ab8-a479-6200098b51c9)(label(result_equal))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 179bb3e7-9af5-4840-a7ba-35ac902c039f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 17f75fa5-10b7-4d78-a802-5a279e0eaac8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8410b506-5ce1-49c2-8d8e-79b7c7077b99)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 8f7a4da8-d381-4912-bdff-885f2dd65b8b)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 55bd2e7f-6c00-46f7-8061-f169efbe3fb1)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2f187ea6-7fcb-4c74-af0e-f7611952a223)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c4aca53-8b0f-4777-bf32-e64498b324ba)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 59e4ae65-8e6e-474e-b62f-089c107ae96b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa85ce9d-bbca-4ab5-bf65-2cc1a1c23a26)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 01eb4b88-bff1-4f46-a448-de6f1b9feaeb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0aa29e86-158f-4430-9cc1-97f8a9b2156c)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 62f03f0a-9ad6-47b3-b754-0ba127b09e0f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 32d21430-cfdf-4322-9938-6ab4babcffb3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9e6406b3-b57c-4cce-8280-7df7b69c7bbf)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a8656e33-4f0b-4aff-81cd-a916d67ca3b4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a091fddf-512c-42c7-b634-60f3bfc91ef7)(label(rs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4e7371e0-9d50-4ae6-9dd0-caf250fc5d75)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1959d35f-7741-498a-8a65-8b6021a8751f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 442c85db-b4c2-45d8-813e-1f83afb8715f)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9a36c91d-7bb3-487a-8d01-bd833982445c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f96ebc8-5086-424f-9731-6ff440b665e7)(label(rs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e8975696-7b9e-4262-8b8b-8f92b825ef13)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3d446fd7-d775-4175-a384-04e11a0d57bc)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0f859803-a2ec-476f-9331-94d48262585d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30816faf-8096-4247-a0d3-afd51b2e1efa)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 97fdaee4-27da-4131-ab6d-cca40eb71c59)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 dff605d7-36ba-449d-ad8d-51ebb7d2c175)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 4d7eb665-40e2-4b80-94b6-959c87b2c69a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 f2532ebc-3400-40c0-bb2d-7ac8df102d52)(content(Whitespace\" \
                 \"))))(Tile((id \
                 90a564b3-8615-4cda-8e68-06c9f94a339c)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1326ffe0-2dfb-4bb1-b424-1dea357dd023)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 fd3dff1d-8125-4d68-a78a-049d334101f4)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 8cd790c1-fb15-4e80-8e95-e259d3232648)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8af36372-d889-47de-b930-fe1d3b959edf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe4c8bbc-29fd-4c9e-b8bc-61ec06bfcb46)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 353b31bf-53b6-42cd-807c-c9dc87f201d5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5590e36d-fb2a-4ab6-ba41-8a4b6cf87a86)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 26dffd51-8ac4-4faa-8ec0-b6d248f19cb6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a4898580-440a-4a25-9df5-e3c5812276e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a78a9797-bf00-4e96-97c2-c4e7be41fe3a)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 6f0b61a1-6a5f-4eda-b312-0f993cda01ff)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 82e40b2c-c756-4ff2-95e6-c7e93b28cd90)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ec840870-abd8-44be-a54b-4455b52bb567)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f8603e20-013a-442b-8d51-23ec1d05e400)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f02c2be7-8bf1-46c1-9130-f8e10cd6413e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 a4f045b4-aca1-46d0-8e51-85bfa9be106e)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 9db5c7df-e0f9-4b1f-90da-ee3bb914993f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0b422f68-9c7e-4f51-851a-534643c4bfb6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e80eb24-de20-4cc6-89e4-77b1cc02442c)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b10cc41c-5d38-4f5d-b657-7343a08ba9ba)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 f7826224-d20b-4f58-8ae5-c82a15de27ea)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 88bd7fc9-21e8-449b-91a4-59bdaafb793a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a2bb8e81-3769-4346-8481-c42d98d9b411)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b763a75-830b-4fbd-bf68-7088e53ccb17)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 67d9588c-24fc-4a6d-9e1c-6a2fd2260a07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 680364b0-1ef9-4f70-a2a3-fa9569b1d7a7)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 798f1db8-f7f2-44a4-aac3-3111557f0f9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 589baeef-25f8-4568-ac70-e82a3e223917)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 faaac3d1-8006-4add-995f-74409e9f52dc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a4a6e8a7-f0c2-451e-a9db-513fde4cf6e0)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 12e7d49a-8fac-4917-b1ac-3138394e6620)(content(Whitespace\" \
                 \"))))(Tile((id \
                 64d1fea6-1375-4f18-8f97-1b729d1a0950)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f309b69d-df81-4b27-84f9-51f4b816c54e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6d4ff3fc-f224-45d4-a765-6acee7dbf28b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5b3e3ee-3daf-42f4-ae16-12fbc3b31e8e)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e8d434fb-750d-425e-bd55-152b8b7dcace)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 23fb53a7-0590-4b09-bf4e-0fad4e3f9024)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 04d857a7-302b-4b06-a8b3-94536bca4ea3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0b3b9188-9daf-4fc7-91b5-0162b618394c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f7fc53fe-9729-48c1-9141-d903475ece78)(content(Comment\"# \
                 Evaluation by substitution #\"))))(Secondary((id \
                 f6bbd3c4-a705-418d-a800-5658ba6dcb03)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a7c42108-349e-45e2-b391-f33db86d0ca4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 634a46c0-69f1-4a5c-a59d-22282a33a9ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88231bd1-85d5-4b8c-8bdc-1eb65dc006db)(label(eval))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d8c5649d-0332-4450-995f-a855e091a6ad)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a811c7ed-3d34-4ef3-aaca-9ea34ae575c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 696e453e-01ed-4860-8b07-28e8859d8feb)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2d491d75-d31d-4243-a87a-928ada8dfa04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 684e6a7b-0550-4e32-b476-bfc1c0962306)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7fdb2dea-f829-4aa4-a5ee-0d1b11fd9334)(content(Whitespace\" \
                 \"))))(Tile((id \
                 221a0fb3-0801-4957-92ae-73a345b0ceaf)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 eb45dd87-334e-4e6d-be4b-28aa59e50058)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f52b8ae1-3a39-4495-a371-e7ae5567c4e0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fa0b95a7-443b-45d6-ba6b-305254a09423)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 33a789e9-4dc9-4b89-baa3-2b6ef6920199)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c51c0795-a894-48e4-bb3d-8151bceac22a)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9aeea9d2-61a1-4a38-9efb-9e56f298d71f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b5a83df7-c47d-478e-ab6e-e7f7cf7f713c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0d56d575-26f9-4665-9188-f38e59788c1c)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5e3f5f31-c80a-449f-bb21-c9a04508bc60)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46a85986-2567-4eb5-b0fa-98159c30747b)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 98c0d55b-73b5-4c2b-aeff-d38a1ee17bc9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 96b93d48-0254-4bcd-9305-fe7109e31070)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2b0ad289-0fc4-47cd-88ba-64a0f5f80736)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1d0a326-79a5-4da7-bfda-32caab18b22c)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e092b43a-7ba0-4bbc-b5e7-d8c3c5201690)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 cc56e251-594a-4879-86a6-075056e4d2fd)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 c9743dd6-fff1-47fe-a72a-f03ced485cbb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d84c1a40-dd47-4976-b874-0ab8c824c5db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 205fc0c8-768e-41d2-abe9-1804e6b8ba9c)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f0f2c7b8-cd48-4265-8984-16552e00612b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f5a92513-ec98-4702-88d3-5b73e460bdc9)(label(\"\\\"Free \
                 Variable\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d8441b78-1069-401c-81eb-e012139ada93)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d9ddf7ca-5ec9-4458-8a43-6a610963d5ae)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9bf48250-2a86-4c62-85f1-0e598d40fd98)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f89b72d3-e9b0-4e25-883c-75f4b9919078)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0a8023a9-1a66-47c1-9392-0398ddfb9fef)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 13a0623d-d671-4f74-9ede-064671816f96)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2f532a2f-ae11-4896-a84f-03ecd05d664f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 896b64c1-cf26-4aad-8391-7e92cc48d5e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e28d3026-fbd6-4197-90a7-b776b95d3f8c)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 49a54bcd-3f06-4c53-881b-8518d415f05a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 56c4f3c9-96df-47c0-81a5-c367b984cb53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf611532-4407-462d-8c91-fba2f854ca58)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 31b3c3af-da81-403e-8704-50ee6ab8b546)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f0460cb3-b6e4-4a8b-a836-9601d32b5f79)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b6ca736d-b2a5-4786-9951-b7cd3c23ccfc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5eaba4d1-b12f-4962-8135-b1af5e67b052)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2bfa88d0-43da-44e5-92a0-83a53f609edb)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 08abe7fd-9afc-4fa9-a414-f966632e51e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c09c1a3-ee4e-440c-bbfc-c37286585163)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 40be44bc-68cd-4724-8424-cd572902c318)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4ef86862-27df-43ca-8b2e-24259947d6c3)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 25d6731c-1214-4008-bac7-a9ff554c257e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7122cc76-4155-46fb-b334-b2798d9f4380)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8688d1d2-79c6-4f7c-a0fd-436933ff1dc1)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 7ab76e2c-04d5-41f1-ad6f-37539ce1cce9)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 148317d9-5c86-4f34-8e36-d65e1e28ea5a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 6f1fd72e-be96-4f47-a621-7a2a8eb0df3b)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 b210b843-5106-4b48-9119-539cc7868a83)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c183536b-f0d2-472a-b59e-eab178b91b84)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 eff3d797-ab3a-46d8-b151-0578cbd43cac)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 584b2594-dc32-409e-8096-ed8815f112c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c994f517-6a25-4bcb-b762-123b595156d6)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0c56df27-dd4f-4f0a-8ab7-e026b3cde49b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 226da149-04f8-4cb2-9bea-64671f4c35ed)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 54d71cd5-315c-442f-a721-718bb63856d9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6c4a3235-2e50-436d-b6df-c22eabbec853)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 459a0ea4-e097-40a8-978c-3243301b015f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 137e5ef6-978f-4516-b412-58cafae1651f)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a618e602-25fa-4ba0-a832-dd7739bf5d15)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 5ebafcd8-091d-4de5-9022-6f1f341b24b3)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 12bf52a9-0359-4cee-b964-d5ad30414c7a)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1c24d65e-7a0d-4366-b5ce-c45907de080f)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c0128188-7663-4563-a742-da82806dc746)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1861c7e3-e573-4405-ad33-5f0c605633d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95ce3d44-347b-4144-b73f-515c5d20845e)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 0bfef7a8-11e4-4d6b-be68-2881e96f5b0e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b29d9597-d9f2-4b2d-82d6-f1a671496c58)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6eceddb5-a02a-42a2-a77e-7e77ead92e2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 67e581dd-2c29-4214-bbba-f7ab94647176)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 00f5d290-92d7-42c7-9a45-f202bc8d7d90)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7e0d42fd-f7de-4d24-a254-8e06b493255a)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8c8d84fa-9fdf-4d17-866b-916561095da5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2ba4be68-07d2-426c-bf9f-81cc89699d61)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e189be1f-2da4-4210-8af7-ed241a19da99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48cc2d5f-d8e0-42a8-b528-c6aa9c36bf6d)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3f71a849-8438-4769-9342-71dd769a95c7)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 57b38859-db21-4bda-a6f4-02fce44a1d39)(label(err))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 5facd9a6-87c5-4e1d-99c8-badd08d98319)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 03a3717b-720e-44a1-bd3f-69cb0326d123)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ddcd030-6efd-4e8a-b819-0c8dfdba5285)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3f78e09e-2766-449c-8462-3ccdf045af8c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 417fde4c-7162-444d-80c8-dca88f074736)(label(err))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ec475c57-df11-45b0-ba70-0ed19bf461ab)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8e906fe2-823f-4101-a78d-f72b1990f2b7)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a3122c4b-4083-4ba7-a406-c7f251538146)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72aa71b3-af21-4f26-ae5e-0f8deed77d8f)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ebfb0d1c-7d5e-4f26-9f9f-f0dc0e12c27e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 631bff1b-35df-41f2-9ad3-f7a44fd94586)(label(arg))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1f1b4029-9e62-4efe-9523-e9ced4e7f46f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eeec4b23-520d-493e-805a-0478132ff72a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92854cc8-294c-4a70-8f43-5608c30b2382)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7aedd89c-6126-488e-9b18-1c32e1573c6c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ff1390b6-5c54-4624-b06c-44805fd980da)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9be5ddf4-d4bf-4497-8598-e6d472fe6e7e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 497e2f3b-7fb1-44ca-99b6-9185100a69eb)(label(arg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 56aab323-fdf7-4508-a635-57b5610c9631)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 097a5a5f-12eb-4797-ad31-11422e97f78b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e51c89b2-b71c-424a-b6e7-dc6afd39429b)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e115edbc-2c81-4d96-a12e-c9cb8bd5e921)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c2a2d4e0-8063-4d0f-8971-416349064533)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9201d2d4-2584-4570-a725-4602dc1fc065)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 a35ad70e-7701-4b70-b477-a60601b8516d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7411698f-0dbb-40be-a6aa-f57fa2b99644)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3ec019f8-ae1c-4944-9714-07268b0e7792)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f1f920b7-edfe-4f15-9587-44888bda16d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d7dccc82-a11e-4f37-82f5-d0b3752bfdff)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 30512301-269d-44f6-bb76-b88797aeaadb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 930d8d4c-250e-4374-9e04-02d74f1e1000)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23069e1d-4c70-49fd-bf10-a97c82a772a6)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d415666c-30a6-406c-ab0d-429558d2e1ff)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4e5dda05-b607-42c5-9593-ffce5398d73e)(label(\"\\\"Not a \
                 Function\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 efbfb24d-336e-4888-998d-4615b2b68aed)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 145d4393-607c-4184-83e1-3addb28931b8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7e833704-fd02-4b8c-b81b-2cdc0b1218ae)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3f62407a-e952-4666-bbf7-b1be87419ce7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 468fc0ea-c1db-4814-9bb8-a669e4da76ee)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f90f88c7-081e-4e7b-b40a-93905ccc772e)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 55b9e0be-2657-407f-b97a-5bf310f706ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b73e7fc-8563-4dbc-9157-b6c0b6e7f488)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b3d5c7db-da4e-4334-beb8-355c5b8da469)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 211a31ef-3aaf-4ce1-89b1-aade64d83bf1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2391e001-ebc2-4e3c-9eaa-8060014af8b9)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 59fab523-3118-4c7a-a078-93d1f1d8ecd9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d593257e-7164-47ae-907e-738c9ac87c4e)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6ffe7a6e-0306-4955-b034-a948bc51974d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9436459c-c60b-4d6f-b0e2-7eacd07eebe2)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 b2fafdf1-bda9-4e4b-8f5b-fdc5316295e2)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 129fb393-65d6-42b4-a4f5-7f73369ba217)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4fc6f1d9-b6d7-4df1-8cb8-c27cefcf299b)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 90a6ee62-829b-4bdf-b6a7-8758e493cf59)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 269fff5d-6206-41c8-b435-cf1993462f26)(label(\"\\\"Free \
                 Variable\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 f14291e3-fb30-4ae6-9207-fbbd4b8eec5b)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 dd5bb7c7-bfe0-46cd-939d-1defaf449bf0)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 40c85115-8f00-40b5-9bc7-6822a590e831)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 43d6fce8-9a10-4995-80cd-9b432756dce9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 aa4f7eb7-2726-4f15-a92f-b4096f756504)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7cb46eac-350f-4d8a-9c36-31a9a39f87eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f372eca4-63ba-471e-bc20-385b286e5da4)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ee8ce941-d9bd-44c4-8633-ba532341a353)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 bd2f325d-ede3-45e7-9e06-04f999779f30)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7ebb3c63-593b-4d14-b404-e3688f0d4f5f)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 91df1e9b-69a1-41f8-be3e-29f9fd76aad8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f77ee645-8ff9-4f43-ab8d-0a192c2edd2f)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e167e913-0310-4be4-b078-7cc26874c602)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4e25f79e-a665-4c8b-8938-5d0bd422fbee)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1223b693-1add-4aa1-bbef-0ad563e24d0f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0aef23c8-b7e9-4534-a206-1a3c12178b73)(label(\"\\\"no\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a71945eb-5a71-46f4-ac2a-126988ad7b93)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6eb48871-6159-4ab9-a223-9aab09ebf7e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8e014d3d-0a3c-44eb-a547-c1e6f783e325)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5bddb949-9648-4b6c-bbb1-26af0f2f7aaf)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 41f6de8c-5d57-4112-a1df-58f20adfc130)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 63c0bcdd-690b-4c24-a5fc-4bd44d770fc9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2c073203-da56-45b7-8114-9a92f521a077)(content(Whitespace\" \
                 \"))))(Tile((id \
                 478f7ec9-1c5c-4889-afa0-68089bd0dfcc)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 76a18320-a390-4cc1-a92c-2ddd51294651)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e6dc5f63-2dc7-4d91-8f8b-a8a297b03724)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                 5bde3c92-ae78-4918-933a-69b66cbe30f6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c8481ede-6f48-4557-9f8c-7d20fefa0838)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bdae5d7d-85c8-4286-b971-cb19fa1722dd)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6a09b733-f770-453d-800d-8d192e57e312)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9678db37-25f6-4366-90f9-b9bfce2ef8b5)(label(\"\\\"Not a \
                 Function\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 e79226ee-8f02-4d0b-972f-1bf9e48721e2)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 b62c7570-dc7d-460a-963c-f8c0e4af4a54)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9849e453-ed1a-4f61-9e19-c946e68861e2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 244736b0-4365-4f57-8430-9e832c633fc7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f0a6055a-b2bb-4dc5-ad88-b10e6dfdfe26)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 bf6f24ec-4b7d-4536-9659-f63d1405c71f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8988b6f-3949-4c68-8ecd-e3e9807dd540)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8bec152b-b818-41eb-8217-f1eeef41a4bb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 93184e38-7a87-42d7-8d95-d2f5637fac74)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7f93b04c-5fdc-45e5-82bd-bfef63ca6ca4)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a7c5b0fe-ed76-4b09-bda6-709be0820ae0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3803e123-ce31-4a8c-8c0a-1072666abef6)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e824096f-5d10-496b-a975-a7d5eefcaf72)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e1517701-104e-4dff-b060-b5cf9cfe2654)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3659afcc-fa51-466a-b124-7748bfcc59e3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ff977574-dc50-4d57-9d47-af2b9768317d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 109d7b9f-db98-46b6-af92-981dbcab1c09)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4eaef6d2-2926-4820-8e38-6d782e5e3d56)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2d559d4c-24b4-43c9-8c97-5038b21e347a)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 4612fd19-fa9e-46c3-8c2d-fa3c7a4856d5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f09fbf4b-1fdd-49a1-b2ff-7a2c24ec740a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 da045e2c-dbb7-491a-87b6-1d9e8756cbd2)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3d97ade0-5c0c-47b5-9cb9-75cc8431725d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 976b254f-3552-4313-9980-ce3e7e05b5db)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9e11b6a5-f6e9-4194-a50a-6475a58c51a2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8eea3bb3-ed21-4494-bb6d-108c1dfd581b)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 50fd8474-5d53-44fd-bc9a-7f7f8c868c96)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 62660116-a0be-4956-85ca-25c21697499d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d26ed763-2e34-4e9b-8bf3-ac7fcff67f94)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 75a61a84-3e40-4463-9270-ffbdcad8887a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a0b3f10d-ac64-4767-b83f-02d19601be24)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 836ea810-e100-4052-9797-b01530f2eef5)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 4a7f72c9-531c-4191-a466-d2fd5de03cc9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d793aed3-c909-4dad-94ed-837c0dc78018)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 17bff740-dbf2-4d72-ab5f-7981faa7cc9d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1fa75cda-a0d4-455d-93af-1b3eac8d74a1)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 dcb5a216-f308-4a73-9ad9-da71ae256e5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56412f83-c9ff-4bce-9104-fbc2ad190d67)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 758e644d-53d6-4238-8404-5897a3dae786)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 31d80ed0-206b-4549-b76b-f692dd16992c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c2fa557e-c614-4d72-8cfa-2692a5f3256f)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fe2c71bd-4b8b-4b75-b061-fa4b87b7f990)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8eee6b2b-16e7-484a-9606-e98300ba386c)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6ec7b837-65bc-4c51-96c9-63954ff11ea4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 18699790-81ed-41d9-a072-677219c62ff9)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ebb01363-a6d7-4cf5-9ffe-f32f30d2f7cc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 eb8cb5aa-f3da-46a6-8e1a-f92c243e14ef)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 83b20d85-121a-4b64-9753-2d41f4341a97)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8812150b-c837-49e8-88e2-3e5992aa7577)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cc30776c-9863-44ba-9546-78b004c57125)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 405cedc5-188d-428e-a78d-815b863bd45e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8bc0178c-9be3-4996-ac6e-97ec8ff4a59a)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 f4a691be-a544-42f5-8956-ae6981108598)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 509cd9be-a498-4eee-a239-81f37c45a236)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1eb04998-2380-44a5-b688-6cd51bbc695c)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2f754537-5abd-451e-a34e-0f19843f62ae)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c403f6e2-c13a-46d6-8f7c-050b3a655e63)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 236e6b6b-603e-4e67-99d5-456f2092a3c9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b369d945-4718-4774-b4be-15d8d2810bf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57fdc821-1043-4710-8f6b-2441e324e886)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f27591c9-23af-4084-9ec1-3ad99eb34bf5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 19ce03e0-7192-4283-8841-46594bf5a8a6)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                 7e8eba73-784e-4685-8044-0987b352b35d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 429eb78d-d469-4593-b3e8-e057ceb65b1d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 98d0da1c-7495-4a91-9d54-741f1e0bf685)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 49f5feda-60b3-4921-be88-07c613827287)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9d53dfd8-7ed2-4d3b-93f6-036a656f3ec0)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 67c55273-804c-434f-99b4-86327f95e4fb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a02d4a1f-e4a0-4ced-8cd1-9b53fd48ca98)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 16d50947-e839-43ad-823f-c35504594b5e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f4043075-82bf-4a56-865b-e61ed31e7cf2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7902c7ba-b334-4ea3-ade5-eb8c937d4c30)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b0179f93-ba9f-4b6a-8c19-3312fb1d6d07)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8a05a117-e885-498c-9cea-d0129f8879c3)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 8e2248a2-c61a-4f07-bc94-5618c292f073)(content(Whitespace\" \
                 \"))))))))))))(ancestors())))(caret Outer))";
              backup_text =
                "# Lambda Calculus via evaluation by substitution #\n\n\
                 # An Expression is a variable, function, or application #\n\
                 type Exp =\n\
                 + Var(String)\n\
                 + Lam(String, Exp)\n\
                 + Ap(Exp, Exp) in\n\n\
                 # Syntatic Equality of Expressions #\n\
                 let exp_equal: (Exp, Exp) -> Bool =\n\
                 fun es ->\n\
                 case es\n\
                 | Var(x), Var(y) => x $== y\n\
                 | Lam(x1, e1), Lam(x2, e2) =>\n\
                \  x1 $== x2 && exp_equal(e1, e2)\n\
                 | Ap(e1, e2), Ap(e3, e4) =>\n\
                \  exp_equal(e1, e3) && exp_equal(e2, e4)\n\
                 | _ => false end in\n\n\
                 # Substitute Exp v for variable name in Exp e #\n\
                 let subst: (Exp, String, Exp) -> Exp=\n\
                 fun v, name, e ->\n\
                 case e\n\
                 | Var(n) =>\n\
                \  (if n $== name then v else e)\n\
                 | Lam(x, body) =>\n\
                \  Lam(x, subst(v,name, body))\n\
                 | Ap(e1,e2) =>\n\
                \  Ap(subst(v, name, e1), subst(v, name, e2)) end in\n\n\
                 # Evaluation can result in either an Exp or an Error #\n\
                 type Result =\n\
                 + Error(String)\n\
                 + Ok(Exp) \n\
                 in\n\n\
                 let result_equal: (Result, Result) -> Bool =\n\
                 fun rs ->\n\
                 case rs\n\
                 | Ok(e1), Ok(e2) => exp_equal(e1, e2)\n\
                 | Error(e1), Error(e2) => e1 $== e2\n\
                 | _ => false end in\n\n\
                 # Evaluation by substitution #\n\
                 let eval: Exp -> Result =\n\
                 fun e ->\n\
                 case e\n\
                 | Var(n) => Error(\"Free Variable\")\n\
                 | Lam(x, body) => Ok(Lam(x, body))\n\
                 | Ap(e1,e2) =>\n\
                 case eval(e1)\n\
                 | Ok(Lam(x, body))=>\n\
                 case eval(e2)\n\
                 | Error(err) => Error(err)\n\
                 | Ok(arg) => eval(subst(arg, x, body)) end\n\
                 | _ => Error(\"Not a Function\") end end in\n\n\
                 test result_equal(\n\
                 eval(Var(\"yo\")),\n\
                 Error(\"Free Variable\")) end;\n\n\
                 test result_equal(\n\
                 eval(Ap(Var(\"no\"), Lam(\"bro\", Var(\"bro\")))),\n\
                 Error(\"Not a Function\")) end;\n\n\
                 test result_equal(\n\
                 eval(Lam(\"yo\", Var(\"yo\"))),\n\
                 Ok(Lam(\"yo\", Var(\"yo\")))) end;\n\n\
                 test result_equal(\n\
                 eval(Ap(Lam(\"yo\", Var(\"yo\")), Lam(\"bro\", Var(\"bro\")))),\n\
                 Ok(Lam(\"bro\", Var(\"bro\")))) end";
            } );
        ] );
  }
