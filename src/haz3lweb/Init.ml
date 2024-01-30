let startup : PersistentData.t =
  {
    settings =
      {
        captions = true;
        secondary_icons = false;
        core =
          {
            statics = true;
            elaborate = false;
            assist = true;
            dynamics = true;
            evaluation =
              {
                show_case_clauses = true;
                show_fn_bodies = false;
                show_fixpoints = false;
                show_casts = false;
                show_lookup_steps = false;
                show_stepper_filters = false;
                stepper_history = false;
                show_settings = false;
              };
          };
        async_evaluation = false;
        context_inspector = false;
        instructor_mode = true;
        benchmark = false;
        mode = Exercises;
      };
    scratch =
      ( 0,
        [
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               81269f55-a66c-48d1-9fbe-83187a492f55)(content(Whitespace\" \
               \"))))(Secondary((id \
               a4e41744-51dc-43bb-b359-47cb9649dcd4)(content(Whitespace\" \
               \")))))((Grout((id ef3fb913-bd26-4ef8-af2f-424a73c5c753)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               3fd71a3d-9baa-4362-9887-450674850113)(content(Whitespace\" \
               \"))))(Secondary((id \
               140c0376-4f67-40b6-8056-8cac787af42d)(content(Whitespace\" \
               \")))))((Grout((id 35a88970-2d50-43a7-a476-f81f5b36728d)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               36ace27f-cd35-4880-b50c-7629d3a8476a)(content(Whitespace\" \
               \"))))(Secondary((id \
               39a56f0c-5214-443b-8bd9-931ac9a7720a)(content(Whitespace\" \
               \")))))((Grout((id cbfc7b9d-7a60-4d4d-9a04-5239fe7008a3)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               83b9a843-4947-43b0-8232-fb9ce31f8628)(content(Whitespace\" \
               \"))))(Secondary((id \
               abca2150-7d0a-4c6c-8502-bdef953a11be)(content(Whitespace\" \
               \")))))((Grout((id f292f825-054d-4023-80f7-5e436bbc25ff)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               520a7c0c-6bb8-4bdc-a548-5431ef003028)(content(Whitespace\" \
               \"))))(Secondary((id \
               dd7c1758-0001-46c0-8ab3-e43a23285e0e)(content(Whitespace\" \
               \")))))((Grout((id 06807411-26c5-493c-8835-258878cb073e)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               c0748728-2618-4872-881c-ccb38dbd0c58)(content(Whitespace\" \
               \"))))(Secondary((id \
               ab68e973-bf30-463d-989b-c7e37921aca2)(content(Whitespace\" \
               \")))))((Grout((id a9b8ab49-ba54-46b5-b504-c85c3f615c64)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               53f99ea5-f1e2-4c4b-bb11-8bb270cc563d)(content(Whitespace\" \
               \"))))(Secondary((id \
               0595315c-7bd1-43dc-8cd5-ef755f9d7538)(content(Whitespace\" \
               \")))))((Grout((id 6ee496e0-c06a-4c46-bfdd-c844017a8bd2)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "   ";
          };
          {
            zipper =
              "((selection((focus Left)(content())(mode \
               Normal)))(backpack())(relatives((siblings(((Secondary((id \
               cdc8e64d-6836-4d9f-9353-969397bfe2ab)(content(Whitespace\" \
               \"))))(Secondary((id \
               82f3fe37-c665-4aeb-af3d-01ad0de37d40)(content(Whitespace\" \
               \"))))(Secondary((id \
               749ce88d-f0e0-4694-b13a-0831f733b0ed)(content(Whitespace\" \
               \"))))(Secondary((id \
               4a7e3e85-8563-4160-a121-bc3c0911118b)(content(Whitespace\" \
               \")))))((Grout((id 75ba0150-8d58-4efe-9253-cc2d7f4df1c4)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "     ";
          };
        ],
        [ ("scratch_0", Evaluation) ] );
    documentation =
      ( "Basic Reference",
        [
          ( "Casting",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 8068a0c4-8131-4ce5-a850-c17e7e7e38a7)(content(Comment\"# \
                 Internal Regression Tests: Function literal casting \
                 #\"))))(Secondary((id \
                 3be72b01-de96-4cd5-910f-b6f3ab6a172e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4d828014-6d8f-434b-abf2-6a662fe33c69)(content(Comment\"# None \
                 of the below should trigger runtime exceptions \
                 #\"))))(Secondary((id \
                 c3af568c-60e3-49fb-b4b6-aceb07a91e97)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 e57439be-1c01-459a-bcf9-cd5f3aa8c65d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ce9fa5fe-b457-40f2-b69a-1dd30d72b19a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cbeba9b0-28a2-4e5f-84d3-1ac692fdadac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c050101-1fa6-4df8-b20b-b19c253a622d)(label(g))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f9aa410a-67dd-402d-bb0f-4a7681401d98)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 364edc1f-bb6f-4b64-b3c8-88889944ab35)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8bd22357-2c15-4dcc-92b8-5aa2f6e4762a)(content(Whitespace\" \
                 \"))))(Grout((id f20dff97-39f2-4f4d-8f31-684088be69f0)(shape \
                 Convex)))(Tile((id \
                 ca20cc5f-f628-4149-9310-d59868ecc7a9)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 5cb83599-6fff-4fe6-8b1b-fbc0f2b16df6)(shape \
                 Convex)))(Secondary((id \
                 b6ed15b5-4d03-474f-8079-ff23578cb9c3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 90761b49-64e0-4fa1-b3b8-2baf37151b32)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8784e29d-9fb1-47e6-9833-13964e30a390)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 881872c6-0d5d-4618-a27f-2d8189d891d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec558c21-a417-440e-8649-5d1e471ac938)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6cafce86-7cfd-4966-890f-1ff63e7c59d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6197a04-5d43-457c-b9ac-2863dc99c9aa)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 8bc5b8c6-b9f6-4f7f-bdc0-cca1ee17ace0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5fa91f17-5b9d-4375-8c21-48ea2a17f79b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 729bfaa3-4d19-45ab-b2ea-0ee7cbf89f6a)(label(9))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 efb902d5-fca7-4ea6-811b-8fa5cacf00a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4b388e57-61f6-4c4f-8288-28300d5c63be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 54988076-8257-438a-800d-614fed5dbf32)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 2))(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6760425e-a4f8-4974-8ae4-59d7cb943370)(label(g))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bfb3339a-8286-43fb-9ba1-28b3ccd4f57c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c39698c7-0f2e-4bef-a04f-8a39d9a055d0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 fa356f00-e672-434b-affe-c17ba6a33b8b)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7816c92f-1592-4df0-b2c9-079260acf77c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 eb8b1deb-50e1-4a00-b58c-c1203e082d85)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c244a67e-156c-4a5b-8976-9c33b02945ab)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 058e2ec9-8a44-404c-a984-b5e3fc44526a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9a0395f-eb34-47a7-9962-cc212a342bae)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 700d354d-c0ce-4a53-b12c-cbee7556a60b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c4653559-da72-40fc-a7e7-bcc45abf19cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd681a2c-d7c4-4483-af87-fc929dd43eda)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1a9f798b-6df6-4aac-a512-699c4f0e4914)(content(Whitespace\" \
                 \"))))(Tile((id \
                 998f8e6f-d099-444b-9c75-891ab31f1768)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a5faaaaa-e2f9-46ad-932a-0289bd5b39ec)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5c84c891-35bf-4c68-a116-83e16699136f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74466136-5bab-4a17-af60-4e45651ef15c)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f9708006-883e-4b2d-bb30-c16eceb968dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b08b814-e7df-456c-b93f-0612e8ae5e9a)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cad35cba-3f46-4c8d-8e95-874ac1cbb84f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f130da22-b535-4868-8c28-bb7d6e114694)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6e17c3c4-3b95-4524-abd9-64fb1a179665)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3d264c86-a12c-4dc2-8d06-9bfc92ff1160)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53ac5aaa-2c96-4159-a15c-be816540dcf6)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3fbf338a-1948-42bd-9d25-93b98002a1a8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5a6b4ca1-8b45-43e5-88e7-0df7ab9a8291)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 5b9cad15-2118-48bd-8a90-930a1a14fc34)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cd49f1aa-27da-42f3-8f0f-02f845ca8cd3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4b5a4b1a-c654-4b0c-a3d3-53794844fe64)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2b4ded00-335f-42d5-af7c-3ddfa436f524)(content(Whitespace\" \
                 \"))))(Tile((id \
                 083512d6-17a6-4288-a83a-faea34192510)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a5916772-9b05-4c21-a6ee-9ecbd54516be)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 666f67e6-24c9-446f-a7ab-38e2c015f9cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0807ef5-df8e-4eb7-9f78-339003a5b8de)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 106fc984-125d-427f-a0b1-acbe77f0ffba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a049e806-20c5-4b1d-a099-7755a9097b7c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 43a50e94-546c-4d1a-8dd8-c5b597a73727)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 a2958e5b-2da8-4f25-903f-220583955a32)(shape \
                 Convex)))(Secondary((id \
                 2a78f83f-11c1-488f-8e1b-81f27b9d6a73)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e24760a0-00f4-411c-bd9e-6ddaafdfa45e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cea3d12d-01b1-4cde-8fab-0249d492ab91)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b1651b66-ecf9-40e2-87b6-1467f19c6199)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76e9f1a4-edd2-4dcc-ab7d-bb5ce3daa033)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 29b18195-9677-4596-a89c-b9fc61267425)(content(Whitespace\" \
                 \"))))(Tile((id \
                 43ab9ff3-2fac-440f-ba6f-67cde6a259fd)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f48bf544-6b5f-45c8-a4b4-2d2b8b8d62c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c73a3100-a782-46b9-a401-3194788d3ab9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1755c477-85d2-427c-8240-a91649c21dfa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 77a083d4-1e8d-46a3-9844-c2a929afae23)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a40bdf1f-e175-48aa-b34f-c6c3c2ac61d9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a08d599a-5b8d-4b5d-8f5f-8a28646b9e01)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a7b66a4e-4c88-4a7d-b325-1ef561c8945d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 506b50ea-1e13-4309-83ed-9e2646874c49)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 08da3a5e-3737-4e5e-9fb3-9b18d8142310)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d0c3bbff-de90-4c18-bc64-f06637a0c534)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bd3ce62f-26d4-4583-895c-b1703388b2b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98686f54-7a2c-4951-b59d-48a861d27aab)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7d9fa3db-ad61-4696-9c7e-9b897a1bf2b3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c3afe279-39cc-4c43-9185-e19ce5ba31ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5965539f-7948-4f36-9160-aa18475144e3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d7403374-174d-4a55-a4bd-423d6c907a22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f58bd61-2540-4de4-877a-275ea138c657)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 84123ab9-18d0-44aa-98a8-08f1eb3c0b42)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6134bd22-8950-4612-a5a7-5fff5a16abed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a1a6bd7-feb2-4476-9be1-81af953c33da)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a2a9a9ad-3751-4443-b863-9075a213e282)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7dd4efd1-b246-4cef-a279-e45029f3f048)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50d4bc14-0110-47f1-9ff3-9173fb4c799f)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1cbbf7f7-82f6-4d5f-8af4-694afddaa340)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07740340-432f-404b-a4b4-58c849178faa)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2f0a5a7a-2a8a-442a-9b42-bb29ace672ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8cfe83fb-95ca-430d-b614-7ad6a1f060fd)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c49a3f96-09b3-4c92-9d9a-4e0e129bfd4d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ccc17a8b-20f6-41ca-aced-8ab1e37b51af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d4cca74-c8be-47b6-aab7-db4a81519644)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b0b5f605-6c2a-4163-91af-2db67039b026)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a52dcc6a-5f33-438c-bfa5-f5065c49dd3a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 823c2d05-bfca-456f-8ea7-3dc327249035)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6ef23420-91b3-4e62-a6ad-acc727256268)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 812d4f83-80f9-419a-862b-7fbcd5ca14ce)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 74ec9114-7deb-4131-95fa-ef6596f0727f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a1e62b2-f5a8-44d6-be05-e543aa063c03)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f8d525db-5cf3-454e-93f4-dd805dbfece7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fe35e44b-a3d8-4d9f-b052-d95ce9dff21c)(shape \
                 Convex)))(Secondary((id \
                 68211388-5a88-4811-959f-947ab63a5bcd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3a3a303f-be27-4df1-9b21-86efdcb6ec46)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 442407b8-023a-4380-ba34-3a9d13d031a0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0fc60cb0-46de-4545-a2fb-4282bd97be44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0c96440-b4ab-4e5f-b547-568097937dcb)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fde88b1c-69da-461c-9b0d-6c47f1a9ed6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32ff1b3a-0010-4190-9ff8-18086ad6e5d8)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 10d76632-a4a7-4622-8872-8c5886c49e42)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1e546a71-cd49-4ebd-aff1-dd6d1aa7c4a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 808bf4f2-1a68-415e-b997-01ff2037d799)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 05c7644a-7a22-4c20-a812-1fd5b8e02005)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5e74bd2d-8a05-4ebf-aee6-6fb3ec824145)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ec28d888-794d-4201-abda-94b6e3f1bbdf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ef7de3a-2a14-4188-a55f-51527113ef74)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9a6d019d-7e68-4607-acda-eba022cdc741)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5aa34837-e964-4ba3-a34c-ad5fd015a710)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7632724e-1f20-4d15-ba4e-e01b565bbf4f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 002d2fdb-a677-4f81-9c2e-1867a5407bbb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 62820ee1-7863-4a17-90f8-e04dad881c64)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4dfb1076-524e-4527-9a36-4f2b33913898)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ea549dfd-7437-4aa5-83fc-c59393a44e84)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3ac5d13b-c645-4a2d-8500-f753ba61e239)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3772fc52-5d80-49a9-a915-10132f4413a7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88657220-85e6-4449-91d5-4b282ca9468a)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 753a4301-8388-45c4-bd4e-626eeac2f55f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 30afec45-3066-4a0f-817b-4f99697585e3)(shape \
                 Convex)))(Secondary((id \
                 e534716b-c249-4bc8-8bdc-c492a72acf60)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bf464cbe-a15c-4dc8-9376-f14dafd49ee5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 00da0773-8c3b-44a0-93ee-552d6d00d643)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ff437f2e-8a76-4186-857b-56cc23c8cf53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 28ba42f0-050b-43dc-a8f4-918c72fa935a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 868be89d-7e08-463c-8dcb-33e6c8f69e94)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7c53b17-975b-4397-b6bb-7f4811d6db82)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d163d4ea-0f31-47a1-aec4-8ae8015d8b51)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 afab5ad9-7a2b-423d-b78a-79c436c4cbec)(shape \
                 Convex)))(Secondary((id \
                 daa148af-3377-4878-bc2c-d694b57ffbe3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 090d5f42-f40d-40b3-8852-e3df2288f312)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7de52110-c900-4a11-ba4e-037c4505322b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3ea7ec25-bb44-4ba0-a941-ec8f0e78e32b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ccb96b2-2464-4556-9643-4c8f617f7308)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b961beb3-fa7b-4fd9-b91c-239f69cbcfc7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93a136c6-b09d-4b15-8b00-d4f06bb96300)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fb29a56a-0ef8-4287-bdfb-8b44145f44f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88f0e812-40f5-411c-8a59-835cd8fbc13d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 28d36d6d-a119-43bb-8d70-6906b2dae317)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f599939a-5936-40bb-89ba-7d21ec9a5f63)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97f492eb-a350-479a-a781-5a08d6abe259)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 aaf0812f-7972-41b6-8eac-ef5929e5c3f8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 34eaa419-274e-4ff5-bfd0-cce8e39cde76)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d9380579-9c65-4d68-96bf-3abf614f7fe1)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5caf86f9-67c6-400a-a3ec-80666f040bce)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ae9cf20e-e9fd-4972-8cd6-17fe70d28f57)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c7ff1ba0-287d-4225-9e45-8c57f590f6ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 80f77759-243b-4ab5-937f-081e5b14ffc4)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5a385233-70ca-472f-9efa-868afa2fa920)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6796dd02-e021-4bf4-9af1-a07ef4b1bc74)(shape \
                 Convex)))(Secondary((id \
                 d110d938-d497-4c04-8e89-066a813de449)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ffe2143a-7389-4770-a4ba-5b0979fbb22a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 06d75ddd-e9dd-4ea4-85a0-9cb33d5e406a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 834a19b1-916e-4db0-8d77-b31479e219cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca32f0d3-79d9-45cb-baab-9a1a818bc52b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 49378f89-aa16-44d9-aa6f-6e528ad367ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c2dcb59b-085d-422b-b984-96edb2fe98fa)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dcbe313a-40c0-4155-89fd-6582504c3f81)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a1b7d325-6d0e-4afd-a524-d1f2f5e082c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1dd3c27d-ad8a-4b97-ae84-4a00f750be6b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0d8bce72-a5c2-4493-9f8f-b9befeb6d851)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 226181f6-33c1-4475-a7eb-8e4b6d096408)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d28519f5-0922-4439-9fa1-c5f0c3ebf346)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a1755b32-1b4d-41c7-9f1a-42c71e180ab9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e70a5d41-2a81-4b8b-9529-15ada2e2d985)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a4d0485e-9be2-448b-b811-10012453597e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6c05f4c4-ced6-4bac-a9c1-ff0cca03b949)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2052fc4c-45d1-4ebb-bb45-fc58fe231e86)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c69713d2-f7da-4b5c-b8e7-8e34eaa355bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bcc0d062-fadd-427a-bbbf-393d2c1453a5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 73554d67-a033-432c-8893-95c4a1107285)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f9277e54-1eea-447a-94a2-c46212addbd5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1d784b8e-e8e1-4428-9fbd-841578540e06)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f6e71659-ddb6-407c-a44c-fa8ac8b1a95f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf419f2f-1813-4df4-8df1-06b6521e381b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8a12e1d1-a399-476e-9f36-f029c3da0076)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba5af55f-56ba-4c36-b8b2-4763bd1f8403)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 231d72bf-ffa0-407e-9a80-dac8576384aa)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6349b04a-89c5-48bc-828b-601385c813ec)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 300af8fe-da24-4862-a697-9fda2ccd15ac)(content(Whitespace\" \
                 \"))))(Grout((id 2e1cb501-4636-43ae-a721-3b06106ca7f8)(shape \
                 Convex)))(Tile((id \
                 7a863ad1-643f-4ace-abcc-4625e9ea7713)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 325d83b9-7bd5-4d03-b996-98219a52d07f)(shape \
                 Convex)))(Secondary((id \
                 914121e7-476e-4557-9696-e8d677b591cc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 08d4b228-9a88-4fcf-86dd-7581a05b4207)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1662f538-d638-42d6-b647-bf9ae68bb3d1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a5131d05-9af9-48d2-8776-4301cee7e196)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b1ebce05-efb6-4e07-9255-dcf4b9632bc3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ee33ac9f-1e5c-499f-b988-c68f182700f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b683cf1-7c30-4818-86e1-ad0102609037)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7aee4283-eb8d-42f5-bc32-d407f9d046a0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6ac93f7c-968d-4bb9-8148-f892ae614c3a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 04ec40c2-edc6-40e1-ac4d-f1a949691806)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3b678b8d-211a-4fa1-9344-729b448d82bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32608e0e-8fe8-4d33-9b37-28e22c243df5)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a73619dd-e865-4ce1-b758-395688fb1c65)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12a9a007-8459-4b89-83f8-b0786a30babb)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f1feb2dc-1c4e-4ccd-a46d-9c44fae706bd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 377b7b06-2219-44a3-a15a-40eefb2e4132)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37969eef-8b95-4739-b8da-f41e471ccfa9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 596e83c1-9c00-4fdb-b68d-f9384c7b1110)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 555b22e7-eb09-4585-977e-99fc324f2db5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9982abb7-950a-4217-be18-20263b12066e)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b8d5affd-d5a7-464e-a101-458fc8ccbb34)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52c76936-5dc6-4266-9f1c-50728358a6b0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fa9b46a1-4671-445e-ba93-4daf9b41dccd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13fcf121-d1c9-4733-a645-16917683156e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3e666899-520e-4d5f-a5b9-741b23fac52f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e3916692-423e-42e8-aad0-992b23ba6401)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 60a5d2b6-b776-416d-a27a-1f38435c44aa)(content(Whitespace\" \
                 \"))))(Grout((id 97f80bde-4e54-4773-bfdd-e1e39bb02e30)(shape \
                 Convex)))(Tile((id \
                 bdd84f49-6015-422b-bf6b-aa51b66be74e)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 53bd85d3-c103-4f9b-8770-a8581176c6ab)(shape \
                 Convex)))(Secondary((id \
                 8aef858f-e556-4ad7-b733-17fb07da11f8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8a80e9ca-c622-425c-9f32-5fe40508e4dc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 959b2dfc-eb6f-4f46-a02d-7fc57321f8c2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7db316a0-00d5-41f6-a15b-121345f0a3c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 430e3009-1f1e-4cba-9ad0-67bf501c5d45)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 cb8887aa-ab8c-4ae3-9ad8-1bfce8b545b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be8254b4-1db5-41d9-a1d0-024dc2fe54aa)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 282800ed-cac0-47b3-83ef-ab6843b4ce6d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4bd0c0a8-1496-4877-9cb1-f2b5a1ad6f8c)(shape \
                 Convex)))(Secondary((id \
                 be99f047-d20f-4b32-8c3b-95ae257327d7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 731bf1cb-a888-486e-a008-52cb805d53c4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d734de9f-b9f4-4799-ba57-85b7bd56763d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 76fe1a22-4d7d-4da9-b93c-723622eb8f52)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db29a378-12ba-4014-88e2-7b1e8788bc21)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4e0e88ad-62f0-4f4d-8187-54829cbd9d3a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b0463f0-e659-45d3-8a5d-478d3eafa552)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0e2b6a75-5330-4cef-bdf3-c4daba073b70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 690f42b0-20c0-41c2-bc14-21afc8e55342)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 07ed1101-e100-4f00-bcb1-351cdb727daf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 df7bc075-fd66-4d14-9888-c27d2372de72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2909eca-2597-49d2-9b20-eda5cb3bc6f1)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d4f0cc4a-52bb-43f0-a22d-c5caa759a828)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2c0ca6fb-edaf-4087-b530-75f284dce06d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 b03435cc-9239-4a01-9877-8cf2ddbb304f)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b82930a2-7541-4e3d-9d48-14b74043814d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f764d69c-d357-4aaa-b10b-80245fe61511)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e021a7bf-f9fd-49fb-9050-226e8b78077d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4b3dc4f-bcdc-4b56-b1c8-04580c305741)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ee00a4a4-6dd7-404b-8403-b041de299a00)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6ee4d23b-cbb3-4c17-9faf-8ac1a858a5e9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 222a12ea-0006-4a36-9e2b-701e74fc101a)(content(Whitespace\" \
                 \"))))(Grout((id bfea8648-7604-4e2c-a04c-b701e0bfa12a)(shape \
                 Convex)))(Tile((id \
                 3406101e-5b28-4a55-a3ad-b2d2e05fa239)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3f63a985-4fb1-4411-badb-3a71c88787fd)(shape \
                 Convex)))(Secondary((id \
                 fd038d1b-0f85-46ac-bfc4-c8936b28fa1e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 061a6f1d-8a6a-437c-958c-ee53dffa6704)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e9b8e31f-ef12-4386-ab71-737ecc8bb303)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 79f92bf1-8162-4a61-a2ce-5dc11b32f5b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae51cf87-8075-4f34-bc4b-c04a2509c922)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 13f9a6db-c358-467c-b63d-aa80de578827)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53cd4541-4855-4b45-9b57-21a5a55e6536)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 419711ed-f234-4d83-a1de-052392ed4f33)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0a1c5f44-56fb-424c-b8a8-02be82320d0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b454ce6b-f260-450f-8fd1-5b061fb59b7a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b1186b20-55a7-46bb-9e3d-0a2597e8ca10)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f025b713-4446-41b8-b8a1-b9deec7170ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b7eb9e1-a57b-42e4-b4c5-f8c8b7559e8b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 59076a3b-3dac-452e-bd83-72fddcd63292)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5460c69-8e8c-4c6a-9764-6c2899686e42)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f549d916-2c17-452f-94cb-fcd429bd40dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 54695a1c-ec86-4a5d-a698-c870608f6b81)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a5db26f2-f9c0-434c-838c-3e798e9aa599)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bfdeadd1-583c-48ff-a3cb-6a1e25de8e27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b83e4de4-dba2-4277-887d-7e7342cc2d93)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fa11dd40-1c46-4ad0-8c2d-cf7714a80450)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1afd78bb-0f0c-406d-8315-5a73551c079d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 53a67607-e548-47a7-8ca9-eda64d28b744)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d3bc7420-82b1-44bf-9f96-8a489511a052)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9fea6b9b-a8d0-40ca-9462-9666163ec246)(content(Comment \
                 #ERR#))))(Secondary((id \
                 3c8ad85c-d1e6-42cd-bc0e-8dafcb0ef8d3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 10450a13-ad0b-479a-8e17-72fc85252260)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8fbbccd0-369c-47b8-a053-4bf41f8a5f5c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 847efb1d-395e-4107-a3f2-f00078659bba)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e4ba3265-8220-49a1-b676-e1b0c7b7e91b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8f0f3df3-dae1-4f53-a9cd-cf0fe15fa5c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18cc88a3-8cbb-4138-ab0f-e21b7e2556b7)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 08f5024b-27a6-4292-b774-150a34a9e35b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edd367c7-2d69-4927-a1ca-9de4cac6e1f1)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 12b2b5aa-dfe0-4ed7-a09b-d661786ba319)(shape \
                 Convex)))(Secondary((id \
                 a030ef24-0415-456a-bc89-543c379922dd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ecf39cd5-ac32-4876-902d-0fc26899d9c6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9139333d-80e3-41a6-91c8-5048f147651e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0ed479dd-00cf-48f9-9c9e-3caff02c00ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 973e21b7-9ebd-436b-afc7-0480b31454f8)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 abae88f5-5ff5-4525-8f11-90e09122b78b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4af1e9e8-18d1-4bdf-881d-ad039219bef1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3de8793a-102c-430d-a326-119e531dfeba)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b5635065-6adb-44ff-baff-849222e1f9ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c57cccfe-f3ea-4ff8-b0d0-5557cb6ecc9f)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 33487927-233d-48c9-bfd6-0368813cb10e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33868a11-98b8-47ad-b7ef-cde8b26ed0b5)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c777cfa2-b327-4842-b6ee-886d567092ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 da697965-271a-468f-a241-d2d182c69bba)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7f5f7a01-d759-4f6a-8658-6ef3e240647b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e45018a7-3a31-4a26-bab5-3dea3da77d2d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b183ee0-efce-4c3f-9b41-e6395eb534e6)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 91fabbec-4dcb-478d-91a1-617f3c0fec09)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8ead3805-23d3-4ff4-a827-fcb998b0b815)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d1051422-7f8c-478e-8cea-291b10007dc3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cc586bd6-8b7f-40eb-b338-af3e2832626e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 002eafbf-5cc8-4b0d-ad74-54a1bd7bed62)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 10def638-b735-41b9-a4b4-44958a75431c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34227ecb-5f39-40a3-b527-77e3060fc15f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2e4fe4fb-fe0b-499f-95dd-4c18f2c86746)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cfb7cf79-3941-4e7d-b713-0652acb4798d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adedeb3d-6221-44c5-8f8a-96953097d926)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 60eabbf0-6e8f-40b1-9756-c74fb9dfd772)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d940b34-55d1-43d2-9651-6229281f850d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 ccbe068b-76b5-4c11-a5de-02bf5c12511c)(shape \
                 Convex)))(Secondary((id \
                 309ba515-2819-477c-9d4a-63a3820fff4d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7d0ca20b-3b04-4dec-9e51-f68ecf607766)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fa6b22ec-4842-44c5-86cc-053bb5676bd7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bc092c89-6f2b-4334-9d03-33fcbf3bcf92)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f240ac06-da74-44da-b83d-384293baab04)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b510947f-7fe8-436a-9896-e92cfc6e95a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79747594-21f5-4d9c-837f-2db67ce70c0e)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e534f388-e8b4-43ca-bfc2-9d21ece36b2f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 356dadee-c7d8-4161-a582-4908e0f40864)(shape \
                 Convex)))(Secondary((id \
                 030f1466-5e09-461f-915b-7d8409966d8a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8a5cbd13-b805-460c-8b27-f403a65f9c49)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a7060bf6-c262-43c3-b510-c0d7155c9a77)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 589cfde4-d192-4a10-8d64-2b696c08e274)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33459de8-1b3b-4b90-9188-34565416cdd3)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cf03ebe5-2c1a-4cda-b22d-8aae648ec611)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24f7b13f-5702-4682-9271-d55c54c4c1f5)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bd2dedb1-8524-4186-b94f-27880af5efb8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65e6daa9-88ec-4bc0-85e6-720617dfd875)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e9e62ff5-7066-41df-aa7f-ac0045157f1d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f4e2682f-8ecc-4221-a1de-9ad09065cc20)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8a5286d-e3c6-4032-8fda-676d2c62b47c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1feb8b74-da74-437c-a190-1b721dccc0d5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b029c2d3-b880-44da-846f-783ef7114058)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 58031230-cd07-4bbc-bbe0-8831a15d1739)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ef306d3d-51e8-45c5-a2e9-06fb5c51c094)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ce8b6223-a04a-4fc8-bafe-929344732e75)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 540caf79-e72e-4ee6-aa19-5d937511cefd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cee423a7-829a-462b-9e9c-6358aa538d77)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5f386074-6bd9-4dad-ad40-d1c0520a00ae)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 de59295a-8148-40fc-ad40-ba710271d679)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d639b433-4528-4dab-8cae-32118b2788c9)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 abc9163e-b23b-4a34-bf14-356bf63dd72c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23b041df-c458-40d5-9693-d3069569941e)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 96a1c24e-5803-4601-b4c3-1c5dacd7adf4)(shape \
                 Convex)))(Secondary((id \
                 e07b7063-17c4-40f7-85b9-613e58020174)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d7ec1800-e79e-416d-b565-2ba6f602e882)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 88e1b34a-9dc5-49a8-aea3-6ccf72a76c14)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 03e17ec5-2a7e-4ba7-beaf-06ce0a8c8bf7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52731693-ca1a-44f8-bb49-c956d5eea584)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e984ac11-b7db-46bc-a73c-e1bfee8ee762)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b1320a35-0415-4b66-8741-5f9faabdafa4)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f4b15672-aa8f-4001-b506-3b0d632e7887)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 78a0d7bc-a7da-4f3a-a46d-2f1ab36651a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c67e498-55aa-4221-94cb-12b4b17cb677)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4cd8a778-6d6e-496d-8cdc-a91177ec34cc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1ed08aee-b8c3-46ab-86ef-20c19c1070fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e461602-eb6f-4837-ba1f-f1c1aae17d5b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e51c2a8d-d47a-4d7b-ba9f-9ce6b11ff755)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0a5f279-8b84-4a83-b12a-5398d19d8196)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f1aae1ab-9373-4309-b756-28aa629b5989)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33484890-2dfe-4f33-a4bc-cabc060f846a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b932b818-c84b-4ca4-8612-ff0cb53b26ee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 aa3d6c49-42b2-41cf-b8ac-083b693ca537)(content(Whitespace\" \
                 \"))))(Tile((id \
                 230c52c7-6470-4756-ae9f-4b9d35485b53)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 807d3924-d03b-445a-b24b-7295b4db0436)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e8b3278f-6914-46b9-8dca-3dfd5762a5a5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a16437a1-ef9f-4168-a489-5024543cf24e)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7f71fd7c-ad20-4f09-a306-2153a6870280)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a5a5c919-1120-49b6-8072-0a6f60656704)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 139f8553-a9fa-4ecd-b51c-5f19d4bdc376)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb8399c9-8e9b-4f50-8673-15d764bc7429)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5786939b-f2d2-4c23-81d9-8851344edd85)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f780a436-b288-4066-ad25-25539b6a4aad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e575c01-2cda-486f-a77a-bb3ca218c75a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2635219c-ea70-4c49-b775-392873cb7458)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49004b33-f151-4e09-8d1f-c8fb71fb8e74)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1771e8b3-6fd3-4ee1-88fa-eda3484af174)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6a736ba-1150-4d21-88d0-08ce100d2e9a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3dc11eb9-1c13-4155-88f9-7e404000b193)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 db87b2bd-43e1-44b3-8982-01e87cce4132)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8be2a7db-bbe3-4d84-afd6-bd4870c8fbac)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c7f3772f-596e-4e5f-a152-736896e548cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8be53eff-63b1-4c09-9265-d4e7b070ca60)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 24418b5b-65c1-4a5d-b21b-10a3ede829ea)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e5c60fa6-987d-4192-ac64-819873e0e19e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 004ff029-4290-4c1a-a043-972e79144369)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0d8de894-511e-486a-9e68-1348ae2276d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8b7f3fa-e086-48f0-90c2-fd03eebc1626)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a8335f03-9b62-408c-8b67-1586b1808b0b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 843ff32c-c9f3-47ba-9397-134ca2634b5a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 40d5ca79-00bc-4d8d-b1e7-f51ad765aac5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4c634236-d833-4c55-87a8-10a074115af1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb424cc9-8cf6-4bef-9657-c17e2727bf88)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 27795b79-de23-4499-a81d-421966b8dbef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 483d5d5b-e649-4fda-84ca-e6ea2b3f767e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 b2f02c29-b676-4de1-975c-847fe2d569d5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5bfaf192-0044-460a-bfee-5eb8b975cf4a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e9052dcd-8da9-43d4-8645-d6fcac723d17)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 353a4ec8-f2ef-4f11-acd3-e00f4f517d0d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff603e69-8b64-47fe-bf43-4214de89096e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a83f0099-fd53-41ef-a4cd-243830c35b16)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 58c882c8-817c-44e4-9c5b-d02967d09cff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 944ae72f-edfa-4c02-bfcf-143ee540a904)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 949a8619-56d3-4784-ae02-8e5ae1d73040)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f8b627d7-fd7f-4486-b2d0-47a4f59d2973)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3ab54a3f-2d2e-4e59-9148-f64309490357)(content(Whitespace\" \
                 \"))))(Tile((id \
                 696f179b-9f45-4327-a6d4-878b7d134c8b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a2435252-6b4b-4121-8341-78e4fbcccca3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a143f8ac-6c26-4f74-9145-2a74fa041f1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ec68868-5105-46e4-9056-5aeb383d4679)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d3048a5e-9bbf-43b7-933d-be81f39adcf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 210a8714-8e9e-411f-9ae1-5c74bac00093)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2f5a5a94-c9ad-4824-aacf-1cac8d5361cf)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 2ed40092-401e-4d29-a9a1-1e68171cb073)(shape \
                 Convex)))(Secondary((id \
                 c63ef644-91e1-4e9d-82c3-4bc7bcde981b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 16217a54-e0f3-4705-a22d-4a19de000394)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 36f1e72c-9c4a-4faf-aded-a88a176d4abd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eb763170-c55f-47a1-b63b-1e0466b17806)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d15e3c67-be75-4070-81f8-6293e96755e9)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 da81336e-2f36-4358-a49c-6aa2d5c2a613)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79f09050-aa99-4ebd-8195-1e99f7c2c0a1)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a5af519a-a045-43e7-8dcf-e3557a425bcb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 133b8fa7-2c8f-4df8-b7e7-2fc18ebcd2f9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bd4db6cf-e8cc-4f0a-97f3-88ab28476947)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f7f5dd63-8fe4-4ad1-86be-7fc537a46176)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec8aa607-2390-4953-ba7c-2f5afdfbde09)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9d2ff58e-d2d0-43be-b2c0-583ccae1ae7e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bbc53dcb-5eb8-42f0-b216-c6000d58ce42)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4364fd68-68e9-4574-9894-16a3956e95ed)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 36328de6-3b6f-4cfa-8b92-39e50750b06c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e587d340-f37a-4afd-ba21-2cc57548e22a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b97a2200-2074-4adb-807f-52356b9fb435)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b53075c-8209-4948-9ce7-776096b5fd82)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 805b9744-3bee-4bdf-b0f2-67b029f95bb5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3058802a-3ce1-4d35-9e01-b66285ba12cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 847c44a3-e6b2-4dc1-b2ac-bb75d953921c)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 aa2bb2fb-b9a7-4f6f-979c-22d0c33256ab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6bdfb38f-de93-4876-bbe8-022c27d3e62b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 353c26c9-5fe4-4e23-90d2-b1a3afc4dd37)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7bb6e07e-5595-4d66-9372-56fdef1c0817)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f89ee9c0-d510-4374-822f-9a5eb871a98e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cb72487c-b3a2-4355-973c-eda6e6629972)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d93e1d3c-1503-4fd4-9265-469568dc7aff)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 8e9eaa84-1980-49df-a8eb-375256e3a927)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5a87173-b27e-4664-b6f6-a5ab393bb0bf)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 98b23989-7819-4d0e-9568-380f4a50c624)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 35022138-f989-47b3-8f71-79ee5281736a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e909e184-ef9c-4329-9dd4-768015c862b5)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 dfc0b1b2-b2e7-427b-b2e0-0c0c3a042bf1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e25285d2-3ef0-4fc1-a72c-db5ddf07f61b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 28c7908c-e262-4380-afc4-db24ef5f8d1d)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 607388d8-3378-4b51-9a7f-8b7d9668dfe3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e7c7068-6952-4d3a-a24d-cfe1720cd7ac)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4a80ae70-7a60-42b1-8ee4-906ad68625c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bb91703d-865e-4da7-b4ea-6ab0168bee00)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 87a2590b-ef12-40b8-82cf-831ce54e7605)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 519c4134-b611-4e2c-bc61-1ce1fc037a66)(content(Whitespace\" \
                 \"))))(Tile((id \
                 480ec920-3cbb-4a57-8c18-4bfb5f7b7585)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e9396887-20a0-467f-ba0e-b3ada37e8000)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4b4482fc-f137-4c88-83e3-14e58f1ccc63)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 f7a9587c-a02d-4cdd-9a8c-4fb3c5713bbf)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e21366f9-7e34-4cc2-91fe-97b547efaf04)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d3450794-e083-4bc3-8c39-d8bc120cfc52)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 08462db6-6625-4c95-8042-c53c96efd1fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2fe1d1ee-8b06-4266-8ebb-498e75a798fc)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 79dde745-c7e1-4a32-96ae-c123339e98f2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 22664132-0459-4655-9515-f67ce37b3e07)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 75be864f-c108-4c59-bd87-096f8d494334)(content(Whitespace\" \
                 \"))))(Grout((id 527d475f-3dd1-402a-840a-d61bb10bb04d)(shape \
                 Convex)))(Tile((id \
                 b7b6082e-0469-40ec-a80f-a7a34c2aa61d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0b9c3818-d16c-412b-86af-335c4ce351b3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4da3cd0e-7dd5-46d4-b9b1-d031b7ea89c6)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b0c1483c-8d31-47bf-8f2f-f43aa5884c22)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3eab2831-982a-44e3-a1d8-09b8b659ac3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d4d20a4-a534-49c5-9a32-76e09040eb1a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7a04c6e0-2b87-470b-8593-609afeda4b4c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2dbb36a-f170-4317-9006-bb355cb2f29c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2d891a66-59e5-47bf-8d5d-4e81fd885dec)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 772da6b2-4737-40aa-9814-cf8e6a301175)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b7cf3d2-9b01-4ada-b516-3cb6d7385476)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 379f6e7b-a8c3-4660-8303-9c3b0858d87d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 873dff3d-0bea-4edb-ac34-db16ac06ef8d)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 db991128-d193-4847-8938-4f806c8665fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b9d56fe7-3f54-4534-8bcd-49e2ef006a26)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6a81ab4a-7186-474b-beec-0805abdbdbf8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1abe8e0c-f549-4125-ba51-5981e8fdc4ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9596c34-ab5b-4f96-8d74-5858bf8e8152)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5a4fa3c7-7607-4774-b8a8-18986dc89193)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e3950884-d2b0-40e7-b32a-01065fd32331)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4f32e87f-a0e8-405f-a291-e397d64767a3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 05ddc6cc-87c6-48c0-9626-94a4efa6b933)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 694066b4-70c4-4f4a-b6fa-bb51de900106)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a3cb892d-00ba-4546-ad7b-7a0d789c7d51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97ef53ac-fd0d-4bdc-a808-e9088abeffde)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 56b028aa-38c5-4edd-99f8-79e7759b8a1c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 17052864-ee4c-412e-8183-77ff429eb1b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e6307017-fc25-4f2c-8c4e-af9eb418e95f)(content(Whitespace\" \
                 \"))))(Grout((id d1d34494-d503-42b3-b8a2-7df3e596d360)(shape \
                 Convex)))(Tile((id \
                 c18f12e6-5888-4299-969b-72b72a2fb3be)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 528c0513-d586-41db-804a-3b2951462da0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bbbfda1e-9479-4d8f-8d81-5e3947c324d9)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7558a507-f539-4c5b-a3ac-8549007de485)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fbc4b010-7a1a-470c-96f3-aaaad440f714)(content(Whitespace\" \
                 \"))))(Tile((id \
                 61ed8466-1264-49b2-aadd-6908f1a9d1f8)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1754e748-c92c-4a85-877b-933e6bf784ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f56d749f-e0d7-4a1d-b695-8920cf16663e)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 aba41e3d-8cc2-4126-8254-0afa3d63c274)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 75a04d71-ee98-4080-b655-0ab4fdab1634)(shape \
                 Convex)))(Secondary((id \
                 358eb656-bf64-4f3c-8644-3a3ae97941b0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 133f9e97-f882-4ddb-883b-7ca1638b7c19)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7ac0a147-6efa-4d75-889e-ac486cf80490)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48cd780a-78aa-41a6-b5ef-b6a4037a943d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c72163e-7684-4e80-bbc9-155f9b7267a6)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f998ac7b-9b23-464a-9cd9-1af495c9e983)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0cba194-e7a6-42ba-b4ba-0b095fff8574)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3d42b016-55e9-4e64-9db0-0f9337180909)(content(Whitespace\" \
                 \"))))(Tile((id \
                 457f8f99-9f11-4c61-9d85-8ceeb02c5973)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c768e601-28f5-45c3-b14c-e6f70ed32ff5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 808aff3e-ddc0-4f41-8471-8828d3304850)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca7da666-83be-4b0a-b52a-9d30e756a956)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d154ac67-5709-4d9c-8fb4-b5519a268524)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 104ec290-6e09-4845-9b66-1e5dd0a27643)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 8de6cf6b-df21-42dd-a0a4-a00867eb53c0)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9ca66412-77c9-4ecf-96fb-b4a0c7a886cf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f3edd654-9d34-46d4-917a-a7913eecdb81)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 48b45298-851b-47ba-9bbd-886e14b38a6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c2cd033-a38f-49bf-8f57-8879c4a74478)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dd92c069-4f1d-4349-bbea-0bb3b22c5edd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9492e529-df90-4edd-b524-1d2fb30369a2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2e2cf058-e06d-4bf7-b828-190af11f6642)(content(Whitespace\" \
                 \"))))(Grout((id 15ccf080-e782-45af-84ad-f78c9de1ea15)(shape \
                 Convex)))(Tile((id \
                 f18005d4-f5ca-4b9c-83ed-283b798c5c98)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 39c1ad08-f549-4d38-a0b9-9ddbbc2a5f57)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35096049-b682-48bf-9450-9fb44612578a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e2220176-cec9-4e88-bcc0-acd6bad55f9c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1fdf07c6-0e73-4189-9cce-afc15dd53309)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55c8a7f9-eac9-4edb-bf31-adf6688b9c49)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a358e631-40da-40ff-920e-82620cf0e895)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18328c4a-5078-475d-bb8c-d4e90a1b0497)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7ce5810b-541f-460d-9476-252c7e65ff28)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 93b31a11-1151-4f2b-8e4e-0a6b37d7d740)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48f0b576-c989-4659-bb0b-4273c9b9d310)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e067f2d6-080e-4645-9b29-844db6dee9a8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c9253d66-4faa-474e-a6af-b66d3424b61f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a72a1cfc-45b0-43ab-9836-b088e976abe9)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7157f70b-705b-49f8-9d25-ae2f34ec0334)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42e8023c-f381-489f-88b7-706cb8eb8f4a)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 638378c9-b559-46ec-b39a-1d3583d613ff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c8c3c6ca-c1a6-4cbd-98fc-88970d20e004)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f5e324ec-914b-44ff-9be2-84edcbbe4e83)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eb3d2f44-a836-4d7a-9a09-36b6fbbcc3c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d22c1b02-a31c-4df4-a4c2-054a58c409b9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 71ca34e9-9611-4e2a-821f-ab7b4c75c2fc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6eab7e2a-5b54-4189-897a-d703a254dd37)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c74ac4bf-c8e1-4cb0-aecd-951a46b6f2ed)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1e4cb0f7-0b78-4ce6-ad15-b3fe169e1eee)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 24f901a9-f6da-4a53-a22f-5f05c0e30da9)(content(Comment \
                 #ERR#))))(Secondary((id \
                 93a274cd-cbc9-49fd-ae6d-3f87e7f7015c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8106b070-02e2-447a-8409-2156ceb7128f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9edc746b-d54c-4cf0-9576-68bf4e5ca2e0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b1972565-5233-4b9f-91c9-a2fae3c6a59e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f87ffd56-048d-404e-aa36-d0de2fd4e5cc)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b438fd18-ba07-497f-b4f3-777060017d7f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f5cff6cf-3e3e-4bf3-b5d4-78928ec0b317)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbbd8d0c-e584-4c09-b213-894971bc1faf)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1127e605-9044-46c9-bb28-f6852d7947d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12846edd-8ef2-486f-a3dd-465462fe4a6c)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f6613404-b691-4a7b-b752-bb166bbdcdb0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 00003fe7-fc85-4b35-9f62-18293b2cbda0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4d0244e-e948-4235-892d-ac5bf023ce66)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a80b2c39-eb3c-48a0-a2e3-17923969fe44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bddcafce-7849-45b1-af50-889c78a390f0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 aa1221e7-143d-4d1c-b409-a06055fd5761)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ba03980-b852-4885-a04e-cfa57275e8d9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f8bc12af-f910-4824-b77e-366be5909df2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fb7af25f-7598-4f40-802e-a8f1d57365c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 820cd996-1610-4f5e-a266-540960f4c4dc)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1333bb7c-855b-4a5d-bcd5-827171f09e6e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1afee2ad-9ad4-4767-822d-d87e99720936)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 e967f122-900e-4e89-8a01-57ff820ee1b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 232cd412-63a5-4d1c-98e6-c5bb5ffb5eee)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c3b0d2f8-eb19-4af3-af20-17633157c84e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de6ced50-9af1-424e-809e-cbf7e7e7e24b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e643707e-b4b7-4f3a-9f1c-e7936f41e676)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 883179cf-135d-4b83-8d12-d2409f8a21cf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3b2da582-d9f3-4e55-8e2e-1508daa5b3b5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0a555ee8-3ee1-4228-bf5b-0d7db686e897)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14bd7839-8eee-43ad-a004-dedecf68e840)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6d5e0294-eb7f-418a-a349-a2bbe3a42a20)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4f240250-1391-4dff-bb8a-1d3f48478741)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c8f0ad05-abb6-49bd-91cf-d0a16b72fd75)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d66d5683-d914-4dc6-b7d5-a19bbc0af995)(content(Whitespace\" \
                 \"))))(Tile((id \
                 142a2136-43c7-4518-905a-4a31ef1833db)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 77c57bb0-6a06-474e-bc0d-ee283bb8c8ad)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 bd14c4ae-02a8-42d9-b247-2e5547436b33)(shape \
                 Convex)))(Secondary((id \
                 1b30100a-435d-447a-bf76-f4f09d78eec1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d0e81340-778c-4ba5-81bd-7f05a8afdde2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 44d45046-c1de-447a-a6ab-6d6b8d4a854d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 62fa65ea-81cb-4476-88b8-a4bde7674ceb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e43f4d29-9356-4316-9034-81bd15301d71)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8019195d-613e-4a8e-a0b3-a7c577861ea9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b75f7bf5-e751-4ab3-9d71-3937cef6fe1c)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f48e9313-1406-4f84-846f-03b25a473d67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc344eca-3cf8-4f71-9b80-9cbefd835a21)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fa5b2245-39df-4d97-b5f5-80ccd7615228)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4f2129ef-0318-4ca6-b5e0-655b969854d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a4ed116-ca13-4cfc-b767-a231cd11b1b1)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5f2b05eb-a2de-41fc-b7d2-9dab487365e7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a94f2393-025d-4798-b63c-48934ba349c8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2cb9581e-59ac-43e1-8cb7-2556aad2954f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f4903a4-e48f-40a3-bd5b-974a07b9c5f4)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d8c418aa-ab2d-40a4-90c8-f26b9c337273)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87d4c21c-3df7-4b91-a381-e8ba84591c1d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9cf3d798-b10e-45f4-a5b2-7d268897692f)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e4075d1c-28d4-4166-8291-de1d3af67fb5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6f2165dc-3f61-4e1e-8789-0330f088b39d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 839c814b-e1f0-415d-a7d0-672f3253a2b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfc577dd-8a74-4879-adde-0aa4ae95c94d)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 aa3a36cf-6347-4722-a5d1-09711312efd4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 06ddb22b-3499-457f-91ed-05add3c22ee0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29786d68-5467-47af-b47d-0e740f4c9a16)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f37e9a6e-4bb1-4e67-8d50-6fb2a14a8bf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3ed678e-f223-4cff-9b59-8b8a7a6b9cd3)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f57a1cef-e80b-40c3-8d30-c57c67d720d0)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dcdca7df-b2ef-4e82-aded-efcc9fae3b6a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 758f83e9-d647-4d54-a6d1-35deaa585f05)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0f584200-d83e-4110-aa0c-9e52a2b137c9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 504dec65-6792-43c3-8321-1ab79125297b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31c5e7ae-c7a9-44a7-ba7a-78bcf230dd5b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 feb1b848-deaa-4736-99f0-4beccf61a5bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ac41ac6f-2360-481c-9e37-7bf709f793f0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 353887ee-58db-4778-a1f4-292626241948)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cee49f88-02ea-419d-b943-5b1d94c2165d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6c317419-f723-4623-99f0-078dbaeae142)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 babf0e4d-228d-4726-9a14-ae21b5edc6c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e2eea74-5c81-4fe0-b987-f952f86e01ed)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 10511c27-d376-4ed7-a46b-113b5b6a9dfd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 888ba43d-2657-4274-a150-e3ec98013f10)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 50cd1923-dda8-4f7f-8f5b-d320681cc050)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d023581-7000-44d8-902e-884f0ebadeb3)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ba1de608-148d-4189-b19b-7f4c638f2685)(content(Whitespace\" \
                 \"))))(Tile((id \
                 277903d5-b80a-4d2c-966f-74cf8859bbe1)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2355d66a-e991-40e3-bfb0-ba8a19f25b30)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8cd62e8e-a681-4e84-9392-c4fadbec7779)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e919bc9e-238e-4e1e-b7a3-b36d823a844a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a264266f-c818-4800-987f-8f159a22edd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb8746e0-8bb3-46c3-abd0-139e174dff49)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a33ede3e-f25d-4bf2-b792-3b362e7ffe53)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 366cce6a-e675-412a-8dfa-6f649470c506)(shape \
                 Convex)))(Secondary((id \
                 08c165cb-2e48-4998-a60e-1cc513301278)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 da19d4c1-9f19-4e7c-b57e-1e1e3952eab0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4ccc0f28-a241-4e7f-b2d9-749160995066)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4c9cab9c-b008-4dfc-b736-063873ed49d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e8b20ae-6b08-4fe5-9f4d-984426b53013)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a5497755-fe99-4b6e-bfa4-2b506ae94ed3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32898186-883f-4598-bf5b-ba99c4fd7441)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 31b7f030-673a-46d2-91a5-802c5d5c0050)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e02d502c-b1f9-4ffd-b24e-048db75550fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a01655cc-bdbe-453c-a35a-81012be7c308)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c1a0baed-c8a2-429d-833c-6ed12fbebbb2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e57b24c2-2862-4bbd-a3ea-b16447fefea4)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c4a4a8f9-4449-4923-9065-f12099c43b50)(content(Whitespace\" \
                 \"))))(Tile((id \
                 479d118a-5d4a-48c6-bf47-03364c601277)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3ed7c3c5-9c9b-4b8c-a6cc-e5d38f0884f7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b8b607d6-5e91-4166-9030-e6062c5bdfde)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09f543ad-eaf3-4890-8f00-364e65bbeb9c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b1f15536-570c-4dc0-9549-dcb2b230feac)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 064cc832-ce4d-4774-ba20-f8ecf0a48b37)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4c73ec7c-d01c-41f3-8a0f-11412cd6a732)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc65cafe-9149-4468-8ea0-f92ebe2d968a)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5c9ee7ae-9db7-4924-93c1-1a8418513aee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7cad16e-7d7b-4115-9a8c-af93f221acb9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fa8e5241-13ec-4c4a-8e28-4699b23444a5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e779d1de-c381-4048-8509-32d2a4fad77d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 78024387-9521-4a53-ae68-0ede50bf3755)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9b5f8ce4-8a09-4d4f-aac9-3b1669efe5ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 050ad0bc-cd58-4ff3-9517-1d272cfb1dc6)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fabc0073-2285-41f6-90a3-748f83b2f6fc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 60502944-c980-4eba-9d89-b341dfaa767a)(shape \
                 Convex)))(Secondary((id \
                 e3656e85-0260-4a63-9c23-b0fe822eaaf0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a0800885-7858-470d-b5c6-f7350dc5a52b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 22fdd3e9-c9fe-4d4a-96ed-e18661b9f453)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a80d307d-9238-49c1-9a6d-114d8cd618b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e95229c-1e09-4428-ab3d-778449bfacd6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 345d959c-c0e3-4325-856a-03b451afb788)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad7d9601-3b12-4f28-9d0a-4edb752606e8)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dd65c356-1ef7-4512-9ab9-eea9984c1092)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 b0782794-c9d5-4083-baf1-32e183dbeff9)(shape \
                 Convex)))(Secondary((id \
                 e0c1bec2-69ca-4a42-8a3b-be53a3eee8ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4f9a74ca-94d8-473f-8e1f-7d9e81720c6f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bcf86e8b-7f81-45ea-a4af-f991a8286ec4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 da2a3eb5-e4cb-4596-ba40-b29ee4fafed7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 befc9916-b11d-46d5-847a-875ee9a25659)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 419fd78b-a064-4bff-85e5-7a58faec7fe1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e93b830b-4890-490c-82c6-1fc52ab97cea)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a54cd696-f9b0-4928-bba6-ce8d008cf0b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee86723f-2eb0-4801-b190-3da7822b3246)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a93e8247-9329-4e31-ab36-bd4fb103c31d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6bc7a8f1-a5e1-4c83-95ce-47ab12734e9a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 922d00aa-39a9-403f-9e2d-9006299c0089)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0cbfc86c-62a2-4e35-846c-419445841b20)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7e1a34cf-d41e-4581-ba52-4bc25e1e3d49)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 a949f8fc-26c9-434f-9b99-db5150203e1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f9b120a-b758-4553-b9e2-4c0c979ed6d0)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 74ff733f-c95f-4053-9369-e5d4bfa47efc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4713350-3b6a-4c4c-8976-8773fcc47b4d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 62f78eda-a670-4b8b-a9de-d6804c10abe0)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3c34799e-b421-4e12-91b9-8f73492669bc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f1cddc20-e71c-4d62-bff2-2c79bb66d03b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9845f59d-6345-444b-b86f-dab9a738fa2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 59cd16e1-b021-48b8-a31a-3eedff49c12f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c765c652-21dc-4c5c-bc0a-9e2bf54c8d4c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 01b55507-fc6e-4f9b-b7fe-2de70d68ae85)(shape \
                 Convex)))(Secondary((id \
                 60a8198b-7d3b-4ae5-852b-1b9279a22496)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 63db4e70-84e8-4ad7-a912-6a10b549edb6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 67dc3deb-0f65-4199-b01b-c4d08bc2ede9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c91a4785-3400-4d3b-a807-8ec7cca6fe42)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f49d514-2780-4f49-99b5-cdc6c21c42da)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 13abf15a-a4f0-4319-9d0b-77645ad48137)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f03b79a-01f2-40cc-8619-7ae5d08848d1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 587eb7c5-6cc7-4678-a196-2808537549d8)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 56a345e3-90b2-4bfd-9cb5-9c47541b310a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5553baea-93f2-4356-8029-cdbac97a9fbb)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 454bcfc8-ecab-47c0-ac70-506d96ae1ada)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 218987e9-922f-4609-822c-314279d863bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7465bb73-9f4b-464a-9f9a-8405eb57c91b)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 369da5af-5f2e-4f1b-a50f-1515222357ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 90512813-86e6-469b-bcac-0267834646e6)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 758cb590-8371-4bff-991d-e69e9dc51bab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97d8cfc9-65e1-4658-957e-8527db8230f8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cf1feb8c-9f0c-4fac-8457-ccec09dd7719)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4a041d67-9016-4ca4-9dce-11ad319cca96)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9680e92-4454-4a3c-9e82-23375259b15c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4bae0ba8-e49a-4e03-8514-633fe6f9fd1f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c8aac4c9-6fcf-4edb-999e-a956693ee999)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 549eacb0-6d86-46cf-9249-e243a9a4fdf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e694950f-4597-4e61-b9fd-a05c746abe4e)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9c2868d5-7874-47b8-93d2-9c506244e763)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddfbe440-e3f8-479b-81ff-3080a5c7b70f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 84d65640-d9d2-419c-aae8-d6b04c12bf7d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 902afcdb-68b6-47f4-af98-7e843ac1fa0e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0559dc24-ae2b-4e1c-9be6-3ac5e82e36a8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 81499aa1-a36d-4fbe-9411-4d82b5501efb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49a4b0f8-81cf-49d4-9766-a99963214b85)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a66248b4-bd53-42c6-96d1-0440d70abe68)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f8502677-bd6f-44b5-a1cb-ab7912281602)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0ccd9911-9e15-42f8-9efa-2e24bb3f9d9f)(content(Whitespace\" \
                 \"))))(Grout((id 334adc6c-9f26-45b0-8561-5045b4d7d5fb)(shape \
                 Convex)))(Tile((id \
                 b2743f3c-ed5d-4a2b-b554-d08eedad8478)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 463a0555-bb7a-4506-9009-8854f2e89e34)(shape \
                 Convex)))(Secondary((id \
                 c7f29ef8-b541-4037-84a5-905e00927df4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0b2edba7-987e-4278-8767-db5bf683bb2d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b10e02cc-08e1-44da-afc7-c6be0cd97ad6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dda0be53-ad2b-4e53-930b-0cb7e6f7948a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f616dd7-2e1c-4994-bba7-3ff2ae2d009f)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0f5af089-9bbc-49cd-9265-ee3ca2cf6ace)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb0f9f2d-7e60-4990-ac6f-16760eb105c5)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6636c11c-4be6-4b48-8c28-aa2a07cc7295)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7c664c94-7dfc-4767-8ccf-a6e76913edbd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81eb253a-5e19-4b4d-804b-f3eaae9d08aa)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4e458e97-f12d-4915-8bde-a822f8d14a49)(content(Whitespace\" \
                 \"))))(Tile((id \
                 333b9d71-ac78-4403-830e-fcd058dc6a56)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 deb814fe-e232-4f27-b98d-7a2ea24c082c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d3b1805-c4bc-4ba8-9706-f5ea459434c7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 79732f22-2121-474b-9db4-46e01e01e900)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 acc847ff-b677-4327-8def-4345e13297f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdb4ad16-b715-49e7-9594-6630dec245b4)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 00958786-584d-4a6e-b5c3-61ebb8af0fd8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ca939cd1-4dd9-42ef-9ab4-a4292d3042f5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 196a55c4-004f-4997-9a82-4c116c44f58d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45530c89-21b9-4da6-a411-be788347ff05)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 19ccad19-db00-4012-8cd9-090e58e34d2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65269777-d11f-44d2-a3a8-2bd0703d931b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1f58078f-b848-4839-bebf-8aae0b139a85)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e45cc210-7d11-4115-8e0a-19aa91b5cf85)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 95711e32-58c5-494b-b912-0cd1da1944ab)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 05d683a4-b423-4906-b626-95272e089863)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19b1c876-def7-4ff4-a273-83a47cec3faa)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c80439ea-f422-49be-bb47-44ce041545bc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7c5999b8-ed2e-4914-90e7-883585f46fa6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b66ca63f-d073-4e34-b5cb-5c6a50be38c9)(content(Whitespace\" \
                 \"))))(Grout((id 92c3c219-ac9f-4f4d-af39-7a87433ee68f)(shape \
                 Convex)))(Tile((id \
                 d5a45e15-9eca-41ee-bd0e-91cf154473de)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9fd54931-31d4-4074-af43-f92faa65f163)(shape \
                 Convex)))(Secondary((id \
                 a51d6bbe-db2b-470e-a677-c5d553674981)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 70408414-c362-4277-974d-5fd9979c2215)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bd381de5-1ece-4e62-a2b9-3bcba8736c27)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6fd64089-30ea-4435-9635-168ceefce138)(content(Whitespace\" \
                 \"))))(Tile((id \
                 199a5e0e-7724-468b-b556-44a91bd42ec3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ce3db5ad-adb4-4b1e-aadc-17e112447cfc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9579cd24-36b2-4cac-9bbe-c79e73be06de)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c47ca04c-8e09-4d04-8690-81d8362a407d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 514b0fa4-cc73-4f80-958f-7a191a068d88)(shape \
                 Convex)))(Secondary((id \
                 b7aa5bde-8384-49d2-aff4-ade2cd61d341)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f77eaa78-6d27-4e18-b77c-593396192565)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 314d6e20-a3cc-4b14-a048-fbd76c4134e9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 de1bb128-289c-45a2-858b-1b625fb2df55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53c7c075-ff4e-40c7-83f0-9aef4ece7934)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c0ac69f0-46de-4ac5-a592-0259cf25c098)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a72518dd-352d-47a4-b054-3778e2240920)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cf693380-0cfe-46db-bf12-02c17a2aa5d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6df1647e-2f6c-4350-b6be-ee0a149948d0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1e5d2feb-aeaf-4b5a-990e-721741af9415)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 82f4bbc0-c3a7-4905-8bcf-24e6dfd3587e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a602693d-8766-4acc-a621-b6bc94edff92)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7efbc47f-66e6-4e95-b592-ca445a855202)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 158da482-bbcc-4b98-abb5-ae7240ca085a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 32241379-1075-4aca-9656-f326aaae6f99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37f36a46-08be-4e43-9d0b-d0cc4670f173)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 33d9fa38-98e4-410f-96ef-3814cda0ca91)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee9cacdc-1f65-46b1-ace9-c3563f8d198c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5d7527cc-81a6-4a4a-b0ba-2fe9d9aad62b)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c4cfa363-28dc-4c07-8388-89511e2b021d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 20243ab7-fdd6-4be4-849e-084f814706a4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 195a02a3-1cc1-4584-b028-c6adab8678d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8914e2c3-7ed0-4184-9de9-c8c71bc84ded)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 45dadb48-ba95-4353-826f-10544c399bac)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 afde939f-0fe4-46cd-9044-7190b86fb63e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9e5b265f-aaa8-4aad-9014-3f91abd0f1fd)(content(Whitespace\" \
                 \"))))(Grout((id f43f1d26-9790-40e6-8c26-d559056442b8)(shape \
                 Convex)))(Tile((id \
                 e6f2d81e-74cf-46b7-9b72-224b017542f1)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 2c8885b0-9e83-49b0-9bb6-eac06b6b1dc6)(shape \
                 Convex)))(Secondary((id \
                 678bd368-85ae-4b6e-8f8a-a2106b84d126)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 722a324e-a9e2-4819-bdc3-0aa7b19e1a67)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5940e2d7-b76c-40b8-b3ca-2f4e83d89c10)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6257d75d-660d-4574-abe8-e7af6b9d1b42)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d357e807-390d-4e1d-a4c6-a258a74260d3)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 53d32836-c517-497a-9c8d-4dd4166d72d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc43d610-5d39-422c-90c1-99c19b61185a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ea17cd76-456c-4f64-865b-97b0809a04e5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9db87b7b-1787-4164-ada7-be267ad1c889)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7b890a4-c153-4935-86ba-712f9b6fb293)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f60a55f3-2c3d-4805-b2a5-6aa5180e1634)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a98d7c92-73d5-4dc7-aa1a-698d699c254f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13163b9a-c8a1-4018-ac8b-f82e9aed7a02)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c379e843-e2e5-44fe-a233-224a671d9cbf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f5eff3f0-1072-4f8d-87bd-624091b2182b)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 079d9993-25ab-46e5-ba58-99e1e13d5730)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36d45045-38b3-49fe-ba96-21a0596aef7b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 57684621-723f-40b5-b736-65f23b7a539f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 06c16dfe-8fd9-4672-afa3-f1de2f342e30)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0af9c58d-aba1-4edb-9a80-5d2e3d3492e5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f57502b2-6858-4af7-bcd5-d899d462eb36)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d67bea13-16e7-410e-8473-cc9ebcbeea7e)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2a06fdca-dcc5-492a-a9eb-b51e87a91fb9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b7ee1179-4557-454f-bb83-05a493d357f8)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e14b3dc7-b3e6-47a7-a734-fcfedc9ae80b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc1a8228-d9a8-4aca-b279-383684bdbe05)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7867277d-b26c-4ac1-901a-5560bf448222)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 81f3dcd8-5598-45dd-b0ac-dc3aa27ebc1a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e5615653-8302-4f02-9aaa-79e7601a810c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 beb40d29-9522-4c9f-b342-9e626d359fec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8571f2b-0932-4396-bee6-337d02c5d442)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 97fd33a0-7de7-42f3-ab59-ee519aafa0fa)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2c0875ff-6419-4b2d-989e-35bc2a1a3036)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de8c86f9-72b4-41a2-8e49-2ae5bd904c34)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f5359fcd-929f-49ac-acd3-cb4e468e95b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cba7e26e-5324-45ee-a040-b99f74a6345b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 92053cf8-4aca-482b-9a75-feaf27038833)(shape \
                 Convex)))(Secondary((id \
                 cbedb124-6ad6-4594-8995-48b938280662)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 92b22cfa-fcd5-4e64-b1eb-42350efa6005)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e92bba3a-02e8-49ce-933e-608106fa1d5d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 20d12cd7-8150-4900-b429-ada033c245c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e31eafc0-adae-4acb-a0e7-dc70ce82d66e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 92e6efec-ea3b-4263-bb24-790ccfaadfa1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 763f4ad4-9606-4a1a-a5e0-92902c767d9f)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dd4eec56-8f3a-42f8-883b-617269d33edb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d587040f-8fdc-44c3-bcaa-a2d3673d573b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8c46661-9b28-4712-8e26-4a5bfd32070c)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a75b25d8-23e0-4a7d-be44-026f8ac16c8b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c325158-7e37-4ce7-8851-04b86f9ef104)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd54c5ca-e661-4d82-8153-491bdd93efd6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 629d7054-84b0-4df5-827d-a53d1d9fcc3c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 45157d16-447b-4148-b709-d756534e56d0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d9102e0d-7373-423f-b803-06bd4bbfa12e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 640ccec8-05de-4d42-8463-99b80258070e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 17d54597-59d6-4e72-bddc-544c46818c04)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 aa80ef64-4be6-443b-a6f3-1e61ed8069fd)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 0830394e-ba5d-40e0-98b0-132c96adf0a7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 133b206d-3940-44b1-a424-7f8244505a72)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 35a3cfb3-0761-4bbe-b332-25bb83b35310)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0e6f463b-c79e-44ed-af9b-544e8e6ee43a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d355c1e9-d709-4fbd-9f06-08c518077346)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2ce5c0a5-2431-49c5-9d55-4eed5b2cbd52)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc86465f-5c0a-48b5-aef1-51dae9a80e31)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ea6e7924-e38c-4ba0-8acd-622db92a1d3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6546cc92-3d5a-4a40-af7e-f081c2b83470)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 eb1aeb12-f9cf-47bb-8a69-c26b20a3779b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c9602d72-4ccd-42b1-8542-7b5c8cb8e7ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 704a5137-6382-4a53-8776-c36c02b40863)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a90dd745-12b6-452e-a3a9-f052298fd2de)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4266fd4f-6ea5-4dac-9610-97cae7ebb411)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9adbdab5-f905-434e-a6bf-791180566e08)(shape \
                 Convex)))(Secondary((id \
                 d8c438dd-8c59-4c3d-b05f-961580fbacf7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1004425b-ab5d-474d-93c4-1911af40434b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 368965ce-b2a2-46bc-987e-d30c517601fd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ffd37850-24e1-4703-accc-aca35365a4bd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f86e198-5228-4a1a-afe3-20887f3b3bcc)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d4a9281e-b622-4a07-83e4-95836388416c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ceaa600f-332a-4512-ae7d-ccd431312463)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0c01a344-f1c9-4c7e-8ad3-573c45ef2a08)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 31b2b80b-fd60-4a15-8201-9fd3775e2356)(shape \
                 Convex)))(Secondary((id \
                 3b8d624e-7cbf-4806-88a0-790cc43db5e9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 eade9501-66a6-49b4-837b-a005cd7b2674)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c0696d59-61ed-46f0-9fab-7577204d5e8a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a5b77b0a-dafe-4e5c-ae2b-95b835546eb4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7406966c-01c1-40ae-8438-894d1bd59a01)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 97504d19-0589-43ad-9def-8eefdf693b19)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b24017a8-1060-45de-8438-0cb0f0c69baf)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 dbe84fb8-9cde-4815-8f5b-68370e14c05a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 235d8af6-52e2-4b8e-bf91-009e4447eb50)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a9d0e871-e802-414e-bd30-bbc48f9f6dc9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2a7c5668-e30e-4e8c-b140-dc0e219c3a94)(content(Whitespace\" \
                 \"))))(Tile((id \
                 742b6c6f-3db9-4d16-9914-a5d914ad92f9)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a0091632-5ca2-4a0a-8693-af46c964b707)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 900e40c3-df7f-43de-868e-529a722d69b6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 7f669310-1611-4150-9484-022033cbbc76)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa13d4f2-71e1-4dd8-b9c8-523e1a2cd552)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 906bec45-5fa8-4910-b1eb-0bd172b39cff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b493b63b-3673-4cf5-9fc6-bebb42db1396)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2e133c45-2e48-4006-b0ed-ca719e6d9ddb)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 871541cf-13ed-4348-9a2f-b93f45dca05d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4d10f655-2634-4c1b-9762-63ed6a7780de)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 45785c6c-5f7a-4373-b7cc-f67910580015)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc8d814f-86c6-4f6c-ae4f-3cdf5b2c049c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 50bc8550-04dc-4d90-a4a9-be8df5d98a3f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ee84e1a7-15a8-40db-8a5a-4d9870cb2edd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a368cd1a-a7f6-4903-b2f7-d3beb1537bdd)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3d1c071c-bfda-4037-b178-019b06488b8b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fe78a55-a7de-4ff0-8787-ecd69bc9e239)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 ec97cc38-d201-44ca-ae64-28806c5d8d48)(shape \
                 Convex)))(Secondary((id \
                 369b2741-2397-4f99-acdc-460eb71dcc12)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8d566925-4560-45ce-af57-70d001f7af47)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2f590b5e-aa3b-4967-b0c4-f7cc737b8d5c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 435c918e-b179-42f3-85a8-1c2de3f745a5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 162de82e-a946-4fb9-9526-1019b2dc13dd)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 19beca4b-cce6-4ab2-b079-15e467628ba7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e2e4f39-845f-4a7b-82cd-571ec45b852d)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 673c44a7-ac40-4ed6-8b0e-90e7220f4134)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 feceacc8-a63c-48bf-82ab-7b0b97d47a90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9478b03d-b2c3-4714-88cd-76863e56b029)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 feacb768-8686-440a-b28b-9e33df74853a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 db49bbf6-b497-4ba7-bc17-5a1971908944)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfde7bb5-0af1-4d93-bcd2-d90bdf3d04c7)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1cea4d62-89da-4bdd-b13f-7dcd4608ece3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae82c61c-e474-4d87-9bf1-f9931d692738)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 dd934033-111e-46e7-aaae-b0df65816e53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a69136a-bcab-4cf0-8da5-f535f7e26d27)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e90533d0-3cce-4094-92c5-72dfcb56abdd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c6226fbd-11f6-4419-85ff-ab3c2498e170)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41bd500b-631c-4c81-9c59-55f283d35d0f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c5598d7f-05d1-4f6d-bda7-d4418e1a4093)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 52536945-61d2-4a8a-b34c-0f97f74678ca)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 74bfea44-e314-4b68-aa4e-8624bdc8ce7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a263c68f-6725-4707-b6ad-7dd38477d1ce)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5fb52d35-62b1-4553-95ec-908a25026114)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd524143-3ef6-4d77-a5aa-1d1607996ea3)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7e8e6138-15f6-4d0a-a1c2-9a8bbc2408f5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6015d050-9f6f-4c94-aab9-b6beb618f94b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9271a2da-695c-4a84-a5f1-782b6d1a0aa3)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 34844e16-5aed-43ae-a080-46a40717dadc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7dee8226-5eba-4a93-a598-fe1c5f7f7320)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2e94a4f6-50ea-417f-bf87-9b38816eb882)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ddfb9fe2-8c13-4c32-a518-c81106179548)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83327e64-693f-4bb9-b698-87a789323c9a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d466c4b0-bfee-4b81-a549-29ff26e5be9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad3e8603-24bc-42ef-a50c-25f61ba914a7)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7a253466-e174-446b-ac4b-1033e9f6b871)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c0fa1d45-e9e0-4a59-9bf7-a5043d8cc002)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 10fd4c96-baa1-4e8f-a53e-100cb36aee6c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6aa641dc-56bb-4b33-82e3-1cbb94491d1f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2efa7e74-501b-480d-9be2-0b91ab400b25)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 bbb4dfea-a227-493d-8320-effb69611423)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d7d6b45e-36f9-4c77-87cc-9564261d6fff)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ac7ab096-53ba-433c-94b6-c05d8703f2cc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8325b5f5-d41a-4dff-8114-99b59ecd3e85)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8f221da-21e6-4912-9914-31697964caa5)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f5bf4238-4163-4d18-8580-2313d921959c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce6a2eb1-970d-4e35-96e5-ce797fa6aacc)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 59e29b21-17c5-4c1a-8b3c-54189c96e778)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d52afa68-cf8e-4632-a745-24e9d0a5253c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e71e9c30-311d-43e3-8dc5-a28b95f9efd6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a8749f93-4f77-497e-b667-a20333ac73ef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9101bfb6-4ca9-4ff0-94dd-98f16a638608)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 16a464ee-12de-4725-bbfc-30139156c5b2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 026be924-3fc1-4243-ab59-d413f936b923)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b53dda25-5f7a-4033-bf2a-cf2d81e6335c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b65ae5ba-d37b-4317-81b0-b9dd36d60103)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9d106574-4e8c-4fae-94b5-2c2464948b43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8c8b27e-2262-43a1-ad3e-9ce34e3eeedd)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b4e87bb7-2370-4ec5-91a1-e7fba20e2b96)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8ecd5c16-c27b-4db8-841e-7a7d84b56783)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 56f5b307-ccbd-4055-a5d0-e15d2eee7c3b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 91762bf5-6066-440f-98de-eedf468d2efc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b10c2aeb-2def-4842-909d-39519641905a)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b5dfa606-edd8-4917-b5e8-e167cb639daa)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 70c3fcf2-0d68-4f7b-ab6e-77544c8ec3bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a8e10085-b0db-442e-8e0e-36280840e81b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e333d71d-9fda-41db-8e17-86e23097d28e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1404d298-b539-42aa-b1f7-2e3d7d4c24b0)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1a889cc4-fe72-42d2-bea1-e2f61e37ac87)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ecb8198-c2a0-4664-b928-3c703caf0add)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 8ba66779-25d9-48b1-926e-5a34765c312d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bd6d0014-9e77-4225-9ff8-c0a7de2825b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29dfd12a-2d87-4df6-aece-53cf6bba0c52)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4294c873-5c21-4a94-bf88-758b5fdf2f7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d466322f-a8b5-47c0-9dc6-bbc7a28d284a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8b147a7a-1da4-46c6-a28d-091fa16861c1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 e5d814f6-aa9c-47a9-b63c-42fd335f953f)(shape \
                 Convex)))(Secondary((id \
                 aaa67ede-7822-4cf7-9f19-303fa33f8aba)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6e552c40-4071-4359-b9e9-37aa5802101e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f5a43ffe-b44b-4887-a2f7-c7ad2c748a8f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 afb4920e-7c70-4c30-80a8-b59dda228ee5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf7732e5-78e7-40c8-a9c2-a8ecb70093b1)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ad412ee5-7ba6-43fc-a25d-326aab9770cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ca89b07-36d4-48f6-8290-1c8b24c034be)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cb3c95fb-ae39-4615-973e-ec79808abb70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a6f26646-3b0d-46ef-bbec-37d88efc98cf)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 205acc81-396b-4654-a205-a613a8eec959)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a47cbbe4-adb2-4be2-aab0-61f00f9562e1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cec4d937-3598-40f2-9397-c6a8e97a8685)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 481fb687-e80b-4b1e-ad16-d118f96518d3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8bec8bab-36e3-4f51-a0ea-e1c25b102bc4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c515d61f-d2ee-4687-aa95-d27c345d9124)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2987470d-5adb-4c4b-b833-a6053464f507)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 921d2a1d-8b94-416f-9310-896690875100)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e9ee143-0c04-44d7-9377-b00d030d9e8d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e0eb56a9-fe68-4c96-a5bb-1d4dd7699c0d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ce674ccd-c248-497a-be93-2d1827321580)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 77df4c7b-d395-4223-9069-a41ccc5bbcec)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 37e216c7-b5bf-4c84-94e2-809bf6f37566)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ba77d63-777a-4ea8-8e2a-b749252fc99e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e7b23270-f09c-49d9-99c9-63cfb246bf84)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0a0f7a4c-5607-4b44-8f2f-7d7fea49ad79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 43c8c778-7a13-45ca-84b0-7f325e56c3aa)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3d7b8db2-5f04-432b-8e91-08dcb9396fc1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46647b2a-ce9e-433b-b19a-34524e8a3936)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2948e58b-e983-47f2-99d0-ee3888d9dd2d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b321fb83-884e-48ab-9f0d-821af55f2875)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 319dd3f4-27e9-49fc-9cc9-c1061871c35a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0bd2e23c-685d-4f29-bfe7-a69b36196188)(content(Whitespace\" \
                 \"))))(Tile((id \
                 acbfef52-103f-477e-bb21-04b4d1bdffa7)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 41fb3d84-8b61-424e-804d-ec69c303fb6e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b5d610f-0469-4739-aca7-5119fecf545b)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c410566c-dabb-4c16-a9d3-529919c4aed7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f58d88f3-e73c-4460-af33-c98310bcf84c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74f034a8-1c62-4a1b-949d-3d5d9ab06300)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 457e1f7b-6d2e-4f14-ab9d-ffdd7504178d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 068412e9-a2c9-453a-9471-5c85ca109a50)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9e91907-caf6-439b-a853-daf48c23c644)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f2c60bb1-7cf7-4e8a-a851-b2777cefe181)(content(Whitespace\" \
                 \"))))(Tile((id \
                 377c0269-ab26-471d-82ed-240201592a29)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9c093f60-be9d-4cb6-a5eb-1da93f0e2024)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef606d34-8568-40f3-9202-3c38e4be1b75)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a4529399-1837-4eec-b112-9938302ef97a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 22922dfd-dafe-4fda-9d42-5edb7349ef5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d60e887f-7cde-4ee3-bee9-380b0c41f4c8)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bebd0d47-8506-427e-bed4-9246a8fa3621)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e8d45f5f-4215-4eb2-9887-f5ec0c765f36)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 a1fec6ee-8077-4339-bd56-4ad81eb6f9f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a821160b-ec97-4f7a-9998-ff6f05d1ed41)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7f093f83-8c79-4361-ad05-6c8f5abd5b59)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1640f07-6616-4e3b-984b-dc450a6bd185)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3c27e8eb-4eec-4d0e-a200-069eea15262e)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 231b5583-44e9-4949-a331-b28a92cd214b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 454d0a65-bb35-4776-a9ad-eb5f8e46878e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 eaddf229-c03c-456d-92ba-61829c5734ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ab0bf37-42ac-4125-8e53-97ffa6cfa810)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9adba8ee-9969-44f3-8b85-d43128748f9a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 98c65d51-bfe5-4013-a0a8-51f782896775)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bfd8acf7-a1e9-47a9-8fb2-5c081e0b2792)(content(Whitespace\" \
                 \"))))(Grout((id 06b65df5-0379-4b3f-aecb-8bf155c70e95)(shape \
                 Convex)))(Tile((id \
                 5cbf5af7-2983-4b7a-b9e2-97d2bd4f0d0c)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 80529a17-bc7e-4717-a0f5-aa7139bc6ec0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c8ccd9cc-6cfd-4669-80d9-65d5b3366014)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b35aca9b-0125-4534-baf8-a3c52144bbc3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1788e3ac-e9d2-4997-8700-1619e77b4c1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d3debcd2-11b8-4475-820d-10af6ec26e85)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2af4105d-3a26-4ec1-affc-268540239e73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 676d9a2a-d045-4e30-aa75-08aacceaeda3)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7b4b3bb9-0ab2-49bb-9535-13a4dc5aa33a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2b90b3a2-7992-46d0-85c7-564d7220d786)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aff6e94c-0622-4fc0-aef8-d37dd2c054ac)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bee3c868-d07f-4ea9-ac8e-114e44b4238a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57a107f3-350c-4caf-90d2-f9b19caf3dd2)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 92398e33-19dd-42bc-ba65-f748223fdf19)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79ba26a3-029e-42ce-a5f6-e608849dfb20)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d275d071-0005-43ff-a9c3-f60932cc73f5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69a16adb-2f59-496f-8a9e-a7f9a4778ab6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4cddd64b-aad4-4393-a692-1229aad0d708)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 31699a11-f0f3-4bbe-835c-2d9a0547c923)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 246914bb-94b5-4385-857d-15a9c726ca94)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9ac692e8-fd99-4364-8fa9-d900a79de82b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56b9fd19-52bb-4f4f-bef4-de4cae06ddc5)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 aad6e147-4ced-4e41-9521-29dd02ca74fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bee762ec-aa9d-4543-aa94-8780dcee6ceb)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 937d88fc-8b2c-486b-9533-462f90c13af9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 209d44df-86f3-47a7-bde2-71f302b90fd2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 59d846d3-03cf-452b-9725-f9c2a788049a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e18f7773-c0d1-4f56-a7a3-595ee339c11c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bdcfa538-d94a-474f-bf0d-a59aa8e41531)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8573f11e-bd23-4638-8864-ab61cc09d5f7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2d10e49c-cc56-4a48-ac01-2b7ab70bf0cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 93713604-8132-4740-bfe2-f91f72612180)(content(Whitespace\" \
                 \"))))(Grout((id b2ac825c-c14e-41d5-b475-0af2705abf9f)(shape \
                 Convex)))(Tile((id \
                 ed7d0ae5-2e08-4d63-998c-316da7727a3a)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6f194494-9a99-4d40-9500-2f07c3f9b9a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9578f84-93f2-49ba-9361-184e182cd368)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a7b14fde-e4f7-48f0-ba87-cf4519b613be)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 16db7155-34e2-4ab2-a55a-21336cbc8ec5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf9a5376-28d0-4a89-9a35-ff5398c40061)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fab475ce-6534-45a1-b664-32cacf8ebc4b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eac01f24-a260-421e-9b9d-29de30d8fda8)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e8603777-8e85-41b1-98b9-188469481100)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 b31c69e3-0aee-4628-a3d2-37fc8cc998d2)(shape \
                 Convex)))(Secondary((id \
                 fc7d961a-a770-42bc-ad2c-0752044409a1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2af44ecd-cf04-4504-8892-763831adf8c7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5f47194e-e532-4f25-a499-cb5a5b98f79b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4a0ea005-9236-45cd-913b-8e19c02bad26)(content(Whitespace\" \
                 \"))))(Tile((id \
                 77b9aa74-7611-4159-89bc-55f3a1e036c9)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3f713165-a7c3-40d2-9a13-10386afc489d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 422853f0-6b86-426a-9b6b-573f627c4314)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fb69c827-bacd-4b12-a1ec-75aed8dc3c8d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 697fcbee-c3e7-4381-b3ba-dadc5121e8ea)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5e9c4dac-6fcc-465c-b41b-e23c7207a212)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5137a4dd-0105-466d-a6b1-004906ce67af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87121951-34c8-4ff8-aa35-d1bcf4cbad6c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 38c7acb0-cc5d-4637-b5ed-4829ba4d553c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 39add059-c1fd-4ae0-9c04-915ae75755ad)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 6bd28861-5fa4-4c01-ac8e-2cc4bd0ebff7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10cf3a56-0d60-43d9-9f29-472c0a9d0f90)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 20e31d77-bfa3-4c0a-9089-7ddbc5acb42b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11752607-8680-4bb6-918f-9f4facdd03e4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7cf700c7-61b1-4da4-9cbe-80dd6b6ea35d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 32735b5e-86ff-401f-8f0b-465d864fba23)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a0f39e86-b682-44a7-bb34-187bba6375c4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ccfe7ff3-74a4-41b8-a225-ac872f661848)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b827ef7-8ba2-492b-aaff-51d0b08ad64b)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ea96e86d-1e1d-4f9a-8e0a-e1bbbefede1e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 13a7fe84-18b7-43c0-85b5-8231c0292f99)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cddb13f6-712a-4f37-8118-d9df403cc909)(content(Whitespace\" \
                 \"))))(Grout((id 06164e6c-0ca8-4628-97a3-ce5f03f898a8)(shape \
                 Convex)))(Tile((id \
                 b84ab32d-27ff-4bea-a65b-e1406ec1ff30)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 047ad6e8-8204-43f4-ac8d-f51e5d516363)(content(Whitespace\" \
                 \"))))(Tile((id \
                 189542d6-5784-48e2-9bde-bc3a1ac46935)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 16f3e09a-cd27-4c8a-bae8-8d8dda88b10e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d27e1b79-0ff3-4ec6-ad1c-fa91837a5db6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 82eb621e-3141-41bd-b780-54d4fe318d40)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c30f6128-442c-4625-bd51-6b207dbd2bdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db25fbde-03f0-4fde-a532-fce4ee36c9d3)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f0e23317-b8d0-4b14-a5dd-62255e38dba4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cbf2e344-bc2b-4988-86f1-ad2ea49ffe74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a7bf2b0-1e40-4c82-8b80-119f187ddaa6)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2f1ea14d-6e80-43af-adb0-ba76c9b97f77)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5fa8455c-642a-48ac-8e63-9d6ecaa92e58)(content(Whitespace\" \
                 \"))))(Tile((id \
                 541d68bf-b628-4eae-9209-ea46219eaa14)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bedbdee3-8633-46e8-bb14-fcef7a175066)(content(Whitespace\" \
                 \"))))(Tile((id \
                 105d38e5-d35a-40a7-afc9-9c3b508a711f)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 70b7a496-5161-4b21-b196-9ae260847a90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33de37fa-e7a3-46d0-9887-5c970f204d05)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f3222076-490c-48e3-9b8d-f54df590d9ee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3a2578d7-b939-4ee4-a634-dc8059abede5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d6390c4-d52d-4204-97a1-30f0e0521f0d)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 74708d45-4c6f-4811-93de-4523189e4366)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 04dfbcc9-1f39-49c5-969c-eb07e2587306)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 dc6bc0a1-9b0e-4567-b1e3-260aded8f84c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 213076fe-ad6d-47f6-9c71-41b1120cfa7d)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 60fb9e75-75ff-405c-be95-45bd10a55acb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3460ed7f-7888-49ef-a6c9-0a9880c01706)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cdbea899-1534-4944-8e68-6205942615bd)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 341dfd2f-b694-419e-82f3-42b57a0ab5c9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1fe0bd69-7327-4bfa-a88a-0f435a111ad3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 58c25d88-386f-4029-a5ce-9a66cd7f3861)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 de4ab77e-dcb1-4e71-86c6-c3beb4dc4402)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddbfd7c2-71f2-4287-a290-7b1ddb64c0f6)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 562ba7d0-49f0-433d-90a2-23878c49f43c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d3b4654d-2960-46e0-95c2-257bbb7cf4aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e04c8c8-2563-4a75-b537-9148ae8df736)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b7615011-85f7-44e3-9639-c42a9ffe4770)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae6bd94c-e8ad-4d2d-8d91-6f1fcfa88725)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f5f77bdc-330a-438f-a0f7-c360c0fff8be)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 d9e63c3c-f72f-43a0-b96f-080430114f0e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86d5cf2c-52e7-43f4-9010-0c4c94d70751)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 cdf4c3bb-f419-46e4-a64b-41c4c1333f3d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bbe7464b-c3cb-421a-8618-b5aa0d0edab0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e4b0bdd-3e68-4f12-8bc2-d17f6056ee2d)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 418df9ee-b794-4953-9cbe-f3279474e4b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 61a97d93-48d8-402b-9f67-5a283b18c5d9)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 09e88407-5edb-4bac-8ee4-1ad7df50e0a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c61d45ed-cec4-42fb-abcc-f4a5f125950e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a917be30-7c5f-4298-aee6-38851c2660cb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7b259183-e434-47b2-812b-db9642bd94c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9301cf6a-70a8-41bf-a622-49c49fd00754)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7b9ee891-5f0f-4a58-a32a-e9db8f1ac4f0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 dda4c579-0408-4a51-b1dc-6f97ccdd2e05)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 27b1be15-9e4a-43aa-87f5-26de83265103)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd081940-f5aa-43bd-8a30-720800cddeb0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f6192cc1-0d1f-4ea4-bb9f-3666c704a62c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 dff5f94f-934b-4588-8789-dfd06aa0fdb9)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6a653547-3323-48ae-8e53-9da33293859b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a18871d7-6f65-4b54-a155-4618c6093206)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 21140476-577b-466e-b4d0-34c4817295a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 031c0681-9ef4-416e-9061-b617ef8b6043)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1fa066e4-b63b-4947-807e-63ba8187294c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c4cf3db8-07b6-48e9-badf-0fefbd343a40)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d0afd46-38a8-4d86-ae34-f16e7127521b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 47cc914b-c68b-44e5-abd4-66cf763e18d9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35fe143c-210c-4fe4-818b-2b6d2ad67057)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bc6a873a-8e9d-4901-970e-4e769873040a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2fdf6d92-bf05-497d-b1be-082ae2bfc752)(content(Whitespace\" \
                 \"))))(Grout((id 08fd8118-cf68-46df-9f4c-e60d517fcb32)(shape \
                 Convex)))(Tile((id \
                 7c634ee5-b6ad-4e60-a1ac-8f56da4145c7)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a63a12dc-947d-454c-b1b1-7161215aa674)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f9334ee-1bdc-40cf-9b6f-58fe6bdc93ab)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 df144b08-6d20-4e11-8a10-0af0c509eb9e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 54d73ad5-4629-4898-8a74-12e979918871)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 31c6a068-93b2-42c9-a71d-b16d3c7f048f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f505df7-a328-4d45-a811-2c6c355cd953)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 11ee247a-91a9-4169-a85c-0a13c293b2de)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34ca9c74-f656-4b8c-8afb-6433b3488aba)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 05af1d13-4d8a-4b67-85d5-501c72bc0d2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fd4bd7c-33b0-42ff-ad0e-7b041c6a4854)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7b8c4ad6-43ea-4219-a589-b62c7573826c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9410ff28-6598-4d54-a937-8d528a4d52f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb3a4fbc-6ac1-4633-9873-5182610424ff)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 20263dd0-e85b-4840-9b0a-5ea37bf300b5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8e8285c8-a16e-4c6d-9c58-180436ca454d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 08e08e23-695a-4f12-ace7-e7f27ecb372a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f7f40d1f-9c7f-4f5a-8509-4651a853ee11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a5f1393-c37c-4e56-996e-1b896672296b)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9ce44ffd-32dc-4159-9f2f-6d71d7fe5657)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 708a3c9e-7292-4e02-a2e2-3debb4ae1184)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f99b4fc8-4211-4b61-953d-b345391a2dfa)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 79116db3-94b8-4afd-8ed6-c05838975946)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4b8b596-cf5c-4544-be05-8f4076334668)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 93fcc7e9-2ccd-4dad-b55b-932bf474735f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c38d970-3c9f-4c85-aeb9-48cddfbcf8d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c0cdf1c5-37e0-4f27-9d65-1f7e8b7974b4)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a4aaa32d-e542-49c5-af81-0f83b813893f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 96f2f3fa-666e-4636-a17d-cf906d747c29)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a2ba97d7-5816-4de0-bfc7-bccecf3f7e1e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e90d08d3-4008-47f8-9719-8e5850767521)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1df5e4a9-efbc-446f-b348-e2729fa7bbc1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9c17b513-e699-4468-b715-e4b3bef16d7a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 dc6fe2ac-0844-4831-a69f-8268fee4cd5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c7a314cb-ce89-4011-814e-cafb5de89929)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ecd17226-2fde-4fa3-8ac0-3c5f2b26c782)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f35b75f3-dbcc-48c9-944c-c64cfa85c0cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 399863c2-7096-4af6-bb8b-69906019eb47)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 113a25c0-9431-4175-932c-78c5f11b03af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9c0de07-5476-464b-8e92-b9a68cb7994e)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bf427820-4f8e-45f6-8284-73d63c6b617e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 359007d5-6cd8-441f-9c23-05b0e723cb23)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9282171f-8148-467b-ad22-90e959765351)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e00be104-2058-4558-81be-4c95e7dba78c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a58bd02c-1452-4a03-9c84-f6bddd6119dd)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2deb153f-02e6-49b0-97a2-7fa27e44684e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 08f00ed0-7003-47db-b330-939913302b0a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6e29a81c-3bae-4035-8877-6e2e7c4d1951)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 596659fb-ee22-4335-b1f4-01f31c811ecb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6500abfb-77d1-4b10-8d77-721c7e37c19d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 51053908-2b8c-4779-b9a4-b02be031e7ea)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 59c89ff7-2830-4fcc-9d7e-fa6772eada69)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fe420401-4767-4801-a917-8196ec3b9195)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6c023ff7-7387-4969-9430-7a5f358234d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb4ad5d8-8a71-4cb9-a6bb-e886aafebc1e)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4e30b722-84f1-4351-9558-3b22b4138087)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 07bd7835-aba9-4576-a6b0-5b215203f830)(content(Whitespace\" \
                 \"))))(Tile((id \
                 734c36fd-3278-44fa-a630-27c45846ad45)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 28c0b0f3-1d07-4291-b94e-84c3edb8411a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be3344ab-13f0-4196-a242-d58ddedb4086)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 74bca819-f017-4337-9415-6c7abcde4ccd)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 433b7881-2167-4c49-b41f-34eedd90a1f5)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c90e84a5-c611-4465-abe0-f4a0f0dad107)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a2c42e1-3f6a-4f9a-afa9-d8544edeaeb3)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 b2e834b5-ed02-412a-a5ef-34a5c16b9847)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 892d9e90-2a2e-48ec-8487-8695555e6dc3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1888f269-f7ef-45d2-8512-8d395f78f772)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2640ea25-cae1-4f62-bfeb-0ee5e6e2429a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 66c241fc-9f12-42ec-9efd-d0695cf61f8f)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 e70a85e3-9a91-4a5b-8464-6757d36b6cb5)(shape \
                 Convex)))(Secondary((id \
                 4a5a880c-1ee3-48ad-912b-5d7ca04bd101)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ec8115ee-947b-4f7e-8bf4-9ecc99e9e230)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e97f099b-5db0-4e16-995e-0e9f35f7d7b1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5e268789-9fbc-49bf-a7c8-be69e68b3bdc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f96c361c-d526-466b-84c8-7de23fbafafe)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 78de6f1c-120f-4090-a3cf-b6970752ec5e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0463001-bf12-421f-b5cb-c2aeb6be4e0a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0433e19e-1b31-4c8f-aa0e-bbcdc08ed996)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21d714f3-d2a5-4ac0-9488-b3db501e68cc)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a71562c6-dbdd-4ba9-a72a-e27d48aa24bd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a8ea80c4-6496-4ce0-a907-98037672582b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bef81cac-3090-4851-b59e-4034bf3f7e9e)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ac4038f0-7de1-4828-9dea-d81b7ff60c8e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ea845820-5614-4597-b0b8-34e9b1de2e3d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f35a801a-8a84-431d-a9df-544c86be377b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8313aa88-1111-4b7f-a9cd-b56edca2aef1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5de2a053-2140-4836-8e14-224bbbf67cc0)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 74dc559d-c97e-41a4-a44d-9ef55f467189)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4f8e4039-136f-4377-89e5-0de00c9c5cf6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4ba7d136-2a5d-414d-955a-8f66fce3bd2d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 89151a29-cce6-4e8b-9c00-6faf9e160b4a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4dd75df6-e379-4371-9ffa-34f0c9ff7b61)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 514bdbff-46e9-4315-94b4-97783bd08e1b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 48378b91-cc9f-406a-98a4-8d74ac57e788)(shape \
                 Convex)))(Secondary((id \
                 67d6ed9b-e49a-439d-b198-b354334ac8ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d6f626ee-ef80-4947-894a-075e9b008492)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 071e4f9e-9bc8-424c-a944-1c5899681627)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5394fd6d-55cb-4d03-985c-02b3d8961366)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0186f29-bd0a-43f1-a0f2-22d73a560b5d)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7c2b3512-7912-44e7-ac9a-799ca399e4d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1f22398-3f61-47eb-9d20-ba339a9a8d2c)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c4bba7fb-230c-4b77-9cef-718a198e2327)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 d1c3d11f-29f6-4d60-8b8c-c11cf055c178)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f933492-4ade-4884-a3f1-4ba1eee9297f)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5680848b-ce89-44dc-8028-81bd8dfe6e45)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 df436f74-5947-456b-9981-9e3018118299)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa6a29ae-3613-4579-bacf-2e07389c09fb)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6e6f3ba2-8ead-4042-b51f-c7bcdaa4c458)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e56f5829-3e0e-4a9d-9631-85f007248a66)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a327b0a0-b24b-466f-82fe-4ea35b4b617b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c4f6c56-8f8f-4d2c-911f-958798355c0d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 981d6f5a-a9e0-466f-a4c0-d4618c137a0f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9b237eee-32e0-4ad3-8e5e-5f3f4d7d5f6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c758ecb8-91ae-4cf2-b482-eb2c61f6a4d5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7c826751-a5ac-49ea-925d-a49796671502)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 78c75c87-1bf3-49ef-a53a-efe6fd959f51)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4a884c1f-cf71-47bf-a614-19d345ffd1ca)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 160f714e-76b5-4219-8495-8bbeb9896f8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b8fcb74-e9f0-4169-b6d8-b695a45fdcae)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 0e13eb73-fcf5-4568-a51e-5a41c56a73f8)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cc9fccbc-dc1c-46fa-9aae-36da349cdff1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2664e9ba-80c0-46ef-ab8c-05c6eb8d2ba9)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 886f81a1-cd66-4790-992a-faef2daf7824)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e91fdad4-82bd-47f7-a425-881b8b422fce)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 68599205-45d7-4614-94a9-ac84b48fc937)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 59c65e10-9a81-4070-ae86-44ff5b9ce289)(shape \
                 Convex)))(Secondary((id \
                 5fe21479-94f2-414c-92ac-a938e4fb0062)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ccafed6f-a493-4e20-9dc6-d8e2669a5ac1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1dd7b3fe-4b2d-4933-8ca4-ccaed398f740)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 51e9905b-6f53-4cf3-8ca6-52fda987cb48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94630f19-e1be-401f-9b44-076440cb1a9e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f44f94cb-b2a0-49fb-b71c-b9baf92c9f24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4622dd39-aba3-4668-adaa-654ed961417b)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ec32b24b-c1f6-483b-b5d5-fd06024a933e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5ce24499-96ae-4a74-b2a9-13cddb41f580)(content(Whitespace\" \
                 \"))))(Grout((id b08c9fee-9e36-4b3b-b30c-b0cf238462ee)(shape \
                 Convex)))(Tile((id \
                 c003c497-f096-493f-9b4a-9c7b547de3b9)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 6ff0b2a2-3cf4-44e5-9db4-5dd98f4f9ca7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34f536d8-c020-4dbc-a269-1f7f0ecfd398)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 13599e43-14f1-445b-840e-51d3be3f6e7b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3fd11cea-d5c4-4ef9-b938-6d0d036ee281)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1b3b4787-4bb2-4274-9835-95747fd0f36d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 577ef5ff-24ed-488b-ab2b-9047226f4990)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 51fdd1b6-69bb-4df6-aa37-b305c5876e34)(content(Whitespace\" \
                 \"))))(Tile((id \
                 15ab0441-624a-4370-8461-c15ff7c5aa2b)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6a0db09c-6038-4b14-9689-ea0e3e922284)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53b13857-d843-47ff-a268-05d4b137f48b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6ffb31b7-7893-468c-be10-c1ca3aeec81b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 aaac161f-e37b-458c-8db6-fb176d656910)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1a524a9b-dd67-429b-845d-e3eebc20f9d3)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d52eafd1-b439-4155-a06f-af8fa57c3c01)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8806bed7-7e74-4ad7-bd03-908bb6b92d3a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8d70c9ef-3c5f-4ce8-8852-c887d0df6bbd)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 588413c7-9a41-4dcd-a663-ce8a71232787)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0918c04c-58ce-4b4a-b674-ddd13bdc9088)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 f2fdd943-38a4-49be-868f-551b5e4e8a62)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ff95314a-2a59-448a-91cb-1ea41665a5c8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 46068684-be56-47ea-8364-a9470099dad6)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fb5080a6-f268-4d34-b2b3-cf14ee08ee6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de1823a7-a2f1-4a51-9b4e-8912840e67ef)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b0c9f1c9-5a83-45b2-8431-4d761a9a398a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f52d917a-eebb-4dc1-9a76-ec3ac7e334c8)(shape \
                 Convex)))(Secondary((id \
                 e1ad7bdd-d270-490f-9393-e6788840d8b3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1949e9c6-c52c-439e-bfbc-3b7e841d9d2d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 769bfcb9-ccc4-487d-bf49-c06aaf82ee64)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f9524197-e261-4db8-9fb6-7d93bb19b088)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db72b634-32b9-43e7-98c2-6184a6c83281)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 547ecae3-5664-4e03-9bff-75a7af435cca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8a65c89-9c1e-4ad4-af3f-4a337006262b)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 22e73070-01e2-446b-94b8-41464ff24175)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d5481b05-3f3f-4592-b1cc-7ca6727dc526)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6bdc3740-20b1-4041-b82d-db05a12f27fa)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a41d0624-2d7a-4ce4-9b2c-00f5ab93b6a3)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c54283cd-ab5a-4296-a20c-668605ffd54c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38ad3d7c-9721-4a26-a60b-a480602c2ba0)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 26a95aa0-4916-41c0-9f6e-bdd8b5dd2517)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c587fda8-04cf-48be-be91-f59793b2a1b3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5e438c36-f9b9-4a1a-bd1f-a951182bd756)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2ad47140-867e-4613-907f-b7238c41b8ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1305879a-485e-408b-ae05-1ab69c10e83c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 223dc1e1-8647-4ce7-8e0e-87e75c67b413)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2669416-0e2b-46be-b416-3df1ad01ae8a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f3d88687-5683-46f0-bb0e-e675637a38ab)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 00f23ebe-6645-4707-a8d3-a068bafc36cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6a4c26e1-181d-4001-8334-582e4251021a)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f5254ea2-ae5a-49da-965c-354479138abb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e14346e0-4390-48d6-8257-df82e4c47054)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 395462c3-38fa-43c9-8436-8ea9f4b3ef97)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8530fc8b-b0d6-4dae-9ec4-c4734ef4f3ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4f9b65e-c96c-4453-9ee5-c4c7f4d56ac5)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 7c160907-2dd9-4ae2-8995-8aa6880e79bf)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eab922a5-0028-4211-a2c4-4abeae1b9db1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 12674297-fbe5-4c49-9e96-2abf3ec9640e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b2dc47f3-8ae7-4d5c-ba85-f3e34b255a2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 806579a2-5f5c-4db4-bb44-c9b1e47cdbdd)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7ad3b972-787b-4d59-beee-cc1e3c4f9488)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 2841681d-6b54-4be4-95ec-4616f4efcfa3)(shape \
                 Convex)))(Secondary((id \
                 04ead003-9194-4751-8ae8-b1cf86b01ae5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9a86e8e2-094c-449d-81a8-63e0b970d42f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 969a0f33-e384-4ac0-aeb0-5feaf54ba650)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a7e7f5cc-cdf7-4fc9-8f2c-f0e7d777928c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 302b82b5-b847-4354-a263-83cafbaade46)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 54fde547-0723-484e-a2c0-cfca6293af04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 080384ee-1937-4627-b9c3-1d44a1039bad)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 fe621d18-a199-4044-99a8-9e5af1cd5c5f)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bddd4394-5867-4787-ad0f-6bc42af709f1)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e9e027f3-f9b2-4dbb-8013-00e9c9e7f64f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8879a754-5312-42b7-8a5b-611b7606e66e)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 034fedcf-7741-4990-b024-1b18224cab51)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9462a388-8f09-4d9e-8d8d-5320871f0948)(content(Whitespace\" \
                 \"))))(Tile((id \
                 120a403b-e816-4400-8da5-17a770d9b144)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 b46908ca-f7a4-44f9-90e0-bed59a3f4438)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6f9cf9da-85a7-4b98-b51a-bbcdbfaf3472)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 49e768ef-d4fe-4182-bf9c-32fb501c9006)(shape \
                 Convex)))(Secondary((id \
                 c3b1e2a1-420c-4b3d-89da-c590b7d57cba)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fa5c7b61-fe69-4f7d-a85f-408d2c19f2d7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a712a55d-a172-4460-a2e3-de391f93009a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3c1ccce6-3740-41a7-9714-61494d59a8f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8c08f5b8-a312-4d43-94b3-e8f41d64ea56)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6006a74c-f6b1-4c70-8106-51763d2b8a88)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2e37fa9-dbfe-4597-9805-ab213a208b41)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3756b5a0-2564-41b4-aa59-612c18b37f73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e46d27fa-0dae-4f5b-a5af-9a39e5fc86cb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e228e125-3501-4076-b6e4-ba2680d12417)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 22ae36c3-b80f-4728-bd82-928629215942)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71a4157f-8ce9-469a-bab7-65f5c72f6b01)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 64af3b09-a5a3-4800-bbe8-eca9cb2fda11)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8f1813e0-521d-46b1-9881-4d4b34f153ed)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 92484a79-9c21-4583-b55d-bca83fb35382)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ec4c1a18-6700-4f65-81b1-1746daf0050b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7effe18c-56ea-40b4-8fb4-a18108acbeb6)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 849cbcf1-5025-45ce-bccf-acf86d3d0c8f)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 445a7d03-d6eb-4fcc-a222-038aed01e048)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5d926892-1a99-4f8b-ae7d-2ba3a482fc9d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 78227439-828d-48e7-aa75-2576047cce62)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73d41a68-3175-4104-9c11-356b5278aa3c)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6bf2d999-641f-4ea7-9688-d338adc5d4b5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2167d358-4a0f-4a69-b711-d2d00fd6b460)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bf4be0e0-b6b7-40a2-9936-5b30c72c55af)(content(Whitespace\" \
                 \"))))(Grout((id 4abf0c94-cc8d-4cf6-9a4f-596b77886b83)(shape \
                 Convex)))(Tile((id \
                 080e0fba-f3b4-406a-bc52-1fc9790f8247)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fcd27852-8867-4366-82ab-8a86f14df4d7)(shape \
                 Convex)))(Secondary((id \
                 71c84ce2-707c-4eef-9331-2e42187d70d0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 649ff12d-0f5c-4c94-a545-c0d935d5cfff)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f3019b50-c74f-4a23-8e00-9ac6b6b1de53)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3d97d579-5011-4e31-8694-1e5e76b459db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b7e5e00-3489-460a-aab3-c9a458041674)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 49ac90da-d790-4480-bd4c-33bc5d4fa016)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6c67aab-bb93-4805-ae55-37c283d8ad7b)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fdd53596-ea98-4510-b997-f2d53ce4561e)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 af47a325-997d-4f97-a611-c82df473381f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb2b1f51-3298-4121-8cbb-134507d84091)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 541cef2f-c9df-496a-a353-876f96bce8b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69bd9432-f733-4df0-af4e-07e6ea9825be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25fe1452-68e8-4b6c-ad91-87bef4266c69)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f6cef768-dc51-4ae4-b5f5-69f1026a3731)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a31e74e4-30f0-4328-85d1-686cd2954c91)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b3f5a747-7a61-4393-9da4-953a89ea697b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 130d4823-80b7-4e70-b9c1-53c8464cc96e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3a0a21b0-0441-4afd-9bd9-d2883ea6985b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 92d3dc42-f5f0-4df8-98e0-5473fab1b249)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79b45272-de5b-4471-b893-b71f6a38e306)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9e3a1f40-64cc-4ce2-ac49-3679b5fe35e2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ae29e4d4-7dd9-4765-b2c5-b286db3e9ca1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 501efae8-0d02-403a-b43d-4b96c6cd5cdc)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 45db7ca5-2d7d-478d-b678-71059ee0bf82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0be5df8-cfa0-4dd2-9833-b0154c70dfc8)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 ea02bd03-e77a-46ce-9402-3d18a13500a1)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 61fc7047-e4b5-41ab-9349-51804a5dad2b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d625be96-fb0f-4ec1-965b-b36a310356c5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3346918c-5de4-4380-b7bf-983770dbdf55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86a0461f-0286-495d-a979-f9a153bf440b)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dd7fe18a-ecee-471f-a879-b26e3d233dc2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 015a5ce2-d221-4141-9119-a9dd23ce66b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3748be85-000f-4b4e-ae7b-eae258f8cec8)(content(Whitespace\" \
                 \"))))(Grout((id 84a4dbd8-f6a2-40ee-934a-0c4b2fa73545)(shape \
                 Convex)))(Tile((id \
                 40f12435-4cb3-4ada-881d-eb1bd59e1677)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f09063ab-291a-4d0c-9357-5bf017bd4ad1)(shape \
                 Convex)))(Secondary((id \
                 697f6c93-47fc-4e43-a210-699c5a91ae1e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a8cff37f-d20d-47c2-8c42-6dcbbb58ebec)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 19748f4f-bff1-46b2-9a5a-3a398b5e8e04)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9f778a0c-0fce-4deb-935f-7c8a5f4ed963)(content(Whitespace\" \
                 \"))))(Tile((id \
                 239d2509-e0db-4696-950d-a17acd4746e4)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2b87fc41-faf5-4f43-a5ae-3a953f6bb3c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 170ed6f3-180d-422f-9ec3-1fe79a017bac)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c403741c-61e1-4a7d-bf8f-aab2714035fb)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a456d165-1014-4ece-8ff3-65d980754c5d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a5dd89a3-6bfe-473d-be59-2d8510084b87)(content(Whitespace\" \
                 \"))))(Grout((id 9a5a57ba-7fd1-419e-88c9-9d678c421528)(shape \
                 Convex)))(Tile((id \
                 4b72b557-625f-4bd0-a3e6-1d893a913fe2)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 ea5dad31-fa71-4ed0-bb9b-3729cc6c60e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f75b5d2-dac8-4e96-b1fe-fb20661e815d)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 293b37af-4fe1-4822-9640-41696a0d5dcb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1c542d7d-962f-4558-a39d-a666ad67bf44)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fe71147b-6655-46ab-b0a7-845eb47fa595)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9261fb87-4189-4b5f-a4e6-1e21b0b4729a)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8eb5744b-358f-41d3-a484-9d1be69a6fe7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0b93445-c49e-4af2-bae9-7260f5f0494c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d403496b-ccbb-48b1-8a6a-79994e331142)(content(Whitespace\" \
                 \"))))(Tile((id \
                 628d219b-3844-497d-acf3-d6906e71ad5e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a3a0da08-2979-4421-9acc-fc8c9f1bf9bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 906625c3-05ce-4adb-b987-bc3931798668)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d5f3fc3-72f2-4e4c-b3d8-8e34e966ee17)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 62c113a4-2abd-47ee-9cb1-49dc00f1ba0d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c1ac6161-d471-4523-9b42-1a39ed0a18bc)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 af1131ee-b56f-4503-978e-1c5fa6af9e9c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5ac33347-e517-4b9f-84c0-ba3a3201b496)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51ee0c63-a223-4cf4-9335-585535f92f12)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 0081d95b-b438-4f2e-adeb-b7b797627447)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e14f57ab-f267-45c0-85b3-b07e5d856269)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 295028a2-3ce4-49c3-91f4-8421256d5aac)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 193e459a-6a83-4d20-a445-d0ca3e0aa711)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1eabaf8-2eb6-4f07-a16b-7e0321ab34f6)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dd895c62-5adb-4ce0-88ed-7db656306bbf)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4690664a-f922-4620-99c0-a13c45a973ae)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 123a2b5b-b75f-4e75-a50f-4e049db0a787)(content(Whitespace\" \
                 \"))))(Grout((id a2e478ba-36a7-41d9-9a39-7831ceb93168)(shape \
                 Convex)))(Tile((id \
                 c4842cac-e5e1-4b12-8190-ae237261948a)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 11b87ab0-b173-4c7f-904c-3f0f6cd38a6c)(shape \
                 Convex)))(Secondary((id \
                 a5e5e515-0398-4227-bae3-7e1d3ab4088d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 78675615-c06b-493c-bdc7-31c9e1a71f64)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0f12b2f5-a0a3-42eb-9841-65fcd4b17622)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ef873d46-5e64-4393-b815-f85d09aaf097)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adeaa9a8-f7c0-491f-a86f-6f4e573119fa)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 480cbf26-7bf1-4dde-a11d-cd9a3c784307)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7685fb4c-98c5-4cc2-a8af-0333ec41b51f)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7d960d16-91c6-4d23-8e9a-ed26f4be9801)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4f83eecc-b187-45f8-9784-877665a45a90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0eaf2fb-0ce0-4a2a-a7e7-5129fb36a9ae)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 13616c24-d14e-4503-9f2d-24311f7142c5)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 08c5bfc2-9989-4ba5-b33d-83c0ad686d8e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f34dc9f4-43f7-4b4d-b87a-2280cb6e68c8)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 bdac8a5e-eb25-4418-b076-6d20660bf23a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a65a5fb4-f650-4b44-b6e7-65d69db483e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36735ffe-8a40-429d-aef1-f28220fc9548)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 62fae7b1-a85f-406f-b42a-c9d23e11f27a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 942bad54-7d36-40c8-b83b-89559a6ce004)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e7baa58c-aae7-4e4b-9b8a-912d761cb415)(content(Whitespace\" \
                 \"))))(Tile((id \
                 05f965cd-de14-4e61-9afe-eafa91fb6526)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 73625cb6-76b2-40ea-ad68-cd5b1ddf9c5c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 34985849-d438-4551-9962-d8076805220c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba2667af-5b5f-4cdf-bf28-b2ec7d910143)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 74d59911-c7fb-445c-b808-f8f6ca292a0f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ae7b522f-710f-4ee0-89ba-4d7656b7a569)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d8ceb537-4e43-498f-ad8a-aa8018986ddd)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 075edc4a-286d-450b-8e34-ad506c9a8e24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a6a5be6d-b11e-4492-9297-b076924e611c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1b91d7d4-689d-4758-9ab4-849c73a95988)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5801334c-7b19-43ec-9a23-a57cd91dfef5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4784be6b-aac8-4074-8fb5-c2d56d922642)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 01ac6af9-3fd8-457a-8309-02eedc9357fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e81fa8bd-0208-4d16-b5c2-070574775b37)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 16792ef2-b245-4c35-af9d-b850563dd853)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dea95d53-1e5a-4435-9ee9-8b5ea379d672)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1087164e-19c8-498a-9049-80f5b349f791)(content(Whitespace\" \
                 \"))))(Grout((id 20e37104-d952-4bbc-b6be-4c2f6e5ed58f)(shape \
                 Convex)))(Tile((id \
                 f32cf633-09b0-4ea7-8a54-0137c0af4835)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 e03c3ca9-0c90-494b-9f41-23bce1b882b5)(shape \
                 Convex)))(Secondary((id \
                 216f6702-fc94-4f2d-b957-95df48f24ffa)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a68f2ba2-b02f-4e2d-9ff3-59db489401c5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0824765b-7b74-4ff7-bbd9-60471f543c4f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3f96ff97-8384-4e35-b711-43f834e66941)(content(Whitespace\" \
                 \"))))(Tile((id \
                 677b6530-a329-41cc-8731-9c2b8695cf3f)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 65182407-4ede-4942-8b97-6d5c3a15dade)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71de9629-e701-4c79-99a4-8ba69db8431b)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4c07c419-2a10-4668-96fc-aa784e388c59)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e9af1d9a-0b31-4c71-80c3-6013787e024b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 904078a4-049c-4047-b96c-e66b938e76b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63fdef68-cc87-4060-a166-fa2108be6d90)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 d46777a3-c613-450a-a8d2-0381e4387950)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6f206cdb-a76a-46a1-af12-e859c5584980)(content(Whitespace\" \
                 \"))))(Tile((id \
                 087add82-77eb-44bf-800b-a0c4d350fd3b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 638a72da-e2f0-40f2-b9f9-b3e0177663e5)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a02e4530-703e-4392-8b25-a63a6116c984)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 bf5dc96f-2fba-4b50-a441-2b201a061500)(shape \
                 Convex)))(Secondary((id \
                 653c65da-53be-45cb-9da4-dd484fd7a703)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 eea31b95-8506-481f-92a6-cba41a19247d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bc23a34e-bb24-4dcc-9fd5-16531dcd2f43)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 77374c90-5488-4be8-a13d-5ada491fd096)(content(Whitespace\" \
                 \"))))(Tile((id \
                 171572bb-446f-4e67-ab14-9a757706f5f2)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3705eb7f-991a-402c-a12c-333b9f8e83df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec6e4dbc-4b5a-4547-9792-342b79568cd0)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 72eec4ad-1779-468b-98d9-11e9dffbddc2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51ba0778-4c11-46f3-b6d8-c72feb736cfe)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cb88c4ba-1178-46e8-9221-9fd4f4f4dc45)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e8f4ded3-3731-4656-951a-ec3e8d4bc07e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 557d9194-e948-4d44-a147-26245b0841f4)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4ab0085b-ae43-4a0b-a5bb-82fcc89d1c16)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 44811b93-d85d-4822-9c16-3fd0a0557bff)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 33d8f49e-8b26-4bbb-84fe-64b520a6c7d1)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8a8ce6d2-f2a8-4e34-8c20-42bf2ffa68c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7a4c3917-381e-4fa2-b933-65b610b9faf7)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 578f4bbf-4d56-4507-81c5-0c21160d6d73)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5a31cbd0-f9a4-4038-a7de-cffb1628f225)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e91ab766-2e5e-47df-9880-416186f304e5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6448c51f-80d6-4afe-a297-c75b7e468a99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a46da087-3433-4654-88e3-4a7f6d422821)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 072d6af1-94a4-4032-a8f7-3def69d0a07c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3038b770-0df1-4212-b762-95cbde5f327e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6fb025a5-c0c1-4613-a067-3866714cbf40)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 abb26a18-8364-4e16-90df-6547f7cffe5d)(shape \
                 Convex)))(Tile((id \
                 914d5f5d-efc8-4fe5-9986-2388663820a1)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 061bb903-58e8-456f-938f-739acda10415)(shape \
                 Convex)))(Secondary((id \
                 d1fac0d0-8273-4b40-bd3c-80540dbb49bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 10e3fb0e-9faf-4a89-b2e8-ee95afebf1ba)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 af376a6f-dfde-49d7-af95-47925e3f6206)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f68afc5-fee7-49d2-a36c-3a47f8b3ac2f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9613b237-74cc-45ea-b5c8-2b17bdd9618f)(shape \
                 Convex)))(Secondary((id \
                 0b31ee1d-0ad9-474b-bddf-00154d216769)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b410dbb1-fabd-41ca-b784-b99cdcfc5eba)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8fd5f496-31bd-4e3f-b51f-0b6a51ca2dcd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3282d800-60dd-4de9-a28e-bf7e684d9b8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7f2229a-768a-4ad8-b720-4dc50f88daca)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5b0edbcc-38ea-4108-9bdc-4c6b8394ac8b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c9f5baf-4414-4d97-8160-774712d7f699)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8a3ef432-cd29-4937-b9bd-312829df51e5)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e83f627b-0105-41ab-aa9b-980231731d2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9751b2c6-b35d-43cb-b986-c3c138ffc44d)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e8b29464-2ce0-47c4-97d1-045478b68af4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c4caee35-a182-46b0-8c31-ab1f14865df3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f89c6f23-2126-47bc-bfbf-03a141f18881)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c8b42539-e3db-4527-92ca-a28898cc31a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 793e3c7f-1a50-4f0e-9e3d-a954b62ebb3b)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 506ee6ac-f965-4695-86fa-4b6f65c40880)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf41bcdc-fdb1-4fba-9c9d-d9a095157908)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a2fcc73f-c20e-46ba-a083-d59bf02a54be)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 01c8a13d-464f-493d-b285-b6884c9cfe6e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 27fa1b9e-1c83-4366-813b-59aaf302a5ef)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 de6c5fd3-529b-4805-83d9-5b123615c295)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 32184568-a8fb-4504-ad5f-4e79abb70bc1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7625ed65-dc23-481d-8cf4-7bec9ce2d61b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2d64477d-0af4-4e4b-9fc5-6759029362a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eac776ea-6d34-4d5f-8b0a-a5bc742778da)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 fb93ade1-a5bb-4e65-9f2a-f7ce945af7ae)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b48b8eae-4690-4248-a8a9-e00392cc76a3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 249e737b-7462-4cda-987f-dbd1c040a305)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ab2e61f8-918a-4a8e-804b-6b8258d068ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 239b9b3a-655b-40e0-ba7b-21e7731d12a8)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bcb33fbd-3493-4e9f-8a95-362ec76b082a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 855d053d-78ea-4ebc-b206-5d5400dfee1c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a9ac697-149d-4cbf-b476-aab022ee05e1)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 8f5dd867-a41e-4363-926f-c989367cd3bf)(shape \
                 Convex)))(Tile((id \
                 a6c7aebb-51f2-4b36-ac15-be1668105a95)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 e4d0ad35-d7d6-4bf1-8ee3-f5e39a905fe8)(shape \
                 Convex)))(Secondary((id \
                 ab7b2100-5e43-4d55-b7bb-f742bb65f8ab)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b393f5db-c3c4-4e85-86ff-0fb0662bc978)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f9757c3e-4bc4-449e-822c-9035e2e36eb8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74f56b23-6fb8-4e03-96b2-06ea9e7040ed)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6d983f70-23d1-463d-adc0-a3709a640952)(shape \
                 Convex)))(Secondary((id \
                 196b58a4-63de-4642-85bb-175c2ea9fd1a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1329bb91-6f9f-4c47-9109-da58bb468bee)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ffada2ab-fc36-4533-86ab-de10f916869f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f40d6acc-e229-4f0e-9184-83d9349dcf7b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c338a43-8850-43e6-afc9-be37ae180fdc)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 534da083-b8ce-459b-b75f-88af8447cd42)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ba8154a-1771-4256-b396-678df94f8799)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b1250a41-9d06-4016-936a-951599368c67)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 776456c4-5444-4a55-841e-3c9ad0e0620b)(content(Whitespace\" \
                 \"))))(Grout((id 0ed1f5e7-1207-4b48-bde6-af2b757e9f71)(shape \
                 Convex)))(Tile((id \
                 649ce7e2-8418-495d-a672-b758cdece86d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 fff13e2a-4465-4e20-b5ba-25100a3b1c34)(content(Whitespace\" \
                 \"))))(Tile((id \
                 820aa7a6-d0ef-4680-845e-05ae7b496bd1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 43a7d8c9-79c2-4a16-8504-4ce744835909)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 072b6626-4224-438a-8153-083b3b5cf909)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d4ca940-d58b-4a51-b2a4-6c46f63bb321)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 61afc556-4ad7-44f8-aa2f-a9ccf2777603)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aaad6f7a-5f9c-4d97-93f3-60736e0e1b34)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9d2e0609-d39c-42d6-9115-6f55f381ae68)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10dbe8de-b23d-4d56-a5bd-195980d3e27e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2681e2bf-4a97-4b26-adba-f80d88129390)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bfb0bc27-e706-49ed-bed3-343d8fe52c92)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c2b5934-01d6-4447-8b62-e1243765a918)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c312395a-d53e-4841-973c-2f70d81bd841)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 31a6bc96-cfde-4df5-b3fb-b35b53bb45f3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d773d0fe-76f2-4e64-b241-48a9802190c1)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bf8dd4e7-b0b1-4926-8d19-b6cdea0b4c4a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52c87b39-533a-41a5-b2d3-ecb7b7e09b9d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 77fc0afc-64b0-4b62-9923-01c233655640)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3d4f21cb-318b-44eb-8f72-efddcfe42c3f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c497a148-10d9-400f-803c-e5548b970239)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4e5f892c-5624-4ba0-877c-ccf641e44ada)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87f24316-5f4e-414a-a979-eae897c60f04)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 020719c3-8271-4308-924e-17d12fc562dd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 901e7b31-f793-4763-af03-e436ea2169ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 856c69d4-5ede-4311-a698-b6f151ed6b92)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 aa044aed-35a7-41fd-9251-d9e3b1964b62)(shape \
                 Convex)))(Tile((id \
                 f34cf6f7-bbfc-48de-b5bb-a95a084b32e2)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 35351958-a081-4ba3-8102-519ceaa460dd)(shape \
                 Convex)))(Secondary((id \
                 fbb9e319-e64d-482b-bd3b-daef1c8e800a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b6383ff0-91b5-40e4-a7e2-c6b5c46e4890)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a0e9f0c3-86c5-401d-b475-59d17c414966)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72657097-7b58-4dff-bc2c-83d73f538d45)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 9bfde24d-4ef0-4dc2-ab96-ac55fb65de8f)(shape \
                 Convex)))(Secondary((id \
                 aa2494e4-13cd-4758-8a46-2597daf09c41)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bfe5e1c9-9c69-43ca-ba16-ae36fe57904b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 04cbb703-fe6a-431d-a7c2-326e4e6fe519)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9fda2313-41ed-4929-b39d-d2e819057b01)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93bc7e43-afa4-4f4c-9862-e580158dc828)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0ac577f5-b8b5-40ef-9d08-173792993634)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae0d121d-1143-436a-9741-a763b0dfdf00)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6aaea59d-b0a5-4e34-996c-5a21aa20bc76)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 33f126bd-42a9-4371-b630-847dc42755b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 493f02cf-9ceb-4e21-afab-d3b601c47b18)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6b92ff6d-971c-4992-9f64-bc8c4451b443)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 cc01eba7-de02-448e-ada4-74122754b6cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d10179e-353f-4668-829d-d7f9efaa9009)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 39136aae-8579-4a60-b784-83b9564d6ad5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8fdf301d-12bc-4cda-8a96-b655327d22e3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 06316225-a915-46a7-8a46-2c2a41ab5c6e)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b1f396df-c42b-426a-af63-e32ad23c66b6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 393997ef-d898-41b1-8ea5-77ccb7688570)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3ea85289-f845-48d3-9215-1e6c6deaea89)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c8ccff99-0b00-41a4-a306-5035cddb3693)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ce407ed0-d44c-4e84-ad83-8934db80e6a4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 90af6c58-0b3d-4556-b921-12f1eb895618)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3668e811-b918-4386-b78d-2dd800c9c0b2)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 680d3017-66a5-49d4-9337-35bfd56d9c42)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 21cdc7d6-400b-422f-b6d8-87863509eda8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4db36db2-d937-469e-8501-5c4a6627a282)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d5fb6cf3-2516-43dd-881a-a13c85df28b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76602b5a-9369-4d20-90b5-1eebb367c4bb)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c243a46a-10bd-4f86-a80b-3b6f94db9b89)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 90b1ad9e-a0ab-4b09-b59d-50124a8a9b0e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 def5d21f-6b1e-458e-a61c-3839eaa2df48)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 63279d89-32a6-4c9f-a91c-b5707e335bd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ad6b968-b933-424b-b840-581a0165cc98)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9bc7a73f-aa25-496c-b777-e0b11ab6159d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 13760f56-38e7-4641-8187-d77e1462bf17)(content(Whitespace\" \
                 \"))))(Tile((id \
                 da0c998e-d726-4174-ba68-20936cf2f251)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 3669c5a2-0c11-469a-b91a-93575fa3afba)(shape \
                 Convex)))(Tile((id \
                 5a694cf5-1c13-49af-8fd5-876c64002b6c)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 24a8484f-e471-4bdd-93a5-4aaf6f44e9d2)(shape \
                 Convex)))(Secondary((id \
                 f04be29f-40d3-4bf5-b424-99c06bf3095d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e18df519-621d-428b-955a-28f5648c3d42)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fa73b247-4f0b-4fd8-9690-51f0249d4144)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de246edd-2272-4f7c-af68-19bc4414c199)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 22631d33-fb4c-4978-816c-e2af9acfef79)(shape \
                 Convex)))(Secondary((id \
                 cfded247-2e57-4a79-b472-95d5a8600d15)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3cfbb36c-12b8-4430-a2a4-b133cfc3974f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bfaab898-1775-450f-882a-3dd08f985489)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c7e883b3-42e3-46b1-9dfc-97656d7be54a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b3140d37-9cea-4dbf-93ba-6134296486ce)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7c4a3fc1-3a07-4ccd-9faa-908668202e26)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d0414eb-36be-4862-a175-e4d6a7cec02f)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 369c17c8-794d-4aab-8bc6-8a7c7ba854b5)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ab148e68-bafa-4082-b20f-b4163ff35892)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c07e8d76-a161-4583-b953-ca7d9bf2a0c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7a7bfb79-5e3c-4591-88f6-22e5df3e33ef)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 acb3da75-9cd4-4baf-b2d9-45c8ec3b39e1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bcb74525-4b49-4270-aac7-7aa74cf41f67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 600185b8-e4f6-4983-b503-5c67dbfb394f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 825efdd7-2e52-4613-8ea0-23f1f08a5c8f)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 416a3ca4-5560-488a-a774-be822a2a7e32)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 15708ed5-f452-4958-a896-ea9fae8491b0)(shape \
                 Convex)))(Secondary((id \
                 37fb9d0d-9315-4c67-987c-236d46a1b702)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9e538009-4c7e-4c7f-b695-3bdc6befaffd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e0631ef1-e8ca-444d-baf8-463265173097)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eba08687-8d35-4697-af6b-2812d6461973)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7c5b8c16-96d3-46da-b8ea-afceae9437d5)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b91ff2ee-f8ee-4c61-b0f5-9115f6c0f230)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a006a807-6a04-4f10-87ec-54487587aadd)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ba3ef35c-d7ea-4e3d-a353-5715c665edbb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 08f3255f-fb50-4034-b5b3-9de9e8a1375b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8628da55-6d68-428d-8067-26879297a915)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 abf3eb4d-ca65-45a5-9113-fefba16ac490)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97122918-d74c-4094-b55f-07f8f9d3fb5b)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3007137a-f275-4b2d-aeba-fdc5c734e34d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 da501029-8b89-483d-b62a-1035209f21ae)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fb3441c0-a2d6-45b3-ba4a-fad65d2cffcf)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6974d029-9697-4cb9-aee8-8a2fc801adad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f02e6fe-3354-42ce-af69-36f1c2411215)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 915fbcf2-5371-450c-9b2e-67e891528a3a)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d9be22e1-cbbe-4821-bd9f-26608d605971)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf560209-22f4-4ee0-bd63-569256e742e7)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 80ac1a50-c17b-4403-a0a6-bcb3b464e877)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b36b16ef-cf98-44f2-a266-72ad615a819d)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 54b60b1b-fb9b-4159-9699-b283f3c8b73d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ad030d34-b50d-4247-8c95-ee49be92279c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd41a1f4-e654-41f1-a9aa-44c2caf97687)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1602a4a2-e4cb-4f9c-91cf-99fdfed14c92)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0d544688-8378-4763-973d-0142b040ef3c)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d9d067c7-ae3e-46cf-9520-f92e45e3bd90)(shape \
                 Convex)))(Secondary((id \
                 607a5271-42e8-4f70-8d7d-7265f5a37e11)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 425cbee2-8599-4d9b-9bb8-7d4b74e07400)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 31e910a3-4e72-4adf-b897-e52c3b081c11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b7a4ac2c-7706-4377-8143-58a9c0dddc05)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 13c642f2-d87b-455a-9803-fa66c8f68c70)(shape \
                 Convex)))(Secondary((id \
                 620216db-24ef-41a9-93e5-7f4c86674e2d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2e0af26d-3f9a-492d-9511-120e00c895f9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 af907e4b-a3a9-487e-bb69-6c6bcfae8c26)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a07a4037-3c49-444e-9406-ac293aa962ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f01f6941-3084-4711-9593-85eaadad4101)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e4e8a34f-56f5-4bd6-a10a-3e6b46fa24e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32161cc2-6f99-4954-bdd7-34cf8f0ca2d2)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9274b0d6-af2f-4ae2-8cdf-fe3678efa264)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 b7ca123b-74f0-4d2e-9c29-8284e2afe6b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0575a076-e79c-411e-83ef-a100b0917804)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 37f0e61d-c395-44f7-ab59-bdb9b162b1f0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f1640ff3-9870-48f8-800e-ca5f4ec1ad9e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38d71e64-e6cd-4040-a7f9-4342a7632a7d)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d2ceb287-f66b-4499-9b64-445ef3f32c07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 04053ae5-9986-43e7-8081-c00a8f699105)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e16a258f-cac7-4e38-b790-a7e57addc69a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 788d35a7-8e52-4b3d-ae51-82e464d5815b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 219ca7c2-32c3-46c4-88c8-c07a44f70546)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 69ed07ee-5be8-4064-84fe-b37022365ea7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a17a73c2-0f68-49f7-bbb7-550085684c5f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b1ae580a-12f9-4106-a59c-86c97ac196ef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d1409192-3c6e-4276-820c-5ac263859192)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ee65c7d4-9367-480f-bc8a-42714e9e771a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9f0c9bf1-ec71-460b-92fe-7d6f8ae80037)(content(Whitespace\" \
                 \"))))(Tile((id \
                 391c7891-6ee0-413e-b61d-ff4b533485a1)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 bf8fa20c-d917-47ce-916d-b4ad8a624d01)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 26afcff3-cf21-480b-b10f-11b60d0f7314)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 859b9124-2bd1-4ef0-a9b0-440c5346fc51)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a4fa0510-6ae2-48e6-8b02-0443d33bddce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c29e4914-8d49-4e69-be7a-c07f72f9de4f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c015d488-3679-442b-a2f0-fb95ec525321)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 15c2b6b0-297e-4582-97d3-369dc2a80d7e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2bc80c4d-768e-433b-926e-c1e6f16c2e40)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bfd24439-663d-4705-8273-e29b751604f3)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 09a3fdf8-a96b-48a3-9ed6-eda73c65a277)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 dff36c0a-c494-4773-85d3-22dc40828194)(shape \
                 Convex)))(Secondary((id \
                 82a71844-313e-46dc-ab39-2168277862cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8d216058-fbf1-496d-9743-0c5e0cd76feb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 14415fb7-a422-4548-819e-44934a15fbaf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2bf97cb3-fa08-4a66-af35-19f80ebbf18c)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 361a98e7-250c-4a8a-894d-3c142b544a3c)(shape \
                 Convex)))(Secondary((id \
                 afdf67f8-ca10-4975-a82b-cee627045d6c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1d7ea360-85de-41a0-8d28-2560fce817d2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cc3932d6-ddb5-4b38-bed5-0f804b30c249)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0646a454-d94c-4a32-86e5-e945b7ba3281)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1376e637-b0fe-4c0a-ab2e-2e3668294b2a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 00d6aabb-0632-418a-991d-3244cbac05f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc3da52c-3df7-4024-bbed-ccfbc627a7b0)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2a8f19a0-cfc7-40af-b6b9-3348b68b8339)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9f76cd87-b09e-4a77-ace3-b7b950b35b5a)(content(Whitespace\" \
                 \"))))(Grout((id ba54939c-0b30-45f6-b043-9b78bfc61ccc)(shape \
                 Convex)))(Tile((id \
                 a3d1bf28-0582-4434-8016-7e16d9c49168)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5224a37d-a8cb-434c-8296-c6848f946521)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a059766-ec30-4672-8936-beb7b9222bd6)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c1602bc9-19c5-4170-82e8-577d85ed7754)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8d33d61f-552b-4d9f-8ee8-be0441e23a97)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 890ff4a2-a745-448b-8115-b2934cc0b80e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf14169f-746b-4146-8c4e-e59d02155bbd)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0df42a3a-eda0-4448-89eb-767d22428d00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33cd3314-9af9-4322-97aa-077b7aee80b8)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 36b0afb8-4162-465a-ba53-dadc8ea48eec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7acfd26d-e427-4492-8b93-486f2ef7056e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f7e38c4e-9f89-4a5d-9d29-8fc5c23f2cad)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fb9d57c8-fe4b-4cc1-acbc-0d1a9892bab8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef1b1b01-cf68-4b48-b7b9-46acb5e163b2)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6e190c88-1aee-4ec5-8e4e-8a19928c9bd1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 78705e87-a0b1-4251-8446-d6b43ef044ce)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 690f7619-8921-4e38-b639-16560567cb1e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eca2e22d-e738-4f42-a208-0ee61aa75be9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0acc0b7b-5590-4de1-8ebf-ce7ce50f99c0)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 5e7aa6a5-67db-452b-bc45-bc6613713567)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c4a2663f-7fea-4727-8ac4-10ef3933c70b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f85f8f98-196c-4aea-a519-ea1002fea7d2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7a1d296d-ddf2-4800-b920-f4116eb01eb2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9897f8ff-41ce-4364-a5d7-b366ec0bb8bc)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ce2d263e-6278-465a-8f81-e831b766b1e5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 967f4497-6a86-4dd7-a089-2f18712a0d67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 416de460-73cd-4105-9a1d-676283260d75)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 38e4b386-735b-48c4-8a0b-ff4cfdd52bcb)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 51976577-6ffd-4315-bc7b-cd2d54f8a710)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fa144b5b-e12d-4105-af0b-621f0d0ab9ae)(shape \
                 Convex)))(Secondary((id \
                 37d11023-6ee0-42b2-aa34-e0c2821a1f46)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e3009bd1-94cc-4f72-9d97-3a936ddd965b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 78c11afd-7779-41ba-847b-bf00b2cd0e23)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ab478a8-af38-4bdd-91bc-b042c8fc1a28)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 ac0f5375-8a7f-45fa-883f-22598fb49e4f)(shape \
                 Convex)))(Secondary((id \
                 71e88bb0-162f-4acf-b4cd-2d8dfcb98676)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 16f969cb-615e-4d0c-a454-9ab748dd889c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 30bd0a7e-1b39-403d-9d29-314fa4015d6f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 63214078-2ba6-41de-ba10-3e4c34a44258)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09178b2f-ef8f-4ad9-a4e7-832c67204695)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d5cf9139-9af8-4179-9e1c-7d101750015c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cbc7411-534c-4f04-ad2f-4c62642f92bd)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2712059c-cdcf-41d9-868f-3b47a023519c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 357f64ca-739e-452c-ac34-6e86af829339)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ef6e3ed-b83d-40d9-80c6-ce7bd066a3cf)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 714e50f1-382c-4b30-ad8f-14a29d00eed9)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0eeed07e-8ecc-4400-a0f8-648a08060a9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0ad4dfb-81fd-4ea6-9436-34e9aa10d2e0)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c11d7f64-46a7-4dbb-9ce6-d5e65854762e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4196328d-f970-4e72-89de-d3dc5a3b800f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ac8ba99-28b0-4c20-b6dc-fdda5208130c)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2701275f-0054-49cc-a806-e50b5a1d9407)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c928d16-9a42-44a8-9599-e9a2ef7cb4a9)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3992ab52-0322-4a10-8993-9bbc73b4a47d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c7dee409-5099-439b-9479-cb196bf85da9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7f472026-0d8e-44fc-bf53-183b363fab7f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 93ca28dc-c1ad-4310-8917-b968a2a3ba0b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d8799670-d6a5-49df-a740-89c175fdb36f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 14eb48a9-3e23-4e76-92bc-3c43c7fff444)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 997a2997-4ad4-4248-b9f1-c96ff5941789)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 43a93d20-b827-4f0c-a3ee-c695e5df9e95)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bcb0c02d-7357-4984-9fbc-f93897b4492b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4736d2f7-5fa5-4544-b6b0-30cbda4b5b62)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3ab4ec30-9a09-4644-8841-92e7f6cb8847)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 487472ef-1212-4ed3-88f2-63c4ab35afbe)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3d02d2c6-650e-4442-b1c7-67b32cf9b8ad)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6191f40e-7c16-452e-8c76-6f33eba1d329)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa4b8668-df61-4e1b-8cec-341034c7a3a7)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ec67395c-ef7f-4eff-83d1-feb53acd6054)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 466da28c-9cf0-4722-8957-eaf632a6cd98)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f79e5b3-e434-4b06-9b0b-3877b56986f4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 a79ba610-804c-4e2e-994a-155e1833c2c3)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 64a932ef-9cd4-45ae-af2f-390ae7392104)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 8b753d04-627b-46e9-8d0e-98d66a31dd4c)(shape \
                 Convex)))(Secondary((id \
                 600dac5a-52dd-4c54-8858-a4c872fa3317)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a800be14-da4c-403d-907c-b550055bc6c9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fa75fd51-22af-47fe-b374-4bf058b6aeab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f527de5-501f-4c14-b543-a9512e1869ae)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 053b4322-a3a3-42e2-9188-0287bd43e17b)(shape \
                 Convex)))(Secondary((id \
                 d85d2f27-47ea-4ad0-bd9b-cf6d50ae2b89)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8029ce44-a191-477b-9785-fa6eb2dc427d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d9665998-b692-44b5-8d1b-97ed7eebd24c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3cfeee66-ca95-4f7c-8969-dd92161cde90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1203feeb-b475-4294-ba3a-71af647f676f)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 25158f94-7af8-4d68-9abb-4a365a130b55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b7d5c04-3e4c-4369-8917-f6bd1d93c9dd)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 db0db77e-bbb1-4738-b207-c118cced483e)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ff1961f4-9798-4200-b14d-fa9ed8c23a3e)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 73fdb448-3691-42ae-ab3c-c35be7f073d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ab4a56a-4cc3-40cc-9609-3dfa2e644314)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 50cbfb20-0b9a-4cfd-b962-0e061a417c0f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ff8c1605-b9e3-419a-9aab-f66d51fb7478)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8e68bb74-bf64-44f9-9385-d1f66367770e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 857912e5-bd11-4c26-b394-dd2cd928636b)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7b50ab67-1a8d-4cb2-bae8-1ec785e3c8f4)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4ccb8f1c-c712-445f-96f9-7f746b28e6d8)(shape \
                 Convex)))(Secondary((id \
                 a90928e0-4dc2-4a78-af9b-e03da908d2f3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 677fa960-7bed-4e08-ba47-baed1f64c898)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 187239a0-573c-4dd4-a576-2c9b9f5663cb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eaed48a1-3bee-459b-b7d6-a039e99046a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b925b8b3-6174-407c-8d0a-2c403a01379a)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 314d702f-4312-443a-ab9e-57da31954acc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79584246-2c43-4cdd-a2a1-3ff950621fb5)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eb20decf-03e9-4149-82e6-d53e2798b851)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26e66a82-4bd0-4c87-bd53-fc3ab14145cf)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 db3c2d25-3ecd-4d1e-8a7f-0cb819336a6e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 78255577-7358-4a9f-b7dc-bcbbc258d278)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd3c7ccb-c3a8-400b-bbe3-3b97a27bf604)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b0902f59-30f9-453f-a11c-f9e0a116cc65)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c22f8cf5-6891-4ec6-bf3f-d8006cdf2c86)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c33216ef-f39a-4f55-a05e-5301d5c8d934)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 29bc4824-fa3c-4e22-a1d3-c0f603df933c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8dd7c29-5f0d-4c00-82a0-711e5bacd6f5)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 da788c99-cf8c-425b-9960-ba7e8d1b3ce4)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2e1ed404-00e5-4130-bd00-654f0cc85eba)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 41a5d82c-8d38-4453-bf1b-0daa6ee4fc98)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fc64a15a-4665-41bf-8ed8-9f586450d3e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 df06f73a-d19f-41f0-a65e-85f120aaa148)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 17834340-4fe8-46df-833b-c36aa06f86ec)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7f43728c-3ef8-4792-b7e1-d5bf79a8f422)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6983e2cd-73a9-44b8-afaa-a3889253a938)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d26b4c0c-a3f0-46dd-96ca-790b4589772a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 156f371f-f261-412b-a0be-cd01f8a543fb)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 88a30c1e-2bd2-4106-b502-ab0183a76da1)(shape \
                 Convex)))(Secondary((id \
                 655329c9-8f7d-4667-b71e-dd487c4298ad)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ab32ae5e-ca30-489f-be79-3265e7e1ed7c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 431d79db-199d-4e48-8ff3-b61ac052050d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ac15623-b03a-4c10-990b-f31481532ee8)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b3f6b8e8-5b1e-4f84-8891-d5591f5696a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 107bc734-b12c-4d40-91a3-132fcc830136)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5a9b1828-9af8-4675-a9b9-c056ba86fae3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1a0fd972-a9c4-419f-964a-8a2b633f33dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd8afcd8-74a2-4004-8b4a-7026fef058ed)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c63a1bee-7e32-4ecb-afd0-a724aef0b98c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 850fb734-647f-4343-8fbe-52636f374c6e)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c04c80f3-4064-4c68-acc1-ca5c299bbd77)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 dd4462f2-7949-4878-a142-a2283001ad6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2cb05f48-5922-48e6-9256-e013f5793cf1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 339ee181-b96d-46bb-8a43-f3ff8229c30d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2061e785-5a18-4a0c-a4de-c1492b30e278)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c811f868-2b43-468b-94c7-bf3bf1456409)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1a797da7-7c1d-441f-a6e9-5e4d028213d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b099d96b-e2de-41e8-bf2d-58c1d7f0f0e4)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 584a877b-b2ac-4742-a979-3005537901fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6077bfc-7116-4a05-ace0-ad4834261381)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f9963f7f-6ae9-4027-9ede-0b3e538aecbf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3f2145ac-3415-4d74-9576-5563fe64e77e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 59a6b6f1-80d8-45e4-9b6d-67fb47835905)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b2efdfc0-231d-41cd-8358-d88497d7c6ce)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ac6c8e5d-e4f5-4d7d-b0cb-199a2d89e8c3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 47d49893-385d-47eb-8f79-932412df8120)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1dffae48-7e03-4120-8147-e8e1c71f051d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4407083a-71c2-4a70-be60-1415bf0a1c57)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 f8cafdb9-5115-454f-8301-cadc935954e6)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 186a5f65-820e-4718-9aaf-89d16d38edb1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9bc20274-fb1c-4393-ac92-808ff1b94a79)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 13991968-82b9-4964-90e5-0359a90e47fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b8c99b5-3e5a-4934-944a-ffd97749442f)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 80a93c7e-384b-4dd1-b4bb-d9a3911a3ae5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1d9f52b1-a850-490f-a292-b47fb7a96138)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0816d375-1b2a-460a-a84c-4c5aefa58e81)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e5f9862a-51f0-4e5d-8ae3-dfb0d14e36ac)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 b758d84b-d213-4a78-9b35-18bf57006721)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 7f9a1bde-138c-48a0-a74d-66c75ed7ab5d)(shape \
                 Convex)))(Secondary((id \
                 f872865f-46a1-4c40-880e-77586055f9ab)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8b11e4a1-23b7-4cdc-8cfd-403715695b01)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8eec57c4-abb0-4311-b0cf-e092c09d06e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2b6083c1-baec-4672-8a01-f6c99b160e64)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 036ccb48-b246-4a19-9742-35c7ad061176)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76481d4c-d3b4-40be-8cea-2cadfe14df89)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e5c48255-c736-4d2d-891e-fef414217d00)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fe4cc3df-ecf8-406c-b9be-0a377d0d7813)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8502085a-73f8-4d1e-8e33-1dc2223a63fd)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1dc44d66-5a48-4ec9-924f-411b68da14c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e3962dae-fd33-47b5-bdf0-82c72409aa11)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5dd93d5d-b656-45b5-ad37-d49fe0ea23ec)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a10f02bb-a200-4a01-89a1-39e27bb8162b)(content(Whitespace\" \
                 \"))))(Grout((id 281e77a2-ba43-482c-a93e-cfe10259989d)(shape \
                 Convex)))(Tile((id \
                 23de4bdc-0af1-497e-83e0-cc5bca1b3855)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e64bc015-5917-4465-904f-8af5124b9bcd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95ce2ef8-d860-4f18-85d3-9822fc8d459a)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 21cd3096-d858-4ec9-9007-2b1ace927c03)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 005f8b6e-a906-429e-beb9-439d29d3a76f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7cb7b539-3bb6-4782-a60e-b06fe71c43ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5be6ea6d-0754-467b-9327-5f1578911f06)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8698ee48-48d4-4696-994e-17077bb11986)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c360a076-4dd9-45c7-9db2-2b3c60c0303e)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6e979b62-9763-4e7d-bdae-d0ffb58c60d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afa9df4b-9fa9-44bc-8ba3-f77e81e75310)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 79d79ed8-f8cf-4f3a-9a1b-475b14198afa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 27f19fbb-97a5-4ed4-8afc-6de00791df62)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c514af7-2182-4b0a-a4ea-19e14d9fae32)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 88dcb230-1f6b-423b-a117-d05d9d446f3f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 690e9897-85c7-4922-991e-0f942c89dc0b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 70533e9b-4c3e-4ce6-a86e-384267c022b3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8730d036-9d20-45ec-b09c-daaaec74ce03)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f0be8c9-658a-421f-baa1-8a502712cd2c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 5719931b-6b7d-4eb4-b867-e9eabb4db935)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4bdc2209-c2eb-4db7-aa63-043d96e5686e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a872ee48-4669-43c4-bec3-1186db91148b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 17434130-d1d1-413e-afa1-6998cc68025d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1bec817d-8b8a-478a-af31-abd16028b864)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c25eea0b-ab19-4c02-8578-87a0d9ab7f31)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8e40e986-1715-4e9f-8fa2-8473bf0d1468)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d634c2c3-b754-4067-b7e2-0f8207ced53c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 b04350a6-808a-49f8-9e1c-fdc388772171)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 fda74513-a1b9-432a-82b8-e3544b49cb04)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 6fe3a505-3d3a-4db2-a2a0-d6c3be91ca71)(shape \
                 Convex)))(Secondary((id \
                 4b53b9aa-7664-41ae-a400-e0dd16b62509)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 92b30817-375a-433b-915a-1c006150cf4c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9de55cbb-b6bd-4c57-ae4b-fdad96a970cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afda7489-fbc7-4d45-9a54-ff7dc065dcf0)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 da2edaf2-06f9-49b0-9c52-0692d36feb1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe5903b2-837a-4bcd-9ef5-1c2428f8a2f1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d0cfffab-e8d2-47c3-a805-8674d049622b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dc042fb4-d72a-4240-828f-dd621adbc20b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fa4a9b14-7e47-493b-b496-b6e38e830ad8)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 cd361305-ba60-4635-9bfc-33eb27e07c44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b14bc419-8dce-4fcf-b8bb-82dd99c04dd2)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8a72a69a-b947-4054-8244-0388911347f5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d44000e9-2c1d-4260-af86-dbef475879a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b3a34ab-f687-410d-b155-b117acda0886)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 07f3c1ff-5677-4ba5-bbb2-a9eea1932cd8)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2b485099-40a8-4b37-991c-c13947cfdcf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb881fc9-3ea5-4207-aa2b-e23e634e7bc1)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a415c809-2799-4852-bffa-0cf66d2afaa0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 45e22326-9dac-4eb5-8835-9da95894cb0b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b9ba7f3-6159-4020-960c-6c725c45c9c6)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ce206ee6-866e-4bc8-96d6-ad8a58194a73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1112cc54-8977-49a9-896c-f45d78bc0c01)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 53909189-4422-49c1-863d-60707bf196ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f76561a1-0b5a-4482-8716-e7bb3a4745ea)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fbe2251e-9b29-4a4f-8f50-eef65cd19f3c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4386e434-302f-4fcd-8350-8b97d76d32b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0b49131-03e8-4b32-a97f-d8903d8b8248)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0348cc19-de1e-4a2b-ac72-fde8ea77dcf7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0d196b4e-3689-4ee2-a64b-0115793d6302)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a428c920-3772-4cd4-a9c2-9bfeb9e2954a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 43ffb69a-14e6-40ed-9038-8fd652c5e9f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6a1223e4-df11-4025-9411-567efebb5f3a)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d44c175b-4647-4a1d-92b2-95084289ba26)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3896cd69-a0f5-4781-ae0f-7b90b3924dee)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0c77a664-2b94-4237-83c2-e3eea15490e0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7c706dfb-7bc0-4154-a062-491cdd811036)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f63ce723-9ddd-41ef-986b-97f097c7bd89)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 74336659-23c4-483a-9d57-8661f8289701)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6b4a8b16-3250-4f2a-a66f-2c63da6f26ed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c0e2f73-2e73-48d1-8b68-6c7b20076c9c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f9dacf8a-5014-47ce-ad50-8c8deddfc5e4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9bed43c0-a447-4aa8-b21f-50da595d633b)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 613b9ada-fe18-4369-b5d9-70d3f1e53514)(shape \
                 Convex)))(Secondary((id \
                 c1b1ee36-9ce6-47da-95df-3bd2fabb54c7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3f8dd0d4-b5c3-4a00-be38-632c75924975)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 210586ad-11cb-4fdc-8598-b61597743070)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2411959f-40b1-45a2-9a26-78b2966fe759)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8468a458-1d6e-4688-b961-30b672c7b784)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9636cf6a-c36b-4a1a-ae3a-088c1fa1a615)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 af7b6ff1-4b4e-4353-a76a-34423b12789c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 df973686-43a2-49e2-afee-a50d321ec924)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b0dc971-354a-4109-8088-cbcb76976b4a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2a59225e-0206-442c-8cb7-d473bdefa5f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f035d365-b3db-4c44-b821-75267fa9e43a)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3e446bad-6497-4089-a3df-cd18e6bf487e)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d31d780d-e1f7-484a-a4df-948428a8be88)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c9906747-28ce-448c-b32c-460c1fbfcba0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f26eca5e-f6f6-488d-bb10-84359f8cfff6)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 f6304be2-2391-41f4-b07c-8577d6723ea6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a97d9656-ba87-48a5-9236-16057c6c8db9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 668b47f7-32b3-41da-aee9-67e935ad47ce)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4a6eb5c5-2492-4036-ab60-2ec5236e63c9)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 d59e589c-fe70-49e1-a8d7-616b473740c0)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 4ad3a1e2-6569-4caa-afe4-da4da9fe3d1a)(shape \
                 Convex)))(Secondary((id \
                 d796506c-840e-408b-a08d-0ad475707e48)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8b2e5ede-ec8b-4884-a35c-bfc00af45040)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 95f218f6-5291-4bb1-939a-d2c9c8dce1c0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 25d47725-d6db-4a27-adf3-41610dd615b4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e450962c-c9cd-4836-af1d-53fa57875af1)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fd4721fa-fa6f-4ed2-bb0f-54c0a151fec6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff6dbec8-5267-488a-b3b5-80ab0d700c44)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e5be275a-a1bc-4ade-b700-b97349739cb9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 033a3db3-6138-4464-a9b4-c4869961146c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 94eda17a-ad4c-4393-ae10-8302b53e2d7c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c7067830-b673-4dd0-8655-5f4d97c2aabc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f724995-67b6-4b4b-bb5f-fcf1893a916d)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 67493c89-eb02-46da-a207-711ea889fdd5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6e16ad8d-fbee-4c7b-8fe3-80c0dc66454e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 52bb5578-b1f3-4d68-a8b0-9dab84317f20)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd6ae394-4a3b-412c-bc4e-281e3aaeb2f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 894d7f45-79e1-4d32-a6bf-b51d97b102b8)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 25bf6d42-5a8f-4449-a3bc-ad9bbfbde8f3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 65967ee6-68c2-4430-bc4f-4fe243a7f89c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf5cdf3f-9ed1-4e43-bd7a-833e2c043ab4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b6207fe7-e2ff-440f-b49d-f772f54a8a4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c5fd7629-2245-4de3-ad4a-e43577f378d4)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8b77b9a3-da5a-49d3-89d9-2389716d38a9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 da6cf85e-784b-4688-b4bb-a5de6895cb9e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c9436d2f-9525-4014-81e0-13627c84fed3)(content(Whitespace\" \
                 \"))))(Grout((id f72daea4-0e1c-46d4-a577-11dc663c4c3c)(shape \
                 Convex)))(Tile((id \
                 3f04215c-9c0b-418a-9fed-5a345edc51ae)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4505142a-ee63-4255-b9a1-45967d536e7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 850f1156-171d-450d-a697-2d7426fef890)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4c43eaeb-6058-41ea-9175-5593ce489446)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4ac2e2d2-5e2c-483a-8419-90979d2acb87)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45721068-5191-41c0-97d7-0ecec60d398c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a10f8fd9-bcf7-44d9-aeef-b09b800ef4e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72f6c14b-dbe3-4b2a-bc68-c3b77326634e)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9a531af1-8ffc-43f1-b926-4dc9f2c11e75)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1e8944ec-aed0-4ae0-9f09-dcadc92e3a1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef3da217-434a-4bd8-8fe9-d314eeebd0b2)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 8fd1e860-a469-4bbb-9ba0-a21f39f8a73a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 34aea284-bd45-4f35-a7ca-dabfac1a7c40)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b3e623d-741c-4c2c-8792-9e1cb669ec9b)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7ea12851-27c9-438a-a64e-60d23f7d2fdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68c7ad7c-cb45-4e5e-95fb-62e8b057459a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4b02ca6c-e206-4d48-ac79-e586dacf95c2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 590fbd3a-dc13-4b98-8b16-cff8ede68c0c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 855f8c1d-1b60-4b7d-b241-00508971446a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6d3a220e-5996-4892-8c80-3a1a2f0db1f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93989e5e-b66c-4af9-b823-b4c08c5f4fc5)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 424566f7-fea8-497b-be94-cecbf4bad21f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 870675ce-55e5-49ca-a65b-dddca83a5ae6)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1187b178-0c2a-43d5-a21d-e143d9341953)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0cb654fb-692c-45d3-ad49-91eb6b083f7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40aacc49-f016-4522-8928-3acda44d7542)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a2a6aadf-7f5b-41f7-8171-6779ec582301)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a8d850ee-6980-4d7b-92f3-36da569100ce)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 61c23189-a6bd-423f-80d0-1835a313ccf1)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 db64bd98-a11e-4305-b7f3-b452b1888ba7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d67f7404-2ac5-4079-ae12-a9ed9fb85d31)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f69341bf-96a7-4961-9a5c-2d3f200f22fc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 20e3841b-6918-428c-a2af-1bad8782d4bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b724bbf3-7f25-4669-ad55-bb9bbf4e8115)(content(Whitespace\" \
                 \"))))(Grout((id cf84f16e-f166-4b51-b4e6-049b1ce1dec9)(shape \
                 Convex)))(Tile((id \
                 0abbf0b4-2348-4efb-aacb-52012081dc37)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d46fa747-5e85-489d-9d69-059a7f07f977)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0a02c22-c092-4ceb-b17a-450331c8d17f)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 639db5f8-2ee5-4660-b6d4-5852f63d6b99)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6fa1692e-f4dc-47f6-904a-f6efc37a65b2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cdf83aa5-2b6b-4e42-97ab-f6c922a3b3f6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 3fe95f8a-f9d1-40c6-bb91-d31bea6aafcc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1ebc17b-a578-4ebd-bda3-8fcdaa59d3eb)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 9d1ddd32-a36a-4514-970b-4d6651f8c4ba)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 51f2df2f-b75e-44fe-abdf-5f5e0179e334)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 67fb4d36-10b1-44d6-a3e7-21dd1060c696)(content(Whitespace\" \
                 \"))))(Grout((id a7fb5a73-2ad4-45be-9217-a15421b94b51)(shape \
                 Convex)))(Tile((id \
                 17a41687-b08f-4ff2-98af-2fea47f95f24)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 c406ea25-c689-4fc6-9a0b-a86d384636bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16ca4023-75c3-4218-878c-8f629b0dedd4)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d926a732-ae87-4767-a100-e9858bd52605)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b640f897-88a9-4dce-b9e3-7473d6e85e22)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f0f645d4-8e99-42a1-ac9e-69bd4f9cd1b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35053850-23f5-4acd-bf45-2f1258d00b66)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ce99772e-caa1-4d3f-ac59-6d4ad3a887dd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 963ab1b4-4cfb-4718-bb45-9dabf362279b)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e58ebe60-aa42-4470-925b-062afaa8f755)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9dc59ff-5f53-4d2b-8633-516f300f2be5)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d66d2f4c-5c43-4cbe-9fcc-b9839b8c81b3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 357f7de3-fa55-47cf-a63e-546895193402)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d85833ba-41c6-4a25-9b63-4b8b3a2fc33c)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 09900b61-5baf-4d89-a9d0-a211fcdbaf30)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3a620d4b-f98c-4e09-90e2-edc51467b958)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 54ed2b3a-d7ed-46a5-b5e9-6062e3d706b4)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fa59e47e-78df-4f99-9887-513007871ab6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20aed684-0a33-4837-9c91-b6f388a392a4)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c3ec9096-55e5-4952-a2dc-c38d9a1934e3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4a957aeb-2c1f-4f0a-8186-0e897c718626)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0068b781-4219-4451-8db5-cae8ea87da2d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7748ffc0-75ef-428d-9b58-4580dd7c3b78)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b1744c1-7cc5-4cab-95c6-c805bfb455bd)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a9b7c3da-34fd-4c13-ba76-8d12eaf8b013)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 668fe660-3748-42f2-8750-1d9233647860)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 792a7fe1-914a-41ec-8e3c-6946087ccb17)(content(Whitespace\" \
                 \"))))(Grout((id 09163549-d4c0-4420-8fde-0ee1b95a3e00)(shape \
                 Convex)))(Tile((id \
                 f21a218e-97b1-4cb9-a0a5-e5f1548d6c1f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1b95ea63-2f0a-4cd3-80ae-4e148633c82c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1b4fac6-c1d1-42b0-b94e-1f77eef60360)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 661f88ef-101a-4467-875d-74cc19f532ff)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0c5f4384-d374-490e-baf6-73c6fba62014)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0a61c54-5ce1-4d07-b2aa-0c24d8d47304)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 03c97a1e-5e63-43ff-9f90-9fb5071cefc4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a269e135-5e34-40cd-bbaf-5d7ca7264d07)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7085914d-118c-4596-8040-01e10f0c7a3b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 38face3d-4e38-4679-b334-0641b60b839c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21029f89-c5b9-4076-aa4c-f2dcfcd21835)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 705b9a88-adf6-46d4-b9a8-5bbc2648d9aa)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 75f847fe-c467-438e-b118-f885dfe12d11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 48befa9f-1d82-43a0-8a9f-b4a551b4f538)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f8e72f0b-089c-4495-a457-0d036c53b27d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b6019a27-ed9d-46e4-a071-aac0a9f7c1fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee76ee53-6f9c-4531-92ff-d337918caa29)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 776ef91f-ca17-4184-a363-23134f2301f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f03b0d0-5251-4546-ae2d-c920f979d361)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a84bf77d-1b35-4af4-99e4-57dd207e2da7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d0a816c-0467-461b-addc-9f0d300a86bb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7e60b9c4-2c25-4122-9031-34457a0806ff)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ddbb24ed-5566-4c35-95b5-860a12dd5248)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0cf8366e-ca71-475b-8399-e9d2a34c9a0f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a0c66c4d-d74c-4be8-b2e4-964ebf05472e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 44160bdc-9ad4-44a8-89c4-6f50038919a0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b34e08c3-7662-496c-96df-e9111bd1f643)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4853538d-a2c9-4fbd-ad15-1d72ee2b3cc4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56533393-9f93-4caa-80e8-59db930e67e1)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d2ca34fe-412e-47e4-b04a-41ad670be882)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 94500cf2-356a-48b2-ac6b-86ccaab5b10a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 36893a4d-0c01-4b09-a92c-59fc51e0fe57)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ebd1fe9e-621b-49d5-a3a1-fd8d35b79b76)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46873372-79aa-46af-8ad8-a39a129c39e0)(label(f))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 54a7084b-48ee-4a0f-8ef8-147037d2a9c7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 40a726b4-63f5-477b-a0bd-e2b03a68185c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e2bb9e20-2b1a-4334-960e-72614d04a5a4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 38119e9c-e593-47bd-88f3-c67c7f07eada)(content(Whitespace\" \
                 \"))))(Grout((id 1039458f-6394-4c7f-b3fa-21bf51f0f87f)(shape \
                 Convex)))(Tile((id \
                 6dc643c3-18a1-437f-88ae-150eb3ac249f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ec6c3bba-8579-42ad-b386-2c2ce60b5abf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f95ad0a8-1d39-4985-9d77-a9a93b7efeb8)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 689ea5ce-5b0e-4a75-b13f-b09246c6a669)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e920b192-de5e-4855-ace7-75ba3208fddc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b6495520-5564-40e9-bbea-d38a193c72ac)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0e052a09-7aba-466c-bfb4-81468b12f9fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2daa0ef8-d29d-48ae-a138-61fc401ad950)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 be490e19-a602-45dc-b654-6715e590f251)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 22e34140-165f-4fac-9ec4-d8e7ab2a3c6f)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e768b6ff-d460-4807-8e1a-576f981d6fe0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76dde3d8-37bc-4d08-adad-a0c2bb6ca9b9)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 7e067617-dee0-4e7e-9bef-685a9c58dd46)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 28a0ef59-40cb-436c-b2f4-ca6fcfcaa5db)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8da29f2d-2d2b-46e2-94b6-aabbaf1d97ae)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f5353b7f-ffe4-4905-a0dc-1ccd0ae837d1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f30a6544-8281-4ce8-84f0-74b01fee295e)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 36b63b80-40b3-4279-9a52-c4ebc96ef1b9)(shape \
                 Convex)))(Secondary((id \
                 90dac9a3-cfd2-40e8-bc28-60071381900c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 763d28e2-674b-430d-b1c0-542d17ea7b09)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a8eed44c-5714-46bd-8ba6-e588a046a98e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5a58bc3a-ca4b-4d00-81bd-45b88acc6ab8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92f7f67d-26e0-425d-8322-757c09aa842f)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2d14adb7-a543-4fbe-8716-dde2b18f1cb5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a8920a0-6a21-4b0d-af8b-b051f64a1706)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 60571abd-7342-438a-bd31-f359378027cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c8fda687-4976-4c47-a208-15ebb62dffdb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0b01d85b-d751-4392-824d-6b52b3b6aea1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7860cae2-6f31-4aa9-b131-39cec4057ea0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ca76690-1cf4-41c4-8da5-d4360b4f861f)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1fe1d913-15ab-4a15-a3f3-81ea684b4397)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ff2435e9-c2e0-4316-ac4c-5fe3d2fb687e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6d0b606c-d057-42b3-9d3d-33679988a5a0)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 617a93e3-6dd3-46e4-9def-86803cd285d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b7f7bb7b-95dd-4cb0-9f3d-b119c4e00b2f)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 c9999f9e-81e5-457f-bcf4-6fe7400c3e28)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 726577bf-09ce-4f2e-beae-8218ea89f6ef)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 52fbcb6c-95d9-4124-85d3-f3ce3a042329)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 177b5c85-7c56-4a83-a5f7-51cdd52cbba7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 12f1f2ad-fae3-4ca3-a8fe-fa6fe4b1b167)(content(Whitespace\" \
                 \"))))(Grout((id 2b4b41a7-f15e-4558-a584-c0cdce779d18)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text =
                "# Internal Regression Tests: Function literal casting #\n\
                 # None of the below should trigger runtime exceptions #\n\n\
                 let g:   ->    = fun _ -> 9 in -g(1);\n\n\
                 let f = fun b -> b && true in f(true);\n\
                 let f = fun b:    -> b && true in f(true);\n\
                 let f = fun b: Bool -> b && true in f(true);\n\
                 let f:    = fun b -> b && true in f(true);\n\
                 let f:    = fun b:    -> b && true in f(true);\n\
                 let f:    = fun b: Bool -> b && true in f(true);\n\
                 let f:   ->    = fun b -> b && true in f(true);\n\
                 let f:   ->    = fun b:    -> b && true in f(true);\n\
                 let f:   ->    = fun b: Bool -> b && true in f(true); #ERR#\n\
                 let f: Bool ->    = fun b -> b && true in f(true);\n\
                 let f: Bool ->    = fun b:    -> b && true in f(true);\n\
                 let f: Bool ->    = fun b: Bool -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b:    -> b && true in f(true);\n\
                 let f: Bool -> Bool = fun b: Bool -> b && true in f(true);\n\
                 let f:   -> Bool = fun b -> b && true in f(true);\n\
                 let f:   -> Bool = fun b:    -> b && true in f(true);\n\
                 let f:   -> Bool = fun b: Bool -> b && true in f(true); #ERR#\n\n\
                 let f = fun b -> b && true in f(true) && true;\n\
                 let f = fun b:    -> b && true in f(true) && true;\n\
                 let f = fun b: Bool -> b && true in f(true) && true;\n\
                 let f:    = fun b -> b && true in f(true) && true;\n\
                 let f:    = fun b:    -> b && true in f(true) && true;\n\
                 let f:    = fun b: Bool -> b && true in f(true) && true;\n\
                 let f:   ->    = fun b -> b && true in f(true) && true;\n\
                 let f:   ->    = fun b:    -> b && true in f(true) && true;\n\
                 let f:   ->    = fun b: Bool -> b && true in f(true) && true;\n\
                 let f: Bool ->    = fun b -> b && true in f(true) && true;\n\
                 let f: Bool ->    = fun b:    -> b && true in f(true) && true;\n\
                 let f: Bool ->    = fun b: Bool -> b && true in f(true) && \
                 true;\n\
                 let f: Bool -> Bool = fun b -> b && true in f(true) && true;\n\
                 let f: Bool -> Bool = fun b:    -> b && true in f(true) && \
                 true;\n\
                 let f: Bool -> Bool = fun b: Bool -> b && true in f(true) && \
                 true;\n\
                 let f:   -> Bool = fun b -> b && true in f(true) && true;\n\
                 let f:   -> Bool = fun b:    -> b && true in f(true) && true;\n\
                 let f:   -> Bool = fun b: Bool -> b && true in f(true) && \
                 true;\n\n\
                 let f = fun a, b -> a + 1 in f(1, 2);\n\
                 let f = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f = fun (a, b): (Int,   ) -> a + 1 in f(1, 2);\n\
                 let f:    = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:    = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f:    = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:    = fun (a, b): (Int,   ) -> a + 1 in f(1, 2);\n\
                 let f:   ->    = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:   ->    = fun a:   , b  -> a + 1 in f(1, 2);\n\
                 let f:   ->    = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:   ->    = fun (a, b): (Int,   ) -> a + 1 in f(1, 2);\n\
                 let f: ( ,   ) ->    = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: ( ,   ) ->    = fun a:  , b -> a + 1 in f(1, 2);\n\
                 let f: ( ,   ) ->    = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: ( ,   ) ->    = fun (a, b): (Int,   ) -> a + 1 in f(1, \
                 2);\n\
                 let f: (Int,   ) ->    = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) ->    = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) ->    = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) ->    = fun (a, b): (Int,   ) -> a + 1 in \
                 f(1, 2);\n\
                 let f: (Int,   ) -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) -> Int = fun a:  , b  -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f: (Int,   ) -> Int = fun (a, b): (Int,   ) -> a + 1 in \
                 f(1, 2);\n\
                 let f:   -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                 let f:   -> Int = fun a:   , b  -> a + 1 in f(1, 2);\n\
                 let f:   -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                 let f:    -> Int = fun (a, b): (Int,   ) -> a + 1 in f(1, 2);\n\
                \ \n\
                \  ";
            } );
          ( "ADT Statics",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 28357f24-0bee-423a-8233-69bbb2cfd787)(content(Comment\"# \
                 Internal Regression Tests: ADT Statics #\"))))(Secondary((id \
                 da7d803e-5f91-4afc-b529-fbd0ec0eaafd)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 505df48d-38fb-4b63-8962-c4aa88f70e50)(content(Comment\"# All \
                 commented lines should show errors as described \
                 #\"))))(Secondary((id \
                 70e54a1b-8e3f-4e8a-a0f2-f132102dcca2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0c97d2fe-e4fa-4699-889c-26f406fc97b7)(content(Comment\"# No \
                 other lines should show errors #\"))))(Secondary((id \
                 d99dce2d-ee04-4e13-bcef-375f0608d8c9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 e13aaf5d-7c93-429f-93ff-47ddd45609b7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 545d83f6-f159-4fe2-9fb6-f9514dd064b8)(content(Comment\"#type \
                 definitions: no errors#\"))))(Secondary((id \
                 b8fe9b8a-9e2e-4774-8e8d-c5202e4d567c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 54e23d33-2b44-4416-baa9-b82dd9d49fcb)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 db084c97-20fe-487e-8d52-80bc76bd2ede)(content(Whitespace\" \
                 \"))))(Grout((id 7273a907-7f31-458a-b84a-ea0a5f3bcab0)(shape \
                 Convex)))(Secondary((id \
                 1b874263-3f04-4e50-8ea6-a6a6631c789d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 540a9dc5-a060-46f2-987e-8c45d8a3d040)(content(Whitespace\" \
                 \"))))(Grout((id 99c6cef7-771c-4b30-afc6-648a2a9b52eb)(shape \
                 Convex)))(Secondary((id \
                 67a07a9c-618f-4947-87a0-229733d058e4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e7b21ff9-855f-4af8-b136-61cdf82cb732)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bbd37bb2-75f6-4660-bc03-060f3fcb88d3)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 28baacf9-b184-4689-89d2-5898e15b4425)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a215e8bc-91aa-4d2a-864f-8779cd29c147)(label(SingleNull))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 5f8c6a8b-efdc-4ae1-a9e1-34ce7faad82f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6689bd94-617b-47c9-807e-08b9cf84b8c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f98d916f-8631-461e-a64e-95d75cccc6fe)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ae3f938a-935c-46ea-b4ca-e8fb41f6e001)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 50a7b73d-2fc8-4357-ae31-b83c1f5bd467)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 41b177e8-90f3-4af9-8186-cda93d46bc46)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5d191c2c-9036-42a0-bdcb-92b382e55d8d)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7a6b25d9-e7d3-4b55-b5e5-404c4c2ae4f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7de9e4b-174d-4958-9e7f-7d36445348e0)(label(Single))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 d45238b2-8b1d-4f18-a077-3827151dc45e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0425820e-04bd-4c29-8adb-4124b681d57f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 01b63189-aee2-41a6-aa07-ffd4bddc1a49)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0f58b71b-37d1-4270-8ce3-21eeb577bfb7)(label(F))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 fa9a4146-4da4-469f-a26e-81cf3f45b2ae)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d110cf17-d811-464f-8726-e6dfc9f8b6bb)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a109dce3-009c-4f4c-aefe-7cab4536d392)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b7cdb552-04a7-48b4-9cfb-998bf9d18055)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f54c8ce4-d565-4704-b224-8e252cb8cbd3)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cfdef333-8e4e-4e5e-8fa9-58da5cb2bbdd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0846c5c-ed48-4e17-8047-68e9d0bfd72d)(label(GoodSum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 16a4fa3b-9c47-432b-baa5-e268c973baf5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 967321aa-0d95-49e6-89e0-9d388d6da469)(content(Whitespace\" \
                 \"))))(Tile((id \
                 149bf644-2adc-49e6-8d53-81b03ecf4d49)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6c0d12c7-17a4-44f0-a149-19aa3f780d21)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7294404d-be7d-4cb7-ab89-fa2b0d214512)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 faac7e2c-4caa-46ff-b72c-fd1b78b76f75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4e5dcc2f-b3f6-43fb-a906-5184199c86df)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 37516c52-36a1-4c80-99b1-a0d857c1dda7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86c8fa45-1c54-4752-9ffc-c889dc6a8e52)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 45cd1be9-10c7-48e1-979e-6549e563f9b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a3ba33f-56e2-4b87-99c7-733ca87a6dce)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ee980372-17e2-419f-bb0f-a575cf8e7fc2)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d95d1829-297a-4db4-8e52-fea95862c74a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 6bc9a1c5-257c-4cd4-80b8-236a3ae60b01)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ca4f5031-e534-43f7-87b2-d78f8cda1acf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e7ed2a86-c44c-4a31-ba4b-608de76fd6a2)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 69656e33-f418-48a4-a438-00db04b42210)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3218a5c6-c361-4146-be25-3bfb3d172ea3)(label(Partial))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 0e2357e6-9d96-42ef-bef6-f72319502c77)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2b2cdc66-752f-428d-965f-1a79b080ea98)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b8972117-909f-48c0-8f59-7a51b7cf5db7)(label(Ok))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 febdbbdb-4585-4803-9e84-bcd3304ffb1f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 c7b12338-0a19-4fe2-820d-1cb076b6dca3)(shape \
                 Convex))))))))(Secondary((id \
                 eab5c8f9-998a-4cc3-b3d7-f453b90dbe8e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 319b516d-0c4c-4a26-976b-bf76cd5c0ada)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 24ae6999-d0e6-426f-86d6-dc39fa14c86a)(content(Whitespace\" \
                 \"))))(Grout((id 5fd021dd-31d5-4767-8e92-1c1526e9d3c0)(shape \
                 Convex)))(Secondary((id \
                 7448de20-a1c9-4b99-8495-60e37df82175)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ed8aed8b-a1d6-438b-a7a4-ed310d325dbb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 10a6df79-02e4-412b-b1d7-f4690c81ccbf)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d77a36ab-5548-4eca-8db5-66b91ba8b3f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8a111ed2-1c78-4c10-bac2-2e855f655112)(label(DoubleAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 1b510d24-fb50-4d72-982c-3b584ef54135)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c5b5b7c-6f48-4730-a1cd-23ad4911baae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 765c6668-f5f8-4fa6-8c23-46cbf50a2169)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 8ab02e4d-1f40-4182-b669-3002e91971d8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2f94abb3-22ba-49ed-b336-378757f564dc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8d517f36-a20a-4cd2-a280-8a24d56a2b25)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9ed73f0a-2269-4eb6-a371-a8710e4f9a1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d78a7e50-97e1-4487-94c5-7620f7057b62)(label(VerticalLeading))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 d0fa2c86-1a80-4489-8dad-0864eac411ce)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c550f4d9-2252-459d-881c-1ea2d5113920)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 48ec824c-86ef-419c-bea4-01af75ab209c)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a3f051cd-9ac0-4f5e-a687-7436f35311fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 110f994d-9f94-435a-a662-650195526a66)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 de227626-c88d-4f8e-9b7d-8d239bc6b7a2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b7b57d94-9f0b-41ff-8805-383757288c54)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e76c4cf4-6b52-4f61-8613-8418c2b54e04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4de6fd61-26fa-4770-842d-f6c40b502f33)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e0d70e07-aba6-40e3-897f-ac02915c5454)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 375f169b-7f67-4190-b980-3292822c286a)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 226a9d4c-109f-4813-aee7-94e2ad45fae2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a38444fb-d2c4-470c-b660-47c92bb89991)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c587c505-748c-41c1-8279-71522de8cec6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 974020d7-e351-4812-9397-5018920e0a45)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 d080e933-5349-49b7-b35b-2e5f24d3b007)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 78af8147-201f-46f3-b58d-4bcf7bfdf0bf)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 d873c642-102b-4997-bac4-f1f0b75a1624)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3676c2d8-c573-497e-b7cb-d61cae5b5da6)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 f1e2ccb2-8186-4934-a292-57abb525d91d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 546c1a20-e58f-4db5-88be-c29ff7e117e2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2f7eeab0-c2c0-462f-8a7f-6dce1a9479ec)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cc3e6a4c-73de-4450-97b7-d622e501a65d)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 50745623-8820-453c-bc0f-62ef453d38f0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f71ed59f-f6c7-4b69-a8c4-e12a77a9b82e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dec0a291-91c0-413e-942e-2cf40b652dcf)(content(Comment\"#incorrect \
                 or incomplete type definitions#\"))))(Secondary((id \
                 ff85a206-092e-4b47-8496-75171a46cb21)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2a29b5e5-296b-423e-8876-ea638cf99c39)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 88097564-6cb6-40df-9dbf-8726f15e8023)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc56b1af-baac-4afa-927b-5b260ef9528d)(label(badTypeName))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 06bec89d-de70-42d6-85ac-73cd1ddc86c7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 46400187-d912-4ba4-a220-7326db747f86)(content(Whitespace\" \
                 \"))))(Grout((id 71cde0f8-e7ce-4994-9f73-381e4eeb7e3d)(shape \
                 Convex)))(Secondary((id \
                 bbd999fc-99d6-4022-beec-290ae2883829)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 06fea02f-7527-4678-b8d4-23eb2895fced)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b76fdcc0-e6df-4bc1-a494-6c22207a0088)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 8557100e-b121-44a6-9904-88d04e14afe2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 037ed10b-ec8f-4e74-91ab-8b5ac06d43e4)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d07a788d-e8bd-4292-815c-c1b2aeb50ce8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fbc61fe-ea41-465c-8229-5c191b82ad22)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 bd55016e-8768-4ffa-9206-24cd3b8ea73b)(shape \
                 Convex)))(Tile((id \
                 5e281346-8c28-4270-9131-df8eadaa2d7a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d70d0656-a8c7-4daa-9f47-f63a09e7581a)(content(Whitespace\" \
                 \"))))(Grout((id f61afea0-f3fd-4f9c-8917-0aa5e600782e)(shape \
                 Convex))))))))(Secondary((id \
                 6f6d1712-ed54-43e9-bfcc-2aca0a93ceca)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d2d9a292-469b-401f-b5bc-525561ca82da)(content(Whitespace\" \
                 \"))))(Grout((id 18cb0921-300c-4001-8c45-e4950ce6b20b)(shape \
                 Convex)))(Secondary((id \
                 4dec9e0d-5df5-4954-8107-2d3a56728db3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e16bf39c-26cb-47fe-89f4-33a1a9970651)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d950efef-1383-4e50-a303-aff3be6cfa78)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 df4aa51e-b18e-4cd1-98f1-446b1c9d721d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e93edd08-38c1-4e34-afd9-33525f28d292)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e3a09bfa-b86c-4849-be8b-7c9929b754ed)(content(Whitespace\" \
                 \"))))(Grout((id bb1bee1a-abe7-47aa-993f-85fa842b6fbd)(shape \
                 Convex)))(Secondary((id \
                 e5a4c884-61e0-46e0-ba51-aa8027c03b75)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b245fa5f-585b-4a67-94e2-2510e636670e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 52ac5bf6-1b0b-4acf-9c46-e099e018c4ee)(label(badTypeToken))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7c99b199-bb9f-42d3-b28e-3940e92a3533)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 248b83b9-3ed2-4889-b3a9-4728d8345ad7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a561938e-1e8d-485f-92f5-0976d7377be1)(content(Comment\"#err: \
                 invalid type token#\"))))(Secondary((id \
                 d7d0d95b-1e27-492f-91a5-97176f237c71)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bbc7927d-aa81-4f04-8190-92349f23e545)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5c927f79-7c3e-49bc-afc5-355081fc688e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f7eda69-8e88-46f6-af92-18e977fc4e57)(label(NotASum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 514b34b5-fae2-434a-b5ad-3c180fc704e1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2158eae1-2f2a-4df9-a342-b5fa191925cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad1bd59c-b07c-4854-878b-fb73b714bb34)(label(NotInSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 acb6b1d5-bda5-45c2-aab3-b9bc747ceb49)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 553dfa61-d24d-4394-ac50-4aa83e07bb21)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 c55a91f3-69b1-4505-b31c-ac9db253b5f2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 51fb312c-cdcd-4db6-9a97-c06f9349262f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bdcdee76-a147-457c-bdc5-911d1b51a26a)(content(Comment\"#err: \
                 cons not in sum#\"))))(Secondary((id \
                 f2cd045f-7c64-4ea8-8452-c4ab758dec7a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4221a1d8-bcd9-4132-9916-5bec3de4a625)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9d96a3ba-2e82-4741-a739-916745c2095a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1fe95f07-86f7-4ba0-83ca-ecfcca5215f8)(label(Bool))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 7fe1273d-e45b-4e86-ad79-a9a30cf0afd6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 290c38f5-c4da-4a8e-b556-8503adc36c8a)(content(Whitespace\" \
                 \"))))(Grout((id c27057f8-540d-448e-855e-692824076cb2)(shape \
                 Convex)))(Secondary((id \
                 c77c2cb7-8f94-4093-9dc1-3dc1c76202b0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e93265b2-2163-4c24-a77b-75e0743fdefc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cd8907a9-81d2-40cf-a12f-cdf75f85ecae)(content(Comment\"#err: \
                 shadows base type#\"))))(Secondary((id \
                 ce97e91b-7183-4964-9dad-5248b847cb2b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 524585ce-43ab-4dc4-a476-d2be15591182)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 033bf566-1b52-4a6c-966d-45efa4175c0d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41911e97-fb92-4173-b3cf-f5d4eed85938)(label(Dupes))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 ff3b3be2-040d-47a2-b628-d957c263c18f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 de16dd5e-f90f-4941-b9b8-232e569d9078)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 94ecd3bc-97eb-403e-9563-421273cac80f)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 adef1e3d-eab2-4810-9a16-263d1a6d36bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d3a0426-5dbe-4845-a282-d11a40094d78)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 b710d3be-0810-42a3-89d3-358051f7013b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 c9d5f791-f9bf-4844-9869-3c064b839810)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9058a038-a0f4-48ee-b778-3fc60e5c9fb7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 afea3bbf-04a5-4e37-972b-6c9f976e7a4d)(content(Comment\"#no \
                 err#\"))))(Secondary((id \
                 f837183a-06ad-4583-b423-4d17dbbc4aae)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cfde56dd-4d78-48db-b5b9-e3349e8ba3dc)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d81137df-f2cf-49dc-bb0e-fe21538e3a64)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02201697-1922-4a2e-9180-4dcea7428a5c)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 4868483e-5f32-49c6-adc4-6630dbe6ced7)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 fc54a64a-a135-44c8-9cf6-a7ce8a527315)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 fadea698-7c37-48a8-8c95-2787e6e3b20a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9571f9ca-05e9-4a77-accb-b92d6f3b5368)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 55aa05ad-0b6a-4f64-ac40-113f79ffe7f1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8c570671-41ed-4e52-a50b-3cf2809fbbe0)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 de416faa-f1fa-4900-9ad2-f32009445925)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7feec638-5149-4401-92cc-65748cf149ff)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1e3d16ab-a72d-4342-b2f9-0b8b1fe14657)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5a84c480-9a4b-47c7-9d5f-cf03b1112f4b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 79a4e032-0c92-41ac-b21f-198802c8ed00)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 3c76eb37-d0d6-4e5d-8b6d-fe3e0a29815d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc4a691d-60ff-4962-96b6-9f50d0fee6d7)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6f82cfca-5b64-4104-8719-e56652b83ca8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55211581-d61b-4ea6-8c0e-43853ab7088e)(label(BadCons))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 541a269d-3b0c-4c6b-b421-cf2757f096d6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f24cb71c-259a-4d1f-8a4b-04e51ed80b23)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a034eadb-e82a-4983-b0da-52deba544b92)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0ae1f486-6b64-49f1-9839-a3071a4d5e26)(content(Whitespace\" \
                 \"))))(Tile((id \
                 576f5861-9df1-465d-aa6a-02b20f6e63c4)(label(Um))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8b8364bd-ddbd-470d-95ca-8d3eca892887)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 327a32a4-b4b8-4537-a4a4-ff819776b7d8)(label(Unbound))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4f4387fc-fbdc-41b3-90ad-1def00e5713c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8892cd2c-0885-4957-80ab-3e2675f4bb75)(content(Comment\"#err: \
                 unbound type var#\"))))(Secondary((id \
                 80d01050-b2ce-4ac8-9ec6-0170deda3533)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2b1798d7-13de-432e-95a5-48bf480f4831)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 469c772a-6cdc-4419-bb66-aaee9d260caf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18f0f427-bf66-4c29-8220-dce417a0dad5)(label(notvalid))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c1667cf8-ac3a-4b57-a1af-5add1cadbdec)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7a66d06b-fdbc-4a07-8f9c-fb0cab06be80)(content(Comment\"#err: \
                 invalid#\"))))(Secondary((id \
                 9b1ca2ba-300f-4f4a-a664-85b57811b1c6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 78cee115-8bb2-4e76-b6c6-3f104bfe0764)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d54d1a84-8d14-486b-a4e7-a47e9a71bbe8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 688f920e-c2a2-442b-b6be-4fd3aaa2bfe9)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 42a13800-3507-4688-b610-92bd8f40b64a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3f5bc604-c1a1-4b28-b62a-f379cb00bf13)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 fac092ba-b3ca-4bfe-b67f-7b1712db8cf6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a34b8831-ac9c-4b15-9f36-bad3a9623bea)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a744ef23-572f-40e9-835f-814d1ced928f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0658f89a-28ce-4de6-a50b-dae44302c439)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 98717293-02df-4aa6-9b0c-0c4adee6cf7d)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 57276d63-e061-4585-9e5f-3c0a07471900)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 32f93082-70cc-490a-bd9a-8b1b863ea1c4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e4902f69-d522-4a3d-9480-c4bf75b44cba)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 5df67ca1-51f1-4f0a-9e42-60b4955d7370)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3723b143-134a-4d1b-9482-e3db9ca2b754)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4c7b2401-fc05-4a9a-8005-5a731405d897)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee423a74-19f5-4331-96ec-32fbec96ac1f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 69b680ca-10e8-4999-a9ac-cd35c530f38e)(shape \
                 Convex))))))))(Tile((id \
                 4127b3bf-d6f0-4328-a943-09f3937d4335)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4b70551f-8d18-404c-abc2-7be85297eaeb)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 c162b951-60e2-40c5-b2ce-7fcf5abb6129)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1f35f928-4a5e-4d0f-83f0-f92ad23e37e8)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 e11318ea-b24e-4c36-bc1e-48f0cf45a6ab)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bb584618-cce6-4fde-9fd5-d7664d50a6ac)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e3341aa8-5447-4345-b26e-771ef6594422)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a89f44e0-6c1b-4392-aa86-a40eb1ad7d60)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 cf28523c-f133-4218-ae35-7036d973c1d5)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d5667e50-e604-4129-a70f-12d796a23781)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 1e352928-e4c6-4fca-ba2e-11cef32fefe4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1ed09b61-a337-4e8a-a34d-2ab888561c24)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 7da994bf-6340-45c4-8599-a03ca2341769)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 671e969b-0148-4345-b52f-d953506f9225)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a99ba882-d549-424a-8bbb-e6bb6cc14f5b)(content(Comment\"#err: \
                 expected cons found app#\"))))(Secondary((id \
                 dafc64b1-0620-41c6-88cc-4d3700a91400)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 63fd0ca5-5526-4d63-8602-7dc819005e93)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 56d28a0f-2244-4055-97e7-d87e504f59e6)(content(Comment\"#sums \
                 in compound aliases dont add ctrs to \
                 scope#\"))))(Secondary((id \
                 a292fd1c-d0c3-4aff-aea6-54fb47db6aad)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4df5e4d4-8390-4158-a53e-ff75907deb30)(content(Comment\"#but \
                 compound alias types should propagate \
                 analytically#\"))))(Secondary((id \
                 3fda8862-fca6-41ca-a9e4-bc4154218455)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c6539017-bac8-4921-b1a2-dbd7686fd99a)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c9219eb3-0e7a-447a-8e35-254101e3dd24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db95f67d-acf2-475a-ab91-4fa87ab97edd)(label(CompoundAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 e6628e78-f477-4bab-a09a-929b617b2083)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4a64ac40-4679-4366-91a0-b89dca268279)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34933335-7008-497f-9d8c-b31135833d16)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 c218a674-f28d-4146-930b-eb6f796111e0)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 76077180-868e-4535-9870-6f45077f1279)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2fdfc791-1dbd-4794-ac3d-acb08e108b1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 660b1ff5-f543-4a94-9e0b-040501cb6950)(label(Anonymous))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ca0b24b7-26e4-4f18-a5ac-03da0194292a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e54319f0-eba4-46ba-8904-e80e9db0b1da)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b5119751-b957-4a65-a4c2-ccdc1ab2bc89)(content(Whitespace\" \
                 \"))))(Tile((id \
                 148914d3-99cc-446c-8bb2-3422cc4a553a)(label(Sum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a3295e9c-3446-4e30-81b6-f9ea31d00099)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 85cfca79-ca13-4d16-82c1-5f9b090483d0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3addc8b5-77a2-4d3f-b240-a42a1bdbacea)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4250cef5-a35d-47b8-97c5-c92a48b1c8db)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f2a63b3a-1ebe-4375-9749-4f12edd18d35)(content(Whitespace\" \
                 \"))))(Tile((id \
                 714cbc78-0a5e-42e9-a83d-41a301f4dd1d)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6724882b-9190-4687-9383-cacd82d63928)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fd8b2b27-0be7-4768-a187-fe62a0a02300)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2da8f63b-a4ba-4fe2-9e78-af2a352f2036)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 503e30a9-aeeb-42d7-a42f-c02ef125b0e2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0d92f21e-fd16-4b4b-a512-afae726117e3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0fd5bd23-a708-4b48-98a0-4e4b9fa05c32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9d77557-184b-4f52-acde-16f41d4166fe)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 0f2dbf7a-c881-4050-ad37-0ad7f6d6a7b2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 917c166d-0721-45a6-bed2-681f750485ca)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bbefa4fc-0966-4f39-835f-64d3516e5f53)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 60bc2aa4-0b81-4356-943c-e68f7ac510ad)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c01847f6-15bb-44ff-9f05-209367f03f7d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 591380d8-d43c-47cc-94ca-249f078a157e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3fd95373-6092-40e3-b2bf-8f178061f9c0)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4fdc4342-3d28-4d46-a063-da90e9b4d7bc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cd43dede-f632-4f59-9e54-d8ad47755667)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1c2481f-fa45-4215-8081-d2eef4f8e8b5)(label(CompoundAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6188a6f5-c684-4af8-ab9e-3256c23b7c1d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 016923aa-5b98-4d44-9372-e16836d64f36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f705a020-a72d-4355-a982-13faad65a8ec)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 eefcbd7f-aec0-4db9-a17a-b9ff21d5bc95)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cbdd1550-c48b-4dcd-95f4-bce1f98a6d1d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 98a3cc68-afee-4809-aba5-75e733c673cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37d16e1e-6da9-47f9-a9fa-50cdc3cd94a5)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4ae0a91d-b46b-4c3b-8f85-52bd7748fedf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 40bb855c-6b45-4476-adb8-20151c081ee9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 39500a9e-b189-468f-9103-b24cf76f040d)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 e7eb628c-3fa7-495a-a51b-e110980554fe)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a1406680-cf51-4b14-8f13-9a43b73fba46)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f98204ac-7408-40ef-8189-a32946d1c192)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbc36712-eb03-4be1-919b-c80e5bf9822a)(label(Yorp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 fc7c8c66-f357-43de-9b25-a108f59d2910)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 14ab2d99-a891-4eb9-b55c-0b38ab4fcb0c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20f829db-22e2-431e-ab11-8eddaa088129)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4fce041b-ac8c-434e-af8e-391e3ae60d74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a301e74b-6e03-4455-9cc3-2e325e942fa2)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 839e1f10-a8d2-40f4-9bc9-3dd5941cd664)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4630be6e-eca5-4718-a71e-239de5a48d4e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 697f3706-b9ac-42b0-917b-2b57ad0f1c95)(label(Inside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 8399b277-7d98-453b-a117-a188b78a2f7b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bb39161b-72ad-42c8-bbcc-2011a7bb0927)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0ca22f75-e67e-4179-8574-0251182b7355)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11752147-f120-4116-9aa3-c51df039f52e)(label(Ouside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3890ca6c-f213-4520-9198-cdb016151871)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3ee55537-d8e2-4bc6-9bbf-fbf18d98d1e5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7a297494-386f-44f7-b248-19650fadbab5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d826df8a-e45f-4b11-b7ef-a6180ef6a475)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2d440fcf-b0de-4f95-a41f-cf7c6458ce03)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3aa15bb2-877d-414f-afc3-ae075ea7d51c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6892ff00-0519-4456-89cd-7592ef4f1bfc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83e43479-5702-4249-a6b3-9715fb2223e9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 56a67b30-cab3-4c6f-beb6-a0624c4fe56e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1100f99-f60d-4b5d-9df2-c8aa54744ac9)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 943e41f1-b39e-46b1-a484-115aca74a68a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 79623b4d-7d1a-4db8-bd72-22f39a30e159)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2cf9dd1c-d6f8-4592-80e2-2deb9c8e0d22)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f295d8a6-6d2d-4b4e-86b3-38c9846765e3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9f125b81-04ab-4719-9aa9-32ad8fddb18c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2e22a04a-a6fb-4372-a13b-f2ef4f66411b)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 aa6021c3-7440-4d98-ab16-742f2f2233a3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 712b71b1-7293-47bd-900c-abe487b15154)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f833c960-f1ea-4226-8132-55f684151f32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44154061-c60e-475b-b9e5-23afba0699d2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6ab2c025-ed6a-4bf4-b339-28982ccf8ee5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 aa8f8cbf-d98e-4dc1-b9a0-841b244cfb78)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7046ff8f-5b8a-4d57-a39d-f5bd20916f68)(label(Yorp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f14c92c8-9975-4598-baad-febacbc66ced)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 65eef072-dfab-48aa-a0bb-9f35dc517f96)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d7f6f4d-2b5c-42c6-8fd5-e6e978b7da18)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 89ddccbf-2594-4e32-ad8b-9f360d7b2de3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30add1cd-018c-4a6d-b28a-ef0301b8f0e2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b0884aed-306d-4f86-adca-5545e9d2cae5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ff752faf-57fc-4b58-a8c3-d6739562f516)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7281c949-f27a-4bb8-b84f-50a2d364a4ce)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fd023a10-7c70-40dd-9b9f-0d0e60a6118a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ee8c6a7c-5bfc-4974-8e61-edbefe26b78d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5202ef3d-18f0-420e-9931-73e28fb1fe38)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 1efcebbb-8780-4255-a3f8-d175f1ee073e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e1e7b69d-469b-4ba0-a495-e75b857b3f20)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 99a1a829-4099-40b8-bd35-b503867a2c46)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36788d6a-dbb4-4e09-9fb8-2b47e13d425b)(label(Gargs))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 aa5ad82a-3e90-49f0-8d2e-a906d1993166)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1975dae3-c37c-4e2e-87ef-3e34676c43e4)(content(Whitespace\" \
                 \"))))(Tile((id 9a270162-b5c3-4967-9e80-4500ba8315f6)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 cf62e16d-8837-43bc-9925-2b62630a4d2e)(label(BigGuy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 67a6ef6f-0833-4b56-9cfa-dc35a26e5267)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e5b4c41-f8a7-464c-aa35-0497b093c87e)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 33950ea1-db28-4d4c-80fd-5d9458c01376)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7d579a3-3c20-42ee-84bc-fd774067d9c6)(label(Small))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 57f96072-51dd-45a6-8a75-1e3c5b61f249)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 01577e64-bf58-4a7f-bd69-f78e1fc244b0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d898b799-4ca6-4d0f-ba7a-05f5457b1f3a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8f4217dd-d8cc-4ebc-a8be-6681e68928c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d68dff3e-1a30-4cf7-a591-82353680b3a2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dabef24a-a341-4bb6-ac2b-11634c79d0f4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 98584257-10e7-42b6-a45e-7c7e92a94153)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19c2c0cd-dc53-49d2-ac55-69c81f10a046)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4c7017d0-6c48-4ee3-9e52-333862ef0c21)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 440e44c5-ed6e-4921-b99f-f2595612e93c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0f6e0333-dac5-452f-b3a4-d45adad5af85)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 90807928-26cf-4f5f-806a-9164725c1556)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0e8a3461-af04-4423-b651-9fe8c006f323)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 50404f5d-c653-41c6-bba8-42e228222987)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d96dca89-99af-4d53-a42c-c655ff25b297)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0ba9f9c6-47c4-4b3a-a90d-60ee56bee461)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c6f76877-8139-4175-b824-ea94b3c86e5e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f6c6001-5a0b-47e4-aa39-5153f95cf4a5)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 dfab673a-1f3e-4652-9586-549617a9401b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 41d055f1-a91f-45cf-9c01-c3b1e2db1161)(content(Whitespace\" \
                 \"))))(Tile((id 8c8bac84-8384-448a-9df0-cfa8bce1af0f)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 1acf9314-5f5b-4fb5-872d-0343ea92d419)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 08897f29-c87f-481a-8c6f-a22edb0788ba)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 92cfe191-ff9d-442c-955f-5f995271f866)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9810fba7-a0fb-4af2-9cb0-dd4b84e377ae)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 66ceb841-6611-4337-a31a-12a5cbd24089)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9c2b5104-6ccd-4899-bd24-18b19dc8ca1a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ff106bd8-3e77-44e0-8776-98534a59abe1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca8e9c0b-34b4-4bce-bc7a-b389e315ddae)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 37b16295-00a0-4622-a632-7147c389de04)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0c26e47f-4c09-43f2-850c-54878337e8b1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 89c04f2d-1440-4140-98ed-728ab5007f3c)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 374242b0-7851-4223-bf66-05a4ec39e3f4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c8c6ece6-9625-47ac-8268-fd5b088022fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23a2160b-4511-4bf4-93b4-1f683c8ea3e0)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0fcfb4cb-206c-4271-8e01-21b8f8b8a829)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2231f476-155a-4eb4-95b4-2ffd29b80a7c)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 487db073-d8c5-48e5-b123-ba3a2f6db6e2)(content(Whitespace\" \
                 \"))))(Tile((id c76ec439-420a-4861-b42a-8e79930609d1)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 e5b75fe3-89f4-40c4-b9a4-b5cb51fac788)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d3dd54fc-7ffc-4d0c-8dd0-bf7ef6457b44)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e339ef99-da83-4e07-b97c-0ea6eb0bf0d2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 07b8ca66-0b3b-4ff9-96a7-1b6c3d1ee2e2)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 f4fbbeba-e830-4d0f-bc01-c7fcf2c72149)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 798f7692-dcd9-4a69-b8d4-d864b81d9e46)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 ccb10986-e1c2-4717-9e40-f483a023fe77)(content(Comment\"#unbound \
                 tyvars treated as unknown-typehole#\"))))(Secondary((id \
                 8fbfacd7-4f4c-447c-8cee-8b8389804820)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 219161a8-500b-4644-96ca-3cc60f837891)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4d5a6229-7faf-48de-947a-95bf207cae81)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fda91126-1717-4aa9-80d9-eccc25f22d69)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2404d77c-60d3-405f-b281-1780231f4fcb)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 afd302f2-0780-438b-aa2e-cc8d650057da)(label(Bad))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 74226087-3a3d-4492-94e3-6e9eec279213)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 87c77f09-c937-4d5b-b4fc-af4cde8e96c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44d53111-ae73-4baa-bc1e-21b2bcdab066)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b6420725-968b-47e3-8088-b6f9ad69191d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d1463fc7-e198-4505-bde2-3194a4b9f059)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a40251c7-b8fe-4d13-88ab-7099f374f1ea)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a67ce1eb-c586-427f-8760-73d05c4a0868)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c6ac5ea-9314-44bd-b505-3555f3fda0c6)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f27a6e17-4598-4c77-97aa-b23ad5c57c30)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f4aa276-21ab-4df9-837d-47fc7f5c2329)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 790d9673-2597-46f1-a7a8-ca126695ca31)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 be418afa-4704-4ff2-8fa7-518de2ea1b7b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1163438f-7bc2-45f0-ab80-d831fe6e89e9)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 96b31255-b1fe-4de1-9617-e368995784f0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 25e19ea7-ad9a-4c94-ad6f-afa03f50fd1a)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7199f788-c3e2-4104-957d-9773562d080a)(content(Comment\"#non-sum-types \
                 cant be recursive#\"))))(Secondary((id \
                 93fdc15b-1031-418d-843d-6337df2d79a5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 89d68700-afb1-4562-838e-9db767e0c312)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a4301ab5-ef0c-44b6-a261-a7d4e497d257)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0e88a3a-e5bb-42d0-a432-76e6acaf6d68)(label(Lol))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 47321d59-c948-4d9b-bc9d-3275566f8c45)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d19fa033-9912-429a-9ca4-5dd4838bac64)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab3fb279-a3ca-4074-81dd-0ae7c80527e8)(label(Lol))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c293d6ff-8104-4f4f-acc5-12d691fb5ac9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f93712dc-73e7-40ae-8a21-42d313da2ff0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6f1c746b-deaa-4270-bf25-4c85957dc2a5)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 405eced9-cd91-478f-b4e2-de6e57f425ba)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b5021224-ac6e-4754-9f51-a2bddece86e6)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 29d254a0-4e1b-49a3-a099-668728f1c89c)(content(Comment\"#no \
                 errors: analytic shadowing#\"))))(Secondary((id \
                 686f7f49-42c4-4b2a-9a23-ee4512165db0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1ff64461-15b5-446b-a76c-24c13d5d444a)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8edfaff4-4919-49e7-9e15-8520f5b09f76)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1becea0c-d28f-4f43-b256-9ae031247317)(label(Tork1))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 3f451423-322f-4a03-803c-a9fe4469059d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 149cde0a-a955-4583-bf00-ba75531fb9a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 525b3590-dec9-42c1-a039-3a89cec2c3a3)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9f71a0e6-300e-4185-9579-11f23a8c84be)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 fd7ac165-1225-4f0a-a44e-9e63673f88ee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 25c9763d-04cc-4384-8fa3-1b7bd69ed0e5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 16d2afaf-e258-4525-b753-8c2bf7f491ff)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a83010c6-6c5a-462c-82c0-61ae9e2321a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30177813-493b-425d-b6b9-0882ddc5599a)(label(Tork2))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 2990e467-ad88-4071-be30-5931c90b96d2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d591a449-b5a5-4b2c-9816-7b60f1319af0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2572cd15-f6fb-4b7a-8d0c-3359281fc897)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 daba6aeb-2bd4-4768-9c28-a5197ed2e69d)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7c56b1d3-71b0-421b-9093-f54733ab5a14)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0a54f1bb-05ab-474b-894c-0b29d1449cce)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 30114535-b613-4192-b939-eef1e5ccb79c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2a304f33-da99-4aa3-979c-e747ef575d92)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 81d70396-09a7-49cf-b791-a13e263e658e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35f31cae-e178-4ee3-b131-a2e4e9d05c3c)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 21d5be43-0bd5-4b04-9451-cd49e298eb43)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 8c464058-23fd-4d6d-8989-199cd73fc794)(label(Tork1))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f917dbed-94bb-4c09-9661-5869bd3c2f73)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c883f5c-19c0-4005-8d64-7b957c3875e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29ec207b-0631-4208-95c3-fbafcfd0b834)(label(Blob))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ded79c28-cd8f-43e0-a245-7b07826d1f68)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9c15d35c-6b79-4da6-94be-31c638ff9d74)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4221f40f-9932-4357-9991-6036e9995b01)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 fd3ef32c-2c79-4426-ae9f-fc1167b82b4b)(content(Comment\"#exp \
                 tests: happy#\"))))(Secondary((id \
                 adf5313e-1df5-4df1-864d-9b7c5a812667)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d4348036-dc0a-4680-bb8e-cf0047c0940e)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8086acf0-34a1-4686-be8f-26863527daee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c5aecbc7-c328-4ba8-8cd9-9d7a00e0cb21)(label(YoDawg))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 7b2e209a-0ea1-4915-9cf2-f3c2be5a2879)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 820a2676-823e-4bc6-b300-5214aa0789f1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d15001dd-0eae-4e73-afd4-482ab7a3162f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d01ffdf-6b97-4486-8ff9-044f5a9b3022)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ac7e5081-6998-4cb3-9c1a-c40837a12f5c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 99b838e7-8c34-4a56-b293-1b1ae38a1049)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 0c38c8bc-db43-480a-841e-10779cde50a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0edf54b-6bef-45ba-838d-7403aadd4f27)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5cab4b3c-9cb4-4ab2-9d42-924eba8f3aa8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e51124be-f1a0-49e6-8574-38bc09feb291)(label(Bo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 64254a44-0afb-4267-b67e-45209d5fd9e5)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 5ef14ff0-0634-4e9e-bb53-8288e942193c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 e9ac6fe6-8a71-45a7-8da8-1f5f90f28133)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 155ba47c-7565-4213-8397-d934baca7e3d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4037ef97-2006-4a44-8e3f-52e9f386789e)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 c29d5af2-a7e7-4eae-80be-e02c08500412)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f193a595-7f99-4b7d-9d65-7d8e43ec39be)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 72564c9c-d9d3-427d-afd2-4ac8076568c3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3f1708db-324e-4323-b079-049b8b24bf4d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 39838558-510b-43f5-94ea-1123d44a4893)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a9543098-44f2-4343-a935-103e66129cd6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4bb3cc6-2f5c-4aa4-b759-64f98efebc6e)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a952baf6-5bcc-4bb5-9b5f-ec904f22493d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c05f234d-1302-4f88-a783-8cdf34d3d40c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41e38c8d-fb14-449f-b631-c92243735859)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2af715e2-01fd-43a7-b51f-11eb8b512622)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7bc5dec7-96f1-4cf0-b1c0-4656eaea8caa)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 acc97d46-0081-47cb-8390-cf07cd221dfd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 faafe673-8162-4792-9f78-ef39ad8711d9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e440a940-5e6f-41d0-b461-d2623f941031)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 108659a4-8b68-4b8c-b7b5-6b789ceef204)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c444ede-7a17-4cc4-b1cf-a5c908b81294)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 73c083d3-7732-4eb4-8805-320031e221c9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55113fea-afb6-4ce4-86f8-2eaeea055cb9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 75cc1d9e-47cd-4b09-a995-76d799797482)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66fde9e3-e01f-4432-ac18-3514f5d420e2)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ad6b6fa0-0ed3-43e5-bc22-33ff5e4474d1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c87c560-68ee-4aa6-ae62-b51025662c41)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7c917f3c-12da-47df-bce7-dcaef2e9d144)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dfc92cba-547f-46e3-a1ce-33d261945c01)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6ce99c4d-452a-407b-a6ad-9ee8c8b794bf)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 5cb5dc92-6dc9-4099-8240-b068232de661)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8d2d26b3-42ba-435e-a638-b38b16e28cbb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e09c21c8-8a84-496d-813a-f55700f64d96)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dd2a10c9-a86b-4cc6-9a2f-8b27d8d75b11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 af04d0da-9208-45b3-addc-a29c293f6f40)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 35ccdacb-fc30-4f21-b258-7f69e627d7e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f558961c-a720-4e29-a7ce-0c632b872292)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ca42123c-25ad-4a5a-b973-594607c93d58)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f52df601-dcc7-48bf-b6fe-b341c76aaa8b)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 147816da-20b4-427b-9dc1-ede6c2110c6c)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 357b2aa0-b570-468d-92d2-6875660183dc)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 20b39e9a-6f5b-44b1-8ddf-ce4dcfa8026b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 c8716d17-a9b6-41be-995d-ef7e42e6f73b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ab2471b5-5cda-4a87-98c4-de2f8dabc2de)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f845997e-ab21-4c3b-92af-c65664ab5dad)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 377ba38f-d98e-40ba-ac48-e4fda0ca1581)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7d3a00c2-3da9-4479-a008-90b827278327)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ee221f24-9e4c-4268-a868-bffb24fcadaf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 68ca6027-bc33-4fcd-a466-07a44cdd968d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a70b3bf3-5204-4dd0-acbe-760d495de0bc)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 297e5d36-9fb0-425f-9580-d5fffbb4cc1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d07da15-bb00-4575-ad9f-2cdce9a942e7)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a2a2fd58-2ac5-4b0b-a55b-01646a3b4dde)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34107bf2-3ea5-46b6-aae0-89f6cd092b45)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 113c2063-7bd7-481e-87b9-2aa4059e6231)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b5dc00aa-d47c-4504-b69f-17fb572dfdb3)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f55ed323-bad7-4e71-b23a-4e6453806221)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 2149c166-a141-457c-b055-391325a4286a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b9604112-c199-4512-809b-c28949c61ec6)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 db69385d-7e0f-43f1-b596-64d0716e89cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed502769-4b80-4bcf-90ea-7475b3465f16)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 5c5a7f0e-8ec1-47d0-aa25-88b1a5d2240d)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f786b8df-f704-401c-8f06-03146741d04b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 408c24ca-b6ed-43f8-b6cb-d173502ce7a4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 f6263446-35b6-41a1-b626-78f8b2081d88)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9240e351-39af-4a2d-a743-047f38fd4264)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44e42610-d59f-41f1-9fec-349064f14fa8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9b5de60d-6b0f-4263-8c72-4400bfa1ca8c)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7f9eb770-d68a-4627-ace4-c65c6b0ab48d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2eff9e96-1453-4d9c-ae1c-bf2556b1d509)(label(5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 18fdf605-7889-4a62-80ae-2690a3eba6fe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 17c91e4a-08af-484b-9e62-38bed1629b38)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 daf02719-0436-46e2-9554-1cc670d63753)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cc5f08f2-7e67-4773-b6ed-089380bd1fcb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a84271a-4bd5-44fb-aaa1-91f75980da76)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dcabb994-94a1-4658-873c-3f4e26f8e8f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e45f5970-56f9-4229-ab5d-2eb80e7c7289)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e1af6915-c79d-4580-b3bf-2ec99966c111)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2eaeb051-8993-404a-8188-3f1f0e0d4d28)(label(DoubleAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4c25bfa1-4b04-4d21-b44a-0f03571f90e4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 da448888-f8a0-4e10-8564-68af9da9e873)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71f1c710-a5db-4a42-89af-ca5f7b12e3be)(label(C))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 63858904-cc65-4fd1-a40d-500f743980dd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3876d9ab-2ce6-449e-8b16-33ff4191f02d)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 23806cf2-01ce-4bea-9d88-13e6981f1bb0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 387918c2-75b1-4cef-bd48-acf19e9e2f38)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f1e04837-231f-41a2-99ea-bdfc6e060045)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 52bd8fd8-f689-4946-891c-2291dc031ac1)(content(Comment\"#exp \
                 tests: errors#\"))))(Secondary((id \
                 b2be6e9d-fc09-46bd-aa5b-b4390c4ef179)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4214db3a-ee62-4418-80f2-85552aceee58)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 09711721-4aa6-44e0-bbca-862af2e9ccad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3bcab3e-81d4-44f5-a4b8-e3a2f9e02b1b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b028f571-25be-4657-80f6-de23b1a02bd3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2c29cefe-10a5-4367-ad6e-d6db9b69f8f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 da5545a4-8e25-4438-be2d-1138f4cddee1)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 19964660-f5a3-4d24-a945-ceb4ae1e444d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 502113ba-b30b-48e6-a02b-8916e4099f10)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 923b8dd5-2b03-49c1-b375-c3bd8a5be1cb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 08f65795-5f41-4555-8738-a9d7ba8f4db1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4ac9fda9-dc04-420c-8de4-71f715a8e8ce)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 9cc1e342-4765-424f-8588-b2da5a3c8812)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7f83fafe-e3fd-4d48-b9bd-c981ab1ec228)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2ed49797-a540-4383-a55f-7f997769fcf8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b0d6c8d9-0f55-4622-8540-ad3ec40e8dcc)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 931434c2-9bfd-47fb-b860-4f94fb56abc6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0c45d1da-dca6-4bcf-824e-086ff3619e11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eeec54a7-e110-4085-9e20-e2587dd419b5)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6dfa2802-7123-4ecc-af5c-d24ab138e89b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4871baec-a321-43d7-99d5-c51059493083)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 74f30173-b867-483a-bf32-5b5649f34e00)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f489bf80-e026-44ed-9f51-b40da91947bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bdda38c1-44fa-4426-a68d-90e1e3f577a8)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 fa273c71-3434-46da-a671-626cc6049caa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7dca8b6c-989c-4741-a4a4-f1ae62408a90)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9cb2acad-feff-4672-8d7c-43c467f8e391)(content(Whitespace\" \
                 \"))))(Tile((id \
                 75506b66-e182-491d-b788-99a377d7e3d6)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d53d0f49-aebd-470e-9c65-7d77561bfd5a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 32bfdb59-116b-4f75-82fb-7b7a7a9ee1fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7a5c1b27-7917-43cc-96b7-68c9a0aa125a)(label(B))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6d2f6d4d-5b34-440c-99ef-1b173e491ca2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4265c52b-55ce-4fb0-917e-79f99b4545c6)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 126bad8c-fff3-4a21-85ff-3a93fc73af37)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 acc21816-6762-458e-b052-98fb57fcd6e8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2e7148d4-ec87-4c83-a879-b1cb5a5ed1df)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 29738cc2-dd27-4ee6-bf83-f0d00a16d47a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c9c142a0-b9d1-423e-afa6-950070639487)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9ca198f1-a4ab-4e6a-b9ee-7af24b2b0ddb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 17ff72b2-b93a-4051-b699-dbcdae7307c6)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 bc97a62f-b7e1-4ec3-81bf-83184a4c8c87)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fec091dd-0e47-4833-ae00-53762c786ab6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 62922425-2203-4454-9523-f8fcbeb03c9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f82a5fb1-5509-44b0-bd40-44313f75c4c1)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 3161a5c3-aab6-4b37-abdd-0b4545799278)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8b7427f3-fa5b-436f-92b8-7d8c6cc0d036)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 89074e55-142d-4e4a-a5e8-7a442c699a0f)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a0d5f29a-cd3a-4686-a1b5-09113a40195c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 af4c6438-89a1-4132-9ad5-c4ffac91fd1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c580e9e7-7d6e-4e80-82be-25aaca79b942)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a8c5832b-cebb-4c6a-92e6-07b5e552505e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 631f4c3c-a5f4-4edc-ba88-8af7bac59d7d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c75d5e54-515d-42cd-953e-a5b2af960eff)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 8e89e058-3e4f-4213-b1b5-21b75a12b3b0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 092d401d-4681-45ca-a417-469e1b9521fd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1f09dee4-0e3a-4be6-95dd-bbe0e77c4996)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bba1db7b-a310-4067-a50d-809d39548a5f)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5d32cae1-d451-4c5e-827e-5452b099b557)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44e0dc21-fd90-4f29-a33d-2939e59e8c1b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 947af2f6-7b71-4b9c-9fa8-0dad02fbe631)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33d7eb57-0fa1-4227-a093-3c10974b72e5)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a1bb260f-f11d-456c-8bfc-75d85af29ea0)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a80caa08-c6d5-409c-9047-32909e2a96c8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 10e0b8c8-f01c-4ea6-9a00-1da27c35bc5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d4b9fb5f-a627-4acd-99da-4ff3a79d4782)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 58f18f4d-3c5c-4dc8-9cf7-eb7aca80bcfc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0e6f2c44-e90d-4caf-9e77-11d74a519530)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 49e4c7bf-e9e2-41bb-8f89-52a403aabc6a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d8476ff7-c2bf-441a-8010-64bc558158dd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e0afd264-4a2a-4076-a01d-578a8ec5cf5b)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 7f48028e-59f6-4128-ac3a-07f1fc61b14a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 76de4e86-0d10-4065-938f-690716ff4bff)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6a975e3b-202a-47e0-84d8-ab9d28b36ccc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 775aba62-6b92-4402-a930-1a8b15b99f76)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 43f8939b-a9a3-47c2-919b-c24b285fd642)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c4ae5b8-87ef-4f46-9482-cae3eb81109c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 316d061f-b6d3-4f58-812e-b7e4e2e1b221)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07bd9186-3e71-43d9-beb6-397adeb4ed79)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 982bbf83-da17-4f60-a07a-69b6caca0038)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a61b9df0-6312-4630-b350-b179a7a9a30f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f6598e2d-13a0-48aa-98ad-3bba23c0d16c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 030259ed-1473-41ff-a554-df49181075a9)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6015f8df-0de0-4ab4-a83e-5846c4f017b5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84f01a2b-fcb7-47fb-a22f-d967376f38e1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 83aeee2b-8f73-42cc-8897-cca9982f73a1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 81bcef3f-ffc3-415a-8b6c-a91f427a7934)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3ae4dd0c-cdda-4e6f-9ddc-7b71bf749d81)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 5b4942b1-ead9-41cf-83e9-330b23a2c28d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7a0bde0e-fe2e-4c7a-8fa9-7c59d491a73d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 facdf460-efbd-4a8f-bda7-5328fbb74d3f)(content(Comment\"#pat \
                 tests: happy (but refutable patterns so \
                 weird)#\"))))(Secondary((id \
                 9672874a-6ef2-4808-9a41-030964442d7c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 87478aa3-bc82-40d0-a792-6c51cf93c3d8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 19b5021a-5029-4545-a177-18d27abb34f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ebea4bf6-feaf-4db6-a7fa-3147fdb83b21)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3ab932af-9e52-4585-bb64-81ed59f509c1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6e6d5123-a846-4667-8ec0-1b27e6e2ec6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 028c9ff6-4c62-4be3-ba26-494728ae1efe)(label(Bo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 61354972-9371-4232-86a2-e92f1d27a98f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e83fe24d-38d2-44d1-aa05-3817739b773d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2b7f6a39-b9e4-4cb2-99cc-d42e1e4b4aaf)(content(Comment\"#kind \
                 of a weird edge#\"))))(Secondary((id \
                 b56d91c3-10bd-449c-871d-be476400ea3f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9da4f6bf-678f-4b02-9ac2-4925236ac534)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8a332dfe-bd12-4417-8093-292b405b2b78)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4934e3fa-894a-4c64-b751-47e621d2a0bc)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 32edf020-9d2c-424a-b69e-2bffa87a176d)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ac0a556b-6a81-4ca3-9d80-1110548c7c60)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 ddbf95e4-acdf-4db1-8a31-a52f98d75fb8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a4ca8e82-2d64-46dd-ae43-dfd45f37faaf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 285005da-5608-4b20-ad60-34aa36ded1a5)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 89b853be-6ea8-4173-8737-247ec8f3dd37)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cf4b208d-9082-4bb6-9f2f-3868800212d4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9b875fb6-0c4c-468d-aa82-18749ecbe1a9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bb1cdf42-36d6-4749-8f12-26f768697db7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f2f1e474-be18-47e5-a81f-ede837a58503)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 37471869-cff9-4e2c-99c0-cbbe41031313)(content(Whitespace\" \
                 \"))))(Tile((id \
                 17306267-01f4-422b-86f6-e90290fc8340)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5a398483-25d9-4d05-af57-c3add5818c4e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 743a1786-64d1-4319-a7db-e5def2b888e5)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 f4913a6b-385a-4d2e-bdb5-902204fc96bd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0e0c4ecc-2cd0-4d3c-afd4-479780fdf901)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfca73b7-2533-4426-8b6d-967344d52775)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ae231901-3e7e-4334-954e-be65ef71da82)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 048dc25c-8c5d-4fc6-8674-b2b85230ef7c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 04b01334-7bd7-44ae-875e-f4d004293df4)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 470e3d11-7f3b-40ba-8882-9ecbef346aa3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1120437b-69c0-4271-b846-5560a4c64ce2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c00fd96f-d9b4-4d7d-946a-794a850b31f5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 603225be-d862-42fa-bca2-d3d5898f15d8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8d653abc-56e0-4e76-976a-e8abc021d345)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ba4a7022-18d2-48fb-a0b5-8624b0252a05)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87220d72-4069-495a-a94c-a2a1e6c3f80e)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ea68f412-bb42-4e52-8533-e2b2876392fe)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 7d1323f8-b578-4b93-91ec-2273541b966d)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 a7042e19-faef-466b-949d-be90992ec96e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dcd5a0ab-43fa-4aed-96b0-4a704f143233)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6829f579-c85b-430c-9d47-307edf992813)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 d9f466fe-3676-4684-aee6-870eb758fee7)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 c9302141-725b-4f6d-bf9d-b3fb3f237e0a)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d2b79cb8-a093-4f5f-b1cd-e8a4e3f5e3ac)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 6fc2c1ef-de22-4fa2-a5ca-eaae9a0dbd60)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8956dc3f-1f16-415a-9565-91cf792b6a31)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34979d88-fd64-447c-ba5f-c90d9e1be29a)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d6145fb8-ec69-414e-99b4-d1bb527f22d1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b3bb92a7-8f3c-49c7-955f-adbac8cd5a34)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 74397f51-da57-40e2-bb99-f9e9c241666a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 99ee253c-d3d3-4ac9-b37f-a5ccceb64cd7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 69ee0c5a-b23f-44d0-8bfb-62b8267da47f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0d21753e-2df4-473a-bbc9-12b716724f3f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0ef0fe1f-8d88-474a-b172-3b5ec3cb2473)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38ca55d1-d269-487a-a9f5-8e7a0b7e5446)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f1d45f10-46ee-4276-96e5-903c3d4b6b3f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 59227245-38f5-43b2-b1b9-29434daf00f9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b59a80b2-6ed7-4ba5-bb10-8dce157ceeca)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 99f4dc6e-ecf1-4532-8203-078a97d19f10)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 27761368-5a50-4e8e-b8f0-41259759a1a2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e5fe4f26-2152-4c3f-846c-711489198670)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f85b493-700c-459d-8626-bc8f043093a8)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 42b17dfc-8cb7-4156-94e6-ef3f49b5388f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0005e1fb-699c-4d2d-ac84-3967e4c3b4c9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 99be389b-359b-4e3a-b5f1-434da0700342)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dcd15daf-e021-4d15-af44-137ce8d9bdf3)(content(Comment\"#pat \
                 tests: errors#\"))))(Secondary((id \
                 c1d20c23-9d73-428f-9de4-d2ce26b62ad3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f7043c28-61f8-4276-a523-22734fe2ccf0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 16ce1daa-556e-492a-a19d-6a7d6c4f2e40)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e134eb5-90ed-4db7-8884-b94ec7e43566)(label(2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d0b3234b-c864-461a-9860-dcf461d1b332)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 0ba9f9c2-a06f-4d45-9fa9-52f59e56e3cf)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 5ccb3070-1bff-480b-83b5-976c5b0ddebb)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 627509dd-1b5b-4b67-b157-8ff4a6c7bd79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c8b196b-845e-4d71-98fc-c11abb4fe871)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 232ebf71-0266-4aae-b171-c3d7291da09f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0db41a09-1502-49f5-9212-3418316cddf3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6e676ae9-c84d-4607-ac2d-b39dce08c6a0)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 a5d07dd2-c839-4a86-a27e-ac069da4196b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2261eb15-0137-4a31-a46d-5043f25d2af4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e85ffc30-fd07-4215-97f6-cff1b2ecadd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0f625d9f-3a8c-4801-b7f8-02f3bc063e60)(label(NotDefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e273c063-be34-4fd4-9a35-b903acc3a802)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1a6295b6-34f2-4fb9-bef6-651891713eef)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 0f61bbef-58f7-4b51-a327-0bcc1a98908d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9cb5a580-8a59-48ff-9d23-87f89f52c545)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c2dd289b-773f-4558-89d3-c71533b1918e)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8711ef21-fb8f-4896-ac90-d8e807c603d6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9ddcfd63-e8f9-4897-906a-ce1835a858b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 225f955d-f886-449c-9169-21343059976d)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 a0f66cdc-ced3-42af-bc8b-95ded65e92cb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fc859cc5-5a32-4e42-82be-7ffba942d78a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6ee1493e-7726-4647-96d9-e324c567637f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd9d06db-d1b0-4ff4-be65-eb761215eeba)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9dd81776-5dcc-4238-b329-d0e580eb7fd4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 919d5599-af66-4117-9e83-75d41dcfd62b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6da4a8d0-c40a-433d-9cde-44badc3f8c39)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a9d80421-dfb3-4924-b95f-f6a7ee7088cb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 40ef07ac-93a1-4395-9330-c4e072117398)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 309c082b-63a8-4b11-8aba-2285ab9d193f)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 0ae6873f-533a-40ff-a569-7ce95ca44dd2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 faaa3700-62e3-48e8-a99e-e02391eb2d1b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 04510299-1775-44ea-a1cc-92023d978c25)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31056011-4c43-4c4e-9e3b-fd949414c4ba)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c47761a7-9949-48f4-8e22-1815b341ef77)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 cb6a6a22-b518-40ad-9e76-8cda4bad6e5a)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 f47230bf-0ec9-465d-acb3-436f7d3ac327)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d44997ac-687e-49bf-9dad-b882ecc68087)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0377840-0185-4f2c-a48d-864237d3ac9e)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 93552a27-48fd-484d-86f8-b30e33acc3eb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fe0ec8d9-e77e-445c-8535-556c6adf09a2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 44532396-e223-4a97-9bd0-005fc9dd24e5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2a27fa8d-a1fa-40c1-a553-5a6f8b9a530e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 006c55f2-7a22-4df0-9914-a74f44528b25)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 ff7e8d0d-1807-42b3-8e01-33cc58484288)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c5929ebf-a9b9-4639-97d7-d45df8ce563d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 14377e51-1ceb-4217-ba71-24186a229e16)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b7ca14d5-2c58-4ed1-b454-d77b649a3f72)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 73942125-9603-4035-b063-cc8f9ce8e8b3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bf2e76ca-ac2d-43c0-a4a5-32e199987375)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1fd18bd-1abc-4a6c-8cfc-957b3167497a)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e1bd4b1c-a5a5-4b76-a8b2-c51089b8635f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 19c5cc34-ee6d-4ad6-81c8-02a31d41368e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6a966cc-66bd-4799-9f6e-16b8efa36237)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b57f4012-d255-4bb8-bc8f-c0ca8ed64c7b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 69b99e6b-7806-4f7d-a721-c4aeac388080)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 56b02ec3-8d51-4298-88f0-5572b396b603)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1ba0434f-fdad-40ce-b432-f91c01300a1f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 01433e43-66b8-477b-acfe-f3101dd678b2)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 43c8e876-a506-476f-a6f7-0e9ce18b22c7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 01a1d3e7-079b-410e-81e4-6c0b19bcecf4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2539a505-366f-4f35-bc49-dd92fa17035a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c8d4db8-d8cb-469d-a818-8f06c6a65576)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2e124a6c-1a98-416e-9c2d-a31b456af5c9)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 06aa313a-1705-4684-88c3-0959c3456677)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 c0eb07fc-ca46-4c7b-bbe1-b437ed8e4f82)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3cbd9746-08e3-461f-8e59-bf1188c06a79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4295854d-6f35-4530-87ad-d5056108d538)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f83eeec3-4a42-4349-9f82-9a23e4661390)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 56ce20b5-fc60-4b53-98af-baa07854f2ae)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5e99023e-fb6b-48ee-afb3-b79638d7fb38)(content(Whitespace\" \
                 \"))))(Tile((id \
                 abdf3b80-2a39-4b01-a168-20c5e1b62e3b)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ec5f0ad6-fe8e-4470-9a18-2aaf51bbfba3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 89289d11-4938-45b7-93c1-8f10c467ac12)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7b8f1c99-37f4-4758-95a0-daf1c8f99266)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 02b2a52a-b5f4-4e41-80b7-a58fe6ea9585)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 500da527-7942-4a58-8238-b0c358ce8b91)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7f530aaa-806f-408a-b53e-c4f023e17eb2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f89111c8-e5bd-42bb-8a23-4692cab1d8df)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 758ce654-335a-43bf-87e1-4cc3ef54f7a6)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 8b69bf15-b0cf-4810-846a-29593e01dd15)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 9d1bab07-4373-4f1f-a03c-25097e994811)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cc5e2c3c-0713-4cfe-9344-a925edd6c853)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1084ff97-4032-4a02-be04-a951d295cb8a)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 085cf9ea-0a85-4760-a949-dc22490dce25)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 c8a2b1f6-e183-4841-ae4e-b609d91b7a3c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 930f5a73-f1c0-476a-ba8c-8a113bc9669a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a5b78531-9d59-4865-a4f0-23232aaad93d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ec7248c7-90ca-447f-8192-89917f308bfd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ea16184-54d7-4ec4-b8c1-0a7146937b23)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 161d2a9b-370b-426a-a206-fbbaf8959b85)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cc4bb669-3a95-4bd9-bf9f-8290ae158a85)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1a9b1b31-8fc5-4c30-b7a8-f4842dd8e199)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eb20e134-9a53-4f56-ae7d-269ee258279c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c248cce3-ba8f-435d-8a74-cf066d9e31be)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 13e9a30a-2b86-4f35-8b75-a70f09c615d8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5a2fdd2d-99ff-4488-9a13-d03a37eb4f65)(label(\"\\\"Thats all, \
                 folks\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 54a6e50e-2bf0-4f4d-a156-5746fe45d59e)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
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
                 + C(Bool->Bool)   \n\
                 in\n\n\
                 #incorrect or incomplete type definitions#\n\
                 type badTypeName =   in #err: invalid type name#\n\
                 type ( ,  ) =   in #err: invalid type name#\n\
                 type   = badTypeToken in #err: invalid type token#\n\
                 type NotASum = NotInSum(Bool) in #err: cons not in sum#\n\
                 type Bool =   in #err: shadows base type#\n\
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
          ( "Basic Reference",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(((Secondary((id \
                 c02465e1-d580-455a-aa60-b6aeb9216493)(content(Comment\"# \
                 Hazel Language Quick Reference #\"))))(Secondary((id \
                 eac6ad58-e3bb-434f-9db0-2e8fd6072393)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8a5b7f9a-b19d-4d34-9d0c-c880eebb5d39)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 33151d9c-2446-45f8-a398-c06e4328a468)(content(Comment\"# \
                 Empty holes stand for missing expressions, patterns, or types \
                 #\"))))(Secondary((id \
                 c8cc13c9-440e-4c52-a8ef-429a39de48d6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2fffac84-5d83-47ae-b058-6d237944ec5f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ca102bc0-c98e-4779-a3d0-29482db11528)(content(Whitespace\" \
                 \"))))(Tile((id \
                 23d28c28-f709-48fd-80a2-91a1261c65a9)(label(empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a19670da-602a-43b1-98dc-be30daf8027d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9be33140-aae6-45d2-b3af-d7236ae2fa80)(content(Whitespace\" \
                 \"))))(Grout((id 10150851-d9f1-4c1b-88c1-6eb9cc5ef8b3)(shape \
                 Convex)))(Secondary((id \
                 2826cf66-55bb-4b97-8e94-d11a05b82536)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f1ca0924-2102-4d29-a917-84ed940bed3a)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f8475082-76c2-4eb8-a3fb-647d9045149b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 23d2e2a0-ff71-4ff1-b0a7-97f7ca53bfde)(content(Comment\"# \
                 Integers #\"))))(Secondary((id \
                 c0b7cecc-18fa-4e0b-a69f-1f1fd0f4bc77)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1a5bb703-4fc2-4a12-956f-28017ffd7729)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6fb983fb-2e59-46ca-968c-ca1e8977e0eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 acbb1a16-353e-40fc-b2cd-1e1e5fbf323d)(label(int_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 cd520e4a-49ec-4ba0-aefa-29ea494ce3f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aad44b6d-55b1-4e71-9a3b-0dd9a5398b50)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 58477ce1-301d-412b-b1f7-1ac9f7aba4bb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee9010f8-c67e-43e5-965b-e4532e62cbdc)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0a73b46a-1d11-402c-b8d0-f9fec6af22ea)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 586e8fad-120e-4a52-929e-85b9d8f28b1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 062a379e-884f-422c-aa61-721b97b3e20a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 425a6af2-5fc0-47ea-96f9-b5f92c2c0957)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d1e0b19c-3d09-4eee-970c-3b50fea7d15f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 aba91b6f-c7b4-4dd4-a962-0954aeb11b3a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5942a084-27cb-41c8-8048-c0c4c6fd2532)(content(Whitespace\" \
                 \"))))(Tile((id \
                 753c2bc1-4dd7-413d-b35d-754a16eb667e)(label(negation))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1d10b712-3445-44e6-ab94-6502ef325682)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7ae5f68d-af34-4e15-8764-257a5a4d685f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 91a73ee8-d7c2-4390-af1f-5e8b8f74c4ab)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 2))(sort Exp))))))(shards(0))(children())))(Tile((id \
                 52032473-7237-4de7-aaef-1572d21778d4)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9fd2d207-8aef-4b51-985e-6b4b0f85cd50)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 35e9fd97-0dfa-4760-94f4-54a56efc7bc6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 994e8699-eae6-4f4a-89e4-e8ad228936d2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a90dbe49-f009-4b7b-9c2a-1c67ebfa886f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe5e28ce-5530-4c62-a241-5d8069f21e4e)(label(arithmetic))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d5090a02-9928-482a-948f-1b616daa5209)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ef75cb3f-3312-4415-92ff-8a8b8cbb4912)(content(Whitespace\" \
                 \"))))(Tile((id \
                 688e886f-8b0d-4a1d-b344-f53a8d213f33)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 696aa524-06f5-4717-b164-62e42ae4b9bf)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 fca66189-a9ff-441f-a9ff-b9a343fa1ef5)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cd9d89ee-03af-481e-8578-4ada75839252)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2baadfe4-7246-4cf1-81a9-3185efc1f88a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 34ef2209-3967-4fd8-bdf8-1d1d4ea84fb6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 772bfc49-b7cc-49de-b663-5c1ad40e7cd7)(label(8))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f3898cfd-cbc8-45c7-92ad-e5324a4fed11)(label(/))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 37791357-7f10-4c88-a855-cf12f41d6a2e)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 79f7ba83-5c4e-4cf1-9a37-730a65fb8efa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 233fa61a-b3a1-44d1-ac9d-468a4da6bc52)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 28c99e4c-2557-4b6a-9148-6dc1ba8ccbbd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3fa533d1-50c5-4996-b74d-deef2a46df2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ad542ed-202c-4fde-aa12-d66033759eac)(label(int_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a59ad00c-eca6-4f06-bc76-8502bc1f09cd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e545925d-3d0d-4ec0-93b5-23ca30e74756)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca563165-e29e-43f4-9be9-194fa465c401)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1305e5ea-5a8d-4d44-8f20-3e61da4b9ce7)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b9ef2359-069b-414e-8799-eb98bc569f00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0175b8c1-ec18-4b46-9155-bf421c7f2b6d)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e71af419-adc4-45be-b7dc-0e7dabc8bb7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca973cd7-e43d-473a-9d3f-575739c51abc)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5497f005-b6a0-41c4-802b-a270309d38bb)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ee4f26c1-cb5f-4576-a06e-777c650fb863)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfa9b54e-06b1-411c-bb4d-cc17c9c15c42)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 98a978e5-494a-45d6-a857-9f8a1120cac7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e47da472-40dc-4850-b65d-c0ecac9b7cbb)(label(<))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1d349a92-ca96-49eb-9e9a-a69c0776b1df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 477c7f95-29af-4c05-9c3c-9158c1de4fda)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c8480c5d-ff1d-43ab-ae22-43754f3a7deb)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 269b997c-d282-41e5-8dab-d322d6d58255)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d107bec-b49c-41b9-81ab-e15c5742796b)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 16314a7a-c788-43dc-8f24-2bbbbc00aed1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 289a01fc-7255-4354-b855-42270b1af4d4)(label(<=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 07533539-c3a0-460b-a50a-cfacd93b624d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d451038b-964a-4f8c-8535-2ff829bf5e8f)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dcf1dbdc-44c6-4b1c-a7ca-a3a6fed31a92)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9a1479e1-d046-42ae-b275-121dcb765884)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9fc4fae9-20d6-4a61-8da4-5336cb74d4e2)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1afdbff4-96e9-42e7-876d-5051adc8fa27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d57fd5e4-cbdd-47c9-ab11-527b431d5bf3)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 39eb1a82-8220-442d-85a8-ffac84f969ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 089c2640-84cf-436f-95b9-d54450d8b6d4)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ce10a8eb-da2c-40eb-aeb4-28663f03871f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cedc559c-d11a-4f5a-bfa0-bade4341794b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8bed2e43-0314-4f22-acc3-acc4d3e8ffb4)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 29d534fa-5a4a-43ae-8f66-3a185856c570)(content(Whitespace\" \
                 \"))))(Tile((id \
                 257e50c9-e66c-4246-9e28-3006b1ebc81b)(label(>=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 559b5241-a336-4121-823b-1d7dcf167fa5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8c70323e-6c0d-4073-a23d-0d3a2d8fdcdc)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8cecb774-93ed-4a93-8c1c-98ce749d0554)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 13fdc7ae-e70e-4da7-b6a2-9666dbaca4d1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f73cb689-d184-4dde-9be8-3a0f61d1d46b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7b53cf5e-1184-49ed-9d7c-4726c8e0b117)(content(Comment\"# \
                 Floating Point Numbers #\"))))(Secondary((id \
                 e12f8c8d-b356-463e-9d35-3f289845d9d4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 10aa8ea5-d891-4d07-97ae-684f3cc0989e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8021f30a-9d89-44ee-8ffd-b1d995c1d518)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f7b293e6-340a-4681-b019-c5124e45c0ff)(label(float_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2f6ed6ae-11a1-4e72-839d-12ca19cd93d5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e432a2f9-7792-4f40-9493-eeab3c60da34)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3ce58267-880a-4f6f-b440-68864aa1efb8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8055bb96-c5a4-421c-bcf2-ccd8c3bd4173)(label(Float))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 0bec24d1-5e30-478b-bcef-3ae4772e8a8b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e49338a9-9ae8-41d9-b647-51bcfc609b03)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4c9483f1-3795-4998-aab9-c3511077b5e6)(label(1.5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ceefb970-0cda-476b-928a-c7d6cda446b5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fc5cd16d-6e3c-4a63-97db-b428ae944398)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b8131072-fa39-467c-b055-ec955668d644)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7a9ce07b-0aaf-4199-93f1-8e736cc615a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed296500-3bbe-479a-ba87-1e336133d935)(label(float_artih))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5d42b5fb-391d-4afb-9d9a-9584ab3c05da)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3659a1fd-5038-4106-851f-99f679d91e4e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e918251-1956-47e4-b049-5ad8ccc7a2f7)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 97dfebff-d305-433e-ad00-96a6efc718b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5b59097-6064-4f40-9a09-f3d44cd5215b)(label(*.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ca8d362f-94a6-4df6-a803-5f248e528219)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d99d9a9-d2cf-422b-891d-616f124b2112)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 377e3e95-982f-4470-b6dd-c5e8132e5e5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b3ce0733-8039-4f1a-b19c-b2e643a6f4ef)(label(+.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 be550181-cebb-4e2c-88f4-56e9ac3f8d06)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9720963b-38c5-44f8-ade5-28d3b93ac28e)(label(8.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 79327816-38f3-4b35-9c61-64d67c0dc8d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87e5293d-5940-49d8-b964-46507d81b79f)(label(/.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a9072680-b79c-4a15-a3ef-3448e283709d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2af4956c-32e2-434e-8b23-9ff35fcbb97b)(label(4.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7741c9e4-a581-44bf-b1f3-196699159b85)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b1bcbfbb-4588-45f3-b5c1-6e622a6c22f7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 13c0ef1d-f81e-4a02-aa26-e2c91e94ee5a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9fc6b971-83c4-4ee5-8432-112dacb86695)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5bc77122-3aef-4f4e-89d3-01002a1696ed)(label(float_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 06f39d6b-8ef0-4e11-9a45-369cdacf2998)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 46124981-91e7-4f5e-ae47-7fc33e3e4858)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ea34e57-cb97-4450-9d60-df6a6524e2af)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4545c34b-9969-4077-96b2-3898dba2acad)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dcf22383-1322-49f0-9596-48309d4bb77b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbbf25af-750f-4abb-b1f6-f521816cb828)(label(==.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 91f1f6e6-c035-4020-a9d2-8c2f419c7338)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 caeb3fc4-872c-4dcc-83ac-803b54a46463)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cfd3c907-699d-48d2-a60f-bad9adebba48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f5bb85ce-27c0-4351-a71c-51a88ece8ca1)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 51283b8c-c360-4468-a559-45cbe3454d72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8da7e8a9-a288-45db-9dd8-aead3ec13f8d)(label(<.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 10048c15-f857-4c9b-a89c-509462d60f7c)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 525c7176-b079-467c-9aaa-c8dfa4d5fc28)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ccb3dbb2-65af-45c6-a35a-dbdb2de55eb0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 882890e3-6922-4f01-adb2-3849a28c6068)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0d952e96-5ece-4b36-bbf0-23fb298da825)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edb705d3-f107-42cb-ab2e-27b45622c9a3)(label(<=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 d845e2bd-f60b-4a23-84cb-d9557b04db16)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6c24749c-9a0d-46d7-b509-290abf75f7c6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9d2432cd-b491-457d-8d3a-ead7face4508)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68b23073-d57f-4327-bb1e-66a2c4a98c01)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 944dfcc5-247e-42ee-9a23-be664c184d68)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f32aa2cb-9347-4346-ac24-4c432d04de80)(label(>.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4438a2e6-cbcc-435b-ab93-71426659fb2d)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 915dae41-1a85-4e35-9f83-837aefb453fa)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e5370413-5b1e-4ec5-856a-635c8b100507)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a2e396ff-220b-4fad-af54-5f40174f68c8)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1e3874d4-1503-4342-aa07-2c1f6c40798c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 517b75ee-a1bf-46f7-8c0b-1bb226a37c27)(label(>=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 c2729f80-24a7-4f7c-bc80-deccb52dabc0)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 6c5b7ee8-f5bc-4286-bb44-b2fd976670d7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dd1eda4c-8e3f-481e-a7f7-b4df78bd7f0e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 48792ba6-48c5-4e97-a8e7-5ad2456a4012)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4701234f-7f8b-4288-a974-74493d1a47aa)(content(Comment\"# \
                 Booleans #\"))))(Secondary((id \
                 b29f594f-41ca-42bf-beae-4c366fc6db0d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b89ffb32-c3e9-4d29-adeb-3403b1e7c886)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d2218c04-1f82-4248-a3bf-94f5563410f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 06ebaa5e-ac57-4ee9-b32d-68006232ac5f)(label(booleans))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a9eabb41-480f-4fd8-b5ce-a2b8132102aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11525367-baeb-4cb8-b43c-ec0dee35fc86)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bc65dad5-9674-4e00-8b83-5d53c42f3f1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0017b84a-a249-4b4b-a9c6-ef0e21976328)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 bf457a18-2af1-4715-a4df-7481611d0eb5)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2632699b-a021-4bc3-a548-16d4c315fba7)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6e417997-cea7-483e-92ed-d3e875bd3c3d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 17c1c64a-dccb-4902-bf6a-1a2849ce66df)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 7f2b15c2-2a1d-4cef-9313-60fb2a87cdd1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f39ac63f-d80e-4669-bf62-fd97661c0065)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5389dc89-11fe-4bd4-bfce-50920c3c0310)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c8ab23c2-f2ad-43c0-ba6b-2ca69043a1f0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 78255f29-5055-4f97-9433-0c7b0a4c6d2e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3a58111e-8c4f-4516-90b0-999ba0db4cfa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b9dcd9c2-f7fe-48c3-a94e-1849355867e5)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 13ddd06c-da67-4e37-8486-85ddeeb8a98b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a09e6f3d-bd02-416b-9948-6ab04eb17da1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 06ae542d-6ca1-4225-9e6c-d8cd47fea13d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5ff510c7-a2e9-4b2d-aea8-4bbdd5622502)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25019522-ee88-47bb-98dc-21be74263f85)(label(conditionals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d186c457-dfa8-49bd-a338-7cd988da6112)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a39c092f-c169-43a0-b94d-7fe57db457da)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ba62a4d9-9cd3-46f4-8add-960015a5b252)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bb015ac9-1e95-47e1-b42c-dbf3babd1fb2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 75aa8a68-196d-4751-a3f3-28526299fad2)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 e92cd801-e46f-491c-90f9-61aa20887bdb)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b220a3ab-eda0-4504-bd78-c8aa5a72c2da)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 de75ac47-32b5-466a-a94f-73665f45f35f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55c42244-25d6-4091-9a4b-5f964b6f7f7f)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 a8c611d6-6b7e-4741-9aff-c6f4966ef5d7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1115e119-2ff6-4b0f-b5f4-a6489526c495)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5b01572-55be-4c94-af8a-59e2cc585251)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e1e9c08a-7fc0-4039-ad1f-f70055edf2f1)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1350068b-2330-4bdc-bcee-27854fc5b571)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38855989-1b25-413c-b04e-dc83ab9f7412)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a7c39806-2f69-4b06-bd77-743bdd88df3d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a604aac2-13bf-4599-94ee-f03612813c7a)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a9b89e7c-7281-4b13-b482-479e2e604067)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 dfee5ecb-7620-42dc-9521-7db3a7e87a90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfe1ed00-63ff-49a9-9768-5e484279aee4)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 165809d9-9678-486d-a22e-b11f342f3ef3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db753586-baa7-4352-9397-9e5da9231c8d)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 481882af-2b8c-4841-b809-5f2b37790bea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9d12b04-5369-4361-94e2-6bee82e2a455)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 37ddd62d-1e25-4f9b-9295-4546f4c6e3d1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f08346b3-1795-4f8d-85b1-2d956b4780ae)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 859845dc-92ed-4678-b675-6eb12df4fb73)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 229d1f1b-c9ba-4442-9d3a-de760c44278b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 970099b8-4c8b-4899-8116-1650ef88e17f)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 712518bb-722d-4c72-ad7f-d5fc8f25d36f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e762e87d-b774-4252-a1ab-c34bea3b6b15)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f3bb6def-0e7b-4c50-8b25-69bcb18bf3c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19dbccf6-f10a-4ab5-a593-ae02caead949)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5c33a7b6-45c7-4c06-b2ba-2b911215afb9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b09f12cc-5e07-4817-9453-eda2f17d0de8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e34a2a9e-3a4b-493e-842b-6660b9f9660f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4698ffa7-5be7-4a30-bafc-9173a684345d)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 fa20e15d-c1c7-4a2a-b289-4e2e63224c51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 769479d5-4a41-4fb3-b311-9694b67ecdac)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e5771d07-cff9-438b-b15a-bfac4e7fb62a)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 6bebf9b7-d402-42d1-832e-d9655757744e)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c81e9cc0-f7ee-4f8a-a423-ccb229c01d90)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 994e1938-e562-4bfe-ac53-ee9870827b80)(content(Comment\"# \
                 Tuples #\"))))(Secondary((id \
                 bd4094ad-28ee-45ac-b9a6-f41706a255b6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5d821910-ebb1-42da-a79c-eee1368d6e03)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 82843ab0-2415-4a11-bc88-1b633c52f922)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c93c847c-7ab8-494c-a6e9-422662c7e8d4)(label(tuples))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9eaf76ed-8964-409d-b6af-3b95c6e5dfe1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd39681a-c09e-482d-8097-b460aba0d9b6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5be55fff-5bcb-4702-a9f4-b7268e24df06)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d321c385-1d0a-4b0c-aaa3-08219a0f753e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 b3335759-3082-40c4-96a7-0bb91ec4b5a5)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 078e0901-8966-4b75-8c49-a4b7538e3c52)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2f09e9ed-8d04-4886-9132-62f7016141ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e840f2c5-8cf7-4c2a-8205-6721b3fdcaf8)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 9a0d92f9-ce88-4739-9542-96da69513ed4)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 84d5437c-c902-4e56-a124-15eb74f4186a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0af23744-9cab-4df9-81d9-6215876eedc3)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f0ead96e-cb4c-4764-99fa-796245a2abca)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 efd7ba45-af47-47e7-880e-3a64aeb9eb04)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b01501a6-df97-4345-bc78-4f0315501c31)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b9b464cd-b0e1-476c-8b2c-6c633e1e483a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 8ac7fc3f-a8b1-43fc-824c-4b6c18070fee)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a575cd4e-ecea-4688-b661-02d63708740c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c7f190a0-f72b-4072-8eb1-8b5c77540988)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9142de4e-e8df-44a9-8fca-6d04fc13158c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 166ef475-8dff-4bb3-b595-1e84944625ab)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fe4aeee1-32e1-4274-bf64-103e3491570c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b8af094-0994-4401-ae76-a1332b3af9e3)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a09e402e-92a6-4f6b-b855-cd8d34161442)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eea23743-15f8-4d76-8892-b00491dd662e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6659b71-12a7-46bd-bf69-a700d2abc4cb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 11784350-3981-454d-b66c-1232d578b152)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 578571ed-ad92-4f0f-af8f-99bb5511beed)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1043854d-79a4-4a87-b984-1a51ba60810d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e380669f-f318-4d4d-aab5-661105700301)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 b1beeeac-1210-4341-bcb7-e5ecea396dc6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1394f1e7-0251-4c39-a51f-8d7151035854)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0a24e707-5aeb-48d5-bea0-c72c22a1feff)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 730775b2-ed6e-4cda-9058-2ab2e381ce55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8c838d69-2f5f-4144-bc7e-d386d25d4f35)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 d587513e-d54c-4c85-98ba-e861d28f5bd9)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ee1e68b8-37dc-4ddb-ae70-11fcf5d15929)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0f0080da-d789-478e-8e4a-6628b9f1bf9e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 554496e6-8ef3-4843-a4c9-8e2def5c7244)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bd865f8f-6aa7-4c07-a96f-1bf7a35d2f87)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 91807c7d-746d-4d3b-887b-11e3d0ab95c8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3300f21b-c939-418d-80e0-c2258d21bc03)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 0c7ee6f1-5cc8-439b-b6b9-42c27efa173a)(label(c))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2e8d812b-b09d-4791-89cd-0254d2ad937e)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a3651e80-e491-4606-a7a8-3f61793c7f50)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e16ad8c4-62ec-472f-99ee-e63f1f5eb4d2)(label(d))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
                 25012974-8c3e-48a9-9788-c95fa9403027)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 400972a1-6061-49cd-8081-fb56d05fec71)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16396d07-b06f-42a8-81f2-d6492bd1f4a1)(label(tuples))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 df60af23-f967-4ff9-bdc4-860afaa2d598)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 89e15fb4-911c-45d4-82b2-69b6c5f841d0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 25dc978f-32eb-4bd5-98bb-9f285f419bbe)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 aaba7119-46ab-4f2d-a84f-7592bef45bc6)(content(Comment\"# \
                 Functions #\"))))(Secondary((id \
                 f44d3b93-1361-4bf0-b905-8a299258389c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 78d576a7-23ef-4ec3-bc22-fc278b11000d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4e1855ef-e374-47ec-97d4-ccfd78d8ac2a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 018babaa-c3ed-4ec9-98fc-465dcafb8798)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e11491b9-4080-4101-bfb4-e494f2143143)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f78cd590-2607-4e10-b542-d8146cde96d1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0173a2d5-2e2f-41f8-bfd6-a073f7748090)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19468059-8f7d-4c76-8c90-85749aa5d436)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f1a3b0cf-b00f-4c14-96a7-8938cb74a2e7)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ec2c7da3-499d-46eb-98b1-acd76e907036)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2d50fd9a-ce13-41f3-876e-8d02f1f38930)(content(Whitespace\" \
                 \"))))(Tile((id \
                 64af5514-8356-4031-a08e-cc595e5dadeb)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7b4a7722-e9d5-4003-b838-4bdefa5caedf)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 34751550-07ce-4ec8-90af-554af3376aea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f20e50f-8b20-4330-9bb9-b19e8ca65a2c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 85852b82-d059-4b20-842c-4901f989b8d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84eda9d6-e032-4d41-be2d-d60a116697c2)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e812ed1e-cde2-4588-b818-e214db723b0b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 14d6d02c-a6e5-4151-a161-01f262720c99)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6d90b837-9847-4018-9cf0-788ea8b1e643)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7ac2f5e0-a88f-4974-9438-3b26b3637730)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ed89344a-b7bb-4ec3-83b8-3ef8d8348fc9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2ec9cd63-1606-4c15-9d51-c385430e1aab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b19e4e41-705f-4abb-a2fd-9478598ccf65)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 c97b3392-0c6f-4392-9dca-37e9bfedf333)(label(m))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 12a742d5-f2ab-4986-ab82-2f77bce7b302)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 28403564-8d15-4a83-8372-bcba2aef2b62)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c37665c-5b98-4ebf-bdbc-04754f00ede7)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bfb06ed6-3467-4b99-81ca-47c3bc03479b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 9b2fe42c-e350-48ac-9916-7ce10c6a18f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8df94600-730b-4e93-8c03-3023049b1d62)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 21a86c61-9700-47ca-b83a-6988d5fbb78b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8891190b-2c30-4763-9b83-da5a512f0497)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab616f81-d96b-48e2-8413-d5de88522b8d)(label(m))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5c095f11-6a61-4769-87b2-7296227ab352)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d1527394-e012-41b5-b514-99e3b2f442be)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e17038d2-e745-4544-82f9-edd70afbe263)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd895bb2-9b48-436f-80df-69d110ca0e59)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3cf62237-e7a6-4a53-bb9e-136fa5da1a41)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b79be3d6-b663-471a-86ab-afa3d5b6106c)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6d877ae4-a7c6-4a3e-ab2b-9a0aa7c1541f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93c6ff12-7ff0-4130-9f29-5a4aa4e48a60)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 77c5849e-5dd2-49d3-a2de-64f3110d7d62)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 65cb9542-9565-465f-90bf-d34da80fefb5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5008fbd3-bdd1-4c2a-a1a2-93157ae9ea18)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 eb863a3e-f0e7-4ec4-8532-4372ac9aea38)(content(Comment\"# \
                 Recursive Functions (arrow type annotation required) \
                 #\"))))(Secondary((id \
                 fe6f4241-8508-4c6a-809f-09e529aeb12c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6021d47f-f9c3-456b-af3f-21beb230e9e5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 71af5914-9062-4019-a727-cf8107020c6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e80ee10-eac1-47f4-ad70-3607d85a55ee)(label(double_recursively))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2c8f5d4d-3967-43a6-8188-9bf00878a5d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba2859cc-929a-43ef-a37f-8d5a4abbffb7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 77c87eca-53b2-423e-a8a5-f15b6a9cc983)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35b83201-eeca-4c96-a14d-054a17a6c158)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 30326705-2cc8-40b0-a555-ae8fd88f3c33)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3471cacc-1f76-4cb7-ad52-c7ebf4b898c2)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d3d4d1ac-3d65-495d-bedd-8da3b3959647)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a5068f9-6f81-4922-98bf-d341ae555940)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7e9e7b9a-3565-4ab3-a609-d03ea47e1b09)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a7a33cba-40ab-4ad7-9f7a-fcde3444a5bb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d6bda773-e64c-4bc6-bda6-e5052186143a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c00e394a-ec9a-4b6d-83ef-402c2249f362)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0306582-db7b-4c38-898e-bce7b292eddb)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1fc1b0df-dce7-417f-a5f4-d5ea3f46583c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c93de8ec-faad-4636-a415-eeaf61a0ab8d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ff02cd1d-7545-495c-80c6-042df0a797e4)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9eb7bd90-5172-47a7-bbe9-195f35ed72c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2054b2c-e699-41a9-9fb3-0ee0fdeff49b)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cbc67dc2-bb04-4f84-8461-14c0629fbbdc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b6785035-f1fb-4420-bb00-3ef83c5d893d)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 10646012-b57b-4e19-8c89-a668b16c98f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 397b06df-1410-467c-88f2-e6cb0c6160b6)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b65c10ff-2e33-4490-bb73-66a08244d442)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b519ba78-08c5-4912-9d83-0c0ca0723291)(content(Whitespace\" \
                 \"))))(Tile((id \
                 587a6001-8bb6-4534-a871-85fa11ba1f88)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 782d7a46-80b6-4ad1-be96-4f3150fadb1c)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 66237ba5-bceb-48e5-8397-3344adb5931c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4eb3b54a-f436-4d6d-9b9f-bf2a67de99d2)(label(double_recursively))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cd795135-261a-4008-bbe8-5c6e68da52ed)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 79419365-8519-415b-b6dc-1adad6e01c38)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c3cfc0aa-8f1a-4bc8-83ee-3b56b065912f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3bee0aa-8e39-41c0-868b-2c65354a9477)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 02f993e6-62c6-4ca2-a70d-94da454ef05b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f2b5629-2925-4b3c-992e-b597a799436f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 76a93d87-c2e4-4b20-b682-29d9c26874f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e684fb6-d094-4b83-a1f6-cf691595c0e8)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d5a052ff-f03b-4776-86c7-0e467b007971)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d7620cd-9920-4a3e-8866-49a03e1897a9)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 209e7f6a-02eb-46a9-ad71-0d1ff589ea18)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 744d9375-ec77-44e7-8abf-e55daef79b5b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f57276cd-1b21-47f9-b0de-79c8a5ccc729)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 21682988-ab76-42e8-8145-cbec7f79d25e)(content(Comment\"# \
                 Mutual Recursion (bind tuples of functions) \
                 #\"))))(Secondary((id \
                 2fdd235d-bc05-4c17-afe4-debffdaa0550)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 795b707e-7109-4d20-ad0d-ed71a5fd5320)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 34636584-d529-4f9a-9899-eee7fc4254e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 468c8a03-14ec-493b-8fad-738ea20890b7)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 24ed2e0c-084c-4768-81dd-18f8a7c0e21f)(label(even))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 33b889e9-edcc-4a96-81c6-2e26ce57c835)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f329bd36-e7b2-4fa2-bf7d-8af168d6142a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 12))(sort \
                 Pat))((shape(Concave 12))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1ad6b08a-539f-4057-a7f2-bc17ac6fb0c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff09f6cd-85b8-4699-804b-68b65b757b01)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 19b2af46-e3a2-4e79-8ff7-14fd27397317)(content(Whitespace\" \
                 \"))))(Tile((id \
                 092849e2-d4e0-4824-9834-f4d102b9fd8d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f6bae77d-566b-4d0e-a72f-37807ea8baef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb642ef8-23b8-4497-bb6c-5e7e19b14124)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 3a249574-39dd-4fbb-b458-d47e0955d0b1)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 15))(sort \
                 Pat))((shape(Concave 15))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 01a3ebbd-88e1-49eb-9f85-67382e210d00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d96c4d1-e4b5-490f-b0b3-b90f4a27664b)(label(odd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5b5481b5-764d-499a-8b1b-88b3a304f6aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d3390775-46a2-465c-890f-044b8941b460)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 12))(sort \
                 Pat))((shape(Concave 12))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 07c9b3f6-903a-4a28-aeb0-6facbfea6a81)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92e70a3d-5e6a-4eea-8c6d-545265822f0d)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b0ece690-3230-445b-9c69-389d7ed404da)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6e392cc-d53b-423b-9ac0-4acb9d920787)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 60350d4d-f9f6-4cb1-b2bf-eabd28761d11)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e78084d1-75d0-4ecd-9c09-82c18a68457d)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 22ef9500-6a1a-4ffb-ad2b-ddf6b3dd7621)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bfcb4fae-e7be-4d95-8063-ee6939d9b08a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 42c2c64d-fa66-4ffb-a9ce-565af4f949b7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 27ca6a6c-4baa-4e06-8166-e8a7faedb9bc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c79048e4-07ce-4712-bb16-b9c6a9b29629)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 83c79eca-ce78-46b8-8c99-adb12bca13b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26ab53ad-5803-4cbd-a25a-b88f44d7093b)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ff69bf4a-d901-4a82-ad50-42ef7511dc46)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0e935a25-d455-4bf1-8bb4-ed3b6e4fdee9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7fbdde16-29a3-431e-961a-62596700943d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c57d61ae-49de-48fb-9338-eba7699b46ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b619f444-4657-47f6-93ad-3762a6472d53)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7d9c7b39-b911-4228-b18f-3c95c6712629)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b475efbf-ea64-4aa0-bba3-05a343505bfb)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 76175f11-f532-4bdf-b4b0-04da417feaff)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29980061-05a6-46c7-ae51-fadb5e0e52cb)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3bda6593-11de-4ebb-a3f2-04fe1273c5f1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 798f751a-5397-43c4-a328-55dea819e2a3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a2ad2dcf-9529-4e49-8948-63d35cb3fb06)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a69273a2-2dcf-47af-8e96-42d81bfc4ad0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6986d4f4-4191-4b9c-b2ff-6ee2e84a7b2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 558b5c64-370b-4bc4-bd0e-fb45d281ecd3)(label(odd))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 37bc5a6d-6d3a-40fd-9f52-fdaf023a3905)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 76a1384c-3190-4081-a831-ef9e6f55920d)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8b855248-7315-463f-b88e-b452d84d5991)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4694a35c-fe55-453a-b0c0-1e105485a95d)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 54bbe267-63c6-4af2-936c-85b0d968d381)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d12a0549-7471-47b2-9a98-3315039b9083)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 019aaa02-080d-48d4-98a0-964ba9dba11c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 15))(sort \
                 Exp))((shape(Concave 15))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ba9dddac-48ea-452a-b8ca-2733098100b3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a92df521-eb76-47a3-8b62-bd83cb10f6ad)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5be4b2e1-4fbc-4a85-b08e-ef0caa32843e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 660939be-3d19-4c1c-825d-a74818cb155b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c286633-1343-4436-a515-8ca3a26ff114)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5a8d6d17-75c0-4b32-b0e0-6a5d5097d5d1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ae62b299-dba7-4fc8-a19f-fddbef3c8953)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57795e6b-d0b7-415e-b723-5afe6a52fb1d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b181684d-e011-4e72-bba3-bfeca88dc33f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ee3433c-9ddd-41f1-8ac2-dd20d29a9579)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 713c9b7e-dda9-4d77-bb51-f5f0bbd727eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 609c9c74-2d7f-4447-bee5-6caba6cc48f1)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5054788f-0281-4c45-8f2b-646024f29573)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe22b068-0daa-483e-bcdc-e0333558634a)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 191cde3d-c5b5-478a-934e-3ef751ed8a92)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8c42c9f8-8e84-41b8-b3f1-f8de8693b9fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46d23158-db60-4773-9924-92108214770e)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8d993695-ea95-442a-a012-1bba3d32022e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c5777d7c-d156-48a8-bcc8-8c11cec85d7c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4fb367c2-05c2-44ff-bdeb-a7b608115fef)(label(even))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 85025033-6ce3-46b8-be42-843aabf8c1e0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 30d0945d-c20a-431a-a654-00138c1d3f8f)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1da0e433-1d84-4595-965e-704d73693902)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ead48485-51fd-4578-981f-26216e9377f3)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6445a5ab-30e7-4191-b378-3d9f83dca28b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c2fb766-8513-4caa-a416-173cf3f1ee3b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3644fce1-3634-44e1-8c2b-6c80621ae068)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c1e2d794-fb97-4e09-ab64-e339bd2b61a3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bac0da86-7b99-4039-a855-e71423c4e243)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 961a93f1-32c1-495b-b2b2-2ace1f642b17)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 78e100cd-53af-4229-ba9e-3ee7e6d32bf2)(content(Comment\"# \
                 Lists #\"))))(Secondary((id \
                 35c19804-5897-4218-ad2e-faf7f4b6eb3d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b4ee3364-3899-4aac-a842-68fbcd05b78f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e96e6634-c77a-4fa5-8d7f-a15284bcda36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 17c3ca0b-431f-431d-9995-32b54a17970f)(label(empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 79d3a3df-8b56-4076-ace5-4126ded2d025)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd6cf356-7705-4477-af1b-2a93bc4c00e8)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7a6009a2-c76e-49ec-9c69-5885da3b52c5)(content(Whitespace\" \
                 \"))))(Tile((id 60c043b0-a18c-4078-9ab4-61163d201924)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 fc97d9f0-c37a-4edf-b433-1965717eb972)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5470966b-c43d-4f14-88f8-742ff48e9cbe)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9795d258-7fa5-4deb-ab7c-ab4ba14eceae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 047d638a-8dc9-49ff-be48-26179ed85615)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5b8cc8bf-7895-49f0-ba87-36395960d433)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d6c76a6f-5bc3-44cd-b72a-e8d8d6055d77)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0cfaa336-d277-4057-b392-b932d0590ff3)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c7fe2de7-957c-4f58-ace2-e05f7e40b246)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c9971c6-9ce6-4ff0-bd8d-6d9964896089)(label(non_empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 56c6a900-7672-4584-b42a-8420648405e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c4331229-6241-4bc7-87c6-b6023d075304)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9c7fd3be-8e4a-4293-8ac3-ab7fd54268b0)(content(Whitespace\" \
                 \"))))(Tile((id 6aba9166-b530-4f2f-9765-dd9d7396454d)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 b4bc8dab-fcf3-4a45-acc9-e8ebaa90ef54)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 91d78a6c-d6bb-4ae2-93cb-3872465754a7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f01f5411-0591-44bd-8710-d2911c719d57)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de00ac61-5f29-4b64-a60b-07be2fbe3180)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c447dd9f-57cd-47a1-8983-57ba22b3c16b)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 7))(sort \
                 Exp))((shape(Concave 7))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 ed93c8a1-82c2-48fe-85a3-7a86bdca69e2)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 032bea77-674f-4dea-8ac1-559fe947f6bf)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 7))(sort \
                 Exp))((shape(Concave 7))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 b2942d52-aa7b-4f8b-b705-6a3c57a67cdc)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d19ebc20-bfff-48db-ae95-185f3efdd46b)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 7))(sort \
                 Exp))((shape(Concave 7))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 9c33b3af-b95e-45ac-b165-77038efb602a)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ded4f5c7-72ae-4476-98d7-6906325de4e1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 715a03d5-5d58-4eaa-9ab5-5b33da845f14)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cf2e40c8-e04b-4711-bcdd-38513d108052)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 11b3cdb3-3362-4f2b-93c8-593710a60c2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2cae279a-a22d-4cea-990b-dee583b7b675)(label(list_literals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 04b8fa4d-70eb-4ce5-b35f-c33276ff9a91)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a23c17e2-0be0-45c5-afc0-45ad662c2367)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3ea34ae2-086e-4424-9508-3498891ff06a)(content(Whitespace\" \
                 \"))))(Tile((id 2007e421-be10-45e2-ada6-742bc2819b21)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 b53a9bf8-7a7a-4fbd-abff-e14d83c48701)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1d386def-e315-447e-b24f-4b86314e0c08)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 57a10c26-224b-4820-b1fe-fe61df094fda)(content(Whitespace\" \
                 \"))))(Tile((id 54d0e0b2-3c9b-439c-8b7f-d8b524bc69e5)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 920d4b0f-1b4d-4f04-88e6-3fe87e7a65a2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5f69d188-7d03-473c-9140-a15f2499c561)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 eec2c6b0-f47b-429a-aaec-d1ef320ee675)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1dfd11d6-3b87-4c62-b2a3-96c4b524bffc)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4ffefd84-9e8e-41fc-80da-263bcf450a35)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 967333b4-fff6-44d1-938e-f7de598266ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db5d965d-a45f-4e3d-977d-56ae69bf0ff6)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c5d264bf-7f1b-472a-ab12-33b1e9c1651e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 15882211-ecd0-4ab8-ab58-6579fc6dfefb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4d1c6d2c-5fda-40ec-a1ef-6cccf71fb769)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 99522f41-83e8-445f-9c64-5888fcd99a41)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b19f467-00f9-4ec9-a59b-b3783d9c480b)(label(length))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 438c2f11-7687-40f1-ac6c-77b52b5ae19a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0a57c206-20f1-4e10-b9f3-bbb7d7586800)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4ece4618-8d70-48be-9c3f-9da04343855b)(content(Whitespace\" \
                 \"))))(Tile((id 06de5f69-5c47-4c4b-a8e9-e412a85b5490)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 7ca778cc-b87e-48e8-911c-958b79e1cf92)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4e580f3c-fb6c-4eff-ab45-b6d665f917b4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4e7f377-5905-4d1d-854a-81a7a8dedaaa)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6dc204ae-c92d-465e-b046-db7ad394938e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2f90cec-a6b8-41c5-83d3-2f145cc05f99)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6058204c-92a8-4b46-b823-4ebd9aeabd24)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 164bd788-aaf3-437a-8206-647a2bc132eb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 18c66069-7229-4d7b-a013-74ec2d67d237)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1946d033-a508-4a4c-9b22-6233aec311ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 644d0e44-e2fc-4fd7-a6bb-eb3306f49eeb)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3710885e-498b-4047-a332-fb855a982b84)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5e52c8bc-9c27-4755-9107-de2867a85244)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 486ec00c-6ff6-4523-9def-90ab86bf90c3)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e5fdbc48-7e48-4c89-8909-31cf2b62cdc0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f1d7ec1-01a6-4fa4-ba9d-85db637c553f)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ce6cdbcb-93c7-43d5-9714-092ea2630cd4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 efe768d5-5682-4768-bf10-c756fad7a038)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e132ee92-6f29-44f3-bcd2-0a21689ea8bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 017ead53-406a-412d-a23d-d21c6313a0a6)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d229c1dc-26a2-468a-b36a-d5eb92d7535d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 36244853-42a8-4b21-8fab-6447580622f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1919b6f6-63b4-4a2f-8411-a49d2b0de0e9)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 117345cb-4180-49d8-81cf-5720bc09a76c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b0c160b-a1ee-45e1-b330-44150a7f263b)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 61f79bfd-9396-40f4-ba0f-cce78ebb2ca4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e59f2cf1-4e84-449f-9cb6-b003d07605cd)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 80c95d1a-b038-48d5-8413-249df02caf5c)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4ffa0ea7-c3d0-48f4-89e1-d508d974d9a6)(label(tl))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c086568f-d41a-4696-ac05-f33a13a3d612)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 28b2008b-39bd-48ab-803d-e3e00d69f67e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7df6c8b5-9636-42a4-9bc0-42d61b5f4631)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 56c79884-7a1e-49d6-a659-ef07c7383eba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d293b8a7-037d-406a-be45-fd9ef543e423)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2fcc2671-4432-40ab-bc0c-085df34a1034)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3cbc29ae-1f9f-49cd-ad81-21cdd6154572)(label(length))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ca76747e-a814-428b-ba6a-d6e88d0b3a94)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 483daf30-152e-4a61-960b-ea2e34e399d8)(label(tl))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 a191cf7b-f477-4064-9c81-eeed31cdb12e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d95e09c9-f6ba-45f6-a42a-07039d5780f4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0b868e0b-e1e4-4ff7-bf91-0fdd16adf9a5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d5824218-661d-4526-99fa-1b195dbd0a7b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fccf51d0-1ab5-4f82-a218-e924c212937a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 947e47dd-107c-436b-8f20-6fc2f7fad6c2)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 f210e7bc-619c-4848-847b-8cd228c43340)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 e8ccaf1d-bb31-4062-b2df-cce7f5a3b7dc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 75d95dc0-f522-4943-ae54-90856201704f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 78c38d95-74fa-4054-93ba-118df77e2a80)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68dc4a80-94b7-45d3-b317-3ce1931ee52d)(label(has_at_least_two_elements))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 42441ccd-6ea2-4579-bf36-6eb359bbd275)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ef2f1b0-58cf-434d-ba60-e84b082c7bf7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e5c0d335-dbd4-47bd-af72-43b9c782cc1e)(content(Whitespace\" \
                 \"))))(Tile((id 4721add5-b41c-46d4-9ff3-b44b1bd24770)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 fdb7d4f9-dd01-4919-b227-f0d3fffc1cd7)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 e14c1521-9da3-4cf3-95d8-59b32257b662)(content(Whitespace\" \
                 \"))))(Tile((id \
                 08920087-d272-4ba6-bdf3-93edaeb021bb)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0c620183-6b6e-4605-8c7d-a07c526a86be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16612644-16a2-4fa7-b22d-b74db2ad9333)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 74a3ee2d-ac2f-4712-9110-ce522f9851ec)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a44e9372-c563-470b-a735-1109ac1dd743)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 961459a6-7b3b-4efd-ae2d-f0e7b724fb6c)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9ee89b19-5be4-41b8-b870-75d5903eee7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2b8680f-34ae-41d7-9885-125463b8ccb6)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 81ee8032-53c6-4792-a186-53e68c504c80)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 81159f4d-fcb2-4c97-bd22-9696c01c3e9c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d183c63d-aa0c-486a-9b47-82d275f02ae2)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6526e4c6-356b-4e5b-82f0-d2f9534c1f1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0389bee6-2ab8-4792-a69a-821b4f610819)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 200c3e5d-86cd-4317-983e-fa56aaab1870)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e44bb1a9-af82-438c-bd25-08eb88f89a2d)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 28c01c20-ac76-400e-ac3f-327a19705d0e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 470c6bd2-f5fc-4523-8a8a-3e77d2ae7466)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1dd29e31-4b83-47aa-a6e0-0e5629837f2e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a2b392fb-58d6-4ae9-850f-7278730f70d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d77b6106-b354-4c67-bccb-4e91e2f4a9df)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6532c791-0e40-41c6-8bb3-5816c36a1919)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1cc4d6e3-777a-44c3-bde5-35748d34caad)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f2709538-ddb1-4c95-a2d4-04f93bcd8825)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3dda5a5-bf3c-4312-921b-2c15dfd824db)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e14d689e-6556-4bbc-a7a9-7e57e71e4f6a)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 aae8d9a7-16f3-4a05-b3c9-2cf987bce74f)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 894e780d-dffe-46bc-9d20-1f46b3d20f22)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 22c0fd84-c1e7-4c44-8d24-32d783b04658)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa21c0b5-6f37-4332-bea8-ce5f16b8b1b0)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 eab907d2-1671-47b7-a390-850ad7261f77)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3e3d0450-5fa6-457a-9ec0-0a3673c9edfb)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 930b7c09-b500-482f-a897-2046cc8df242)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7001fdd3-9677-4c8e-8aee-8584ad908830)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d26ed733-a684-4163-9df2-13070819d911)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 7))(sort \
                 Pat))((shape(Concave 7))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 b38f41fa-3940-4c03-91ad-6835f8b61c55)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 01869da0-f0c2-4280-92cf-9006f82beaf7)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 7))(sort \
                 Pat))((shape(Concave 7))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 acfc3a3a-6e41-498a-8c13-ea99fc347d7e)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0dab81fa-5f4e-402b-a32f-f536b7919b90)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0e6808c3-004e-4a9c-8de2-dfa7d8cc2983)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f515da4-5076-436a-bcc6-3f7eafb1ec57)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e9889fed-8b2a-4c88-ab41-a5090959af3a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e26de303-8012-41b2-b4ea-220f9af7ea76)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 91c4cba3-5758-4231-8a47-289da8a1df17)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8cade609-6be5-4ee9-9b21-4447c6ef69c4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9c89af83-7b53-4adb-8cb7-0907b7734acd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ba102ddc-ef92-487c-b01f-3e1dc2efa6c2)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 33dd8e04-138e-4c10-a0d3-21170aac493f)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 d74ffc8b-d059-4d7a-9e68-c03ea87db97d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 325d816c-fcaf-4734-a327-3c7e241b7d9c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 272ef5a0-043b-451d-a6e4-3f18571d2c60)(content(Comment\"# \
                 Strings #\"))))(Secondary((id \
                 4d0dd9cb-c943-4384-a682-8884ac792b31)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 26109999-748a-492d-8991-dedcf22f3c8f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4b4ec423-bdf0-4f77-9cff-633eca5eb8f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8f2bf699-8a6e-4a4a-acce-4a801b081fc5)(label(string_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 76fa4320-9686-418b-bae1-2e16981ef6df)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dc5fbac0-a612-4966-b776-a5b222ee2837)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb0ff38c-7c16-49f9-9547-019753f2f677)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4561c14d-dc9c-4e62-9c83-2258ba03da27)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8387e074-4d46-46a6-a37c-d10aab222c02)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a6f7d9a5-1f93-42a7-9365-fb6f3beb8c74)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d573bc1e-fc6e-4c9b-bd47-b21d74a42b50)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0a7aab63-f372-4baf-863a-99f536c5764c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41148c0d-9e2b-49b5-be97-c798c36ec89a)(label(string_equality))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 fdbb6a30-47a8-4665-bd2b-709aad04126a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bfb84ac4-7973-473c-b655-71b846366ae4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2e0ac51-b1d5-47c5-9d9e-28671583db5c)(label(string_lits))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c14d9212-5436-4359-aed1-4c12bc651145)(content(Whitespace\" \
                 \"))))(Tile((id \
                 479deac9-99cd-44c2-afd7-83aa87dc31c3)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 92d22bfe-5056-482f-ad46-5ed1c4787fa1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dec5173c-0001-412c-acec-9d1dc2ca4424)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 242b38cc-9ec3-496b-a61b-180d2dab6eee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2fcdc646-7a16-44a7-bdbd-2db157c25c29)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0916a810-eeaa-4b3b-a5d3-3750e01e4ec9)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3f1d1253-4e0e-4331-bf9c-180e141079b2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 29d208e1-0c45-44d2-9d94-99224e7b7bae)(content(Comment\"# \
                 Non-empty holes are the red dotted boxes around errors \
                 #\"))))(Secondary((id \
                 394efbca-78fe-4391-aed8-7cf3e9954cff)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5501da01-0362-43bd-90d2-2f73051ebd5d)(content(Comment\"# (you \
                 can still run programs with non-empty holes) \
                 #\"))))(Secondary((id \
                 83366e95-3f7a-43c0-b6dd-b8b856c15bff)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8ff64362-463c-4a28-b9fb-c5874d923bb7)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 17))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 907bcaba-6a39-4298-be9c-a9933d6bb965)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e3cc5308-0a4a-486b-8505-50d4759fadea)(label(non_empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3869b181-7c6a-4947-8188-34ac4be2e935)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6116797d-7f61-4299-8d03-b3b55489807f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2c2c4658-b792-43a5-8414-1cb5aae4c7c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3778ca66-6e86-4b5a-984e-de82e05a54e1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7406cf71-6145-4be2-80b9-c2a2a292175b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3d632ecf-da09-4334-8577-c8b9ede604d0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f5fb9f60-65fd-4b86-ac03-f79a42e79876)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d9032067-95b2-4bc6-8813-402fb0516108)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1add6c79-d411-4ab9-b51d-581ef672ed1d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 efe99dcd-cd32-4bd1-ba0a-32508d520ddf)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 96260b30-8ae6-454d-8da1-c532a1efa904)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 376a24fd-5524-4558-902c-6fc859f4e3b0)(content(Comment\"# \
                 Tests, separated by semicolons #\"))))(Secondary((id \
                 aa070fd5-bc99-4e35-bf85-988cf2e6f881)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 25740099-dda9-4234-9886-803c1d4d51af)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e86c5199-a0ce-4e44-9ad3-692c435fe93d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d6db200b-52b4-4ed8-925f-579fdf8b2c6d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5f310898-295b-4f16-9d11-b01123b2ea2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e38b972-1809-4feb-8c1c-4f67d652d375)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8ed6c237-4bd0-412a-971c-dcf07e161f93)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ac3b7c8-a4ff-4f33-bfa2-d218822fcd19)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f572a698-4a16-4dbf-a54d-6e6945297665)(content(Whitespace\" \
                 \"))))(Tile((id \
                 132cb08f-c280-42b5-b6fd-c1e85d17ec5c)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 709f0d1f-d997-42e8-bb16-23abf054df5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bafddf28-368f-4c9b-9759-a5e16424a41d)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 eba3a299-0151-4d0e-b13d-0d61577a6733)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 607a9a2f-36fd-416b-8f2a-0dcd654a3f56)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3a0443fb-e5e5-4675-a280-c675d359ed87)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cb3e52f3-b846-49c6-813f-2066ca14e149)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a51bf06d-4934-4799-ac2e-a4652aaaafe7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2c74fe8-2500-4886-a23e-5a5d5155ae8b)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 09d392c0-e2cc-4307-89d7-0483627378b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b85776e4-f63f-49a1-9b09-77c013375af7)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a4c131e0-fc31-4fcb-aa84-b16c046436ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18d3169f-ed3f-4138-b5b9-9610e304c013)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0b7c6984-8436-4b21-8258-c37dd6be30f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c63b9193-5ad3-4d09-8187-b9b13b9a5210)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 be000067-f14a-4cf6-a64a-384a94fa32ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 005342e2-5425-4530-b074-13babb8f58df)(label(6))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 12cf1b4d-dcf5-4070-a839-a70b836df238)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 6dce00b6-58ad-4711-922d-182cb8ae2b7c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3f85c74f-7e59-4d18-ab32-2fc003551cae)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 69c51fcc-36be-4bc1-a336-cfea114e8780)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 45576ba4-4586-4df3-888d-3bfef94972be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ea9e7253-a352-4836-881f-0127b1c871b8)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f3e14f9e-507a-4d26-a2df-78c11523af0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 becaa096-cb19-4568-9e9a-5863d1ae1c96)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c450606e-1ef4-4fbb-9773-d9566f54d260)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60e938aa-3416-4c33-81a8-25f37ef306d9)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cd32360c-d3f4-430d-8ad2-5dbd660e8158)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ac66d29d-6f93-47a8-872c-b26ae92463a4)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f58eeaed-a321-4813-a71a-d99e1684c500)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ee4b755a-2962-4a97-9b71-1fa874175270)(label(5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0efd8178-50ee-47dc-9f5d-f6f503733d3e)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 01eef806-3025-4681-841a-138b95f6f827)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f59bc277-d399-48e3-8d1b-72164b9e13bb)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
                 446ce379-8fef-47e5-85d4-f7d3fc16dd6c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 31a81066-ecfa-49ce-a762-34904947da99)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 77d8a251-42c9-4631-a941-094827a503c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 546b42b3-2f6a-4f08-9e69-854b8e88b1be)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 28d8c0a0-4515-4637-98ca-22e78630cf20)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09d04b4d-2f29-4aef-a73b-badd00299b3b)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
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
                 let float_comparison = (10. ==.10., 1. <.2., 2. <=.3., 3. \
                 >.2., 2. >=.1.) in\n\n\
                 # Booleans #\n\
                 let booleans : (Bool, Bool) = (true, false) in\n\
                 let conditionals =\n\
                 let (x, y) = (2 + 2, 3 + 3) in\n\
                 if y > x then 1\n\
                 else 2\n\
                 in\n\n\
                 # Tuples #\n\
                 let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, 3)) \
                 in\n\
                 let (a, b, (c, d)) = tuples in\n\n\
                 # Functions #\n\
                 let y : (Int, Int, Int) -> Int =\n\
                 fun (m, x, b) -> m * x + b\n\
                 in\n\n\
                 # Recursive Functions (arrow type annotation required) #\n\
                 let double_recursively : Int -> Int =\n\
                 fun n ->\n\
                 if n == 0 then 0\n\
                 else double_recursively(n - 1) + 2\n\
                 in\n\n\
                 # Mutual Recursion (bind tuples of functions) #\n\
                 let (even : Int -> Bool, odd : Int -> Bool) = \n\
                 (fun n -> if n == 0 then true else odd(n - 1), \n\
                 fun n -> if n == 0 then false else even(n - 1)) in \n\n\
                 # Lists #\n\
                 let empty_list : [Int] = [] in\n\
                 let non_empty_list : [Int] = 1::2::3::[] in\n\
                 let list_literals : [Int] = [1, 2, 3] in\n\
                 let length : [Int] -> Int =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => 0\n\
                 | hd::tl => 1 + length(tl)     \n\
                 end\n\
                 in\n\
                 let has_at_least_two_elements : [Int] -> Bool =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => false\n\
                 | hd::[] => false\n\
                 | a::b::[] => true     \n\
                 end\n\
                 in\n\n\
                 # Strings #\n\
                 let string_lits = \"Hello, world!\" in \n\
                 let string_equality = string_lits $== \"Hello, world!\" in \n\n\
                 # Non-empty holes are the red dotted boxes around errors #\n\
                 # (you can still run programs with non-empty holes) #\n\
                 let non_empty_hole : Int = true in \n\n\
                 # Tests, separated by semicolons #\n\
                 test 2 + 2 == 4 end;\n\
                 test 3 + 3 == 6 end;\n\
                 test 2 + 2 == 5 end;\n\n\
                 2 + 2";
            } );
          ( "Types & static errors",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 c2890a35-b3f1-4653-9767-8d5f9752ead5)(content(Comment\"# \
                 Internal Regression Tests: Type errors #\"))))(Secondary((id \
                 090c5ddf-5c26-4a14-a1b9-eab92cb073c4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5a4d6644-ce5b-4818-a4c6-2905ca7b9d43)(content(Comment\"# Each \
                 line should show errors or not as indicated \
                 #\"))))(Secondary((id \
                 49670809-d955-4be4-8de7-a13e0c26ec98)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8b8ae6b5-2df0-42a9-a2e7-d53e2e0517fe)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d69298b9-16ce-48c6-a813-04d4c6acbb6f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 87784971-c398-4a3f-9537-9a2eba847372)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9cc5b09c-65a4-4a8e-853d-62d8f15d4e24)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1e775429-5e8e-4b11-b66c-9cc3c1324739)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 04d76c28-7577-41c5-964a-fc54eeae9062)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51f991bb-2c1f-435a-920a-a20148cbc2dd)(label(unbound))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bf68d8ff-145a-4e77-a735-120746818a4b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4491f5f7-9ada-4420-acb7-fa9e6ec73fe9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 474b7d65-a3a0-4c3e-b0f8-cf2a0ad43887)(content(Comment \
                 #err#))))(Secondary((id \
                 ffe1df31-0a25-4513-97c2-c36fc135468b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ff782a2c-aef7-4a7d-9ae0-848429b11fbd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e2eca969-e0a4-4a91-a89e-878e09aec2f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c035ee1-6d4f-47fc-a76f-94f51898e718)(label(Undefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 dc02b1d3-06fa-4b3e-aa76-9766d01c3bc2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1d42290f-3641-4c65-a280-94551377a573)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b3c4696-05af-4c34-8ab1-515fbae5550f)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b56b3924-e775-41ca-9e61-ac4112c6d40b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c2c7deac-19e5-41d8-9cc2-fe9e73a29960)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fec4e3ce-5421-40fb-96fa-8474e5a9fab3)(content(Comment\"# 2x \
                 err#\"))))(Secondary((id \
                 02f65251-ccbd-4e70-b60e-cbe1e13adc6c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cadfcea7-7c09-433f-8e03-df9e89dfd9e2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6c52236e-4a4a-4b7e-b318-84bd95c543ed)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 557dc615-9e1c-4a9e-a00f-15234e53a953)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13897887-613e-408c-b120-59b8666fc0b6)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 34fd4c5a-2433-40a8-9ded-9c6500142356)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 83207004-3161-4071-a526-cf9459533d67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f5a444b-0006-47ac-bd94-14c9ffd80548)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 503d2940-fcd4-4a54-879d-81d129596ad1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e425406e-6252-47b9-8a4c-c8870d3c1376)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 13255468-04e2-4816-9461-74dce40c9b27)(content(Comment \
                 #err#))))(Secondary((id \
                 9d763cf1-1e07-4c47-b8ef-2b6435de5a49)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d2541b33-1f10-4474-9939-cd2e164493c2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 26b81a4e-e0a8-4a8b-8b59-e0278cb81758)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f412044b-ae99-40e1-ac43-a8785e7a2ce1)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 96732395-1218-4909-b454-484f2dc33583)(content(Whitespace\" \
                 \"))))(Grout((id fdd65dce-3851-4173-8c0d-a8ac07ced2b6)(shape \
                 Convex)))(Secondary((id \
                 3dae5dc2-012e-4f8a-b54a-3562c4f88afc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ba4613e8-5288-4f23-a6d1-7e3206f398d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c873fdfc-ead5-4cab-bbf9-f319e65eb545)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4fc68723-e8b9-4d8e-ac28-3066739c26f9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f36c50b4-776a-44c5-b033-a70e463f1813)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 537690ac-06c0-4b1d-84c1-6d4acc73e474)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a1d0648d-8c62-4713-b170-bc7db0c20504)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5247456c-6550-4072-8cc9-980f61c699d1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2acd1b5f-7510-4a74-a56f-36267b0a512b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7616aa0a-4eeb-4db8-865f-5b61dc1263b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81bfcb19-79c1-4bf5-b34c-3dcc7cfd192c)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4b260629-7c2f-4a27-9081-844c47be8ff3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c71f0361-b8aa-41a1-8b35-ac1d81df2bda)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 abe0800b-ccae-4935-b9f1-0db885b79a53)(content(Comment \
                 #err#))))(Secondary((id \
                 a6632e31-7765-49e2-8b18-3359f88183b2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 890064f6-db5c-48da-b11e-79a014c051cc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9c0e9437-19f4-4071-aada-9b464e0479f2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d821f513-71c8-47e0-91f0-012fd7269dd2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81c8a5e4-c8b9-444b-b66b-29dd2bd98f39)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 955b5d47-5dfe-4140-8ebe-f1cdb5d166df)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 60ba9fd8-2ae1-4031-aab1-0d31582db521)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09d1e608-b3e5-43c8-b5bd-f3986790dd19)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d0d97c16-df70-43e7-8c19-4b2618e3df63)(content(Whitespace\" \
                 \"))))(Tile((id \
                 67304d61-8d7a-4779-84f0-e4d60a495118)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 da91c6a5-b71d-4e9b-8206-f9b83d98dd89)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 979e7fce-a579-46b9-995b-464e1d6d5210)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4728443e-07bd-46eb-a07a-876ab64a9b5f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 317643ae-3383-413d-9708-f183ee4971f1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0dac4f18-67d7-4bca-acf1-50183a763e9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e4f414a-719a-4eda-93c0-baab66a8296e)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2f303db8-f575-4235-8ccd-dca273aae20c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 526338c8-55fc-432a-9c0a-b9b6f877b413)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a9d1100b-abd6-4d20-be7d-a58c5ed507b5)(content(Comment \
                 #err#))))(Secondary((id \
                 a3580f56-6d80-4396-9869-c50ee7735d62)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 50e8b8e5-6ce5-4536-bda6-3ddf728ea643)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a5e847c7-5a2d-400b-8328-943165b1db2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4e3d9e1-8d16-4e74-b645-9e9c10b71d13)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fa1d3aa3-edea-4d2d-afed-e613ec30dab2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 a2bb9040-1c54-4168-9f49-8716cbc12dfd)(shape \
                 Convex)))(Secondary((id \
                 ded9718d-58e0-44cf-83fd-1b6c5a46562d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5acbe062-4da5-47a8-a6c0-4f2ead2bbdb6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 36a49252-eb7f-4210-b283-24e08b053000)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e648f5f6-a693-42fa-8995-b4a20961ba07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46b21c79-7191-46bf-94e9-4fb884c21526)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7d306a94-ff74-4f62-be10-af0f06529c21)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7276eb67-791c-48e7-94b2-240b9d7e3fe6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8ad575d3-bede-46e4-a4d7-f942c2aa8640)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b32b622a-da19-48f3-af59-95accf570e1f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 275151f6-414f-46ff-8927-653979248dc5)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9ba58510-58c1-479a-a9f6-6a70a07df983)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 78eed0fa-3bcb-41a4-b6e7-fa15bbf05d29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5513e260-636a-402a-94c7-c2a8cce3d454)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0ed17b54-7b3a-45d8-bd6e-1604f8af3da3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 72fe8d09-0a4c-4c0d-8ca0-50d5057a9eeb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d95a7a94-7e7d-4841-b9d1-fd772e2d80dd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d8e95474-b6f1-44e0-85a7-ed0cc7dbde42)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73906d81-9d17-41e3-9632-7bf31280bc99)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c556f619-c3cd-4aed-8ee2-92848aece57c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 260c4a39-fa49-4a42-8ca5-f6bfd86d705c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3e638a7-2c00-4098-94cd-05563acab569)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 cd497267-e360-4eb6-8b8e-7384fc549dbd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d696e851-3001-4a34-ac58-cc213ab1f833)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b5f73e45-81d0-44c7-b1b5-39f0ed28d790)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 895d56e8-04dc-4c2a-8edb-aa307da44814)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7bdc158c-09c4-4303-98a2-95ac217c0d7b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7f09b95b-c5ab-4274-8803-eece59459312)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bc2998b3-bf64-4a57-aa06-33d6e3f57b29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 503aa496-5d44-4876-b72c-22f53e38841e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0c5a8275-a28a-4f93-817b-4dad704c3671)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bee57263-7e8b-4736-bc4b-177652f48018)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24bd3d9e-ef99-4acf-b267-51739222ba13)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5c7038c0-6b3f-4f97-905f-2ac8a826a9a1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 79209500-88d2-4efb-ae12-36961422af5f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c15476f2-b945-4bd3-9d0d-b56187d74ac0)(content(Comment \
                 #err#))))(Secondary((id \
                 9d3164ad-7956-48c7-9060-1585f2263536)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8f8e5371-c0a9-4882-abab-4793d534ae95)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5171b496-42b3-4ad6-afbd-734e7dfab18a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 597937bb-e437-4f79-8336-62f0bfe633b7)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 93961230-3aeb-4fe0-aa51-724650152a86)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bad1b945-64ed-46ba-b27f-6e6ec46b3207)(content(Whitespace\" \
                 \"))))(Tile((id \
                 936b28c1-68b5-4e53-88b7-838ee7cbbcd5)(label(Fake))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f1a2e20b-212c-4dae-88d4-fd0e86aa2998)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 660312dd-2c63-4eb7-a6c0-ab34eae4ae85)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8520a1ed-7cc4-43e2-8a2b-66821f37871f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c65d4f9e-96fe-4c50-af8a-0acd2f008513)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f09612d-cdf3-4a5a-8634-b7e7f4edd704)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fc30bcc8-3a0a-4801-bbe4-54e44fef77e5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 19280f06-f9c1-4ecd-a813-9783f1377a5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed3868a2-eeb4-4028-b399-7170800c18dd)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e180c714-6f53-42f0-a7bc-c014691ea0a7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 862e99a5-2143-4e3f-bfa7-fec564a23095)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f88e61d-db92-494f-8b44-fa1fe0518da0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 412a6380-f0a9-495b-a742-f8c4600f6638)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d030eb4f-0f88-41d1-a12e-1a8aa9d7c4c6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 472d7373-2de2-4699-ab83-06dbf689e446)(content(Comment \
                 #err#))))(Secondary((id \
                 5946e251-b954-444b-8571-73a07f028d0f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7c9b44aa-0b0c-4493-b4d1-b596ccfb5860)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3d187f37-a8db-44b9-b5eb-79988c3e2c5a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f4e0850-7a82-4784-a8c1-e0cdb072b720)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 37ec8304-8678-40ac-a78c-e1564cf1096a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5e98976c-354d-4f26-86f2-cefb7a80ee5c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79281c40-1b91-4475-8293-ffdb3420f211)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3dc98b9e-8880-4035-a61f-346a23cb05e8)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4cfd5f82-9500-4f00-9fbf-de9d15dcb2c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 78fe5eeb-03df-4ff0-b565-41e9c49c239f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ede7a6c1-c15f-4648-9310-48732cd24e51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0db9775e-926b-43b2-b3f5-d078c14cf73a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b03a6005-6308-475e-8f5e-4eff52656710)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9a9b8d8b-d056-4aab-9156-470737187412)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb245d33-d282-4db3-a227-b645e9b75790)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2cd250fd-dc35-4a9a-bc4f-9426959354e8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 787ba6f3-84b1-4f93-b5dc-a8b870e9ba4c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a45395d-bcf2-41c3-b21b-cedec3e6a113)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3105b687-fd5e-473d-b967-9da7c0647341)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 db3f8bb1-72d2-4aca-a6c4-c7c554a879ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f0d098d6-5609-4f15-a04e-0ede743aeb94)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 4d693f72-81cb-4c75-bde0-23592716a92f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d8b04ba0-8b64-4a94-9a97-70c2920e2530)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2a8bf909-8ac9-434a-956d-cf4efbcc8af8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f6d6e84d-8550-473e-853d-b77f12bbefae)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 02f88e70-2821-4de0-b7a6-73287b575c61)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a1648162-6ae8-4d24-8b93-6c537e8aabcf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 253ac0e4-fd26-4abc-aa21-1149fad7d43b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0ba56612-4438-4f63-9617-78d3c4ffa5bc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ad6af5c1-5817-4b0c-ba82-321f042705f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ba4458d-bee8-48e6-933a-18d06c343a42)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 608bb807-8d6c-4844-8d59-d3efe58d3843)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 77d36a72-5ba6-4ed4-aa9a-12b37970424c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5af082f1-4d3e-4854-8ff1-1ffdaad55851)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d48e22a5-f65a-4767-a9d0-1a40cc32b3d9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9609e0a9-f5f0-4fa6-b677-3a82f5b3c887)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff4f682c-091a-45ea-ac96-feb28081efc3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2cce8860-ee1b-4ec9-a66d-c8a748a30290)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 324ceb15-0215-4d88-8cf2-060903955c6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e9a7860c-6db8-4a81-8ae4-bfdd79d70282)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 275a0228-3bff-48ff-a304-8f6edc108009)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 17047b05-1174-4151-937d-b4833dfd6d1a)(shape \
                 Convex)))(Secondary((id \
                 13edcba8-486c-44b4-93d8-c1bf41f98fc5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1bdbf758-cf85-4d57-9d74-41588be677bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3a70753f-0da6-4a2a-bd04-12907709acb1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 aa17c661-b339-4613-87f5-9cc5ab23d5cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 80b2cb6c-da1b-4a39-8396-bbcb94900dbc)(content(Comment \
                 #err#))))(Secondary((id \
                 d1468a7b-ca13-4ee5-be11-c9d8ee744a90)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 11a94a5f-356a-4c36-855d-dbb81005f92d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bd717dc5-9699-42e4-819e-a605ae33c705)(content(Whitespace\" \
                 \"))))(Tile((id \
                 90b78761-c7bf-40e4-a659-a899afa81b5c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1c9d9db0-dd3d-42eb-b038-aaf42bd3b7d6)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9fca87f4-9b31-4e5c-8982-03f73b8d1b00)(content(Whitespace\" \
                 \"))))(Grout((id 29f35a2c-4d0e-46f3-a17d-cdb6f0b58159)(shape \
                 Convex)))(Tile((id \
                 3d63fe1d-8723-4fc2-96a7-579952450f50)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 8cff05d8-1ceb-4dc5-abd5-601a0737609e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae9cbd45-3c84-4a8e-8580-4fae57ee8c20)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 25aaeff6-8269-4919-a321-5ae18d116be9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 eaacbf10-9ca6-47d5-8f8f-e286b53ce6ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c286055-6c42-4648-b43e-330a792f075b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4db679dd-6f17-4ff2-b8f1-2a08acc594a6)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d22f9463-63f4-451c-9850-be72b1d06b56)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c5d62b24-fd05-404d-a7c0-21cdfeacd1f4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2336603c-c334-408f-a1e6-5f61132d0687)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bed23ec7-8aa3-4470-8236-81e76586c7a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6fc53068-7615-432a-a026-eb54d842336f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ffd73edf-b438-446a-990e-1b05534f81c4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ee921cb7-b3fb-4dd1-a280-c91d2d9223d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3d27f98b-ba04-4145-98ff-501bc97c9b23)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a9ae7e4e-dcaa-45cc-823f-edcb1cbdf947)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 4e543e67-9133-4e9d-b1ff-b835632a681a)(shape \
                 Convex)))(Secondary((id \
                 c39d6789-d1f4-4a6b-a29c-c8c25b5a0838)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 92e67be4-ea93-497b-a2ea-45c9966fea4e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 110afdaf-1fde-4e5c-ac9b-6e0bbb6d1d30)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5189f641-bafa-420d-b91a-204be24e470a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c3c3b2ea-b824-40df-a919-9a4e441d8759)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f9090b6a-5094-4660-a053-22404727e172)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 12e87a6a-048e-48da-82ea-382dc457ceac)(content(Whitespace\" \
                 \"))))(Tile((id 8e8628df-8459-456b-84d6-2cb36af4b9e7)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 cf25941f-eaa9-4a1a-b357-8c5b117c95ee)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 3a1a98da-cf5a-4fcf-aa2c-548aef9e0b9e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 67cc35c8-7838-4a4e-ad76-367c9369ab0e)(content(Whitespace\" \
                 \"))))(Tile((id e262c9fa-ae87-4fc8-9057-a9461bfd9bd5)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 a7254a23-019e-4314-af62-6d9c7bfb672b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 10ec1999-7398-48f7-863d-d1d5e36f3d2f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c7ac74b1-6e15-4e71-a746-39bc1efc8c27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba2f5cb7-3230-4ec9-a771-b691c357108a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b7fc917b-738e-423c-904d-05a96059bced)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dd935528-15f2-4c7d-8804-a4d472b14489)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff4d570b-28f6-4b30-a316-3a409235447b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1fb75734-2d7a-4769-9753-b17b1592087b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 eed1455f-cad2-4bae-8f40-ea91ebc25208)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2371b8a3-f2d4-46ec-bbc4-a60cbf89ef6d)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 f1e569ab-92b8-47e3-a64f-35fdc0407aa3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 342903af-c89d-49a7-bc25-5de96ac97281)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2836626f-426e-4d0f-abb8-e947f32132f5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3d735aba-e54e-400b-8573-7e839a3b9594)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1855f1ad-a3ba-4224-99f2-d575807cf9f4)(content(Whitespace\" \
                 \"))))(Tile((id 9e393b21-55dc-4ce9-ad66-de202a408331)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 ea98ca45-1496-4863-93f8-3c2332b1b479)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 f2df6024-92a9-49a5-88aa-39ffdfe6dd94)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 62c5a63b-cd57-4076-9551-1ae18d2b5025)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6e249a9-c461-4c0f-acce-c5fb18660415)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 28e5f8e2-b712-4c28-98f0-be264ed0d9d9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e243b1c2-c613-4578-be3c-c3a352c1b602)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e155439a-5123-4b48-891e-d66bd6c15636)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2fbc7089-202c-447c-ae1f-0befd34a6f25)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 457ce868-2756-43d6-85fd-3dbd77b0bf43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 aa2b1a04-2234-4c8d-bf1f-dde6283f8c6e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4cb364cd-2ae7-44b0-9e42-20c673269007)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 84e6798d-a1d4-4688-83bc-941ceb1f1eca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e910694-b45d-4b2e-9f3b-6f3e92f40280)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 6e1b78c6-08fb-4eea-ad24-8a25bb546292)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 60c6c426-b0f5-4c23-b7ad-2d2599c327bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e0a7fd21-7379-4620-be7d-9fc89c0748c5)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 f50e71ce-8170-4aba-ad79-d299d2f91a57)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4dd92573-6d7c-4296-9208-937e7a2aa82a)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d77b580d-b094-478f-9565-14ce2c48c3cd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f042fe33-b554-4f20-b618-2fa3c8e72461)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 da5f7a94-d56f-419d-81a4-e90f5e4ad103)(shape \
                 Convex))))))))(Tile((id \
                 c005aa0c-43a6-4084-8b73-db752bd727db)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 597ecf17-421a-4853-9600-eb6e87ad1fc1)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c6d281a5-7366-4fc6-b93f-a5b506a71e24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 565daa13-0e6f-484d-9db2-c2c4412a54a7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0606bb3b-03a8-4d74-beb9-0d8fc732f979)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cf5c6a9f-abe1-4fb9-b6dc-a9dbd6d7dba8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1998792d-d014-4ac8-9359-2a97c35a8f4d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d55d2bae-a087-4fd1-8d06-bc046516f54f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bce25bdc-97f5-4167-b0a5-28a7838bebdf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a37487be-086a-4d06-9e37-f4a6e0c74f15)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 0dd1749c-08c8-463b-8c08-d7540b7fd5f1)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 570a141b-ab07-4691-840a-ea9e46b4aa3d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 387cc421-9cee-4b2e-b8c9-64420e4e3e0c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8000090f-6960-4cc4-8972-4b3b4934bbdf)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 21e644ea-f7ec-4444-be5f-6a628e76b1a5)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9dd67d89-f568-45ca-a855-b39658cf456c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87fdc732-9c35-45c0-b05d-841c3f76fcb7)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e426f078-9c62-4ab7-b8e5-f3846f02e372)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9eaced5b-63a5-43e5-9f9b-65877c1e2646)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93aea0bc-7454-4307-bddc-19b83469ac2f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 00645620-caec-4675-8b29-deddb545f394)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f38ed27d-79ac-4416-893c-56aa5b086d7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56d55ff8-a6c7-499a-9c41-e7249a9f2789)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 92403899-717e-4808-ac3e-3767e0eac20d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0f4bf478-ad4e-4543-867d-48080fbacb3c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 634ebd5a-061f-426d-8278-caf13efe2fcf)(content(Comment \
                 #err#))))(Secondary((id \
                 6341373a-1b18-4f9e-a516-25b97197b5b6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 91b8fd2b-50d5-46f3-9e2a-f4114ad5d08e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 35bef15d-fb86-4914-aea9-f26da914d8b5)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 55037480-08df-43b7-885d-c02860217f6d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 66791c1d-5fd9-4219-96fb-4bd8ba9165e6)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4534c71d-fbf2-49b3-8733-70c5c53f9fb6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5936b2bb-c495-41ac-915b-84720407aa82)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dd27ba1f-0e47-47dc-8a19-b1ab49ddc6ae)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a8d6513a-d522-478e-a64e-ed20e95ced90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6b75f592-c667-4990-b872-45254ae84f58)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7d563839-85f2-4636-99f5-0e0eab764be6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bb0e52cd-b00d-42d8-9697-52cda117a7be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11b0bba6-2add-46c0-80ea-eaae5fe2a612)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d4989258-1177-43cc-85c8-0dece96af223)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bde6f2f6-7dee-41fb-8ec8-7ce0a8d5de20)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1da33e8c-40ca-40fe-8e25-c58069bda77c)(content(Comment \
                 #err#))))(Secondary((id \
                 f7d18d01-968b-40cf-a368-ecc331878420)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c84b88f4-3ab1-449c-bac0-1788f3910edd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fd353163-012c-4c4d-a5c5-e394ecd16ae2)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 048898ab-996f-4b90-b66c-bff87aa60b61)(content(Whitespace\" \
                 \"))))(Grout((id 859388a0-5d8a-48a1-b60d-d1a89e219d08)(shape \
                 Convex)))(Secondary((id \
                 10d220ad-0935-4d34-a650-a3cd9d04f606)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9e5262aa-2218-4b3c-a2ae-81123cf669d7)(content(Whitespace\" \
                 \"))))(Grout((id 64517fe7-e255-4bc2-8834-b72704af952f)(shape \
                 Convex))))))))(Tile((id \
                 783434d2-9a4d-4a4f-879d-214deb152945)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 33189c28-075e-45a0-b322-bf95b999fe4a)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a56657fc-4b1f-46f8-882c-ddfa6b681f37)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7c2df4aa-19e5-4677-bacd-5b7a724798a5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a336b9e2-f9a1-416a-982f-96e525d7a097)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fa4494a5-f20c-4f3c-bdac-93367e9f675d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 becfa6ad-81a4-4043-b34f-f9cd6968bdd4)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e636ba9d-f4f0-4b6a-a857-f72272703edd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c4907dc5-0d6b-4f19-876e-2a7d837d2526)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afea6b91-26f2-4c46-b1ea-15e893de8d44)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a4226d0f-0630-440b-b925-355c4323e26d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 01ff0dc7-27f6-4bb5-a4fa-018d7296f307)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 953c3984-84f7-4724-b92a-a9a829788351)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 38407f72-07a5-493b-af4a-23ddcba9b306)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 808ba58b-c96b-4222-996e-d20bc439b39d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71431cc7-a1ba-42a6-9e18-048bf7ed09ca)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2b68db99-6d3c-4256-bc00-252ced2dd688)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a0235b54-2e45-4d56-b677-baf460eeb39d)(content(Whitespace\" \
                 \"))))(Grout((id 30ecd689-7fdd-4a7e-b180-65fd090457c9)(shape \
                 Convex))))))))(Tile((id \
                 c57eb10b-f7b5-47af-8678-f6ea4067c6f7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3ddcb139-95a9-4888-ba0f-5442df48f9a7)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e695ba82-cf68-46ce-925e-31249261cd8c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0486935e-9996-4670-9f7d-c454f07554a0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 528c71f4-4aa9-4397-b56a-b3fa05d26e80)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fd2ee037-5c94-450e-9bb0-00783b2adda9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc17fe9b-be91-40f4-b6d8-334ae02cf696)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 28ac697b-72c1-4380-a4ac-77a6d620ce07)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5d6063b7-7b37-4a0a-b94e-2d416668609e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62e9e671-3419-4b81-ab5d-370a5a394aa9)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 055bba99-c1db-48af-9aa6-4ba45f916bd5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 12c3e8d4-f091-46c0-9755-5635c4f2b5b6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a331d7b3-e0e5-4503-8eda-98fb4017d7b8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 93dd678e-5279-495a-aa88-2fa862389909)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4597f735-731c-4066-9851-cd52d6542cee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87294766-b061-4b33-84e7-b820bc4fb3cf)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 394ec58a-fb30-407f-b376-432f454bc179)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 41c4021b-e111-4501-8940-762c147bfc82)(content(Whitespace\" \
                 \"))))(Grout((id c347ce4a-b918-4de4-8400-cf7174b04eec)(shape \
                 Convex)))(Secondary((id \
                 092d4568-73e7-4dab-aeb2-a96ba06aad7c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 89f8d64f-4dca-452f-a5e5-405b0dd06197)(content(Whitespace\" \
                 \"))))(Grout((id 3fcdc9aa-d3ea-48dc-aa4e-50d47fe5def0)(shape \
                 Convex))))))))(Tile((id \
                 0affb0e4-3f8f-4a78-b155-9cf118ca8eb7)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bffb5f4a-a206-4568-b182-a633e4b7d67e)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7da6ae55-bd62-4139-8b5c-b3467b28d15e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a38e5299-1158-4600-a4db-05d26b529068)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 56db0b7d-70ce-482f-8bdd-f261e4bb033a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 366cc558-76f6-4454-ac7d-a42dd3da2584)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d875eaf-40a4-4826-95ea-ba72c0cd2c81)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5d888c4a-19d5-4e34-9401-04e577bc1c48)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 85c80d5f-538f-40d0-b5d6-e235a7f5141f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74612f3f-3d30-472f-86a4-fbc1857c058a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 470469dc-d8c3-469b-9c18-3d8e008b193b)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d1dc00cb-ccae-41d1-bddb-107de0e308be)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6c61507f-9013-45f0-aa48-1a3d015573cd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84bef4da-1203-478f-85ca-9ee88c6f3beb)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6a9d6562-d5e6-4b2b-a031-11739b0c2c3e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33442197-55d4-4dbd-9138-5e50ff98d573)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8cb1eae4-c597-4b00-a2eb-5b57a125a194)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6465c91e-5f51-4910-918f-9d204e1bcbd3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55757bfd-ff22-42bb-9cd6-5608f5f015ff)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f99eed79-6ba7-4182-ac8a-299afd43bbe9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 420ac88a-4caa-46b4-9d0a-0bb5b1b14db6)(content(Whitespace\" \
                 \"))))(Grout((id bc42c4d1-dd82-433f-b065-ac7de6dde15b)(shape \
                 Convex))))))))(Tile((id \
                 d9dfdd76-ecde-417a-ba68-8cfb016ab80f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ea8ee01c-941d-4c66-a647-7918eefbb221)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 eb92a4a6-8137-46ff-8763-0f1c1cce4e0f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3d6f8955-5d58-4093-9901-4cb799e4da68)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5c39d9d8-1353-4c82-95aa-cfd3c710d2ef)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dbd5746c-73d2-4bfd-b1d4-9bb4ddee1844)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3b3c0c4-46c1-4190-9173-0ba09eb387a3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e8ee62da-8d62-4ef9-97b9-159f1b4a212a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 50dac4a7-2903-4ad4-a0ba-647854776727)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6c624d4a-3555-428a-954b-a312f3029cab)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a73dcc1e-7ade-4348-a466-c52a96e93884)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9f00a5ef-4f7a-40d8-befd-c7c0d3c8a9ea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e8ad89fd-898f-4e2b-a3eb-f1220739d5ee)(content(Comment \
                 #err#))))(Secondary((id \
                 0d081213-9d46-42ee-b251-c3833afa9268)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d38dbc1a-10d5-43bf-9b21-c0fb792ff103)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 35f80ac8-05d2-47c0-96b0-fe39ffa828ba)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3ff96410-6cde-468e-b92e-60c5dbcdbb1a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd0d9dc1-2890-40ad-bb56-9bff2cd214d9)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 09519fa6-8a8d-49a7-b97b-9446691ac068)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7d48115d-4ed4-49d3-9026-f7ad7b3258dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8438b733-c653-40e7-bbff-5d4b99a40808)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 103b9289-b28a-4c91-b38b-16ddba0518b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 673dd059-8cdc-4b7e-a624-2f4006e16b66)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4d32ab0e-aa4c-4440-a471-858d48f0649e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 76eb013f-ee4d-4f01-958d-945f5cdc1b0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2867227-a84e-48c0-aefd-0a6d27dffd3f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 536f6d84-2485-4700-856a-629a00f30c51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fb05cdc7-856b-413c-9130-26a7b33fb7a6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d3cb1bb1-3557-4483-b9ef-8df266b162c2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 277e68f0-754d-4e80-8a7b-d56b9b9aa9f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a9256755-3fd2-4341-8307-7806781ac7ac)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dce7ceae-84ee-4d61-8c22-b695f164af14)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fb80563f-83de-49db-8e2a-a9211d8c0b0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce8523a7-3bf0-46ed-9e10-c06b9cbb72a7)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ff99f97c-f2ff-400a-bfc8-0c9a6bcff924)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 695a14e2-4ca6-46fd-99eb-862ef7629ffe)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 592c2ffd-79a2-45c3-bc3d-283cb6aa7a76)(content(Comment \
                 #err#))))(Secondary((id \
                 bcb2d205-cf7c-43ef-acc3-978957fecbe9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 25ed166a-87e0-4172-818b-2b62a8bda24c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9868c377-1e5e-4a84-a575-cc38ab948866)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4918c91e-8e50-4735-883b-a356720ea9d2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a52427c4-8fad-4952-9ce2-071dbbab89d9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8fe334cd-5fa8-4d94-84d3-34385126ca98)(content(Whitespace\" \
                 \"))))(Grout((id 653091cf-f907-4593-90a5-0f8c07a68006)(shape \
                 Convex)))(Secondary((id \
                 991a094b-7379-4445-a8ee-87b39301f44b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c7ba46b9-2795-4e23-9257-c980fa33f40a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4839dfee-a097-40eb-b968-4762751a00cf)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fb185c7a-882a-4d4d-a696-b835827a067e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3386719d-5ae9-466e-bcbb-76c10b1a7cd9)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 cf81b859-0f34-44cb-84c4-ee0a294618b3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f0a26556-e334-473c-8cf8-c57092e4f36a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5d5e598-a807-4bf5-bfec-fe007c7b3a22)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c8e619cc-2d91-4991-85d5-546da420b09d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3a199739-00ab-427d-868a-d401e27d83fe)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ac76f6fa-c89a-483b-bf41-4593f9f1b21f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 be7ef864-67a0-4fe0-9bf6-4f882754dc82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfa24cb1-5893-41e9-a062-bd4ec11e605e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 50514c7a-0b13-4c44-80d5-a3357fcc0012)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 490e8e38-b288-4122-bc9f-ec6882b9ff61)(content(Whitespace\" \
                 \"))))(Tile((id \
                 af9ae9a8-74b0-4d58-8107-7dba40c5e153)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d8cbf47c-8aa1-4f01-b414-892b1512aa51)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0083e628-0325-46bf-a55f-b2d9673a1778)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6c7603d2-1f45-46e9-9fc4-477dc7140cca)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 37df505f-e3b7-483b-85e1-99d1a420a9b6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35390017-9451-4508-a83b-7524bad86fc4)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a3206d48-65ee-4b0e-b65c-addfe0e9c62e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b7ecddd6-8846-4963-a461-1f0b9bd84979)(content(Whitespace\" \
                 \"))))(Grout((id 4da9fa71-62ee-4931-bcd1-1a33bb41840d)(shape \
                 Convex)))(Secondary((id \
                 b3f4fb40-e05d-466c-ab85-39d6e23e0767)(content(Whitespace\" \
                 \"))))(Tile((id \
                 def384f8-b29b-4dfb-a36d-85a809641dbc)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3bf09644-328a-4419-8c85-f02f99efb9d2)(content(Whitespace\" \
                 \"))))(Grout((id a95b9978-19ec-48f9-ba13-6d458ca30443)(shape \
                 Convex)))(Secondary((id \
                 ec61c941-3afd-4226-8ffc-fc1b54122412)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b5fd0857-94d1-45a2-8ac0-41c72af48c00)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3677a6f-6aa9-44e8-a44e-f4643498eec4)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 17fcc7ed-aa0e-493b-97a8-aa78cf51b2fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a25707f6-8e51-420a-aa12-05df1b5b9152)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d22d1ad5-68bc-4ff3-ab87-1eadd3a003ec)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 90fdd949-668d-4977-a390-869262719fab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0557157c-33b3-43b4-a046-c7b881d19216)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 755a7811-722a-41f6-9b56-80cdfda1b034)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b63343b-a942-4302-9ce3-56ef30b62d39)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ffbc6847-3a7e-4b9e-9514-92e5d8b3e277)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bc4fee4b-bfc4-4d8d-a21c-32ddd20d4115)(content(Whitespace\" \
                 \"))))(Tile((id \
                 61ef3fd6-dc5e-4173-a46c-21a4e9e903b7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1a42a366-5df3-4b08-b9b8-ddc50e9fa073)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d36add28-c6d6-4d4a-ae6b-3b53f65a813c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6fb11d1-572b-4992-bb6f-c3ce42c31a5b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a1564a6d-ddcd-4382-b57e-1852880686af)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4c9de5ed-d3f5-40ab-957a-ab2b7ebacf82)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e9a91bdd-1b8a-4628-805f-0a21e5b22197)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2d6fed2a-48c6-4b44-b0c2-ab7d7f51af72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2cdc1f9-3351-483c-9638-f771257810d1)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8e161d9b-fe98-4adb-846d-3183117c15fc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d7e9e9af-16d9-4be7-b772-47fe800aa248)(content(Whitespace\" \
                 \"))))(Grout((id cf9694be-3b9f-4fd3-ad55-858faa4e5f02)(shape \
                 Convex)))(Secondary((id \
                 de591d6c-a4d8-4591-9c3d-016e28ab0985)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57226fd7-6fe7-4284-a58e-3e828da4c10b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0d276a95-1daa-457d-ab45-dd7874086238)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c187a0a-a6d7-4dbd-bf2f-1254faf967c8)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c5305727-7034-47c4-9b13-76e2ac8b158f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 66945bb7-c5c7-4a6a-bb95-9ab4a4096aca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dcf922aa-beb1-4e2b-8b31-79696c1ba35e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b86c3ae2-f820-4b20-8f4d-c8cd2d034d4c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 78164bbb-a765-4907-a883-0b43f38c8f40)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7c476086-ecce-47b4-b7e0-6eda1e7f99ce)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bc736e1b-63f4-4e30-b612-8cce38bb2f4f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 774faf15-02ca-49db-aa2c-ed9951c7e422)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dec59fd4-6edc-4055-a1e8-db02398bd11e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66dc2eb8-49b8-4770-a380-03ecb1c39654)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a6da3ecb-cbbf-4467-b699-f33c2e198794)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 97c06b6c-f2fc-4f85-8233-e1e24f55f183)(content(Whitespace\" \
                 \"))))(Tile((id \
                 49f69120-67f3-4f12-8473-25854331d0a7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e7adea10-9083-4988-afcf-d748d5c83dc0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c4bf9155-bdf3-4583-810d-7fbc5963d1a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 736d66cf-946f-4560-89b4-3fc126888dc1)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4492e518-d63e-4b85-91c2-26cb60446eb3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1e2692c3-306c-409d-b336-cef082edcd8c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ae09d83c-ae42-4b0e-8765-8aee441f0616)(content(Comment \
                 #err#))))(Secondary((id \
                 03b03c43-459d-491b-ad03-a72757614270)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 092b9051-f708-4479-9166-d6161e6b254f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bd68afd2-4abd-43b7-b370-539fc05fc7f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4ed8e27-adb1-4436-b2d2-fe1faf8d36f1)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c21f3089-a8fa-414a-b6f4-d07fc101fccc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 583749a3-eba6-4e91-9cfe-36ff52355aca)(content(Whitespace\" \
                 \"))))(Grout((id 4e5cdbcf-6fc5-48e0-abf0-2fc5054d3574)(shape \
                 Convex)))(Secondary((id \
                 5a84df8d-23ef-4dac-8388-201f94248eb4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 edb29728-1267-4abc-9081-195f6663856f)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8a308706-b82c-4215-a4bd-c8cf8fbd1a8d)(content(Whitespace\" \
                 \"))))(Tile((id 6bfd0ba4-08a4-4f77-8b64-126378c1907e)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 e89b84de-7ab4-46de-a7d5-94abf6588ec0)(shape \
                 Convex))))))))(Secondary((id \
                 f7dde1a7-74ac-4b68-bf6a-812a666fd963)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4ee83851-34f6-4867-a055-ca51562dfb48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bba577e3-68f9-4353-ae2d-e24914773aa9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 342ca27b-4acf-4a91-aef6-2426ae0200a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6271d837-e1af-407a-8f42-902f46019fdb)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3857a916-2ca2-4b91-9ce3-08bdcea49694)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 830c1a4e-d2eb-4d27-b5af-9edfe71db503)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c18044e-489b-458e-ab15-649911a03030)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 389ce73f-f26f-4be2-a221-dc4713f67f29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c4d1edf-c38e-4a8b-a654-eda7ac54fbfc)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5e8c1968-ee4f-426a-a161-a578efb13975)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 802b0b26-b087-4fc5-b422-fd7727778899)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db1691b5-be53-42ea-a42b-48615ce08542)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3815c49c-0867-467f-9881-1ebb7e272bbc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 90d59c52-5f6a-4dd5-a008-08a674362dd2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 77a1c18c-e388-464c-8a96-99d509f09a60)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c4b13785-8b02-4432-98b3-734f7f00dee5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c4f76c84-7f32-4d47-b5f1-81bf03e05f9d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f791a637-6547-4952-b2af-2df75636a400)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 b280e9ea-b425-4fed-a57a-3299273288d4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 12f499da-76c3-4ae2-93d0-5ca9e69753b2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 232006c7-a8ef-48b8-9a78-3cf60d23ceaf)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 c4738729-fba4-45b6-8020-a79d3a26c31f)(shape \
                 Convex))))))))(Tile((id \
                 cffc0f76-df2f-4dae-a3ce-3d509789faf9)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 f6e93383-434a-48e3-a0b9-9186037e3047)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a9cda473-0dbe-410f-b9ad-521eb61cf321)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cb5da907-a7ee-4650-9c61-27b4e3d65121)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b6fc370c-918a-434d-b3b6-cdd8eabff95e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed44da3d-79ac-4881-98a4-e782ba366300)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fba985da-7e06-4f50-a970-38d7c6ae21a7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 651eb65f-6715-43ac-812c-f1f48bd31f4f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5866ac7e-9854-4845-86a3-02fb42bb4d6e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 955402bf-b94d-4e9a-b95f-440e8a942c04)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d4fe1496-194e-4abb-b7ab-7f0aab65d877)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3157ede-51e6-4eb4-a998-15ec007ca8fe)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 d5ae6ad5-5f2a-4934-9fa7-ffb96b15e9ce)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2e1b0250-b0cb-438a-addf-8604f846c3af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a3a9866e-fb72-4fc0-aa55-e54dc312fd45)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6367200c-4fd9-412c-be16-02b8bebead75)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 a56c9c39-7aea-4e94-ad02-25c9017c06ee)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 aaa9b7f2-0032-48e3-8658-07f826c0a19c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 accb22c4-cf7b-4bc2-a1f1-4d0c80a4635d)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e1b1486e-f426-4623-869f-be7d4b147b6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9a4bed25-55cc-4951-9efa-027aa0216064)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 63eebaad-cc2d-4899-bc2b-22fbf548c4dc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4f914036-bad1-4d1b-b8cd-01000178c611)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a07cee91-fc9a-4f3c-aae3-5e82cca71f06)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 33c9f869-287f-4ebe-8bdf-f8e1e03558a2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e5ef7784-cd98-410c-b653-22ab8af2b11a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ab4da6a-7aed-487d-b0fb-3113913c693a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 09efe17e-bc3a-44ac-99a7-a868e3452897)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 af722baf-632d-4d28-92d3-c2f914bd31cc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f2e03598-6017-43f2-be61-b8442de25fce)(content(Comment \
                 #err#))))(Secondary((id \
                 88af9827-b2be-471c-add1-70a096838b49)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e01b6ac4-8715-4662-9fc1-87aeb2060bf9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6ada54c0-de79-4600-bdd5-f42f2f4744f8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 54ec4955-7647-4737-87d4-8e21a18943b9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5c6fe323-e200-4fe9-8e5e-3a890b129f68)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3157b120-93b8-4d06-bc3e-cfde87fe9690)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d4a9bddb-e237-4100-b2ed-305e25afd150)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 065b6501-e20b-48af-94f3-9265d968d3c3)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5e4eb96a-5665-41e1-8b9c-b4cc34bf0c0c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a56978d8-b5e0-491b-8a2b-7648e836e8fd)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f9c291e7-47a6-4d4a-8554-85853d267d7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 230c9d16-397a-415e-863d-3da8904b8bdf)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 23eee358-279b-4887-acd9-be029dc2a7d2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 388b2fee-7809-4967-9e99-ad49f557a9f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec0c9439-18fa-47c6-89f4-68e8bfd33cb3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d15453c9-318b-447a-a077-6c85578cde91)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b3fd18c7-e008-4eea-8532-ac140f0fe666)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b89d7d7f-95fe-42a5-8136-dbcfdaffe43b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 adff873a-7d51-4403-aa11-d637d7c536b5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 692d2d7a-9aab-4a1e-8ea0-024c787bd0f2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b2bb1ffd-3dbe-4c43-83ee-d0e2f9cbbdab)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 47be2cd3-b03b-45a8-b26f-f92060bc3004)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5d198806-3a30-42ac-95c0-1df2acdffadb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 82dc0544-9ae9-488a-a338-819aed1a7c66)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a1005730-3689-4276-af36-eed9216be5cb)(content(Whitespace\" \
                 \"))))(Grout((id 700bdac9-76cf-4be6-98b1-c032587de447)(shape \
                 Convex)))(Secondary((id \
                 c5844890-0517-4035-9bf9-160b2dd905c6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4d4ba1e9-1383-4ca6-b828-421d145b1214)(content(Whitespace\" \
                 \"))))(Tile((id bf7c57c8-0bc0-447a-86fd-455f1f8de839)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 e47fffd1-3404-4eec-8a0c-bb44253a8a3f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9dd9e624-583a-441c-8649-338da0a92678)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a2b0087b-2c53-4d62-b2b3-e1222c73f72c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db8f24f1-e424-4ecf-819a-384144315f5d)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 94ff5cbb-c5ba-4d24-8db0-06a3ac347599)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c0f8eeaf-068f-4d16-bb5d-b7453ed807a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ce9a10c-7085-4b41-9ce1-c17cf835b689)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3f19f742-2f54-4c88-9157-76e4fde5317e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a9ca07b3-0f93-4e17-aae0-418f77d6cf01)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 029b56d4-87bc-422d-9b26-05af5d126a65)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 09dd70d6-502d-48b2-846e-c5f8efd647fd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52f2c094-8b57-407b-80cb-925fe808963a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f57dce69-2cf4-4a2d-ad9d-7d40490ba01d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73fab713-03ee-4900-93aa-2c247ab1693b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f0653d97-c095-44b2-8384-aa7ce7488e3f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 47df5f2e-3a90-4ccf-b553-316fddb81a57)(content(Whitespace\" \
                 \"))))(Tile((id fc2dc4d9-a9e5-45bd-a9d1-6507d203d663)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 0b59e204-0333-401e-8afc-d6b4c63394c5)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 034534be-8504-4e02-918d-d368e0fddce7)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2b1f1e0e-b99e-4c62-a294-ff9dcf4b5520)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ea91616-8aec-412d-82f9-4e9351bbd291)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fbe8cbf8-4245-4ba8-a68a-b03eb53c3399)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 007fb3dc-eaa2-4910-b467-287968ed26d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e77e5eff-3894-43f6-96e4-13961e664183)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d4ed2f71-1740-4928-81d2-b0c0151b5a7d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1943c974-3ce2-4578-940a-728197ec1ec3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 48c7f1ad-35cc-4f97-95e7-c68ffe8023f7)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 0f0be7c3-2c77-4e05-aba3-ae8405f61d16)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 03a920d7-95a2-4754-8416-1123bd6f2f11)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 826ad200-168b-4a5f-af94-df033e42e4b7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca9ddb7f-5f91-4956-ba30-44aa1648f245)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6dc24ff9-bf61-4a92-8f2f-b7c3f67217c2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1499fbd3-ca66-47ba-8411-7500ca3fe394)(content(Whitespace\" \
                 \"))))(Grout((id e6ddec97-cd3c-422a-b214-ca8fd61c2331)(shape \
                 Convex)))(Secondary((id \
                 8d2a046f-1546-4749-858d-626905d77eff)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ae4808bb-0f13-4a47-8d93-7dc155ad32bb)(content(Whitespace\" \
                 \"))))(Tile((id 32ae3fb5-4544-4cd4-a792-f9256875488c)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 ff441609-be5e-4d1b-b544-9799d7a3011b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b6463e8d-e7b5-4545-9fc7-4924aa2693a6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cd358f94-2daf-4f24-8548-3d5bce3213c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c5e0a263-a161-49ed-91c8-598982eab8ce)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1f911b96-1c72-4e92-92a6-7f2d5a352c90)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5b6d3a14-25dc-405b-8f4a-c0577ded0d7c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 433e239a-9d57-4e8d-88d4-3d051553fc95)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b8f71dee-301b-4173-8fd6-f5daf5c8c00d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 534a6cef-6e24-4f16-a7ba-2edbd118aab9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0051a088-619a-4934-8f53-0219031c1bb2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3df4acfd-98e9-4fce-9853-95fb92574871)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1cfe8911-f9b5-4422-95bb-443dae0d0105)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25d47a4e-06ae-48dc-99d6-32a41983492b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3959b315-bd21-4c0d-8385-3dc34326cdb5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 41cd46f9-c78b-483e-bc9d-5ba5248ce0c8)(content(Whitespace\" \
                 \"))))(Tile((id 33511225-301f-493f-9ac5-0c48a3b2301e)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 a7219f30-6814-4af1-9fb1-560e9131e0d9)(shape \
                 Convex))))))))(Secondary((id \
                 09487b14-597f-48f5-bf3a-5f01ce9e2bed)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5370e5d1-290d-4e66-9ba7-4c03e6723ff9)(content(Whitespace\" \
                 \"))))(Tile((id 6b62996f-8ade-4ed9-817a-6b538a6918eb)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 9a080685-90e3-41d1-8f92-e7d253dc7245)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 369a1435-3a95-444a-9322-4055172d6863)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 d6a5c2d1-aed3-4c53-a454-2d3922735b2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d58190ee-5471-4175-8fef-aceb5596ec09)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d8eac610-d479-4c9e-9577-cb9989ee9e8c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 03e22831-29ab-4096-8d75-455d5c164e84)(content(Whitespace\" \
                 \"))))(Tile((id \
                 782f78ca-f815-4a62-a570-fcaf036f954d)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2797e94b-90e6-4c45-8cb7-e069a6e5c16b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 72fe75f0-3e31-4113-8f25-fce1dfafcf1e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fc4a0da5-2b7f-467b-9d51-c79d28bbd2ce)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 663ff7ee-c38f-43fe-9615-7c4489dfa541)(content(Whitespace\" \
                 \"))))(Tile((id \
                 66a49fc3-2189-451d-8f5b-87a4e7a4a1fd)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4ede6a03-d0d7-4332-8382-c88b3e0c2d11)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2bf08639-ade9-4636-bb33-b789e670937e)(content(Whitespace\" \
                 \"))))(Tile((id 971f3ba1-d70d-47d7-a9a1-dc52bc83fd36)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 5632be0d-1afc-4c45-9fba-7b815246c884)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 e016ef9f-b73f-4ca5-87c0-6e1b9d52fb73)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 40558f24-ca8d-4dda-a0b3-13a39b1927f3)(content(Whitespace\" \
                 \"))))(Tile((id a949b592-988a-46d0-ac9f-176609db6dde)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 06fa3969-d9ca-442b-825e-dc407e6f6723)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2bb73b05-ca08-4c0a-a04c-dadb433de26a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0f424bab-a2f3-4fe0-8a09-8f2cd90c3a07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a87620b-a433-4fb9-9d1a-09c9b3beab6a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 512129f6-6a3c-49fa-bf77-83e9e10f4705)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3ce93584-77cc-4b97-a406-c4462d511dcf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37022cb4-317b-4348-a086-5283db30d7b9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 67645fa7-6a97-4584-8064-fa31c7466741)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a8ae0eb5-666a-43ff-a24c-b8f82ebd4e9c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c1852265-6f47-40cd-83ef-8fe57cae7e70)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 ce5d18a4-9cc3-4438-a7f3-431f8d3a2461)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dac99d0f-a7ce-4629-9f71-f49bd6a718c6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 440becc1-96de-4739-b860-ca47255d3d93)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d4ab6143-0bfc-4c9e-9bfb-b9ee50bd5289)(content(Whitespace\" \
                 \"))))(Tile((id \
                 620340ff-4a97-4b5f-b39b-76f560bf58db)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e7ff297f-b0a7-4c0d-9772-47acfe9a6d9c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6f00d686-a3ff-4431-b4c2-b70d3b4eca14)(content(Whitespace\" \
                 \"))))(Tile((id 00df244e-2507-40cf-a767-3a29432ad8fe)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 130f2a32-39da-49cf-a550-754622dc404a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1eab60a0-5c07-4a92-82db-5971b1db666a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f10579e0-e416-410d-8ae3-12925c91671a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16944a0f-57f1-4e24-a573-1d9231d16e57)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 317f017d-a9c9-467d-8cb9-e56c0495a060)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 6aa18463-de32-445c-8338-c11780d4d4d0)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4a870256-b389-4bbb-95bd-47834fae994b)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 369ea7b7-0d09-42c6-8b54-6cb084d8270f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5c3c92c6-a066-49fb-ba63-589c5b309625)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6dbcf6a1-4176-46e9-802a-b16c71d040ab)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9ef098ad-fe36-44f2-9487-6d9ff6656d48)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1d8fcf7b-fa79-4de7-a9f4-55fff3ada3bc)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cca7629b-c4d5-4045-a00e-197b16dffc60)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7eaffe3b-5b20-4937-a693-400f5a3d7987)(content(Whitespace\" \
                 \"))))(Tile((id d1ce8a09-d6e6-443e-8fb6-77904043ba41)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 f2827f6e-aef6-40eb-9e99-959cbeb8f626)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a5a96d03-071f-4595-bb82-1f287d604bb3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 aa9f6dd4-afc8-432f-9060-c140c31c2b5b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c1620d9-ae11-41e1-ad5e-663b10aa1c56)(label(1.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4c545fe6-4860-489f-aaed-de76ee3722c2)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 90f05b86-6ed6-4da8-a1da-5bc842610088)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 357ac56b-6008-46c3-823c-f71e965f9072)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ef444fb8-d541-400d-9576-a500ff7bea51)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 125ff66a-6b89-4566-98fd-2a802fd970b5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5939a641-eb88-4f4f-a322-69b1571271b5)(content(Comment \
                 #err#))))(Secondary((id \
                 7baecabf-72e3-4455-9cfb-16777b0057ac)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bd06e95c-6b0f-4708-b642-e09aec6b32ef)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 bff70791-8893-4e8b-9f92-e0ac69a6e939)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34216c66-e05e-4f03-91a9-61f892a850f8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f7733b0c-dd77-44b7-b56e-d0c1a6636b6c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8f04892c-18d2-404b-9894-3e9aecc981e2)(content(Whitespace\" \
                 \"))))(Tile((id b48afdf7-f131-4d9f-b829-0bb01434f348)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 2546b193-bb89-4e2c-a1e1-c0c3aa775d8a)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 925587ff-8973-457c-853a-0f6c71cd9c4a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5ca988e2-330d-4e80-91b9-e9031eba4cef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed424387-c3f5-4d4e-a276-ae526e114f39)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 41bdc089-5d46-461e-91f0-3c4d5c8c975c)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 75eaeaea-6d82-4f4f-8ff2-c3d0163c6017)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4516c234-075e-4464-965c-666a0de1e8cd)(label(2.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 cb0e4361-9db0-45cf-9b66-9f1812a87332)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1be9f5d9-ceb5-4b89-811d-6975be2757f4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8cd1685f-b46f-4d3e-81e3-632df69a6af7)(content(Comment \
                 #err#))))(Secondary((id \
                 0193a545-1ce2-4a17-94b8-be94999b8a06)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b5361ea7-4e12-48c1-8444-5e38cbb19a59)(label(\"\\\"BYE\\\"\"))(mold((out \
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
                 let _:    = if true then 1 else 1. in\n\
                 let _: Int = if true then 1 else 1. in #err#\n\
                 let _: Fake = if true then 1 else true in #err#\n\
                 let _, _ = if true then 1 else 1. in #2x err#\n\
                 let _, _ = (if true then 1 else 1.),    in #err#\n\
                 let _:  , _ = (if true then 1 else 1.),    in \n\
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
                 let _:   = [1, 1., true] in \n\
                 let _: [ ] = [1, 1., true] in\n\
                 let _: [Int] = [1, 1., true] in #2x err#\n\n\
                 let _: [Int] = 1::[2] in\n\
                 let _: [Int] = 1.0::[2] in #err#\n\
                 let _: [Int] = 1::[2.0] in #err#\n\
                 \"BYE\"";
            } );
          ( "ADT Dynamics",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 1f7bcab0-da00-4299-b43a-3ca1ef8ca2f5)(content(Comment\"# \
                 Lambda Calculus via evaluation by substitution \
                 #\"))))(Secondary((id \
                 a927feba-9938-45cc-88da-4ca88fbace46)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 59554d6d-5be9-43cb-a4d6-1edf55e3c098)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 41573bf5-78b0-4f95-b6e2-3c4ce9dbd6d5)(content(Comment\"# An \
                 Expression is a variable, function, or application \
                 #\"))))(Secondary((id \
                 5e7af976-9c1e-4841-847a-70c966af0583)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a76c3b61-221f-4e94-b8fd-5b45183df229)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 40bbb356-6987-428c-a8ed-2a6b99066f39)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b4a5ae1-4e37-4f29-abcf-0ce0108de2c0)(label(Exp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 1cc38bcf-e0a0-416f-8cc3-1655722f05a4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 09d316ec-a283-4c84-a902-cdb1b0d8fcc7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5c00fca1-9f11-4ed9-bb7b-507bdcb1fae8)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f097f93b-c8d3-423f-8301-953e7fa560a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f02ccc58-351d-499c-87ed-687857f5aafa)(label(Var))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8096ab83-dd0c-47c0-bb76-b0e811b0c1bf)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 a375c863-da66-4010-a25f-778944a6db48)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 8ee1d2e1-06c2-441c-8fc4-138532d469d2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bc72ff0d-8a9d-4ee4-ae45-b2c94f67eca9)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c7ed4279-1d9d-44bf-af67-23ca84632b04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2c11b78-0c54-448e-9cca-e4c8bfc8bbb2)(label(Lam))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 4d2df657-924d-4f14-9eec-0ecd7f5c21c9)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 a64e19fe-5bc6-4139-8b40-96e3583fa4fc)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f3daa7e8-f243-4222-8ee4-ddc0f9155c46)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 447e6442-bdde-46f7-8882-6bb1f68d67d7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 15875387-e3ed-4473-9c6a-453a68e8b117)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 0f46618b-71ec-4bf5-88ca-75d3bf0a549e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cd1bd397-be90-494c-9ea6-17847e56b805)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 04185bae-3d12-4049-affc-da427e740d6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 559ea70c-729f-4295-a48a-c27dd9fad885)(label(Ap))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2e0537e7-5ee1-499d-9edf-c2eae0b53264)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2bbfcd7d-a9a0-41f5-9f2b-65e6f4f0fc50)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 40bda1cf-399a-4368-9a93-1b34120aafac)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4f27a6c1-9f31-4f48-9f67-81fb4bdb8815)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f9b126eb-6ecb-42a4-9b11-255fcea67577)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 297a96d2-948f-4fdd-9884-8e547ed933a8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 60f3914c-75d8-45b9-91c8-408f8d99eded)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0a6f99c3-1727-45b2-a08f-1194554cea5f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 dfed049c-1560-40ad-adef-7de21ec01615)(content(Comment\"# \
                 Syntatic Equality of Expressions #\"))))(Secondary((id \
                 02c2cda8-cd30-4c87-8633-79f6279f2923)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d1c59a7b-15de-49ae-aaf5-056c65738d18)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 321c3b7f-71b6-433f-8ba5-3a176f503ee2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20a41284-a854-463e-8f08-880a85b83d3c)(label(exp_equal))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 78495729-2b1d-4e8e-8f1d-c9d28f6a7a60)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dbee2db7-3f10-4979-9024-bc34f7453770)(content(Whitespace\" \
                 \"))))(Tile((id \
                 72d5e73e-ff5a-4bbc-9d1d-7e6e1581e413)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 cf35fc7b-7318-4d81-8163-b650fd7de4f0)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 3907ffc4-af80-403a-839d-82b1232395f2)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d7aca28b-65fd-473a-9fb6-65ea96ef78d6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 22f01e9c-afbb-4e08-8acc-d315f402817b)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4ab91533-0de3-4d84-b8b2-7fbe33be34a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c59e6b78-fc5b-47bb-b830-9d4fe67c0b13)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7b30dd6b-c498-4637-ae4a-f6afea955a90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d000d521-0821-47e0-b655-52db20eb828a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 af54a209-ee0b-4c7b-961a-d2565352172e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5668d661-45f0-4cfc-bfb1-264fca05b8d8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2b529252-636b-410b-8ff5-c917ee8e8b64)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 64120593-aefa-4317-bd9d-7901b0b718dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24182c8a-50a3-4da8-be1b-25521534c38f)(label(es))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 db377abb-9d27-4f80-aaee-29019fe3fbee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b6e75de0-9f67-4707-b9c0-c1728ae46b3b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 42e957de-23fb-4f1d-b783-5e0467dae1bb)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 41b2af43-6a43-4d11-a81e-d9d17f4bbfd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f8796638-2f86-4f8a-b536-31faba9bb8ac)(label(es))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 31770179-3d0d-4744-81af-ca2b9883eab1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2c2de1f3-9f64-4731-b40b-2ae4c85c9487)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e616366b-7709-4c52-9c3e-b4b841ec4de0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26d3098a-59f8-4a5c-8b5c-e0fa8621fcbd)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ef5324c2-ee25-4eeb-a37b-cfc90fcd42e0)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 15ec3550-c2c7-4ea0-97c4-367815d9b5b7)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 cf5cc86d-6091-4092-99b1-2fe5cf19e552)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 583b25e8-f45f-40d9-b4c5-cb84f2061a99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ec7b427c-4313-43f3-bf80-c847e66f65b6)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 257f3750-3762-4685-b1a6-6a6fa861007a)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 8006ee75-37d1-4341-8dcf-5a7bcb32460e)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 d57b3841-3010-4604-b2fc-c1f1d0aef0e8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 65d17c9e-a107-4192-9068-c0b205c0069a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7be050aa-cbc5-42cb-8922-53e6e6b12824)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 318d3c65-2497-491f-96f7-cfe6394a8b79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 853da4a6-5aed-44d8-ab43-75333dd82023)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cea85df5-b3bd-4017-a952-d64ad8459841)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7c511c7-5050-44da-b175-da1e4ff92d52)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4f3a5193-c3ac-4370-9c9e-c8df5a37167d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 eb08f193-8966-4277-bc08-9e79add2cde8)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 09f89651-2b39-4c39-b0ca-b69b08429670)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81672889-56c2-48d7-b53e-bad5b4d1df31)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e4c0cf87-1985-4aa9-b4be-8abb8278e176)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ce2d28c7-d1d1-4ad5-909e-6421f2f39739)(label(x1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b279ac2d-53a0-4899-a3e7-29e45637f1d3)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 54c7c5cc-5009-4243-baec-df198852f9e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 64075130-f088-457a-82f6-ebe5d0be8451)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 e094bdef-fc38-445e-92b7-a730961c8fdf)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 e88a31af-6149-4000-b8e2-94f31e45d5fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1ce5a32-e138-43d5-8775-e6f75efe37d1)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c6bf010d-869d-4271-b2b7-828dd4f1b553)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 bb0a3fd6-4dff-422b-bdf1-8460c7d43829)(label(x2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ea64a246-122f-41e6-8284-a7b00542c4de)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 6a6b2ec6-56a9-4ad4-8497-ae680f9b1118)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e17d1a0c-3041-4aae-8312-2164ad00b76a)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 d3650922-ea7a-4f27-89d3-b919d15786be)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cd0e2d13-9e19-4cf8-b896-32cf152976ce)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c948dec9-9f62-48db-a69b-7c4734cb8e9a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8820244d-86e7-467b-b6ee-412309891bf5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ad0085e-1640-4bcc-966c-63535c4dcb4e)(label(x1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6a3c75e8-867f-43b4-af30-e3c33700b8ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98252b63-4ae7-4104-81db-be1b271d1b20)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e85acee5-ac98-4fdb-9b7e-3da31e9a1cdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 517d59c6-6e8a-48e6-9810-deff80d4837b)(label(x2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 64892ab9-5f46-41ec-83ec-830a3771bda2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 654196e6-5c40-464b-882e-f000185256ae)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 358e8514-a640-4e35-adc2-89e552b48f63)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7e712ff-e6a4-4736-a2a9-1051368e4621)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 be1cd4d0-77ec-4b24-806a-d44ab9406dff)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2b9171e1-b36a-4b4d-ba94-b1b383e3d876)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ee427415-9cbb-40bb-be41-483d6b055dde)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c2233df3-e617-438d-a3f3-9764587db421)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c9f2177c-7b5b-458f-a2c8-f1e5e4b6e938)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9350c7fe-fcd8-4395-b2a5-51f3b4bc28d0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4565265b-2b39-4e9d-b8e5-4eac2118eaf5)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7982873e-6820-415f-a686-58b3b38f3af7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc624d2e-62e9-42ea-a935-f0e009fffbc6)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 39ebe962-9adf-45d0-8874-8bc30d5d4c9f)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 c3628f0d-0cb8-4954-a16a-e791d0f9a30f)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4cabbfa5-38b7-45e5-962d-6310e522dc2c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 9c108b07-27cd-4d7e-b2e4-6ae975db2eb9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e8c0903-e399-43b6-ba2c-f014879fc966)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 ad3766ab-7abe-4e5a-b44f-9a9cb6badf3b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3170e2a0-ebdc-4b38-b2b7-a09434ec3f1c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 545a9cc8-240b-4267-adc8-0b2bbfe70a91)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dbc20a7c-0050-4672-bd5b-b298221a315d)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1a06a7c6-f321-428e-b28c-a4f24b3ab556)(label(e3))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ec1e7e19-8acc-4a77-8ef5-0b9907518462)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 bba30512-7bbc-4930-b198-d12e690a5b93)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d36e488-3e41-46f9-a972-23750bcece8f)(label(e4))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 4b4d9e3e-ac05-4a5f-b08f-d5563460b5c7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f282b2b5-5c36-472a-8375-d1a36a8649ae)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f3bd6390-5a54-48a1-9640-ccfa45160df0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7df542d2-05f3-4e21-8c5f-64955731be10)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f60edbaa-954f-42fe-b431-b9bcc014a9c6)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b1f04c3d-547f-44e7-8e7a-8a39c04667eb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 194d5a82-cc1f-4a18-8b5c-c61c904af17e)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 75f2818f-2f9c-4049-878f-c81085700db4)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7b3280d2-d974-4cf6-9bb0-516576217085)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b4990b5-c883-44de-baa5-6298bdcd9c6c)(label(e3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 e67b82f1-96e5-4d33-b100-3bfe9707c8c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc816646-333d-45c6-87de-126307d9157f)(label(&&))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 9))(sort \
                 Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cda90ee2-15a7-475b-a7b7-b58f95ac0541)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6cca4d7-4919-46de-8ffb-52071913367e)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 46608290-3577-4831-b399-c80d5d767d86)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cea40954-b0f5-4ea0-abdb-8f3a0a210bcd)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5d251e7a-9910-4012-9c61-96c776587f87)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ad817f3f-75bc-46fa-ab0a-86718ad8e04e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c9c3dcc-3d6c-463e-bcd7-fba48da6ecac)(label(e4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 a58fe0b7-4a35-4a50-97be-441d4274c616)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e7572e91-07c2-4d65-abca-dd76ec42dbbf)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c7d795b0-3d34-4bde-aebd-a6712a065da1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68ab1574-8fff-4c61-b8dd-81e4b42bfad5)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6ee1936d-6b0e-4790-b4fa-a3ec667659a9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 109c64f2-39ac-480c-b33e-96b08c515ffc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 215955f2-8a04-40c6-9f1f-14652d8f2637)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 16e0c010-73cd-4dac-ac53-2536fa7068ca)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4f239c3f-102e-4227-9388-89983d93b887)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8172db9a-32d5-4ceb-a908-3837509edd80)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d3329425-9616-473c-9f10-82c062d01611)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5ed2278f-50d0-4255-94cb-f201169e5a88)(content(Comment\"# \
                 Substitute Exp v for variable name in Exp e \
                 #\"))))(Secondary((id \
                 fdf34800-63c7-4baa-a41e-ec2517696344)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 980a0de3-6dc8-466c-9fc5-57c0161c32ed)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 968f19d7-ca3c-4318-a56c-ddaded903c52)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5a4093b-3f2f-4543-a9e6-d915edb78d16)(label(subst))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 bf012f47-7904-4b04-b435-261c6898a74f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 292ae481-93fe-4e75-b4d1-85a12c13d583)(content(Whitespace\" \
                 \"))))(Tile((id \
                 365f62b2-81b6-4a77-aa1a-c14fafc129f1)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 54243b79-d02b-4ca1-a5ea-b6136ac1bba8)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 15ef7894-18f2-4632-8549-55d65bd68d5e)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 04d28679-49cd-4169-90ac-3ba15295a145)(content(Whitespace\" \
                 \"))))(Tile((id \
                 61e87e55-f097-4946-a07e-9608191e9e8d)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 fbbe13a3-aa6d-4dd8-b425-66d31fa6ab5c)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 76d1a094-2cfc-4b1b-8491-faa073126a50)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11012759-a911-472b-92c4-13c296f014dd)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 36c53e8b-58b4-4b29-90c7-bcd8283d021a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 337cc1de-7007-44a8-b7c0-e2d62dfe038d)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2cbdefda-74b9-4e12-bb77-968981f48d24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6669c981-844e-4bdb-80e8-49f63a37c170)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))((Secondary((id \
                 c9570d59-780b-491a-84f6-f38720034df1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cd3956e2-872a-48d0-b61b-49c9627f7da0)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d889bd03-eae8-466d-8ab2-6a37b228101b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f2f5c27-b0c6-4d64-bec4-f59a3d62907e)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b1a377a9-9f1f-46ea-b9c8-4a735177c681)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4e8b2929-daa4-4997-a50c-41215cd5dc97)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6e2f602-5f94-4779-9f7d-6150753e387d)(label(name))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 aa15bace-4f95-4777-ae5a-86efcb21d80b)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 13bc3e73-cc08-426f-b842-8f326d5e06fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 70cea4ed-3f34-4564-b948-5b37698e0059)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6fd33e5b-3792-4bb2-9d66-e7c7cfd82c09)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 90f64cae-e6b1-4c17-a7e8-f843bc7533d8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 419ebed6-b5b2-4540-a431-3096df690104)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 7f57f5da-29d7-4afe-af81-9ba4cdb84290)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84bee910-aedb-4d4c-baff-a4571eabd806)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 75df8104-bdda-4020-aa34-461d8ef9a61c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f680450c-a013-4374-a0d9-3b7395b2d291)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 5ba10a8b-3767-4f49-ad59-b4d685b3e8d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c85b62c4-519c-4ff6-b53d-084bfc4b3a4a)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cbe676ad-d210-4614-a6f4-f1288961392a)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 b3d931dd-62c9-49d1-9c23-57e51491e2fd)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 39dcfa1c-d518-4072-925b-5a857363d84a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 28da4de0-f438-42e7-8193-7123c709da52)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 de7081b2-0362-4667-be82-f6b027b192fd)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 185f0a9a-db51-48ac-9671-7308af2566a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 94fb704e-4ce9-42e6-ae34-d1b88af3c424)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9298a278-bffd-4a4c-baf9-1aed6d1562dd)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1b07d13e-1cc4-4476-b7aa-1d947539db97)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18e2d5d1-c566-4eb6-85a9-8185ac89e46b)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3487f0ad-8f80-41de-905f-5c7681cab87f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 53fc5638-04b6-4552-82f6-17a089316a9c)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 955e949d-0d06-4724-bd32-3869a60d1509)(content(Whitespace\" \
                 \"))))(Tile((id \
                 714480e4-4cc9-4f0a-bb16-fa5eddae8ba7)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cff5290a-3b75-4adc-a07f-57162f45e70a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fd1933b0-7135-44ad-9295-1cdff91a4700)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f54eda3c-4175-4cdd-a22a-bd44d64754db)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 80942fdd-f28f-467c-be15-f40257293d0f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 59fe67c4-9e46-41da-80c5-94a08d421127)(content(Whitespace\" \
                 \"))))(Tile((id \
                 053268f5-aa5f-47d4-bc35-7dd044c4d016)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 48783eb9-b6b0-4995-98f9-9dea00a51ea6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 446ff674-42cd-4716-9ec5-caedd30229e1)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 026fbda4-70e7-4982-966e-131e89f8b063)(content(Whitespace\" \
                 \"))))(Tile((id \
                 970904cf-54f0-4476-949e-6bff04296230)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1c58d46b-701f-4fdd-8122-3a8b6b717fd1)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 8ee0764c-6ae1-4a7f-a43a-da8e9e2ef1ff)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 139f7e26-a096-45dc-b86c-b1759995e640)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 584fe536-95b9-467b-9d45-8ff79f932322)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44b651bc-58e2-4d0f-8fe7-f9f4ef2df975)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 d8b7e4b6-2830-4ae4-8765-ad8787599005)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 df1a1724-1680-4c13-86c4-cf1739d1f2f5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b12dc4f1-3584-4d70-9df4-56748637b5d9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 789ee2e2-b17f-46d2-a729-97747b3d3b3c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eb31c313-cfaf-4a04-97be-5d02a39afa8e)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ebcd5284-9b96-4e09-9e05-1c856050c226)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 33d26cbb-f822-4b7d-a3a9-a76ae159a0c1)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f688d2b2-b8c9-4203-ac30-acb9b78b218b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 47729e37-9963-4286-9683-323f9b87bceb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5b9286e-a2d4-475c-bbc6-6a0ec96cfa1e)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cec07007-77a6-4c7c-9ba7-4d851d46afab)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cc33ccf8-2ba7-420b-b2c6-21aea451a89e)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7d8a5934-f197-4252-a244-ab397fd8f6a5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 cc112f0c-4bf7-4662-917e-846de267fe34)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b02b84ec-218c-452a-bf1e-9617cef8490d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ed7443d9-4404-44fd-a4e0-3f6559f33e82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddac8323-ba4e-4d64-b5cc-7e69c920435a)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 a26fdd56-19a1-4583-96b1-b2e6f4dce75b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4a409d0b-c2f8-4b1c-b160-ff917085bf6e)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 da613248-93ad-4ad8-8958-3b005c4c4629)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de5333b3-77bf-44f1-a7b6-64378045d95c)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b29c0da4-d842-4d39-93ea-68ad05af9261)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1e386e4c-0118-4aaa-a61b-7c459652561e)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 ff5111c7-24e2-441c-ba22-b00b9570b868)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 a5e422b7-f453-4fb9-bf2e-d8fbb0f1fc19)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 2bb31630-3fbf-41d5-a19f-37006e884723)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 334192b2-d1f8-4e50-84a9-f63119f5ea66)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5cdcd943-3223-4e31-a4c9-230ddd7cadbc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 04ea99ba-c0d4-4f96-8514-512d2679d38b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6698f873-dfec-429f-aa2e-5b753a0e0b13)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 fd041cbd-11e7-483f-9894-149d647575df)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9b6f0264-1a4e-436d-a922-f0325bc2e4db)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7a495fb0-7163-4e39-9013-81f4b1b49d70)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 54fc8758-e83a-451d-bb69-409eb19c6735)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c3e19a5a-f9b6-4f24-8f7b-64f099967f9f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 71fca11a-3419-4757-b711-fdd18405d97c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a763abee-d9b7-45b1-8424-3ef1a0a271e5)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4faf4138-d473-4173-95a6-eea2542573f7)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9e46b718-8343-46e5-8691-99218fd48457)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a42a695-0600-4732-b130-fdb906e39384)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9346f2a0-c777-409b-91ae-ac99409e69dd)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 175d0889-9c8a-40ce-a0d3-9e16bd3e343a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c57d2e8-6a01-4794-9165-25cbc0175a5e)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 05ee19ae-081d-4fb0-affb-ecad3fc03096)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7c14b792-807c-4156-af95-90f4cce11fba)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3341286f-c3ba-4879-a6f4-24cc00343cee)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 80da2600-590e-496c-9670-31ea4bc2e302)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a0ea697-f4c2-4fca-879b-a4b7ac213c8e)(label(name))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2d9420c7-44dc-4781-8c4f-fabd96d95ad4)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7bf8c927-0575-470b-b3c5-a29a553b6830)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5739b8d1-04a1-4835-8688-50bb5a0929dc)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 cf9ea30f-5bce-4d8f-bcf1-f47ab170f0a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 140b4e40-9a69-4603-a8ff-f54f43833aea)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 83a2646d-03af-4380-950e-4ffbec2efecc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 60a803a0-45dd-4d26-9e50-9d41b42e060b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 55f117b1-90ee-4043-97a5-eaad60fbf8d8)(content(Comment\"# \
                 Evaluation can result in either an Exp or an Error \
                 #\"))))(Secondary((id \
                 75edeef0-b93a-4e52-8eb7-956406120f02)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d9758f2e-b154-4d74-9d8d-ddb629f96371)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4e0f5968-053b-43c6-8ee7-f9e839b0d176)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ee7d546-e23e-4b2c-808e-01af4f749706)(label(Result))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 bd8a61cf-b940-40b0-9fbd-c698a60e9df6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8a5beda5-5224-4e5c-8dc0-79e6db7f1fd3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 91cccf23-4d44-46e7-a81e-a046a2566144)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 24bf5ab5-af15-4566-9884-a2061fb39d79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d58cbe8-1f96-4129-a6bc-5fa73566b2cf)(label(Error))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a9b509d2-8f56-48d1-8e5f-3fdafc9415e5)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 9f7a0898-680d-4868-a84c-5bb556f1612c)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 689974ef-89e4-498f-bd4e-34a6720427d3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52b2d13f-c806-4877-84fe-6d32ab1182b6)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ecf58e9e-0efb-4e01-ae05-1fe9b21d03b3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afce6860-9293-4f8e-9ef7-dd0c60ca95ab)(label(Ok))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 aea698dd-d63d-4fe2-9fcf-7f224c41a59c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 40472e30-a169-465e-af55-751ecf617aa0)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2cf73080-004a-4f90-b789-9221717f6081)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b27f5376-58b7-4974-beaf-4de3558d93bf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f43fd6da-318e-4965-a2a5-42ed801cc41f)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 82677a8a-59a2-49cb-845a-3c3876b5b300)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4f966c26-0371-4ef8-a075-127d1f0321f7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 621fd573-caa6-4583-87f9-b5825e167918)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cfd16b5f-c56a-49a9-a7f2-44b0be14f475)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8581c765-76a3-422c-9479-8eb2cf7c104a)(label(result_equal))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 125cc189-7080-41e8-a196-b7ab8ad54f77)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 db9bdbd0-7b2e-478c-bd6e-8f8b5c508ecc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1041388d-9c06-4d8a-a535-05b540bff54d)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2ed6f843-67da-461a-80ca-5d01c34c12c5)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a73ace3b-2a20-43c9-8697-861ce8173c07)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 320fb96f-b4ff-4d76-a951-a09df19091e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd4f3152-7c7f-4a71-be23-f9d7c0388c9d)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a92c9445-e047-476f-a541-5edeeeb864ee)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ca0ae608-6978-4d91-a8cf-9103dc0e87c6)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6b67160f-a805-4eab-a904-5786bc35aa7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbd87072-37ba-47ef-b34d-4d2da770afe3)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7c817c8f-ce88-4e4b-b96a-e26c7da1c387)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 64f3bd36-89a3-46b3-bc72-98972d735233)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5f64f54c-d03a-4e99-b8e8-cf42d0043c9e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 0286c456-4807-4a6f-91d4-10e73c6aa351)(content(Whitespace\" \
                 \"))))(Tile((id \
                 075698c5-49d1-4818-a2a0-925e47ebb73c)(label(rs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 79fb8f75-8463-4067-bc1f-8a01459d9cd4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 055d2b42-baca-4a18-94a8-2dd4163c3cf9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4666bb19-2d4f-4d4e-acd4-3b375a5d357c)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 53c9f8b8-bc8b-4251-ba44-04c921e86785)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a21b674d-34f5-4d6d-9f85-144377e105c3)(label(rs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 36300ef2-f0a3-4e40-99de-9e3d54bfcfe0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7d884095-55c3-4b3c-add1-d24317fc7080)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9a9b1e20-4bc2-496c-8619-b0a889a7aef8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 262fd22c-29f4-48cc-9b52-e7176e683b94)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f8074977-f93a-463b-98e4-d69bfc55307b)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 9bb655c2-b54d-4741-a1ab-0addfd13c98f)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 f9451c6a-d4bd-4b58-b077-397aaa290272)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 6f3b3236-cdce-41a4-9232-0006322b7b57)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2d471ef-1c46-4a9c-a54a-fe1ef3cbb19b)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 60028cb7-d928-4ee8-aa7b-9dc38e3a7e18)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3a1ad870-b7bc-43d0-b716-12cb9c23597d)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 c50a22f8-4eb0-49cd-8144-2300c10e107a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fbb838b6-e6e9-4dff-a09c-523e350c630d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00557cb6-1130-4a7d-8368-3412f00d393d)(label(exp_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ba4eb367-ef3f-4525-93f9-bfa92d9d7c97)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2502b290-8870-4d81-8f5d-76609e24860d)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f484106b-41ee-4800-9a7a-a6a885e60b45)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 96e06e4d-be79-4633-a71a-a32b52a6153b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4b1e526-e611-443f-b5aa-6be2f7aa6253)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 73ddc6f1-7d14-4b25-9602-96415acda0e6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5f5c67e0-4040-410e-92f6-62a141252bd0)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ab885d68-e990-49bb-a610-a53b37d77456)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cfa1b087-0510-4b36-ac15-5a61f8aafa38)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 74c5a935-852f-4da5-878c-887bc26c98e9)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3ff6542d-a8bb-4716-99da-9367ecbd49a9)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 58f2f26d-e37e-41cd-b13f-8d7e74228172)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1e2b816e-7642-4f4e-b95f-adf12abbc8ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8416cd9f-2529-4db1-9035-20229b62df28)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 dff4702f-08b6-447d-98c5-0f1fd993e456)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ace2fdc0-80dc-4300-97ea-843487707e22)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 21a5a76f-b56e-4eab-bc27-4365a8a5c5fe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 987928b6-2cd7-4cd8-92c5-dca894fee268)(content(Whitespace\" \
                 \"))))(Tile((id \
                 39426536-b6ae-4d23-b940-c09166c487f3)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ddc21c27-b545-4704-9b83-73a54e7f97c5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b46541b7-7399-43b2-acee-ef0325e8909d)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c9a08e3b-dc53-4576-b101-94199ea10081)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4aae0476-095c-4045-843a-f7dd32925cdd)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0606e959-ddcf-4d32-b61d-68b1bf791355)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 70cb209e-2fbd-4019-8a9c-f55ae2b12881)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ea405901-ebf8-4432-9bc9-2ff5ee8500f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bbc620b9-6f94-49ea-8ed2-4c7b6bb323ea)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 460c3b9c-9811-4fe0-a6a3-453ffaf1290e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e36568cc-f82c-438a-9b67-2098c7bfb269)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c3d6b48-108a-431c-817f-138af3f8a50d)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a61bcd07-c82b-4dd6-838f-f4d9762a0a2e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0e699793-d7c9-477d-834f-c62eb4458558)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e06c3270-8e19-4c45-8888-e9301f3629cf)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 88cd77a5-2e10-4d67-86dc-c0acddaf3a7f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1f1fecf6-bddb-4813-9c11-306183d4da10)(content(Comment\"# \
                 Evaluation by substitution #\"))))(Secondary((id \
                 5ea9e27d-1731-4316-8506-fc45c5e70003)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5fb6f0e7-3581-4553-95f8-a84bd4161861)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1af286e3-978a-4ff8-a94f-5f2891068195)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1e44354-cb44-4d2b-8560-e8ad5adb5dd3)(label(eval))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 25a60777-31c2-4b6b-b06d-cf6d67d61e3f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b3f75514-9770-47c1-9545-9e19f15df048)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62603e3e-6bba-44d5-bc4a-206542a22f33)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4f14df0f-7731-4364-9efe-7a353e852f82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 202553fc-31fa-4c18-afc4-2ebca2469d9b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 846d17c4-4bc7-44c0-8167-cb7b5f7338a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc4991bd-f326-428d-a8cc-44bd3c40b891)(label(Result))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 90e6f91f-19c9-4e74-9ece-c08e8cfdb26c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2916646c-3df4-49f6-86de-25e96dfaa42f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 070d68c0-e8a6-4ccc-80df-51fd8782e5e9)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 10982210-ebae-46dd-8fa5-34a0e418a12a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 083a4aba-2a8a-4bd0-a272-ac89effa0f9c)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e2da86a1-b62f-4be6-ad50-bf10dfe5e93c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f4abddd0-6c1e-4227-beba-dc836f87d938)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc129b5a-7dc2-4b69-8c7f-28ba0e70a587)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 6c3d599d-67f7-4278-ace2-e76739314f6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fbdd7d29-dde8-4fe5-944f-cb29f3309bf4)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e4b8056b-10eb-4155-a896-24716bd904a9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ac689c9a-3569-4419-8081-5276776f7435)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4244d236-5133-45aa-8b62-20cca6dc5a77)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cef34e10-3604-4ce6-910b-2fa21cd6d2f2)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3cb54504-edb9-4812-be0f-bea0fdc875e0)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3e8b501d-9a2e-4a7d-833d-89a526a5fb8c)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 191e6fef-b0be-4e08-a147-64ae2537fdf7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1de92859-2011-4c76-866f-c882d0c1cc38)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d00194a9-72cc-45ca-9c73-5d46869f610a)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c0a0ce90-81d8-4b09-ba8a-98934dd238bb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 17469555-3ef2-4223-81a3-3e09a59f7c61)(label(\"\\\"Free \
                 Variable\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 f5ae48cd-7b58-4b89-99d0-ae90a87cfba8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 28d4ec8f-4748-49f2-8af0-aed3a76b2701)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 52fc7222-801f-4ff1-9367-4021e1a8cc1f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 82301ff9-b986-49ee-a14d-eed128b05b7b)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 13384c5c-89b2-4e49-9c29-ff28cdb24c5f)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 2e67fc4b-20fb-48fd-aec5-5c5e6a3c9cf2)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 117717d0-9155-4b0f-8082-a371cc2d3555)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 9ab8747c-aee4-46b5-855f-07d5c1188b32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ce6325f6-7892-4116-acbf-aa92206a96b0)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 89c70863-8308-4aee-9a98-28d74dd74526)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d38b5a87-d86d-4461-8ee3-e4b504aea6ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c0309d83-842f-4163-ba0c-00274c4c765a)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a168e86f-de75-4996-b05a-b76c84dc9c83)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 37b7507b-be1a-4d12-a966-9ad509579bca)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ae281f5e-25e5-4970-86f2-1ddbca9b48d9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f83349a9-1cb1-445b-9d8b-0cb26cf0b432)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5315a959-83e9-4805-9bc4-dc227d30c63f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7a01c28d-bc24-4523-af0f-a19527a9d15e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c658b71e-92ce-427a-99dd-d14e07d4b579)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2ce50808-b44c-435e-87f2-a277c5be92aa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1380b225-c096-4d8e-a111-39f805c15707)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f0c7a241-6ee1-4097-bdd8-d39550de3ad8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d41bc9fb-4280-4c3a-86fe-724f87ca056c)(label(Ap))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b63a9734-7644-4072-a5bb-d8b2397a7dd8)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 2aa0a5e0-c746-4ee7-af22-61de73933464)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b1308751-e597-4f83-8bdd-f11a6cd7c646)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 df669107-279b-4daa-a84f-0749a93573c7)(label(e2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 26880146-1ff6-4797-8c69-313cd096525b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2fcf5303-c991-46fe-92b1-8dc91a89f05e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8067ba50-823f-4583-b983-fe007004442b)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c2900d76-5805-4b52-b24f-2f09d3078093)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b19a74d-4f02-4bb0-a68b-0f2b9459060a)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0443a8d6-3210-49a2-bd1a-43350c73bc6f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 966493b6-5a35-434e-ae30-f28eaf26ac7a)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 18904f6e-8219-42b7-bc23-a52869c83453)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fb1b10f7-a7aa-4b2a-b1e1-868c9cf6700d)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 aef8a32d-571e-4aef-92fe-a133e61561df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1622c6b0-1aa1-4a9f-9ad1-0ee2f44e440c)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 53c740db-a127-4286-bbaa-fc1f6bfd897e)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 86981a7b-1855-493a-bb5b-bd50acbb393b)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5e484724-9f82-4027-82b8-3b9c2201e4bf)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 8b69eb6e-d632-45a7-84b8-c2f552f1cb2e)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 516a2ade-2d02-46f6-b79e-6f262e8774c8)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 94ecff3b-c7f6-47a8-88d5-26fbb6602902)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88490ba8-3f86-411b-a6e9-817f4f662690)(label(body))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 6ef6102d-78b5-418d-84e5-9349d627fbfa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e35cd324-d16a-4d68-a97a-b6ca15cfe3f3)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 446fdd16-d54b-4be9-9b0c-0d8eeafc0543)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2cd4a45a-528f-4945-998a-3c0b9cf9c2c6)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3c39677a-03f1-428f-b295-7a33f2fe9466)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1aec83cb-0253-4eab-8bfc-73f5c6ddc33b)(label(e2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b0b29140-8d63-441e-a576-1a3ab3f24521)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c15d05bf-8519-455a-b0be-d1b096b074df)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4d0f5b8e-1a10-409e-b434-2bdff88856df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 136333af-71db-4b8f-bf85-bf623512b58b)(label(Error))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f0793b56-f2cb-4a53-823a-49dd57145a53)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 9456aff2-b841-41a0-b360-ba6adf6af864)(label(err))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 cb066a61-a29e-4636-9e26-a15ca426b0b5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4ee95925-905e-459a-8bb5-b39ec5767d45)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c2afa8d-cc20-4d23-a6fc-64fc225fe1c1)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9e259835-32b3-4d25-b557-fd1b9308b451)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e6c214e7-1c90-4ac4-9d87-82c9c3f5f71e)(label(err))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 e2d4aebb-8638-43d4-a0a9-b7dfaf103905)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c79c0790-1bae-4fb9-8d15-28b090a7bc01)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9c30c747-f0e7-4cc7-a456-7dfe67178cf9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ec5195e-9152-40f7-8116-ccc26b2c8209)(label(Ok))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3ababe4d-2781-429f-98a9-f502d9dded33)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 49228f0b-a42b-4651-a72b-1517623527be)(label(arg))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 a42e030d-5e30-45ca-8417-58a64f6ff13c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7b39247b-9eb3-4eae-b44f-90edd3df62f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0595a373-e5e2-4e31-a669-95d86097e70b)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 82a478f5-4725-472b-a5dd-6cfe2e76f11c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b6be841c-aaf7-4070-a3e5-a726db8865d9)(label(subst))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 144a7fbf-4378-46bb-89ac-79cb25a98c3f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e30e8889-ff79-4ebd-8fbb-9402f15aa19b)(label(arg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7d914adc-3684-411e-af89-b4e8071ec029)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1a41d8cd-22a5-40e7-844a-ab4a97c3bcfe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5480aef-5c47-4ace-b998-5be13ee9629b)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a78fb2be-cb85-4ead-af7f-55cce846673f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6bcb096a-7abc-451c-a5a9-ab38b4af844c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 079f72b2-aba6-494b-9058-3e20faa9d8e2)(label(body))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 13b45363-8f7a-4f40-8523-60c70ae7c46c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 83893eed-46bd-447e-b7bd-83d79a38dbb4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2d63cd2a-1308-4775-a9bf-061a4d2fea11)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 401c3082-3f59-414b-be4e-a13645eb647d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb0c360a-dec4-4a40-a1aa-24869a58fc13)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 898f50b1-2368-4f52-a9c6-cd969565f49a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9798505e-5af9-4e57-93c7-6f71648ff0c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef2d1259-52ef-4da0-94ec-570086258675)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5aada76e-7e1f-4ca7-b2c6-8dd086becf92)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 01634b4c-d027-4844-9f86-dadd64218852)(label(\"\\\"Not a \
                 Function\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c9b5cbea-1cf6-464c-8cf4-0ed77a1e18f5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3d67d23c-c9c3-4c4a-b56f-a16e505ce1a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ff921f44-34e5-46e7-993c-6f7dff22bf38)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3fc86b20-6a2b-43b4-8034-94d052a0df3d)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 673e832f-5fc1-4af6-bc50-4cd857d08c52)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f3613090-1dfd-4946-bbfd-3d652d389e93)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 3447e25a-ee47-4222-8bb1-828d0830b231)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0e96ed0-2f67-4271-904c-07744c73b7c6)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 95ba3fae-6bf1-479a-b63a-6035111e0ebe)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 d846ac6d-a212-4661-a1ed-2bf31f39fd8f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 798271d1-e065-4eb1-8e4d-908ab5f140fe)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 adb0fce9-0053-4a86-8b79-6784f476c68b)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ca7069de-be87-454c-9b0c-3d643c480ed4)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ff999918-31f2-4bfa-b172-5a905e962eba)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 607cbd1d-522a-40d0-bb3d-b8b45de732df)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 6e383875-6c3a-4f2c-a401-91ec7bc58276)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 aa7b31b1-3aef-45b3-ad22-0e5d7059a49e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f7b206f6-d2a5-480a-abc0-b9cacabb4689)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8945a9d0-81eb-4b24-966f-a818217214ef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8794ac1c-4080-48ff-8840-6e796567dd87)(label(\"\\\"Free \
                 Variable\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 70900efe-dc3a-48d5-9f91-c70edcedaa33)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 e3c9daf5-57f5-47ef-9b24-a7e6d6936805)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bad9600b-50a3-4377-8ac7-da71b362083c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a6e44360-03aa-490d-9e69-7783234649f9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 64a486fe-c95c-4d78-862e-b0d2cc480928)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 931264d4-b6f4-4c62-92e1-0196ed942eb2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c9eda3f-b714-4c05-9c74-506bd14f6fb5)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4cd76acd-9533-406c-ab10-13cd09fbfe12)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 afdaeff5-0984-4feb-9862-b326d63da2c0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 93745552-a939-4d1a-94a8-0b642e9da80b)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e0aa7d92-c7cf-416d-98c2-737ed6338a36)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ab486f19-c1ed-4a4b-9604-4be2afe0b0d4)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b233e5b7-65d7-4be3-af87-c80e69046876)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f414241f-8fdc-40a3-a775-3d8b903ac358)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 395ceecd-3b18-4153-9800-e329c7a5598e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b9ea6977-82ae-4e10-8f43-f3ebe314826e)(label(\"\\\"no\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 ec9c2830-8c19-4483-8b29-90f246a6067a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 83f58d35-a260-4168-a7c9-ac3f29f9d5af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e9ce79a-243d-41bb-a607-18a5b570fc7a)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c121d549-80c7-4b6b-aed9-f6652df7d1a9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7b74c555-9efd-4351-8d0e-d6bd96f522c5)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ed288520-0baa-4871-b514-49e06f702b9f)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 aa546db0-cfa4-4049-8ade-00c91c2d9a55)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd5050cd-9f89-440b-a764-46510d8a918d)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dd639cdd-c2ab-48d9-b7c1-bf7d1e5359e6)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 78ea24ff-db16-4799-941c-563b8f536566)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                 65acf282-5b10-4c18-9d05-ccc5b7a17286)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bbba711a-f484-4101-9aaf-286054cc8f2e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9fcc45ca-c64f-45d4-9ff7-13acf6aaaf73)(label(Error))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a361013a-f8c8-4887-9a10-26b954165ab2)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2a3a4f6b-dc4f-47a2-abcd-10af0387e047)(label(\"\\\"Not a \
                 Function\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 8cb6eac9-4e2a-44ea-aa95-e39f67531832)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 9bd804ee-2b9a-432d-a097-988afb9b59c2)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 18dcc81d-619c-4b42-af2e-4056040039d4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 bce6c1d2-113d-40e3-a191-b64c2935564d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e8df04b-e852-4d0e-8f1d-fbc121575946)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 edcaf4e5-eb6e-4180-8a6d-0cc69001dbc3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 27acf41b-ea89-4ff0-899c-8c3d9be0b3ba)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d6debeb0-6fb3-4ab0-86cb-d4bb187c8c63)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 4030662c-5407-4ffc-9c27-e4b3e8d5ccfa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0ab33bc9-c61e-474c-bd64-1a4650e90b3d)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 be5d6f5f-2e9e-4c55-82e7-0f2950749625)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6ec85bf2-374e-4ad4-82c9-9394b268f779)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b618b7e5-b03e-4014-b091-604e08e9eda0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f856cd2d-583e-4b89-80e4-77bfe5bafa3b)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 62e293ef-5518-40f7-8782-404f25940817)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 04647702-4dbf-4851-a845-ef76cea684a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b75e01ef-ee6c-421c-8599-991fe24e1e13)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 002b0979-7e4e-4e55-964d-634f97797462)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7566c730-a883-4559-aafd-36ea07e85266)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 87489b74-6fea-4bb7-b188-cc0f293f1501)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5f9cb66a-3c69-44fc-98f9-8d33c2242e62)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 305c178a-7da5-485c-9212-97518172660b)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b9c49a17-ddb8-4412-ab2d-e84a31674c1e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 6d928f5a-76ba-432f-84c9-8985bdfbae21)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dd04ac7f-c000-46e2-9346-68de5ccb2c37)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 09183f09-b197-4dab-9712-2ad6ec8053c1)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2bac98eb-578c-4187-8d5d-0e428872fabe)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2f510267-53e4-441f-8c4e-164a43cb1d76)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f53f3e1-50c4-41f6-9e69-d60a4bac4562)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 37e690bc-a872-42fb-a204-ac507f930218)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bfa62c97-6b4f-485c-91a5-f28b7d3429dc)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 4c3b2e45-d2b9-42a4-9096-00e583a9d66d)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 2fe2dd64-bbdc-483d-a33c-8607a5a74f99)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ad91ebe7-fd4d-4455-bd38-c279a70de5d3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 461d55f6-6f2e-457a-b986-ba720f4d3dd4)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ab21e019-9c01-4fa7-a20b-de67b02612f4)(label(test \
                 end))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fed3fa2c-11a4-4564-8eff-e7704b696268)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1fc38e53-5324-4b3b-8a05-04211a30616e)(label(result_equal))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e63a04ff-39de-4da8-af96-438a294a9c92)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 22edf468-f96f-4297-bae1-f1db449a5712)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dd1f6a2a-39be-4937-9dca-ea18760871ce)(label(eval))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3ce8a7ac-fd0d-48ae-b434-59ee497324c1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 567af97e-40c4-4b66-b5c0-c7dde61131a2)(label(Ap))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3a8e1695-9c19-4860-be5a-b2bf92046582)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5c422f00-d26f-46d5-92b4-849556a6cffd)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2eeb6db0-38aa-4479-bcfe-197543180463)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84ff238e-dcf0-4449-b9b7-8d1ecd08d65c)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5eef209e-e9e3-46fd-a7d2-ee1d841fdfd1)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 627c15eb-037b-461e-a999-b5c720c0c27b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e329ba5b-ff65-4a54-bd79-11119d2456b3)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e07b3d06-1fec-4d51-9424-4863f3d93d4d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 f029b961-fcc9-4f8b-899a-0e7fa1cf11d1)(label(\"\\\"yo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 729a33c6-6475-485d-a8ae-d48ac1c2bf6c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 87f21d69-952d-449f-84d7-1ee2ab05d40b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 075b586b-fb78-4d80-8704-b63a67e7cd3b)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 94246c4e-444f-4b3e-bf20-fd2a13fc27ef)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 208a8991-100f-4cef-b433-eca8159ac882)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 53653980-ea4f-40b8-9d29-d359d33fa7bf)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 ae6269c2-ff33-4f78-83c0-cdf02b2cdc28)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d26a17d-feb7-4d81-b23c-d80ae50d9dd6)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 71b62601-4be1-4205-8377-9ea8c5bd26c5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ed958ead-78f6-4f5a-bd6c-94d57f8393f7)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                 15383e63-cf89-4209-a0bf-6c3d86d862d6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd335c92-161b-4e37-988d-acfd90066a8e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0a5045cb-aada-489d-ab45-dbacd4fe44ed)(label(Ok))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 aa0170f4-3445-4a3c-bfd6-9dd4bc5349ff)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d68e6fef-70ce-4b20-b210-e41015a4027a)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cda0a2c8-1d02-4e25-ba06-7fb9900cab81)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 36f04861-35cc-4eb9-83d0-01085dcf7656)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e502a97c-aa0c-4e34-a1c9-9d86454fe1c5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 62eb476d-c91c-4c67-bd46-be123833cbdf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ff88e88-1e7c-4117-8cfa-6431b1c60b5e)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4e3e4ddd-946c-4afa-ad49-4dc4096d8f9d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e9180871-be73-4a92-a949-010ae5b1a3f8)(label(\"\\\"bro\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 88a4c943-608e-454a-94ad-7dcc08046493)(content(Whitespace\" \
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
                 + Ok(Exp)  \n\
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
          ( "Programming Expressively",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 73cb2f1d-94b4-42eb-9d77-a832748556b0)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Composing Arithmetic Expressions",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 41444a61-1cf6-408b-82c6-464f3ca6750e)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Computing Equationally",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 c7e3b9bd-efaa-41f2-800d-9986a6e814d6)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Variables",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 92e68905-30cc-415b-b920-6323180c56d1)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Compositionality",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 661cbe22-ffdc-4fa3-b227-f7b56a1c8ed6)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Scope",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 f213697c-e203-41c4-8888-fc12e3ac46bf)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Shadowing",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 3d8e1707-1e20-4160-946e-73cdb9e98ee1)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Booleans and Types",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 f49430b2-1265-4e87-a6d6-795eb57c37f8)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Conditional Expressions",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 e2b7d6b9-5b95-4fad-9278-60097f30375f)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( "Functions",
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 b60e7d0e-e290-4b23-b03c-7fe121fb5dcd)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
        ],
        [
          ("scratch_Basic Reference", Evaluation);
          ("scratch_Programming Expressively", Evaluation);
        ] );
  }
