let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "b1000001-0001-0001-0001-000000000001");
    title = "Table Column Projection";
    version = 1;
    module_name = "Tu_TableProjection";
    prompt =
      "**Column projection** broadcasts `.label` over a list, extracting\n\
       that column from every row:\n\n\
       ```hazel\n\
       let fruits = [\n\
      \  (fruit=\"Apple\", color=\"Red\", qty=5),\n\
      \  (fruit=\"Banana\", color=\"Yellow\", qty=3)\n\
       ] in\n\
       fruits.color\n\
       ```\n\n\
       # Task\n\n\
       A book collection is stored as a table:\n\n\
       ```hazelnostatics\n\
       Book : (title=String, author=String, year=Int)\n\
       ```\n\n\
       Implement the function\n\n\
       ```hazelnostatics\n\
       get_authors : [Book] -> [String]\n\
       ```\n\n\
       that extracts the list of authors from a book collection.\n\n\
       Example:\n\
       ```hazelnostatics\n\
       get_authors(^^table([(title=\"Dune\", author=\"Herbert\", year=1965),\n\
      \             (title=\"Neuromancer\", author=\"Gibson\", year=1984)]))\n\
      \  == [\"Herbert\", \"Gibson\"]\n\
       ```";
    display_hint =
      "Use dot-projection on the list parameter to extract the author column";
    task_reference =
      TaskRefDocs.compose
        [
          TaskRefDocs.function_definition;
          TaskRefDocs.column_projection;
          TaskRefDocs.table_construction;
        ];
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        relatives =
          {
            siblings =
              ( [
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "d346e77a-ab83-4a4d-a806-35cf3b98c530");
                      content = Whitespace " ";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "cceab3c2-1591-4e76-9e48-36b2e9d2ee69");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "586abd4b-1bae-438e-be47-f6246e24e251");
                      content = Whitespace " ";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "840065e7-1d24-4587-9cc0-d9bd3f9db26c");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 45; sort = Exp } );
                      };
                    shards = ([ 0; 1 ], [ 2 ]);
                    children =
                      ( [
                          [
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "673bb101-9731-45b2-b9d5-6c80e800a75c");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "74302deb-ae80-4b27-b6a6-f093e7e25f8f");
                                label = [ "get_authors" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Pat },
                                        { shape = Convex; sort = Pat } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "7e7c444d-4484-489d-9169-363f691ee7d5");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "f3169fb7-b81a-4fe0-ba01-67890d8c547d");
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 24; sort = Pat },
                                        { shape = Concave 24; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "347d8123-fe28-4ad4-9769-2bd905580ece");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "fe1ce950-3bc4-4db0-b10c-4825186354ae");
                                label = [ "["; "]" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [ Typ ];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "4cb8db70-b14b-4447-891a-8bb8c250680b");
                                          label = [ "Book" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Typ },
                                                  { shape = Convex; sort = Typ }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                    ];
                                  ];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "510ef683-0b24-4066-bc29-8d9a31dd4a21");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "4e6e528a-f009-467a-869c-e282eadce96c");
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 13; sort = Typ },
                                        { shape = Concave 13; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bd3b0788-9aad-4b91-b695-eadaf0e74a64");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5c2fd6e2-86a7-4bee-b604-a0fbe72de200");
                                label = [ "["; "]" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [ Typ ];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "f97c353f-8bf4-4f65-bb77-a389e92d7302");
                                          label = [ "String" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Typ },
                                                  { shape = Convex; sort = Typ }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                    ];
                                  ];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "12cc1812-7512-4814-a282-3f4135f5e0e9");
                                content = Whitespace " ";
                              };
                          ];
                        ],
                        [] );
                  },
                  ( [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c486b5f5-223f-4dc5-b353-de786335f3e7");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
                            };
                          shards = [ 0; 1; 2 ];
                          children =
                            [
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c9a0773e-d631-457d-ae7e-85609c152496");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9f8a8497-f0d3-42bd-829b-95e059e55d09");
                                    label = [ "Book" ];
                                    mold =
                                      {
                                        out = TPat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = TPat },
                                            { shape = Convex; sort = TPat } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c128018a-b878-4354-87d3-56035fa8505d");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ba0fd5e2-2f47-478f-bfc2-9f889a953212");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "552f2855-8658-4f54-9022-3c46d4980297");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [ Typ ];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8e6e5001-63e8-4b64-8228-a768afb95485");
                                              label = [ "title" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "7d46e6bd-1967-4aa6-9bae-508aee06159a");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "097996cf-cf3d-4b38-b9bc-be13a66179dc");
                                              label = [ "String" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8e549939-a2af-4f94-808c-ba9e8b89e38e");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "628b7679-fd3d-459a-954d-63848f3ccff5");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ad729d31-55b1-40d8-9d67-e2bc92cde2e0");
                                              label = [ "author" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "7ed2baf9-5ed8-4b83-a5af-3b8ae0df035c");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "32d38f23-8361-4a22-8dcc-97946b265b36");
                                              label = [ "String" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "1d30f54b-e71c-4478-bee6-0d642806fa79");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "d2a1f38a-4fde-4acb-93d4-1c31d2fd775c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "c94d8f1c-2ee0-4391-976f-359b85366786");
                                              label = [ "year" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "faa49f59-6118-4227-aae8-51deb634dc05");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ce1d5759-b8d8-4376-a258-5c27f83bd9b0");
                                              label = [ "Int" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f7330061-2c1b-4ed8-b0f9-1809a9a3d143");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2b524aef-e6b1-436c-be4e-63d3448e49e8");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b5b711a4-2b83-471c-92af-0a53049496f4");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "eb698c8c-35e8-42b6-bf89-c6ebd02dc4a5");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7238d1cc-5f85-4751-902c-f2b725ad8228");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c4876be0-df0c-4fad-a26a-f99ae6566546");
                          label = [ "get_authors" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2412602e-2059-4536-9917-ae1b7b90362b");
                          label = [ "("; ")" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Concave 23; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Projector
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "aaae6b2b-5e78-476a-967c-1c4b05c520f5");
                                    kind = Table;
                                    syntax =
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "2d0c89d0-cc27-4b6b-858f-d31acb2f0290");
                                          label = [ "("; ")" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [ Exp ];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0; 1 ];
                                          children =
                                            [
                                              [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "bd7f5f2a-ed7d-443e-a322-f98378f3eac6");
                                                    label = [ "["; "]" ];
                                                    mold =
                                                      {
                                                        out = Exp;
                                                        in_ = [ Exp ];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            } );
                                                      };
                                                    shards = [ 0; 1 ];
                                                    children =
                                                      [
                                                        [
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "5fe50b1e-50d9-49cd-8d9d-84b3cedb0c14");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Exp;
                                                                      } );
                                                                };
                                                              shards = [ 0; 1 ];
                                                              children =
                                                                [
                                                                  [
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "dd58b597-8fc5-4fcb-a528-f45f7e178d9f");
                                                                        label =
                                                                          [
                                                                            "title";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "19d50af7-5588-4865-8dbb-93b4ada7efde");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "4d7ca36e-3a90-4e2a-9602-764647863338");
                                                                        label =
                                                                          [
                                                                            "\"Dune\"";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "aa774f9e-ea87-4543-a9b8-e3e7de9800c4");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "99674e15-a7dd-4745-83ca-dc1f2542029f");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "5de1a5f3-06cd-41b0-80b7-75f16dd4b562");
                                                                        label =
                                                                          [
                                                                            "author";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "c70e39a2-96da-42ce-97d8-80da38ac1de8");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "330c1a27-f302-42d9-abef-e3b72e2f989a");
                                                                        label =
                                                                          [
                                                                            "\"Herbert\"";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "38c51b5c-1ed4-4cee-82ef-3f409afc6c44");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "9d66cb0b-5aca-4efa-b440-5422dc09c373");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "5fc44728-49ed-4eea-9854-0c2b06a641bb");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "785c7c12-bf7d-4c83-8c36-78f0989e385d");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "b330b4fb-4983-4819-b862-8bba21ea0f70");
                                                                        label =
                                                                          [
                                                                            "1965";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                  ];
                                                                ];
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "bdaf8048-e4c9-40fa-af44-e04ee9f20a33");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      } );
                                                                };
                                                              shards = [ 0 ];
                                                              children = [];
                                                            };
                                                          Secondary
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "0f2dd2ea-beb6-4781-94ff-fc6cc532bb15");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "4e683503-a1ee-450b-bf62-c16f513fbace");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Exp;
                                                                      } );
                                                                };
                                                              shards = [ 0; 1 ];
                                                              children =
                                                                [
                                                                  [
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "e63fb28e-4f81-43ab-8d2b-78d22b6fc7be");
                                                                        label =
                                                                          [
                                                                            "title";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "d949f62d-db81-4511-a98d-381b6269f3e0");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "bdc60f86-4c77-4c63-a1dc-d9a4f0dbc53e");
                                                                        label =
                                                                          [
                                                                            "\"Neuromancer\"";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "ebeda6e1-9969-428a-a218-873cc0f8d170");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "d8520ff4-676d-482b-aba7-581fd2fa2ad2");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "1b603b95-934c-426d-b8c5-0e4819aff537");
                                                                        label =
                                                                          [
                                                                            "author";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "aa41f63f-4607-4e43-b3e6-b32bac3b3126");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a62697fd-499e-45b5-84c0-d2767854b977");
                                                                        label =
                                                                          [
                                                                            "\"Gibson\"";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "b6d86a28-671f-48d5-ae1a-c9ad2d40d76f");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "9829f400-2a4b-491f-a0c8-adb0c8d1076d");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "bdd8461f-f24f-4df9-9b91-833ad722f7f0");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "1104a379-211e-420c-a2f1-051afd3fa7ef");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "96479ec2-769a-4932-ae42-d12a4fddb6dd");
                                                                        label =
                                                                          [
                                                                            "1984";
                                                                          ];
                                                                        mold =
                                                                          {
                                                                            out =
                                                                              Exp;
                                                                            in_ =
                                                                              [];
                                                                            nibs =
                                                                              ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                              );
                                                                          };
                                                                        shards =
                                                                          [ 0 ];
                                                                        children =
                                                                          [];
                                                                      };
                                                                  ];
                                                                ];
                                                            };
                                                        ];
                                                      ];
                                                  };
                                              ];
                                            ];
                                        };
                                    model = "()";
                                  };
                              ];
                            ];
                        };
                    ] ) );
              ];
          };
        caret = Outer;
        refractors =
          {
            manuals = [];
            autos =
              {
                ids = Haz3lcore.Id.Map.empty;
                ephemerals = Haz3lcore.Id.Map.empty;
              };
            sample_cursor =
              {
                call_stack = [];
                index = -1;
                pinned_stack = None;
                indicated_call = None;
                time = None;
                seq = 0;
                step_range = None;
                pending_focus = None;
              };
          };
      };
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f914e458-4596-415c-b4dc-4e2549ff8e45");
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d293a193-a5ec-4cde-87fc-7d8994989cf6");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "32988721-2d4b-4594-a664-36f4b9f587e8");
                                    label = [ "get_authors" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0feadccd-845b-4b60-9150-78232516f569");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "64189dde-6873-4a69-b92f-e8846e10e8e8");
                                              label = [ "["; "]" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "959c420a-6c8a-4fd9-a6fd-2d7e1b85c30d");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0; 1 ];
                                                        children =
                                                          [
                                                            [
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "1e0ea8ee-3135-47bd-b56e-6f30057c0b61");
                                                                  label =
                                                                    [ "title" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "b34f54ef-0f7e-4487-b183-56cc432d0fb1");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "8bbce096-2659-4394-9d01-8ef6b62a5127");
                                                                  label =
                                                                    [
                                                                      "\"Dune\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "08ae7d5b-f2f7-45cb-b012-dfa61c6d4fdb");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "1f7bf409-d583-4119-97ff-5271f6b6d642");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "31b6a9c2-fc72-4994-b6aa-6a71c7e4d4e7");
                                                                  label =
                                                                    [ "author" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "3da70df6-3a9d-46de-9b40-d7eebd0894db");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "60fca440-1b4f-4224-80e0-f22482f2d5ce");
                                                                  label =
                                                                    [
                                                                      "\"Herbert\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "f131dd37-dd04-416b-b3d1-dbcb2c8120c4");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "8381f8ac-9860-4582-8183-c9b53d64a1f0");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "0b0270ae-9163-4150-a9bb-0a8851fe47b6");
                                                                  label =
                                                                    [ "year" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "f26ae188-2478-4805-8e05-a1ab1d54f80c");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "cd7277b8-6552-4ad1-886a-99e4c04cabc5");
                                                                  label =
                                                                    [ "1965" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f119933d-b3be-423b-bc43-c11db698eeec");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "847b4cd6-07ca-4c2d-a2dc-95f2a0c223ad");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f0150cf3-096a-45d3-bf1b-a31a4bef61aa");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0; 1 ];
                                                        children =
                                                          [
                                                            [
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "ddca39ab-08cc-4ff2-9c1c-2793aec1ce6c");
                                                                  label =
                                                                    [ "title" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "b556e407-0676-4e44-bb13-4ad42decca48");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "6e5c628b-a920-4a4e-9a33-21c9f83886f3");
                                                                  label =
                                                                    [
                                                                      "\"Neuromancer\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "811eb187-5fc3-4ae6-ab3d-2c41cbc8d1f9");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "c67dded1-5a0f-4d4e-a640-114e98f72198");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "a07d2ca8-132f-4bc6-a191-e8e5a6e9f9a6");
                                                                  label =
                                                                    [ "author" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "d2a17e97-2305-4244-a93c-e11f17d712e5");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "0ad72e88-ecb0-4a7e-8177-fb30cc673c7d");
                                                                  label =
                                                                    [
                                                                      "\"Gibson\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "99b0d8c9-15b8-4d43-9835-c7791ec16444");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "da3410c1-599b-46b9-86f1-e5d0f0fe1a7e");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "96b217c7-2a85-4156-919c-b28a3dbd392b");
                                                                  label =
                                                                    [ "year" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "b3e34af2-d85f-4c6f-8b55-7812b2ad0377");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "c8e7310b-04b6-481e-abff-273fecf75a2d");
                                                                  label =
                                                                    [ "1984" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d97a57c4-4947-46f4-bd1b-1e4f1e12f159");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e13e8bb9-1ebf-4d8a-b360-a4dbdd7a630b");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ed729e59-d900-4857-a243-e20346d95912");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "316b431e-a216-4737-ae2b-03d9ba6d5563");
                                    label = [ "["; "]" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "9ed2aa69-9ac6-4d56-b8df-69907c59a93b");
                                              label = [ "\"Herbert\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "995ca915-928e-4171-b32c-953d6e137507");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "481da2d5-96e1-4cbe-9926-064b3ee6ec5b");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "9f5e7bdd-70f1-4fcd-81f8-60ec2c09b5f7");
                                              label = [ "\"Gibson\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "abd2f2ef-8f5f-46f1-9e0c-79c91c3c4055");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3382dfd2-e2e2-405b-8b5c-fe52462ec0ab");
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 35; sort = Exp },
                                  { shape = Concave 35; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3d35db6a-b35f-4acd-b305-69d3323316df");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "32e247af-5449-4785-86f2-bac939e79f76");
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b326c26f-b3d2-416c-a772-2a3051eacb5c");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "352c1a31-d21a-4aaf-b58d-e621a3dd3c6a");
                                    label = [ "get_authors" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1ff36296-2f29-4752-9280-99a36e6f357f");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4b861310-1ffd-436f-90ed-3c1be6f6d6c7");
                                              label = [ "["; "]" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2af3bf83-747e-4f05-87cf-98238454843a");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0; 1 ];
                                                        children =
                                                          [
                                                            [
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "a87967bc-2f07-422a-97ee-501aa0f3364a");
                                                                  label =
                                                                    [ "title" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "ad50e1dd-44b2-4a42-9d6c-91883f3addf6");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "84e8ecfd-d01f-4a2a-8e50-935fd4b21635");
                                                                  label =
                                                                    [
                                                                      "\"Foundation\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "e74b27c1-adb6-468d-8db9-379332ac91b6");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "a2ccf941-276f-4420-a4ca-0768d6a96b49");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "49297943-2ee3-4e94-a0a4-72a5000b6cca");
                                                                  label =
                                                                    [ "author" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "e2ee17a7-81f0-46f4-a5c1-715728150dbb");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "87db4522-fd16-4863-8673-aaf954ba2303");
                                                                  label =
                                                                    [
                                                                      "\"Asimov\"";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "1747fc4a-8c97-409c-bd48-a4bac23cb854");
                                                                  label =
                                                                    [ "," ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                44;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "170815bd-bdf5-413e-9ecd-73e3398edb14");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "71979213-bb0f-473f-80f3-c68650056e64");
                                                                  label =
                                                                    [ "year" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "24268813-a059-4fe6-9e91-d141f8d005da");
                                                                  label =
                                                                    [ "=" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                39;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "092edfcf-b091-4f09-96cb-641898503ea2");
                                                                  label =
                                                                    [ "1951" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "4d40cc18-0794-4518-9d76-a46185c82248");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1723fe1e-eb55-4571-abfd-b86678c361e4");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5c9dded6-f92e-402d-8b13-40fa5afe5221");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d29a037d-0d7e-4e8f-a110-0581c0e0b7a2");
                                    label = [ "["; "]" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4a230cac-a3cc-4d77-9c4f-b55de8023f48");
                                              label = [ "\"Asimov\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ae6cd019-f361-466a-8f0d-a6dbd5663d8a");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d014d108-b896-44ae-83cf-9a4f3820639b");
                          content = Whitespace "\n";
                        };
                    ] );
                ancestors = [];
              };
            caret = Outer;
            refractors =
              {
                manuals = [];
                autos =
                  {
                    ids = Haz3lcore.Id.Map.empty;
                    ephemerals = Haz3lcore.Id.Map.empty;
                  };
                sample_cursor =
                  {
                    call_stack = [];
                    index = -1;
                    pinned_stack = None;
                    indicated_call = None;
                    time = None;
                    seq = 0;
                    step_range = None;
                    pending_focus = None;
                  };
              };
          };
        hints =
          [
            "Use column projection (.author) on the books list";
            "Remember to use dot-projection on the function parameter";
          ];
      };
    wrapper = false;
    show_report = true;
    setting_overrides =
      { Tutorial.default_setting_overrides with display_tables = Some true };
  }
