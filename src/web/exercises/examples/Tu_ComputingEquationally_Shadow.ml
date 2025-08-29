let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "b8c9d0e1-2345-6789-0abc-def123456789");
    title = "Tuples";
    version = 3;
    module_name = "Tu_ComputingEquationally";
    prompt =
      "Products, or tuples, allow us to bundle a expression (potentially of \
       different types) into a compound expression. Write a function that \
       \"rotates\" the input triple, following the given type.";
    display_hint =
      "Use a tuple pattern after the `fun` keyword to access the elements of \
       the triple.";
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
                             "60195574-3255-4e02-b9e4-2c21391a1df6");
                      content = Whitespace " ";
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "3e86e7e0-7cb0-4f8d-979a-2a1f082e82e3");
                      content = Whitespace "\n";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "1ae46989-616d-4366-be3c-686a95fdc9f4");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "fa0a54ff-d059-4955-9d3a-4dbd6997cc8e");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "d5fd8508-f3c7-4dea-8cb7-4eb7a5bb7d79");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 40; sort = Exp } );
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
                                       "8fdbf83d-d88d-440c-9c36-9a6d0a8c39f4");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "48a0ea2d-fdc7-4def-bb3e-66fd6ccc18c0");
                                label = [ "rotate" ];
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
                                       "827ad95f-823c-4d7e-9628-2cb4f2c8f456");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "84d3d61a-42e5-41df-848c-84976d9265eb");
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
                                       "c741b87d-f9f4-4de9-a126-16a5549b89e4");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "6902264c-b9e0-43ad-ab82-f734e979543a");
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
                                                 "422800d8-8ff2-4082-8082-46078dc84e4f");
                                          label = [ "Int" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "285836af-11f7-4d80-b56c-f1a50f7ee016");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "03537432-4888-435c-ab95-3dffaa8f22fa");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "54bc3920-5ddc-4912-9c87-73773783eca4");
                                          label = [ "Float" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "dff55206-d683-4422-ba59-50cf46bee725");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "cb68396c-7390-4c5e-9f46-feae57eec6dd");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "25aa1cd5-7225-4bc8-8e16-7f127aa2d267");
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
                                       "6e5201c2-3610-4f8e-a07d-06a10a6475b2");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "7306a4a2-2420-4bbc-8fab-724ddb2bb073");
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
                                       "ecba7f9c-32ed-4ed7-9539-edb259e5f3f5");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bc67c5f6-a269-4f21-aeec-5ca31722313a");
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
                                                 "5431a206-f901-495a-8737-d565f6ee0490");
                                          label = [ "Float" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "acf0e2a2-793c-4874-8d50-e57f2f438f57");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "b15ec800-298a-4e17-a073-6014097ef80a");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "a09b0591-17e3-4da4-b0a2-998558c71dd8");
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "e2e82408-4255-4fe1-9549-ecd0352fc5c5");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "dcfc7057-0553-4dd7-8419-d23e19e10886");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "bde11026-606b-45e0-8287-3ea24a827703");
                                          label = [ "Int" ];
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
                                       "3f0586b0-b206-497e-8733-a33961e5f7a5");
                                content = Whitespace " ";
                              };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "6b713c73-b067-404c-be23-2e1be364c16b");
                          content = Whitespace " ";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "59c490a4-2b13-4fbc-bab8-fb9a8aa373a6");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d30c2a12-858c-4672-86c4-936d76a27544");
                          label = [ "rotate" ];
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
                    ] ) );
              ];
          };
        caret = Outer;
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
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "618fbc43-cafa-4b5c-b293-2298adb4975d");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "544f4aa5-2e99-4076-a5f2-a33d2ff4f95d");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "4f16f0ee-a273-4593-8407-a8f8cb2ba1d4");
                          label = [ "answer" ];
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
                                 "e66f739e-47ed-4b5b-87d1-4e610e310bbe");
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
                                           "6b8af27e-c99e-48cc-9981-8c3dd22e893d");
                                    label = [ "1346798" ];
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
                                           "2c264283-d97d-49a9-832f-55f167c84170");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "1ae797f9-4ecf-40b1-8b6f-9260046df1d6");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "bbcc275f-4e6f-4062-aa39-8bb88d076104");
                                    label = [ "2345.43" ];
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
                                           "086cc8f0-dcbe-4cef-9028-a2a8c2f65780");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "bde81654-8396-4426-88f1-e7a23549da61");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2ed6fa95-e85e-487a-a693-e59741021104");
                                    label = [ "\"yo\"" ];
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
                              ];
                            ];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "234221f8-cc8a-47ec-a5d1-26f6e3ab027e");
                          content = Whitespace " ";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "6348084e-7395-4e29-bcaa-92dbd1aa70c2");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "981a240f-4bff-48d8-b23a-6dc3c7cfce90");
                          label = [ "==" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 31; sort = Exp },
                                  { shape = Concave 31; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e6a4b616-ff81-427f-9f60-778f27e80800");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "5fd0d600-f71f-4856-9a1f-639033575335");
                          label = [ "("; ")" ];
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
                                           "9c43510e-070e-4a69-bc1c-29e6370da2e8");
                                    label = [ "2345.43" ];
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
                                           "69fdb032-4a1c-4c23-97d9-ee21f34a4878");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "eea4b524-ac44-4981-9f09-437797aa0b73");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "fc4a137b-63d4-4145-87a6-72e372501fde");
                                    label = [ "\"yo\"" ];
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
                                           "45c95aa5-f9b1-4231-9698-b86c02645fa3");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "bf636c9d-7013-4e91-b768-393e0c854da6");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "29ca4e8a-59dd-45dc-a01b-6a64d33f9571");
                                    label = [ "1346798" ];
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
                              ];
                            ];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f7e6743a-5539-434a-af3d-82bfc596e2d0");
                          content = Whitespace " ";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "4f7dbb0c-b967-4062-8382-0fcd35ea8624");
                          content = Whitespace "\n";
                        };
                    ] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "7c50a573-2e5a-4e93-9052-816b6a711ffe");
                        label = [ "test"; "end" ];
                        mold =
                          {
                            out = Exp;
                            in_ = [ Exp ];
                            nibs =
                              ( { shape = Convex; sort = Exp },
                                { shape = Convex; sort = Exp } );
                          };
                        shards = ([ 0 ], [ 1 ]);
                        children = ([], []);
                      },
                      ( [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "d800f799-1f94-46c1-a52e-1d5ca12d0ae4");
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
                                               "c1653461-1310-4cf1-ae77-1f19f032cb71");
                                        content = Whitespace " ";
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "88a9b140-cd2e-477c-9bd2-d59f6d57a356");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fba0c5ee-eb1a-4e2c-bae9-c56082b493e4");
                                        label = [ "answer" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                { shape = Convex; sort = Exp }
                                              );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "68e3484d-adb7-4aad-8279-f6381c7db997");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
                                                  sort = Exp;
                                                },
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
                                                         "207a71bb-cd1d-44ff-a8eb-d83e3804c5b0");
                                                  label = [ "134" ];
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
                                                         "1da39b8e-4db9-42eb-9279-2409fb05b2b1");
                                                  label = [ "," ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 47;
                                                            sort = Exp;
                                                          },
                                                          {
                                                            shape = Concave 47;
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
                                                         "5739b294-a7e6-432d-9013-ba37b145cd4d");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "880f2690-d7d9-417e-8dc1-8b9e794a36df");
                                                  label = [ "0.43" ];
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
                                                         "38c09747-f5ba-4760-bc04-73c47f5648d7");
                                                  label = [ "," ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 47;
                                                            sort = Exp;
                                                          },
                                                          {
                                                            shape = Concave 47;
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
                                                         "ac4ab8a4-22b3-4b7d-852b-d5d408b2aef1");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "30cd5c8f-5219-4cca-818d-39bc3cab862f");
                                                  label = [ "\"blog\"" ];
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
                                               "342b9d2f-5a2b-4195-aaf4-c29e8ebb1568");
                                        content = Whitespace " ";
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "86814430-a048-4203-b670-e5ebf706b690");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8a58ea52-301c-44d8-8399-69278c67118a");
                                        label = [ "==" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 31;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 31;
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
                                               "96984e75-ac4c-4895-93ce-b576a11b0613");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e4c0a96b-7b54-4528-885a-8fded1761310");
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
                                                         "5511ee62-a48f-44ab-8e2a-ad1a2aefea0c");
                                                  label = [ "0.43" ];
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
                                                         "72652aa3-1e08-47e0-9fd7-7d0fb4820e70");
                                                  label = [ "," ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 47;
                                                            sort = Exp;
                                                          },
                                                          {
                                                            shape = Concave 47;
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
                                                         "d9d0c86d-4561-4931-a0f7-ed29fe1051e8");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4928fc41-894d-4122-b844-717419b7f0f2");
                                                  label = [ "\"blog\"" ];
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
                                                         "c0525aec-c703-4139-a30e-c0390af8eab1");
                                                  label = [ "," ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 47;
                                                            sort = Exp;
                                                          },
                                                          {
                                                            shape = Concave 47;
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
                                                         "32fac4da-1de5-4211-89bd-1c196cfa3c49");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "ba327fed-c214-4996-9610-ab1e182e6364");
                                                  label = [ "134" ];
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
                                               "6dd1aacd-f4dc-46d0-aa4e-e0ccc1420d5b");
                                        content = Whitespace " ";
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4a3d06bc-a200-4b56-b853-0f204fbf9f3b");
                                        content = Whitespace "\n";
                                      };
                                  ];
                                ];
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "46cb0494-4a51-4a86-b03d-071cb3a2b4a0");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "39658afb-1e11-4021-a1b3-4db96d0f3d31");
                              content = Whitespace "\n";
                            };
                        ],
                        [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "3f246954-eeb3-426c-a51b-d427d3f78e11");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "0b6d0a0d-1eac-4891-81b0-ca421f1d533d");
                              shape = Convex;
                            };
                        ] ) );
                  ];
              };
            caret = Outer;
          };
        hints =
          [
            "The output triple should have the same elements as the input, in \
             a different order.";
          ];
      };
    wrapper = true;
    show_report = false;
  }
