let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "f27114c5-6789-0123-4abc-def567890223");
    title = "Higherer Orderer";
    version = 9;
    module_name = "Tu_Higherer_Orderer";
    prompt =
      "Write a function that takes a list of functions, and 'squares' each \
       element (in the sense of composing it with itself).";
    display_hint =
      "You may or may not choose to use a let expression to define the \
       squaring higher order function.";
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
                             "7bb267b9-a1dc-4f85-981f-ea8baad4e4f8");
                      content = Whitespace "\n";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "1266a506-518b-4d4b-b2b8-6f0298458517");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "c730e9b9-e081-4c95-a531-0e0c3dc7fd1b");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "36ef785f-44eb-45f7-8c1f-5936efab8a63");
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
                                       "937dfab3-3895-4040-a2d9-aea2a830d8e7");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "7bdd8763-ad43-4c91-b15a-a3350ea7ec70");
                                label = [ "square_map" ];
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
                                       "d9695ae7-924a-488e-9baa-a5cf1c5770f0");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "8c1e97c7-24e4-4c67-8e64-2e853e4e963d");
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
                                       "cb207a33-e986-4165-8bbb-d7dd811b38fd");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "f3034c31-8330-4d47-8605-d7c91fdc984d");
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
                                                 "b5b2b474-2498-47a3-8a65-41d8b16339b7");
                                          label = [ "a" ];
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
                                      Secondary
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "e70178cd-749e-4b19-a6c1-86aecc1f47bf");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "ecc99685-2215-4929-9a78-51118755ecae");
                                          label = [ "->" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 13;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 13;
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
                                                 "e0dd3c48-0816-4c15-aae8-e1ca150e861a");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "5f198c43-d775-42b7-a920-f15aceeb787a");
                                          label = [ "a" ];
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
                                       "2f53143a-42d3-445d-ba0b-c418c910b460");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "390ffcbe-7939-44d2-bce5-7a8b5d1c0d9c");
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
                                       "e6a443af-c064-49f6-8d25-36cca4e2bd6d");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "de58b37a-e00f-451b-89fa-ad45b488c544");
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
                                                 "3436f0a8-07e4-40cf-a36b-cd765dd8ba2f");
                                          label = [ "a" ];
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
                                      Secondary
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "c3ccb1f7-e7e2-4b92-993e-8da55c1ccfa5");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "968d2e24-8344-4e3c-8c40-75812953a64c");
                                          label = [ "->" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 13;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 13;
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
                                                 "53570117-0fae-4513-a03a-820bbc324420");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "555e3c81-3886-4220-b5b1-c118f29baa62");
                                          label = [ "a" ];
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
                                       "5cf074fe-5d4a-4d30-8b6d-663a6f830b5a");
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
                                 "87e18fac-8ef0-465d-be9c-30c60a87ea45");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 40; sort = Exp } );
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
                                           "e6bedbe7-c3a1-41f9-a382-7ecef48122e0");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8fd61954-5871-4c75-8d41-d38583e32dcb");
                                    label = [ "a" ];
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
                                           "25037f03-0c9b-4a6f-9043-a847814cbe36");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "369bf346-760a-455b-9bf3-aa14f45beda8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5f5bb5fb-255e-46e3-8c34-87f069795cef");
                                    label = [ "Int" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d863004a-06ae-4613-95b9-2162faaa3d99");
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
                                 "4550edca-c2f5-443b-9cd5-6b90bd026892");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c674dd21-2341-4477-83a8-64e251e98e80");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "667cc1f3-e8ec-498f-bfb0-10a2b77fde0e");
                          label = [ "square_map" ];
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
                  ( [],
                    [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "310f77f9-26f6-4f76-b502-ffd622c6fda2");
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
                                           "17295acd-18f5-4ed5-bec1-7fe227dd6cd8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0c2a60f5-8b09-4f86-837c-4d1f202032bc");
                                    label = [ "hd" ];
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
                                           "04c2d5f0-76c6-4106-818f-906557c6e2b2");
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
                                                     "01f96aac-3b3d-4635-b297-abdab9a58b37");
                                              label = [ "answer" ];
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
                                                     "eff5c178-ec86-4452-b192-2fcdabb76452");
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
                                                               "16c7c121-a2df-4da0-a0bf-b2fe059181cd");
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "08ebb6c9-6dfe-4151-a28e-8c84757570be");
                                                                  label =
                                                                    [
                                                                      "fun";
                                                                      "->";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ =
                                                                        [ Pat ];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                36;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards =
                                                                    [ 0; 1 ];
                                                                  children =
                                                                    [
                                                                      [
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "cefbed97-460f-40f1-a1ec-7214b56eb435");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "a9f55964-cae7-449b-b269-ce3f48a6a1d3");
                                                                            label =
                                                                              [
                                                                                "x";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Pat;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "2052766b-d11f-4782-845e-dcb13e8f0fcf");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                      ];
                                                                    ];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "27a4ae6f-74f5-4291-b826-58bb698207c7");
                                                                  label =
                                                                    [ "x" ];
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
                                                                         "b0217b0f-cda5-44f4-8f20-1645470eefb8");
                                                                  label =
                                                                    [ "+" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                28;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                28;
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
                                                                         "e4813aec-1057-4226-a04f-47259eca0e6a");
                                                                  label =
                                                                    [ "1" ];
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
                                                                         "94a49ad2-0819-4350-b5b1-907b68af3592");
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
                                                                                47;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                47;
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
                                                                         "055f62cc-2f92-44b8-b6cb-c7c2cc82935a");
                                                                  label =
                                                                    [
                                                                      "fun";
                                                                      "->";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ =
                                                                        [ Pat ];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                36;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards =
                                                                    [ 0; 1 ];
                                                                  children =
                                                                    [
                                                                      [
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "6f548e90-a284-424a-9394-96e253ce3fb1");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "a1eddf2d-fc05-4890-a1c9-c1c45eb6d223");
                                                                            label =
                                                                              [
                                                                                "x";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Pat;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "fc3ab58f-c43c-4c32-b257-32212451eb9e");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                      ];
                                                                    ];
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "bac7ac0c-7703-4a1f-bd7c-c2463d54ebb8");
                                                                  label =
                                                                    [ "x" ];
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
                                                                         "27751f17-8e50-41e3-b1ab-bb60070d4344");
                                                                  label =
                                                                    [ "*" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                27;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                27;
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
                                                                         "12e15910-3f9a-4e5b-9777-226df3a676db");
                                                                  label =
                                                                    [ "2" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "6fbe4007-db25-4b35-b994-5c4a32866d0d");
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
                                                     "263d0c50-0523-46cd-9f88-46663fdffb65");
                                              label = [ "1345" ];
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
                                           "2f3b37d8-f8a8-4ed8-9980-8311a905957a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "198d8f35-3d4b-4dd9-8aa9-6688194ea557");
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
                                           "21e4b5f1-8c7f-4d26-8fa5-3111ed7dc343");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "04a29edd-2557-4c79-a71d-29a08d944d21");
                                    label = [ "1347" ];
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d3eebfd8-678f-4f58-bd4e-1981ece4a4e1");
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
                                 "90d00e22-52e8-4170-af61-2d76a89efe07");
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
                                 "09659658-c2d7-46cb-8905-d9250887a1ca");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "94b956f2-aa03-497a-91e1-23104db2c0d3");
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
                                           "4ef1c764-62b2-4b0f-b422-08da38903b5e");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "40cfdfba-feb6-4c1e-b477-19c0dcf725e0");
                                    label = [ "hd" ];
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
                                           "0377c336-7506-4c68-9dc8-48c898663bc1");
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
                                                     "b01ad36c-1541-4d65-8ece-8f75c95d18d0");
                                              label = [ "tl" ];
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
                                                     "e26486fa-c71a-4d58-86a2-dbcd0747caf0");
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
                                                               "ff6ec466-a534-432e-b186-54b1013d5caa");
                                                        label = [ "answer" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "8cee439f-8e81-4ba1-ae88-6076f681fc6f");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 23;
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
                                                                         "e923bc7c-35c3-49fc-b432-6afbacee811f");
                                                                  label =
                                                                    [ "["; "]" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ =
                                                                        [ Exp ];
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
                                                                  shards =
                                                                    [ 0; 1 ];
                                                                  children =
                                                                    [
                                                                      [
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "6d707521-61d8-467b-85f4-854bf6c5036a");
                                                                            label =
                                                                              [
                                                                                "fun";
                                                                                "->";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [
                                                                                Pat;
                                                                                ];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                36;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                                1;
                                                                              ];
                                                                            children =
                                                                              [
                                                                                [
                                                                                Secondary
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "6efb1737-b301-475c-9f70-4ba484af8876");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                                };
                                                                                Tile
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "210cd079-cd97-4710-a76d-526e119500f3");
                                                                                label =
                                                                                [
                                                                                "x";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Pat;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                }
                                                                                );
                                                                                };
                                                                                shards =
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                                };
                                                                                Secondary
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "a32a0603-fae1-459c-8851-39006347a3ea");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                                };
                                                                                ];
                                                                              ];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "25b7f848-5943-4fa1-aa2b-28d6ae1fc95b");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "00468678-6067-4117-850e-99b80835f66c");
                                                                            label =
                                                                              [
                                                                                "x";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "583838e5-3c85-4476-b932-f243f0db8731");
                                                                            label =
                                                                              [
                                                                                "+";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "d83de675-17a5-4c44-922d-9017a5c230b7");
                                                                            label =
                                                                              [
                                                                                "1";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "8fa6ccc2-1be7-4854-9210-22130f87bf10");
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
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "158ba31e-0417-4db8-8335-1c968a5edc89");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "d89f8e8c-b37b-4448-8f5c-b0048170b291");
                                                                            label =
                                                                              [
                                                                                "fun";
                                                                                "->";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [
                                                                                Pat;
                                                                                ];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                36;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                                1;
                                                                              ];
                                                                            children =
                                                                              [
                                                                                [
                                                                                Secondary
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "8baac82b-462f-4275-b754-16b63c04d654");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                                };
                                                                                Tile
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "ac88df86-5d78-4abd-98d1-4933494ae072");
                                                                                label =
                                                                                [
                                                                                "x";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Pat;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                                }
                                                                                );
                                                                                };
                                                                                shards =
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                                };
                                                                                Secondary
                                                                                {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "87c8d492-9a66-42e9-b1f6-9d77051b4210");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                                };
                                                                                ];
                                                                              ];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "9883dc19-a276-423b-bbda-c379dcfdb8a1");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "c50f7035-833a-4fde-b2ad-905cbce40dbf");
                                                                            label =
                                                                              [
                                                                                "x";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "ef66f73b-dbf3-47c4-9ca4-97d26617070e");
                                                                            label =
                                                                              [
                                                                                "*";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                27;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                27;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "87857d13-83c6-4c63-97fa-18166c1ed9ba");
                                                                            label =
                                                                              [
                                                                                "2";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                              [
                                                                                0;
                                                                              ];
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
                                        ];
                                      ];
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "71010ebf-1ee7-4eb0-87d0-8ba34a44925f");
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
                                                     "8e697894-c8e2-449d-b47b-aa27f308de25");
                                              label = [ "1345" ];
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
                                           "ab9ee360-d6d9-4948-b733-859dc391c032");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "568659be-7bb6-4f94-aa30-6ed3550d993b");
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
                                           "2d3db614-9feb-4157-ae70-0ffcfcc039d1");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d0a5d934-0309-4d64-a9fa-c1a4a2b61f2f");
                                    label = [ "5380" ];
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0f69979d-9b4c-4c75-b158-f7750672cd67");
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
                                 "8b8fea86-0ecb-4836-a29d-698d43f78c03");
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
                                 "c7f71917-eb4a-4e1b-b201-ade6e13e175e");
                          content = Whitespace "\n";
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "6b43540c-436b-4ab7-9f40-ebb3d45968a1");
                          shape = Convex;
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c9c72e8d-6685-4970-aa49-935b1e7df04b");
                          content = Whitespace "\n";
                        };
                    ] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
      };
    wrapper = true;
    show_report = true;
  }
