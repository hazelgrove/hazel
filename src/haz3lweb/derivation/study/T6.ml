let exercise : DerivationTree.spec =
  {
    title = "Task 6 of 9: Debugging";
    version = 0;
    module_name = "t6";
    prompt =
      "Try to fix the derivation until all the node turn marked correct (turn \
       green).";
    prelude =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "7681cd37-ab2a-491f-ae94-7f8e193bb522");
                      label = [ "y" ];
                      mold =
                        {
                          out = Drv Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Convex; sort = Drv Exp },
                              { shape = Convex; sort = Drv Exp } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                ],
                [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "616da489-8a3b-47f3-9710-c96d9e52a253");
                      label = [ ":" ];
                      mold =
                        {
                          out = Drv Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 48; sort = Drv Exp },
                              { shape = Concave 48; sort = Drv Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a21391b2-c5d4-4902-bd59-7263cf128a00");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "4a0e1f3e-0a6a-4f6c-be4d-7a70a6f46c8e");
                      label = [ "Num" ];
                      mold =
                        {
                          out = Drv Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Convex; sort = Drv Typ },
                              { shape = Convex; sort = Drv Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "0033369c-a9c4-4c4f-a89a-7fbb68237188");
                    label = [ "("; ")" ];
                    mold =
                      {
                        out = Drv Exp;
                        in_ = [ Drv Exp ];
                        nibs =
                          ( { shape = Convex; sort = Drv Exp },
                            { shape = Convex; sort = Drv Exp } );
                      };
                    shards = ([ 0 ], [ 1 ]);
                    children = ([], []);
                  },
                  ( [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "13162edb-cbfc-4b8d-9ac2-bc0c3f4d99c4");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a0b1867c-1ab4-4509-8f07-52844728a33c");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "4706dbda-9f6d-46bd-b7c0-fdea3bd8fb43");
                          label = [ "::" ];
                          mold =
                            {
                              out = Drv Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 29; sort = Drv Exp },
                                  { shape = Concave 29; sort = Drv Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "1aaf5ee6-ae83-4658-8258-179e808b99d2");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "8b592db1-4f0a-4120-a1a1-bee933a975c0");
                          label = [ "$" ];
                          mold =
                            {
                              out = Drv Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Convex; sort = Drv Exp },
                                  { shape = Concave 21; sort = Drv Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "bb8c9543-693e-4583-956d-27da50e1f03b");
                          label = [ "gamma" ];
                          mold =
                            {
                              out = Drv Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Convex; sort = Drv Exp },
                                  { shape = Convex; sort = Drv Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "89d253a3-c8e5-4f55-9f56-3654dd297345");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "2e17b125-7d7b-44c2-8f0d-7a0c2023534d");
                    label = [ "of_ctx"; "end" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Drv Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Convex; sort = Exp } );
                      };
                    shards = ([ 0 ], [ 1 ]);
                    children = ([], []);
                  },
                  ( [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "70770060-6c6d-481d-b59f-8f25d2ae41df");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9f4d8a86-76e8-4483-b46e-37915c9c5d28");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "8a8a3b11-d57b-47c7-8ef0-b5aa3ccda8c8");
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
                                       "005f5678-6637-4804-85e2-59626e06c962");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "62e785c5-a103-45d1-b8c4-84f9e3b83299");
                                label = [ "gamma'" ];
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
                                       "ce3cad1f-f145-473e-a3e6-2327971f8d3f");
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
                                 "c79205be-1da7-4820-a705-62efc481c4f8");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
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
                                           "7ea8b6e9-adbc-45de-b7e4-df4209df0382");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2836edb6-1c77-4c7b-b30e-2888734a8d4a");
                                    label = [ "gamma" ];
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
                                           "ce439467-2040-46b2-86a6-b93d2ad90890");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b596aa7f-3763-4e2f-9b2d-d0f83b8e519d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "bae237ac-412a-4143-8a30-18289b1a33ea");
                                    label = [ "of_ctx"; "end" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Drv Exp ];
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
                                                     "38446fd4-e113-4f95-b0a9-d9d76c98a348");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "30355f4c-08ff-4edc-a7d8-4f08f593a3cd");
                                              label = [ "["; "]" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [ Drv Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
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
                                                               "72a4fa7c-c934-4ccc-bda4-7fcb9b8ca01e");
                                                        label = [ "e" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Drv Exp;
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
                                                               "f7362fa1-26e1-4c66-9b27-dc1afc31c58a");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 48;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 48;
                                                                  sort = Drv Typ;
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
                                                               "6a53fa75-eb6f-4089-a44b-7a59e1d50cf2");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "8c3e44af-a4c7-4db2-81d2-9ee34f113d1b");
                                                        label = [ "Bool" ];
                                                        mold =
                                                          {
                                                            out = Drv Typ;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Typ;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Drv Typ;
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
                                                               "c0aa90eb-456e-43d5-bb09-0edc80876eba");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "bf43ce89-28c8-4867-986e-9c77c6ce78b4");
                                                        label = [ "+" ];
                                                        mold =
                                                          {
                                                            out = Drv Typ;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 13;
                                                                  sort = Drv Typ;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 13;
                                                                  sort = Drv Typ;
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
                                                               "74f21590-cbcf-4f43-aebb-36b7d74c8cce");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "cb905a7e-68ef-4f3e-a9be-7a37e1043581");
                                                        label = [ "Num" ];
                                                        mold =
                                                          {
                                                            out = Drv Typ;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Typ;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Drv Typ;
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
                                                     "182bb260-0b21-43b8-8694-c943ca3549fc");
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
                                           "49ea9466-22b8-4cc1-8cfd-54f94fb49eec");
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
                                 "a35f4d1c-d36c-408c-a14f-10e8f499b100");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c95b8f8e-f74f-41af-83c3-f670e45b6836");
                          shape = Convex;
                        };
                    ] ) );
              ];
          };
        caret = Outer;
      };
    setup =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings =
              ( [],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a3a0c7aa-4423-4789-805f-883b1bcce441");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
      };
    corpus = ALFA;
    trees =
      [
        Node
          ( Just
              {
                jdmt =
                  {
                    selection = { focus = Left; content = []; mode = Normal };
                    backpack = [];
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
                                         "3c56e581-b675-4b8a-9c52-46c36c4999c0");
                                  label = [ "$" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Concave 21; sort = Drv Exp }
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
                                         "ac77e5a6-a6f7-4064-b2c5-d109e487d8af");
                                  label = [ "gamma'" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "3984d501-e9f3-4198-8c47-f6c511399af0");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "6a410774-139e-465e-9c53-3ed76ed07cf0");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 49; sort = Drv Exp },
                                          { shape = Concave 49; sort = Drv Exp }
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
                                         "a23ce567-376b-4077-9a18-f10cd863f7a9");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "5e6a49eb-014f-47c2-86c0-1d5c8f458b29");
                                  label = [ "e" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "2e3eba36-841e-4a1c-a3cc-b74c878cb2b2");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "258d3522-dabe-4aca-9be3-c345e6261e63");
                                  label = [ "<=" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 48; sort = Drv Exp },
                                          { shape = Concave 48; sort = Drv Typ }
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
                                         "300493d6-d9bf-45c9-9686-99fa17182583");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ef19bcea-a7d7-40fb-b154-d614b166b67b");
                                  label = [ "Bool" ];
                                  mold =
                                    {
                                      out = Drv Typ;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Typ },
                                          { shape = Convex; sort = Drv Typ } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a59b3ef9-0ae1-4ad7-b6bb-019f46c14894");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a0033927-087a-490c-9518-bbd8219ed70d");
                                  label = [ "+" ];
                                  mold =
                                    {
                                      out = Drv Typ;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 13; sort = Drv Typ },
                                          { shape = Concave 13; sort = Drv Typ }
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
                                         "8094d7eb-a9ea-4ac0-b440-42be9cf7383a");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "933a85a4-5e43-464c-8e9a-1e135ada77be");
                                  label = [ "Num" ];
                                  mold =
                                    {
                                      out = Drv Typ;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Typ },
                                          { shape = Convex; sort = Drv Typ } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                            ] );
                        ancestors = [];
                      };
                    caret = Outer;
                  };
                rule = Some A_Subsumption;
              },
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3081fd15-1cf5-48b1-a4c1-c0508dddb2c4");
                                        label = [ "$" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 21;
                                                  sort = Drv Exp;
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
                                               "fb1a1a7c-856b-451b-a6a2-f6d7b77d62f8");
                                        label = [ "gamma'" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ],
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4e12d220-32a3-48de-8470-556c79fe81d1");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "cdcf259c-0028-4bca-a11d-8ae700f309b3");
                                        label = [ "|-" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
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
                                               "d047f4e0-3c9c-47fb-bba0-6f5258e680c7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5ef24a36-39a1-47d3-ae29-dfcd2e8c198b");
                                        label = [ "e" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
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
                                               "5b0fd577-87b6-4d85-97b8-eead3360d0d2");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8f118027-02d3-4952-85d1-f66db2f3832e");
                                        label = [ "=>" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 48;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 48;
                                                  sort = Drv Typ;
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
                                               "fe74b79d-76e8-4d76-ace7-9f8185bdb0ad");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "939a22eb-1b42-485c-a94b-272b3419766c");
                                        label = [ "Bool" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Typ;
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
                                               "df440b73-082c-4e38-9f85-05215be52a74");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d1a04603-fbb8-4431-85f8-c07700a10c40");
                                        label = [ "+" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 13;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Concave 13;
                                                  sort = Drv Typ;
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
                                               "d9dffc1d-a01e-4013-ae73-c7b22d64b36e");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "48486618-1e0d-4602-a027-8b9238ec04fd");
                                        label = [ "Num" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some S_Var;
                    },
                  [] );
            ] );
        Node
          ( Just
              {
                jdmt =
                  {
                    selection = { focus = Left; content = []; mode = Normal };
                    backpack = [];
                    relatives =
                      {
                        siblings =
                          ( [
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7c3f25f2-2897-43bb-ac9c-1fed8ba82f64");
                                  label = [ "y" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7aa9d8e7-521e-48d5-b656-abeb10a15482");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "1c74217b-3106-4074-909d-5f1581d56ab2");
                                  label = [ "==" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 31; sort = Drv Exp },
                                          { shape = Concave 31; sort = Drv Exp }
                                        );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                            ],
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "61393029-cba5-481a-b52c-e33f5d7c6cad");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c48a7468-7dca-4f18-92db-f82b75bd5e45");
                                  label = [ "1" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                            ] );
                        ancestors =
                          [
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "686c7689-3e9d-42a8-966d-701054d46dce");
                                label = [ "("; ")" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [ Drv Exp ];
                                    nibs =
                                      ( { shape = Concave 23; sort = Drv Exp },
                                        { shape = Convex; sort = Drv Exp } );
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
                                             "1892b0c1-e6a7-4cd1-8959-147844157a00");
                                      label = [ "$" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              {
                                                shape = Concave 21;
                                                sort = Drv Exp;
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
                                             "d796836f-8eed-4ec7-b3a0-a1453b609cce");
                                      label = [ "gamma'" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "5753886d-fa4f-4956-9a22-53d4ce51a9c0");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "02be7efc-dbcb-4d66-ac78-36a08ae8ed76");
                                      label = [ "|-" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 49;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 49;
                                                sort = Drv Exp;
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
                                             "83e15904-520b-4b6f-bad4-8d252f18e282");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "dcb3f0a0-bd04-4d1f-b6e9-a38ce94bbfb5");
                                      label = [ "L" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "cf42b930-8c67-43aa-b2f5-2d716314e803");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "006f955e-a75a-42c1-ae92-9f7fb82d45b0");
                                      label = [ "<=" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 48;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 48;
                                                sort = Drv Typ;
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
                                             "dfee44e6-f1b0-411d-9e1d-81dc2554fd58");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "4437c6cd-d99c-4e74-a7df-f97b885acff5");
                                      label = [ "Bool" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Typ },
                                              { shape = Convex; sort = Drv Typ }
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
                                             "3c5366b7-4080-49c2-aaa2-725fce58afdd");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f57dfcdd-88d3-4359-98a7-bd38bbeade83");
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 13;
                                                sort = Drv Typ;
                                              },
                                              {
                                                shape = Concave 13;
                                                sort = Drv Typ;
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
                                             "a3399957-1d2b-4146-8a74-483ccb03dcad");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "3f82c78c-ceb9-424b-9001-8f374fc4f558");
                                      label = [ "Num" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Typ },
                                              { shape = Convex; sort = Drv Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                ] ) );
                          ];
                      };
                    caret = Outer;
                  };
                rule = Some A_InjL;
              },
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "72fe7197-9562-4558-a5ef-ee43fa324f9a");
                                        label = [ "$" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 21;
                                                  sort = Drv Exp;
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
                                               "c84d8de1-10bb-4749-a65a-d450716dff48");
                                        label = [ "gamma'" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ],
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "56942c55-271a-42d6-9ad5-d046c8673340");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "25589edd-9324-4220-9ef4-84b5f8005d49");
                                        label = [ "|-" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
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
                                               "18c33c51-0e45-4940-8c1f-b5128758545b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "36e0a95e-c5b1-459c-8e32-543de472bfe8");
                                        label = [ "y" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
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
                                               "ac67a79d-ddce-4a9d-9a7e-42df2295a072");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f479c5cc-f8bc-45d7-95a2-44164c8856b6");
                                        label = [ "==" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 31;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 31;
                                                  sort = Drv Exp;
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
                                               "5430cc0b-7c6c-47af-a759-96b6542b86a7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "47ccac81-cfb5-4a87-bbc8-ccaff34f1ae9");
                                        label = [ "1" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
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
                                               "b2c142b2-e0b5-4f63-90ff-3319f5698d4f");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "55db065d-17cb-4372-943d-f4312c088111");
                                        label = [ "<=" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 48;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 48;
                                                  sort = Drv Typ;
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
                                               "b9597a76-c0e7-4dc4-97ac-171661f8ceaf");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "dd67a6d8-957c-4941-b51c-9cf452536b39");
                                        label = [ "Bool" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some A_Subsumption;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "9ace0c25-fb3f-4d27-91ac-3be506987337");
                                              label = [ "$" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 21;
                                                        sort = Drv Exp;
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
                                                     "659c1c6a-bc7f-4029-9302-e835de14af3b");
                                              label = [ "gamma'" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
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
                                                     "289991cc-28d7-46ea-ad67-a07bb0225ed2");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "48ce0bf5-fd67-49ab-80b5-27145131c04b");
                                              label = [ "|-" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
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
                                                     "1578350c-0df4-437d-b547-7d1c86c53e8d");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "cd9470de-f78d-48d6-83aa-fd35e895045d");
                                              label = [ "y" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
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
                                                     "004fdc1b-0750-4655-9420-e5aae256f6d7");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "b8920e00-6ac7-488b-b94f-d7cc642d6682");
                                              label = [ "==" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 31;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 31;
                                                        sort = Drv Exp;
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
                                                     "24f9b10f-1371-4f0e-9b36-6e3c5a466e16");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "77960e49-005e-467d-86fb-48b927c36da0");
                                              label = [ "1" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
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
                                                     "b491b30a-7836-45f8-84ab-cd6819d564ef");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a001f399-c907-41a8-8179-9a9549daeb9e");
                                              label = [ "=>" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 48;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 48;
                                                        sort = Drv Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ],
                                        [
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a39000ad-fc76-4187-9218-99e801480002");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "5cb85fab-5939-4f9c-ac36-b0de74436b3c");
                                              label = [ "Bool" ];
                                              mold =
                                                {
                                                  out = Drv Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Typ;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                              };
                            rule = Some S_Eq;
                          },
                        [
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
                                      backpack = [];
                                      relatives =
                                        {
                                          siblings =
                                            ( [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "3aeb5e05-cbd9-42ba-8c07-45a449a3814e");
                                                    label = [ "$" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 21;
                                                              sort = Drv Exp;
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
                                                           "22a74d73-b908-4957-a1de-d97bfea9cec2");
                                                    label = [ "gamma'" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Exp;
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
                                                           "b3e33398-f831-435f-9e2f-cf2dd876d65e");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "16608fd8-110f-4ced-a0c8-3eee1510fdfb");
                                                    label = [ "|-" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 49;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 49;
                                                              sort = Drv Exp;
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
                                                           "39cdbf33-8e86-4549-be01-f0ee73585577");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "b1978c07-6bd1-4395-a8cc-cb3b8f493ad3");
                                                    label = [ "y" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Exp;
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
                                                           "088f1651-f84c-46f1-bbf7-626ce8b5ab8b");
                                                    content = Whitespace " ";
                                                  };
                                              ],
                                              [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "86eda77a-624e-4983-9ac4-498778962285");
                                                    label = [ "=>" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 48;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 48;
                                                              sort = Drv Typ;
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
                                                           "98563d84-0bb5-48ff-a8b0-01a7c07ae445");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "aafd3e3e-c03e-4309-a0e9-553cf39c117a");
                                                    label = [ "Num" ];
                                                    mold =
                                                      {
                                                        out = Drv Typ;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Typ;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Typ;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                              ] );
                                          ancestors = [];
                                        };
                                      caret = Outer;
                                    };
                                  rule = Some S_Var;
                                },
                              [] );
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
                                      backpack = [];
                                      relatives =
                                        {
                                          siblings =
                                            ( [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "44509863-a147-4624-92ee-499cf2ff058f");
                                                    label = [ "$" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 21;
                                                              sort = Drv Exp;
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
                                                           "7d488be4-f8cb-4fa5-a73f-94b8fd5be18d");
                                                    label = [ "gamma'" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Exp;
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
                                                           "4472cf0b-cc99-4fc6-a439-19a3f5ce92e3");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9ad22d17-9468-4532-a62c-8e426d75cc56");
                                                    label = [ "|-" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 49;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 49;
                                                              sort = Drv Exp;
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
                                                           "4496c36d-e001-45f5-9d45-646cfef2ad5c");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "1e4566cf-9685-4882-9056-4548832d080c");
                                                    label = [ "1" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Exp;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                              ],
                                              [
                                                Secondary
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "8981f52f-182f-44bc-8c43-53bebab11142");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "0376210b-8443-4614-a567-c6c35c3406b8");
                                                    label = [ "=>" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 48;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 48;
                                                              sort = Drv Typ;
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
                                                           "a2cc20c2-952e-4050-8b4d-e87d6a5b959d");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "83292812-78a9-4de8-8f44-3d35ddd1eb79");
                                                    label = [ "Num" ];
                                                    mold =
                                                      {
                                                        out = Drv Typ;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Drv Typ;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Drv Typ;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                              ] );
                                          ancestors = [];
                                        };
                                      caret = Outer;
                                    };
                                  rule = Some S_Num;
                                },
                              [] );
                        ] );
                  ] );
            ] );
        Node
          ( Just
              {
                jdmt =
                  {
                    selection = { focus = Left; content = []; mode = Normal };
                    backpack = [];
                    relatives =
                      {
                        siblings =
                          ( [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "89c40f0c-542a-4cae-80d7-9980b44eec96");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "fa512120-25e0-4eb6-b238-e5d6efa43523");
                                  label = [ "L" ];
                                  mold =
                                    {
                                      out = Drv Pat;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Pat },
                                          { shape = Convex; sort = Drv Pat } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "479e800a-3948-4196-aeb6-89f59d978244");
                                  label = [ "("; ")" ];
                                  mold =
                                    {
                                      out = Drv Pat;
                                      in_ = [ Drv Pat ];
                                      nibs =
                                        ( { shape = Concave 23; sort = Drv Pat },
                                          { shape = Convex; sort = Drv Pat } );
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
                                                   "e9c70b14-d073-4e41-9cae-5db3abffc343");
                                            label = [ "x" ];
                                            mold =
                                              {
                                                out = Drv Pat;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Pat;
                                                    },
                                                    {
                                                      shape = Convex;
                                                      sort = Drv Pat;
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
                                         "4c410d6c-fc4c-43bb-b55d-20bbaa348723");
                                  content = Whitespace " ";
                                };
                            ],
                            [] );
                        ancestors =
                          [
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "84306eca-9295-4dc5-9828-19dd7eeb8cd0");
                                label = [ "|"; "=>" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [ Drv Pat ];
                                    nibs =
                                      ( { shape = Concave 43; sort = Drv Exp },
                                        { shape = Concave 43; sort = Drv Exp }
                                      );
                                  };
                                shards = ([ 0 ], [ 1 ]);
                                children = ([], []);
                              },
                              ( [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "a7cac24b-9ac8-45a1-9fda-bc6d633df5ff");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ad10542c-e96c-4760-a883-0ad4098c186a");
                                      label = [ "e" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "102819af-25a4-4e4b-809d-37639c76cad1");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "07a0d46b-89a7-425e-aa1b-6827dfd3272d");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "7191ecfc-b7e2-4eb8-82cf-3afeb3e30f87");
                                      label = [ "e" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "4aa357d8-aed1-4313-a699-00807ee517c3");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "4e4b64b8-f89a-480c-a49c-d1c5f79497f3");
                                      label = [ "|"; "=>" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [ Drv Pat ];
                                          nibs =
                                            ( {
                                                shape = Concave 43;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 43;
                                                sort = Drv Exp;
                                              } );
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
                                                       "fe3420d9-8a01-49a7-9ade-d4e97fe7a7a2");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "2d30daaf-43be-4365-abf2-c46d2b7a2757");
                                                label = [ "R" ];
                                                mold =
                                                  {
                                                    out = Drv Pat;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Pat;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Pat;
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
                                                       "da7e14a5-e18e-464c-9bc3-d9b141f15078");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Drv Pat;
                                                    in_ = [ Drv Pat ];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 23;
                                                          sort = Drv Pat;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Pat;
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
                                                                 "f637ee7e-8e0c-4981-9055-b8ca0a265c62");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
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
                                                       "5d671984-0f11-40df-8032-b0fda972a2c3");
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
                                             "f4069828-6727-42d0-a6cf-5cf7f7e4e3ab");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "a182400d-5c85-472d-8c98-a2ce3f99d8a0");
                                      label = [ "L" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "ed728ab9-5a1b-4c83-a89f-1d0516eb55a7");
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [ Drv Exp ];
                                          nibs =
                                            ( {
                                                shape = Concave 23;
                                                sort = Drv Exp;
                                              },
                                              { shape = Convex; sort = Drv Exp }
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
                                                       "9682cbf2-0b5e-4928-bfed-db542d376634");
                                                label = [ "y" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Exp;
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
                                                       "1841258a-7346-4848-bef4-da65dbdffdd8");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "956a0534-5fd3-4e29-8ef7-05958f068fa6");
                                                label = [ "==" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 31;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 31;
                                                          sort = Drv Exp;
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
                                                       "20706df9-c001-4700-a44e-45cb4690bcd2");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "5f1c21a0-48bb-4d9d-8724-f17a0d4a8079");
                                                label = [ "1" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Exp;
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
                                             "16a36f46-34b6-4d23-957f-f8bf088cc841");
                                      content = Whitespace " ";
                                    };
                                ] ) );
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "a4cbda5f-a9a4-4b16-99fb-8bef9a095355");
                                label = [ "case"; "end" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [ Drv Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Drv Exp },
                                        { shape = Convex; sort = Drv Exp } );
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
                                             "11222919-dbea-4974-98f9-47d4706dc368");
                                      label = [ "$" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              {
                                                shape = Concave 21;
                                                sort = Drv Exp;
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
                                             "d761a619-2a52-4d7f-ad1a-e31047144450");
                                      label = [ "gamma" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "c36340c3-cb1a-46ee-b737-f335a022aa8e");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d7a04535-fa6b-4226-9021-cd6582599ea0");
                                      label = [ "|-" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 49;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 49;
                                                sort = Drv Exp;
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
                                             "32646044-0d61-4e8b-9316-bcb996d02556");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f785c715-ed57-49ef-bb37-03f94df23d36");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "37e6bbee-59d2-49cd-8e0a-2baa0e8cbe96");
                                      label = [ "<=" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 48;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 48;
                                                sort = Drv Typ;
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
                                             "1d453c53-253c-47b6-9ace-0fb6f2d1bf7a");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "07261c7f-035a-4256-96b8-24ed2d2c6f5d");
                                      label = [ "Bool" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Typ },
                                              { shape = Convex; sort = Drv Typ }
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
                                             "845450b3-03a3-4283-9afa-c539cfc8c592");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d69530cf-9be4-4abc-a9e4-5ed466fa7dd8");
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 13;
                                                sort = Drv Typ;
                                              },
                                              {
                                                shape = Concave 13;
                                                sort = Drv Typ;
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
                                             "8020b375-d404-4bbc-9fdf-4841857cdd57");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ab0f4112-da03-4936-b0bb-454490de00b4");
                                      label = [ "Num" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Typ },
                                              { shape = Convex; sort = Drv Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                ] ) );
                          ];
                      };
                    caret = Outer;
                  };
                rule = Some A_Case;
              },
            [
              Node (Abbr (Some 0), []);
              Node (Abbr (Some 0), []);
              Node (Abbr (Some 1), []);
            ] );
      ];
  }
