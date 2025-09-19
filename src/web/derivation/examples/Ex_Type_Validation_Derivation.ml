let exercise : DerivationTree.spec =
  {
    id = Haz3lcore.Id.mk ();
    title = "Type Validation Derivation";
    module_name = "Ex_Type_Validation_Derivation";
    prompt = "";
    prelude =
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
                             "2d391623-2177-43a3-ae2c-7b3790b5afeb");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a0d9e8e0-1238-4b23-a9d7-92fc744b505c");
                      label = [ "A" ];
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
                             "fa13eb3d-de40-45ff-aadd-43bc508c0941");
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
                           "813229a8-2d88-4d5e-9509-58b7fc3e9ac7");
                    label = [ "valid"; "end" ];
                    mold =
                      {
                        out = Drv Exp;
                        in_ = [ Drv Typ ];
                        nibs =
                          ( { shape = Convex; sort = Drv Exp },
                            { shape = Convex; sort = Drv Exp } );
                      };
                    shards = ([ 0 ], [ 1 ]);
                    children = ([], []);
                  },
                  ([], []) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "cb7a7fdb-8155-4c3b-9d31-decebbb4d5f6");
                    label = [ "["; "]" ];
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
                                 "a1ab2949-9945-4dd1-92cf-9fadef1653b7");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a828b61f-2cb5-4598-9acc-b43855c26e4f");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "17b70310-333d-4e61-8209-353f36514b67");
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
                                 "22fd0d0e-b81e-49d2-876f-2111469150ac");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e1efd65b-b2dd-4022-9b23-18d55f1eb3ad");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "b533dba4-a7fa-46d1-91a4-4e2b9d611641");
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
                                       "bb1c3b18-0d2d-45b9-b67b-77d3709d5397");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "3424e53b-8aff-4a49-9876-81ea7fa4e01f");
                                label = [ "$" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Pat },
                                        { shape = Concave 21; sort = Pat } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "24d4008d-2a18-424d-887f-eae2ab5de29b");
                                label = [ "delta" ];
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
                                       "b3c26f13-96ad-44bf-85f6-1dec0c472d7c");
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
                                 "fe55387a-08c3-404a-9155-ebdf04c52896");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a65faa52-2532-4b24-b645-e0c6e19c6c0d");
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
                                           "9ead2ece-9013-49e1-b17b-78aab0634cd5");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "3aae24cf-bf68-40e9-bbc5-50ed194dfd68");
                                    label = [ "$" ];
                                    mold =
                                      {
                                        out = Pat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Pat },
                                            { shape = Concave 21; sort = Pat }
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
                                           "80546bdf-7fc9-44b9-99b9-5d5b6cdfafcd");
                                    label = [ "delta'" ];
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
                                           "eea8d8aa-dc05-41d5-921c-4d567d2af3be");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1bdb9f96-9c6e-42f9-aaa0-49a83d1ae554");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f888fbaf-21ce-414c-9f60-54a156da0203");
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
                                                     "42f9665b-8336-4ea5-853c-0aac68614b8a");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "3a9ba3f5-7f53-4852-b4ff-8a80b72fa046");
                                              label = [ "("; ")" ];
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
                                                               "65d8fb26-5263-418b-852e-b4129c0ad836");
                                                        label = [ "x" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f3198eaf-e6af-4170-81db-bb7d83af53d2");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "dce83bd2-c219-4fc6-842f-ed8c430549cc");
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
                                                               "7ad9948d-0e27-4e1b-b327-732098f5d0a0");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "b4ea8b66-0d7c-4d11-bdd4-4b00d6a87c0f");
                                                        label = [ "A" ];
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "7f29f6aa-8c0d-4425-b0d5-74ff806dc6cc");
                                              label = [ "::" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 29;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 29;
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
                                                     "196d08ca-3af5-42ee-8568-6e5cb21acff8");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "aa7cd2f0-9628-4c38-8c63-240f56312bdb");
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
                                                     "b7de1966-3d9b-4b2c-804a-60f1468cb8a1");
                                              label = [ "delta" ];
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
                                                     "5b3d3d85-14ea-4677-bb88-80bd1601546f");
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
                                           "e1a53219-1543-479e-a528-d32959bb719f");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d9f841ff-4f5c-47ae-98cf-e6eea4cddba1");
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
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "33666b5a-066c-4dca-8edf-ff22bc65c7a7");
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 38; sort = Exp } );
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
                                       "24597667-0e6c-446f-80b6-015b667e8c6b");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "7aa9ac9e-77bf-402c-9c1a-bdb0c4c8d5ab");
                                label = [ "delta'" ];
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
                                       "19c0e23a-ad83-44bc-aa31-25dd27dc203f");
                                content = Whitespace " ";
                              };
                          ];
                          [
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "57b6c8db-6962-4345-8a09-a937ea2a2a9e");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "4e8619b2-bd10-48b4-85f7-ae2a064e489e");
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
                                                 "f3402c6f-6815-4ae6-8af4-a24e249bba15");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "2d190d38-e17a-487e-978c-ad8f631f7fa8");
                                          label = [ "("; ")" ];
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
                                                        (Haz3lcore.Id.of_string
                                                           "16befc60-f39f-444d-a339-8ff2728a45a5");
                                                    label = [ "x" ];
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
                                                           "71ca1887-af9b-4e5b-bb1c-c7319137da13");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "2e1babec-fe12-48d5-8e7e-e5331f4f0640");
                                                    label = [ ":" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 23;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 23;
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
                                                           "19d6cc96-b907-451e-bd54-df5227f1411d");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "0144e6b2-98e5-4493-8680-271eadd89b6a");
                                                    label = [ "A" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "868cffa4-f53f-41a6-b366-b04451f095e8");
                                          label = [ "::" ];
                                          mold =
                                            {
                                              out = Drv Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 28;
                                                    sort = Drv Exp;
                                                  },
                                                  {
                                                    shape = Concave 28;
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
                                                 "29673b14-82dc-4318-b714-9a4e369d6678");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "0bdd5fb3-e3ad-48c5-8d00-67dc412d0fca");
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
                                                 "8edc280d-427d-4f46-a1fe-1d4e165e768e");
                                          label = [ "delta" ];
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
                                                 "f51a9ada-7cf5-4ff0-9c1b-c4ec06495d02");
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
                                       "676d288b-964c-4346-b636-5b0c48c029f8");
                                content = Whitespace " ";
                              };
                          ];
                        ];
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "81f98b60-b5b5-48c1-82e9-4ff42474c069");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
      };
    corpus = RecursiveALFA;
    trees =
      [
        Node
          ( Just
              {
                jdmt =
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
                                         "7dd6994e-fd98-4092-ae12-3de0294ecec6");
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
                                         "c62109a2-2c49-44a6-8e4a-d8856dd418f2");
                                  label = [ "delta" ];
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
                                         "32c6cc78-d990-4a12-a0c1-2f7ed9b83aea");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "1620c2cb-09c4-4d61-80f0-a28963f14694");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 46; sort = Drv Exp },
                                          { shape = Concave 46; sort = Drv Exp }
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
                                         "fc8ba8e3-9b35-4aae-899c-dd09dae9e548");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "5901997d-d506-4493-be38-7490561a7616");
                                  label = [ "("; ")" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Exp ];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
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
                                                   "561e8a44-5228-4eb8-b288-1f624b5e6101");
                                            label = [ "fun"; "->" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [ Drv Pat ];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 14;
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
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "43462108-e539-429e-9f1a-1969db14bd52");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "d9e2699f-bcc5-426c-aa5f-a4b64f21ce82");
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
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "ff2ac15e-76ad-4ed5-91b5-370655a5be37");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "76992270-2973-49bb-9cec-02ff67d43f4c");
                                                      label = [ ":" ];
                                                      mold =
                                                        {
                                                          out = Drv Pat;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape =
                                                                  Concave 23;
                                                                sort = Drv Pat;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 23;
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
                                                             "218c7a2a-a00e-44eb-93f1-4d8a6c0d404c");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "68585ba6-a584-4212-8464-bc1081550497");
                                                      label = [ "A" ];
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
                                                             "b7c47e34-54b7-4519-a489-ce74976ca98f");
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
                                                   "c3e2d208-2134-405e-9dfc-ebaf7d9a8ab7");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "a16c9d9a-1167-4749-8074-e0c93dc605a0");
                                            label = [ "x" ];
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
                            ],
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "4583f952-1a29-4271-84db-d626700bdae3");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ebe1f8b9-896c-4515-a52a-f15e7554ace5");
                                  label = [ ":" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 23; sort = Drv Exp },
                                          { shape = Concave 23; sort = Drv Typ }
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
                                         "4d48d124-84f8-4342-893d-1069c0118405");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7325b8d1-941b-4628-877c-9fdae132597a");
                                  label = [ "A" ];
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
                                         "74123937-04f3-4d8c-bdd6-d2e87ee6828c");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "511a9893-5ed7-4c3c-a168-c7192cfbafa7");
                                  label = [ "->" ];
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
                                         "0ed6deb5-0f14-4bdd-9761-41aae97b980a");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "f8b4dd23-f0f6-4cbc-a178-709507db394b");
                                  label = [ "A" ];
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
                rule = Some T_FunAnn;
              },
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          relatives =
                            {
                              siblings =
                                ( [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a03aee8f-fa77-4ff4-a788-789084bc3c9b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d910ae79-04a8-4970-af02-8c9897da5bd9");
                                        label = [ "A" ];
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
                                               "b7438e96-4819-4cb6-8766-ca2d507ebdae");
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
                                             "30d3fb94-32f3-4dc8-b289-3b1d2c57f8b0");
                                      label = [ "valid"; "end" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [ Drv Typ ];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
                                            );
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
                                                   "42eb55d5-6e45-4dbb-82eb-5b7d4c63ce95");
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
                                                   "763e39d1-cda8-4b33-a85a-db41511b172e");
                                            label = [ "delta" ];
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
                                                   "c85b6d42-b9f6-4c69-9978-e57a80130218");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "ad03f8c2-874b-4a09-b51f-4e24e9374085");
                                            label = [ "|-" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Concave 46;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 46;
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
                                                   "1abfcbd5-6e20-4088-8d0b-e64198578d35");
                                            content = Whitespace " ";
                                          };
                                      ],
                                      [] ) );
                                ];
                            };
                          caret = Outer;
                        };
                      rule = Some TV_TVar;
                    },
                  [] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ae5a28db-d32a-4ba1-a401-01607f1a139c");
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
                                               "341e5bab-2e42-481c-8175-142f3333d6ab");
                                        label = [ "delta'" ];
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
                                               "67790b73-ad76-43fc-81d8-8fa5d802166c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0f37ffec-06b9-440f-afbf-028c3d0daf59");
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
                                               "ab8d8af0-d634-4fcc-bf00-4535a3d95f9b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7432d0c8-7236-40a2-80d9-84c393bfbdd1");
                                        label = [ "x" ];
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
                                               "c34c7630-b5fe-4b60-a41a-98fb27fbe421");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d8f4e879-1bb0-4c78-84b5-8b6185638502");
                                        label = [ ":" ];
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
                                               "b3a7bb27-9404-453a-ad1d-fc93fc2573b3");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c4e3fedd-15c8-4970-af47-85a9d2f1b86a");
                                        label = [ "A" ];
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
                      rule = Some T_Var;
                    },
                  [] );
            ] );
      ];
  }
