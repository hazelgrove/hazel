let exercise : DerivationTree.spec =
  {
    title = "Task 4 of 7: Debugging";
    version = 0;
    module_name = "t4";
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
                             "c15c7bcb-6c48-49ca-be30-51592d74f0ab");
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
                                       "4c24d3f0-81da-41d3-95d2-ff79a00e3118");
                                label = [ "x" ];
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
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "f7362fa1-26e1-4c66-9b27-dc1afc31c58a");
                                label = [ ":" ];
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
                                       "6a53fa75-eb6f-4089-a44b-7a59e1d50cf2");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "8c3e44af-a4c7-4db2-81d2-9ee34f113d1b");
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
                                       "c0aa90eb-456e-43d5-bb09-0edc80876eba");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "fa2e5f0d-3edb-4653-b038-0f73f98c6736");
                                label = [ "*" ];
                                mold =
                                  {
                                    out = Drv Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 12; sort = Drv Typ },
                                        { shape = Concave 12; sort = Drv Typ }
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
                                       "0bb8a4b9-cc82-4141-aa8e-6e6d8a3c64f2");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "cb905a7e-68ef-4f3e-a9be-7a37e1043581");
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
                          ];
                        ];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "26764017-1f85-433a-b8a4-ba1944d1d078");
                      label = [ "," ];
                      mold =
                        {
                          out = Drv Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 47; sort = Drv Exp },
                              { shape = Concave 47; sort = Drv Exp } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "232c44e2-56cf-4be0-8a9b-7e5bad5722d5");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "0b801439-7268-4d50-b56b-3ae9ee60264a");
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
                                       "098c6ef0-8a06-4a53-b041-b9efadbde440");
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
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "2fce9f68-8d20-443c-8e5e-c8c20a6c61fa");
                                label = [ ":" ];
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
                                       "d578e7af-213a-4f03-8a24-9ec34fdd0285");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "da188580-e24e-4de9-a51f-545cc7011112");
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
                          ];
                        ];
                    };
                ],
                [] );
            ancestors =
              [
                ( {
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
                                 "38446fd4-e113-4f95-b0a9-d9d76c98a348");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "182bb260-0b21-43b8-8694-c943ca3549fc");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
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
                    shards = ([ 0 ], [ 1 ]);
                    children = ([], []);
                  },
                  ( [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b596aa7f-3763-4e2f-9b2d-d0f83b8e519d");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "49ea9466-22b8-4cc1-8cfd-54f94fb49eec");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
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
                    shards = ([ 0; 1 ], [ 2 ]);
                    children =
                      ( [
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
                                       "e38f91ec-d1a2-47f3-b674-5885f160dad9");
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
                                       "a006b52c-3419-4eb2-9796-ffd7cfb23aae");
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
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e1ac5694-0fe4-4c7f-a973-42b2f490b9c9");
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
    corpus = ALFp;
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
                          ( [
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "17e56f47-7213-4256-85a2-264ffd3cf062");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
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
                                         "1185c5b4-6a24-45bc-8a5d-eeb813b61e4c");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "f8bd49b6-3201-468f-857f-8b0e03a57c2d");
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "1301d10e-c4bf-46cd-bfef-1be166d8865c");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a170b29c-b8b7-47c7-acd7-83c31f31e759");
                                  label = [ ":" ];
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
                                         "f2a2e00b-38ea-49a7-aa58-ba4ab96bafa7");
                                  content = Whitespace " ";
                                };
                            ],
                            [
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "b785c546-6846-4e97-9a02-ca2a8aa5c0df");
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
                    caret = Inner (0, 0);
                  };
                rule = Some T_Num;
              },
            [] );
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
                                         "f70ae12b-17aa-4239-bea9-30c09abf762b");
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
                                         "841df9dd-639b-4a18-8d07-88e661f0d310");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "13471dec-cbf6-43f9-99b1-e447716868f7");
                                  label = [ "*" ];
                                  mold =
                                    {
                                      out = Drv Typ;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 12; sort = Drv Typ },
                                          { shape = Concave 12; sort = Drv Typ }
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
                                         "5ae13836-b2ff-4beb-a4bc-08cf88955946");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "53b4d60e-b80a-4e1f-bee7-af492f6559d1");
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
                                       "30761b0c-8a9a-439b-a0f5-9041d3b86f26");
                                label = [ "("; ")" ];
                                mold =
                                  {
                                    out = Drv Typ;
                                    in_ = [ Drv Typ ];
                                    nibs =
                                      ( { shape = Convex; sort = Drv Typ },
                                        { shape = Convex; sort = Drv Typ } );
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
                                             "44e8a013-e675-40f7-a697-02590060ea06");
                                      label = [ "|-" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
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
                                             "3b31ff5c-e09d-4519-a7c3-08dda07d9d86");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "efe9bacc-b114-4978-b8a0-99c797efc4e1");
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [ Drv Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
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
                                                       "4c97829f-b11f-4de9-af67-b091f997e6a7");
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
                                                                 "6de9c05f-dff6-464c-8724-17aadb14d16f");
                                                          label = [ "False" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
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
                                                                 "e4d427bb-6326-414e-aab0-b29c7439e6b0");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 47;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 47;
                                                                    sort =
                                                                      Drv Exp;
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
                                                                 "84f8c6fc-313b-4d41-8e7c-129882199a82");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "90008e63-ed27-460c-aca1-38b09e3bfa5a");
                                                          label = [ "1" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
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
                                                       "9a8f6acf-df8f-4aa9-9375-12bc79b69d03");
                                                label = [ "," ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 47;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 47;
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
                                                       "1b5f05a9-cb49-4365-8105-77d1217f19b4");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "a1419e78-410b-42b2-b33b-492d9b82fbfc");
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
                                             "f138b362-8394-44db-929a-d5c96581eba8");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "16002ec1-9149-41c4-ab55-3d7eed0ce9c9");
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
                                             "da94d04e-c091-4db6-9578-195c177a4f19");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ba05276a-cd51-4ad1-9428-2f9e1c47f183");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "51ff3610-bdae-4c41-8b9a-38e85277cff4");
                                      label = [ "*" ];
                                      mold =
                                        {
                                          out = Drv Typ;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 12;
                                                sort = Drv Typ;
                                              },
                                              {
                                                shape = Concave 12;
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
                                             "98defe00-93a0-4e37-a4f9-42872369acc5");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ef24acb1-a598-40fd-a892-800189789d43");
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
                    caret = Inner (0, 1);
                  };
                rule = Some T_Pair;
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
                                               "7cbdcfbd-d97e-428d-9b2b-0f7b26d39045");
                                        label = [ "|-" ];
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
                                               "2b1ff913-9d7f-4861-ba07-0b30ca8a2429");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "bbf7a5c6-2a54-426f-91dc-1a2b0f47b3d7");
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
                                                         "e8f52d6d-9dc7-4900-8592-0e393f8a00c5");
                                                  label = [ "False" ];
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
                                                      (Haz3lcore.Id.of_string
                                                         "dd00b1e0-6365-42ac-91a8-c265361a4d1d");
                                                  label = [ "," ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 47;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 47;
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
                                                         "f437a89c-8472-4986-a356-8e5147803a08");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "3aefa985-223d-40ee-afcb-06ed3c257f7a");
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
                                               "b39ade9f-34e7-4182-91d2-a49c25c6e41a");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "896744b8-1d61-4c17-92c1-10ba3df57655");
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
                                               "ab8cfca0-6a2f-437c-b979-cf2ad86f2581");
                                        content = Whitespace " ";
                                      };
                                  ],
                                  [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d9788804-95d4-4ecd-8683-30822aa9d456");
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
                          caret = Inner (0, 0);
                        };
                      rule = Some T_False;
                    },
                  [] );
              Node (Abbr (Some 0), []);
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
                                         "429b25e9-edf6-47ab-88a5-6aaa0cdd0c2b");
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
                                                   "86013d2b-1244-489f-8c7c-df836e3a7cd6");
                                            label = [ "False" ];
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
                                                (Haz3lcore.Id.of_string
                                                   "ce162dc1-57af-44c5-9dcb-2faff74d773e");
                                            label = [ "," ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Concave 47;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 47;
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
                                                   "f7f86441-5323-4bde-8a16-a82ce56bb3ed");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "c0c59e35-aa1d-4343-99d2-8b1bcd5ce385");
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
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "268bca04-4a2e-4c64-b97b-b30d5216d56b");
                                  label = [ "," ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 47; sort = Drv Exp },
                                          { shape = Concave 47; sort = Drv Exp }
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
                                         "5f18bbb7-827a-4531-a33b-589a85b7fe76");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "fe142004-0526-43c6-9365-676e3115ab93");
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
                            ],
                            [] );
                        ancestors =
                          [
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "169e5ef8-0561-4e74-83f8-76ec411d46d3");
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
                                             "ec726a15-329c-4087-9041-6001b9297f26");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "bc45b0f9-0d1e-4bb8-be3a-5926cff11737");
                                      content = Whitespace " ";
                                    };
                                ] ) );
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "ffd740c7-9943-42c1-bb20-5900b3b33300");
                                label = [ "let"; "="; "in" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [ Drv Pat; Drv Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Drv Exp },
                                        { shape = Concave 40; sort = Drv Exp }
                                      );
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
                                                   "8dd82582-0e81-4261-b1b0-f04bc9187511");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "30fb4bd3-cb7e-44ab-b60c-f2537233a0ce");
                                            label = [ "("; ")" ];
                                            mold =
                                              {
                                                out = Drv Pat;
                                                in_ = [ Drv Pat ];
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
                                                             "31e9942a-bff5-4b69-adbf-8ee694e79338");
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
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "fdefdba2-40e3-4ec7-9ae0-d464f8e01d7d");
                                                      label = [ "," ];
                                                      mold =
                                                        {
                                                          out = Drv Pat;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape =
                                                                  Concave 47;
                                                                sort = Drv Pat;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 47;
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
                                                             "bbb2396d-f4e9-4048-a334-7f4c58e5595b");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "230bdde0-3899-4b94-8006-1d7b5fb31003");
                                                      label = [ "y" ];
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
                                                   "bf9a7910-3dd8-49f9-ac5d-38d6957cdec2");
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
                                             "9cce0f2c-1cee-482e-b8e6-ee0f56580925");
                                      label = [ "|-" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
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
                                             "5df1f1ee-a02f-47f8-a7d0-f77190c68c25");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f4804c4b-3e1a-45d3-8543-2c0e31f8b5f3");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "0399a809-e11b-4aca-8244-ff15e83ca54d");
                                      label = [ "x" ];
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
                                             "85065269-139c-4f47-824d-05ee6efbe1b8");
                                      label = [ "." ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 22;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 22;
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
                                             "8e708dcd-e999-467e-8401-39a02116b611");
                                      label = [ "snd" ];
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
                                             "567ae007-b9ba-470a-ba11-4dfa1eb7a2f0");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "c48bb527-8dcf-4986-9171-99f6e6c3fd54");
                                      label = [ "+" ];
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
                                             "2a59c83c-88ca-402e-b185-108bfb8204d5");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "91b169ac-6149-4438-9160-46a5ca73dfed");
                                      label = [ "y" ];
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
                                             "82c9b9c3-41eb-486e-af32-ea6944a2a0c2");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "73723884-252f-4721-984f-a290fc6c7f1f");
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
                                             "ab0f0090-a63e-4a47-b01e-e3dd9b9d40c9");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d3fb90cb-faa8-4d22-aac6-795d89857911");
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
                rule = Some T_LetPair;
              },
            [
              Node (Abbr (Some 1), []);
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
                                               "b22e4249-91a7-4a67-b45e-8c363931d3d4");
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
                                               "19d7f7b1-7343-4096-82a9-8c1b8a80cf0a");
                                        label = [ "gamma" ];
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
                                               "2dd695fa-551d-4dce-b36e-251c73c74341");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d4d22fb9-64c8-46e2-a0e0-00111ee0132b");
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
                                               "075d05f4-a9ac-4949-98a7-7c8bab0c1d13");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8d4906e3-532d-4b81-80dc-fa2e3c27823c");
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
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e5bc549e-c3f0-4f28-81e4-2d0e2d66e153");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "71f7ec6e-cf8d-4cdd-ab9c-74265f7823d7");
                                        label = [ "snd" ];
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
                                               "bc6ea338-cc64-42e1-87d1-9de9a5732959");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fa669ee3-ae3d-4ab5-98d6-65acc870c4ec");
                                        label = [ "+" ];
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
                                               "71ab1aeb-fb81-4830-a7aa-33743096f4cd");
                                        content = Whitespace " ";
                                      };
                                  ],
                                  [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7d2c46d7-7b6d-4bae-aa54-e0cab07bfe1b");
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
                                               "352f3bf7-bd0a-4e32-a634-920d367e2a72");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b8b76485-4795-4088-801f-b8bcf717e7f0");
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
                                               "72b7c58a-77ac-4de3-b558-3d7cdc2aff40");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "51192cde-f501-4a5d-9d92-24a4b704d445");
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
                      rule = Some T_Plus;
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
                                                     "cabfba75-2f4b-4c14-800a-42a0453dbe77");
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
                                                     "6262e5bd-2d66-46d4-be31-4813f7b07409");
                                              label = [ "gamma" ];
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
                                                     "6f69f523-875b-441e-a39a-8a611fc7c23c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a6d46cbe-b405-433a-acf8-3361d18c57bf");
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
                                                     "c26f56d6-4a62-411a-9967-48531b9648dd");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "303c7963-c78e-404d-8837-a50cab467ed0");
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "7e7c8b19-135f-415c-9573-48743234b7bb");
                                              label = [ "." ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 22;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 22;
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
                                                     "59a23b8e-3b70-492d-ac3b-f6ba3adfc7f5");
                                              label = [ "snd" ];
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
                                                     "c6010731-5055-445a-8a3e-894a3e13b82c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "162f5a2e-7a00-415a-9d6a-1f897023b2d4");
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
                                                     "31432963-cba3-4aac-b8ce-b955edfbc0ad");
                                              content = Whitespace " ";
                                            };
                                        ],
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "bf916772-8081-4697-9ae0-7525fe863874");
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
                                caret = Inner (0, 1);
                              };
                            rule = Some T_PrjR;
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
                                                           "71593035-03ac-498f-bbd5-44f50b6ec55e");
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
                                                           "0a706e90-d817-49d2-8e2a-6171a477ed1d");
                                                    label = [ "gamma" ];
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
                                                           "3c1c7231-741c-4818-8793-bcba1a26f57e");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "7834d395-cef7-421a-9438-42538dfa7b94");
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
                                                           "839787a7-c31c-454f-be07-2321472b9ffd");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "d8d6f833-7da9-4694-8f57-a79e95c470a1");
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
                                                           "261260bd-5516-4773-a229-320bacc54681");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9b051f52-44c4-4f43-8c68-f5d478df5d4f");
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
                                                           "2cd40805-5782-4fae-ac38-54c0485479dd");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "cb4407b3-a464-463c-aaeb-3408954c286c");
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
                                                           "c3f507eb-1fbe-4c09-a878-cfba7b16bc89");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "b1edc29c-55b3-412e-a2f4-90d7d31bed5d");
                                                    label = [ "*" ];
                                                    mold =
                                                      {
                                                        out = Drv Typ;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 12;
                                                              sort = Drv Typ;
                                                            },
                                                            {
                                                              shape = Concave 12;
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
                                                           "43690d08-cd58-4afc-9c93-6abde7c168e3");
                                                    content = Whitespace " ";
                                                  };
                                              ],
                                              [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "a79632d6-0960-4317-a956-9e4c0536a71f");
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
                                      caret = Inner (0, 0);
                                    };
                                  rule = Some T_Var;
                                },
                              [] );
                        ] );
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
                                                     "e3157828-c82e-4a1c-9cfe-3937c7a17ea5");
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
                                                     "884a1890-d372-478a-8239-8f36d5a02d41");
                                              label = [ "gamma" ];
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
                                                     "0b03949f-e90a-4f19-bc4d-b959ddabc89c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "795ff6b6-51be-4816-b55b-c4631891f57a");
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
                                                     "0977ceab-6a03-40b6-8468-cfa20312a8e3");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6ff7a2e1-ae2d-4897-be7e-63cbe05154e1");
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
                                                     "eca5599a-901f-4218-b678-c0a41fe8e991");
                                              content = Whitespace " ";
                                            };
                                        ],
                                        [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "cce9e6af-d223-4ff3-bb06-87248cdf52e3");
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
                                                     "effc7d75-fb74-4ae7-9072-3b618a6682b8");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2dfb09c1-8748-44f1-a5c2-789fe88f8caa");
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
                            rule = Some T_Var;
                          },
                        [] );
                  ] );
            ] );
      ];
  }
