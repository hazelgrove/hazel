let exercise : DerivationTree.spec =
  {
    title = "Task 5 of 9: Transcription";
    version = 0;
    module_name = "t5";
    prompt =
      "Transcribe the derivation tree into Hazel Deriver. No worry if the \
       derivation is not correct.";
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
                                         "102819af-25a4-4e4b-809d-37639c76cad1");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
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
                                  shards = [ 0; 1 ];
                                  children =
                                    [
                                      [
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
                                                   "479e800a-3948-4196-aeb6-89f59d978244");
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
                                      ];
                                    ];
                                };
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
                                        ( { shape = Concave 43; sort = Drv Exp },
                                          { shape = Concave 43; sort = Drv Exp }
                                        );
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
                                                   "5d671984-0f11-40df-8032-b0fda972a2c3");
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
                                         "ed728ab9-5a1b-4c83-a89f-1d0516eb55a7");
                                  label = [ "("; ")" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Exp ];
                                      nibs =
                                        ( { shape = Concave 23; sort = Drv Exp },
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
                            ] );
                        ancestors =
                          [
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
                rule = None;
              },
            [] );
      ];
  }
