let exercise : DerivationTree.spec =
  {
    title = "Task 3 of 9: Transcription";
    version = 1;
    module_name = "t3";
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
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "2192643a-c493-4cf2-9989-f634c55b8c47");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a164e47b-b77d-4783-b477-ce382eda978e");
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
                                       "a12d17f8-31bc-4943-8762-b2654eaab912");
                                label = [ "b" ];
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
                                       "593ac0e3-cfad-4224-9023-960a856f9ab6");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "722a98e0-55f2-4bd0-a4e4-bdf5184827de");
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 12; sort = Drv Exp },
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
                                       "9933d7b0-20d3-4743-adbc-fa7e4c8c604a");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bf70f862-9f4c-4b1e-8fc5-d7b82c870fb2");
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
                             "83088692-35ee-497d-8894-fdab2eb673ff");
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
                             "e2ac179a-7336-4a55-b8ca-da604f6701a6");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "986db874-8778-4345-81d5-3ac626b29e04");
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
                             "029fe87f-4ae1-4784-89b4-ef626dbf82d1");
                      label = [ "gamma_a" ];
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
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "df136289-239d-47ed-ade1-6f616d49b824");
                      content = Whitespace " ";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "8f1e0b03-1d36-4858-a466-73c26b309c14");
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
                                 "f2aec451-304e-4399-a03a-9cd00d4a9d99");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2c3a853e-a2a7-49b8-a064-22c0d586481f");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "25b68ecb-caf9-4bb1-b2c7-2aae751663b2");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 17; sort = Exp } );
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
                                       "d9a87fc4-969d-4d53-81bc-d24da32621dc");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "b5e875ef-e924-4553-b58c-12bddcbb6de9");
                                label = [ "gamma_ab" ];
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
                                       "f61c020a-14fc-476a-86d3-2a0fedbfa65e");
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
                                 "9a557ef0-7d77-4c8c-8b10-ffb268135c9c");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 17; sort = Exp } );
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
                                           "c8d04bff-85bc-4cf6-84b3-baf2517bdeb0");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f3d1b147-5319-4cb7-a0a9-735ca9f0c4cd");
                                    label = [ "gamma_a" ];
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
                                           "cdfc37fa-c913-4c51-8fae-673b3f64e8ef");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "cd318be1-fa2d-42b7-8aa1-b0eb9151ad7c");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "656bc72f-0719-4d0a-9cd6-c41e823bbc71");
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
                                                     "d639061d-58b6-4253-8fed-0343ecf32010");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "d742a41b-b642-42d9-850a-05981a84a00a");
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
                                                               "568831b2-59e1-48f0-9873-6d9a0710bd6a");
                                                        label = [ "a" ];
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
                                                               "d2a122dd-5493-43c4-868c-ebf144cabe05");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2e83b2c0-1a03-4473-8664-fce55b245505");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 12;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 12;
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
                                                               "83ff71b4-6ee8-442b-9931-65d83b131973");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "c9829c7d-5e53-4b3f-a60d-e47444e9c517");
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "070850ce-bee6-4a84-a6f2-bbb300501a6c");
                                              label = [ "::" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 7;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 7;
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
                                                     "6986b0e4-f833-42ee-849c-cfe5ddd847e8");
                                              label = [ "[]" ];
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
                                                     "25bf1471-b7c2-4f7f-a5bb-258ecaa1e8fe");
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
                                           "efcbabe0-dd12-4027-9c3b-1d75fdd38ff5");
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
                                 "f2696306-5eb4-4c8e-8949-887c0b2a23c1");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d6b83579-1bb7-40cb-be27-5616d3331009");
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
              ( [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "7a205d3c-14f7-4b39-a13e-f0b4bba49577");
                      shape = Convex;
                    };
                ],
                [] );
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
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "6153a96f-6ebd-490e-be3f-43a15587e44b");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Concave 46; sort = Drv Exp }
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
                                         "8fac760c-4d2f-4361-b09b-cedea9c7262d");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "8dfab0e3-e734-442b-ab3c-fc75dd399a95");
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
                                                   "33a40f4d-087b-4e9e-825c-8b16d9f0c1ea");
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
                                                             "f4d11a3a-4cb6-4a07-9dbc-8f2a7545f9a4");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "449fae2d-a732-476f-9dcc-84e3a69ce4c8");
                                                      label = [ "a" ];
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
                                                             "1ec32a16-35e5-45f8-9c8f-e12487c1107e");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "44c14fea-6ee0-4c22-8e82-8edab2d3216c");
                                                      label = [ ":" ];
                                                      mold =
                                                        {
                                                          out = Drv Pat;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape =
                                                                  Concave 12;
                                                                sort = Drv Pat;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 12;
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
                                                             "85ccd03d-0cad-404b-99a6-96e1de803642");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "f92bffdf-f057-40fb-8075-ef13ec7e946b");
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
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "0f032e67-0674-4314-af87-e533bb485712");
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
                                                   "1d967800-3083-4289-a3d8-f35ea2dc5597");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "db6ca8c2-5533-40c0-b9d5-39645600b55d");
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
                                                             "7f0fa801-03c5-4288-a6c0-6615060f813a");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "9bad651e-192c-4087-b9e1-382f855ee7c8");
                                                      label = [ "b" ];
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
                                                             "30d8b3a2-ab63-430b-9052-80d0b95ed4b2");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "ce51c665-a481-47dc-957c-e17716bfbfc0");
                                                      label = [ ":" ];
                                                      mold =
                                                        {
                                                          out = Drv Pat;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape =
                                                                  Concave 12;
                                                                sort = Drv Pat;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 12;
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
                                                             "4c32c65d-5157-4dcc-bbdc-de2474ef55e2");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "b1765627-c1f1-42d2-95ca-3b9e474b5fd5");
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
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "1af7650f-b286-4888-b06a-d941081b87dd");
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
                                                   "ccb1a462-509d-4a07-89f0-fbf7549bcde3");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "43ec61b6-64e5-49bb-9ac1-b99f54d02bf7");
                                            label = [ "if"; "then"; "else" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [ Drv Exp; Drv Exp ];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 13;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0; 1; 2 ];
                                            children =
                                              [
                                                [
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "ae8555dc-d238-4cf6-8af3-cb369171dd3a");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "2faa3523-5c1d-4664-808d-5ee31a0f8ab9");
                                                      label = [ "a" ];
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
                                                             "1d2dfe4a-c703-49ed-a322-126cfbe65ac6");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "68c11f2c-07c5-475d-b6bf-755440091b3a");
                                                      label = [ "<" ];
                                                      mold =
                                                        {
                                                          out = Drv Exp;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape = Concave 9;
                                                                sort = Drv Exp;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 9;
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
                                                             "206d72ed-2367-4d21-9c76-3428e675fe28");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "3f46fd48-59d5-4f8a-968c-7ae59d94540b");
                                                      label = [ "b" ];
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
                                                             "84401726-ddb6-4e74-b665-0f2d2bdd3cdc");
                                                      content = Whitespace " ";
                                                    };
                                                ];
                                                [
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "4d2bc1ab-8b3d-40ae-9d43-f9ce04ec040c");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "3da71d96-9af4-4b86-9249-73a391379bb6");
                                                      label = [ "a" ];
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
                                                             "a4884900-aefe-43a0-8636-342842050657");
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
                                                   "b5fd92d9-4e52-45e1-8a1e-36e50973451b");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "0865451f-f2a2-42ae-875b-3c668042c4ea");
                                            label = [ "b" ];
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
                                         "0d639075-9113-4bd0-bfb4-4e8c0582f733");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a928d7b2-8a89-4943-98b2-13406bada42d");
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
                                         "4f937986-b75e-45a2-b886-1881698c4bc0");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7a97cb34-a66f-417f-8277-69f54c907b8c");
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "f1a54919-6387-477b-9d38-f39859e04998");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "88eae50b-eedc-455f-ad3d-c109fd420eb7");
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
                                         "8162904c-bcc3-4848-8103-437ddc297465");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "57644540-2045-4612-83d2-90e4880ab1ee");
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a2d870ab-9bf2-4f26-9879-47080e59c615");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "3c8e75a5-013f-4259-833b-928d05127b26");
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
                                         "2e183b32-def8-4245-a4c0-764253db6229");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a064f3c2-7f1b-47d9-ab31-a67869df4ad8");
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
                rule = None;
              },
            [] );
      ];
  }
