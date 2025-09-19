let exercise : DerivationTree.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "de0ffb30-7983-4d1d-85af-d04f63b8f121");
    title = "Task 6 of 7: Deriving";
    module_name = "t6";
    prompt = "Derive the following judgment.";
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
                             "e79a0eec-5f2e-4b06-afdd-cc500f643468");
                      content = Whitespace " ";
                    };
                ],
                [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "fc950cdb-1932-4a89-9bfc-d8e33822905a");
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
                                       "2dd4e800-5531-4de4-bc62-3a1956fa1923");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5cdc724c-3887-49b5-b068-046b1cd27b57");
                                label = [ "["; "]" ];
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
                                                 "197b3a85-1c6b-4e01-912d-e5b15ca38e3e");
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
                                                 "072c987a-549e-4326-8327-07c6ec3d7800");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "2307505a-1bdc-477d-85fa-d3dae12e2db8");
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
                                                 "01208b49-3f8b-4a40-ad4d-71336d751570");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "b97678c9-0b00-424a-a0b5-0f9303864497");
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
                                       "866e107b-f1de-4142-a992-94ac97bfd798");
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
                             "cc773616-eb22-4fe3-9103-0286934c91bd");
                      content = Whitespace " ";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "1c3887bf-6021-40d1-bb05-d7adcfcfd5e7");
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
                                       "fa6568da-36a2-4b6b-9f2a-c7ef642d5547");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "ea375a47-c744-4c40-b957-654857332f23");
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
                                       "6b7b91b3-18ff-40e4-b90a-ddc02507bbf7");
                                label = [ "gamma_x" ];
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
                                       "b03219ba-e658-4f63-8412-93d0542eeb81");
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
                                 "c29564a2-4555-4bc3-87cb-f5f57db5deab");
                          shape = Convex;
                        };
                    ] ) );
              ];
          };
        caret = Inner 0;
      };
    setup =
      {
        selection = { focus = Left; content = []; mode = Normal };
        relatives =
          {
            siblings =
              ( [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "bfdcaa4b-dd16-4ecf-a2fe-74808f14a74f");
                      shape = Convex;
                    };
                ],
                [] );
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
                    relatives =
                      {
                        siblings =
                          ( [
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "e01fdec8-9017-4071-8988-cb136ef0b3fd");
                                  label = [ "fun"; "->" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Pat ];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Concave 36; sort = Drv Exp }
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
                                                   "ad9e0e8a-11d8-44b4-aa23-05e42ceada53");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "263dcad0-7047-4fd1-9805-e460a374c939");
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
                                                (Haz3lcore.Id.of_string
                                                   "a85e5844-ee46-47d0-8c1c-53fc2e289fd2");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "6cdceadc-6e28-4e9f-8394-ddf24ac64654");
                                            label = [ ":" ];
                                            mold =
                                              {
                                                out = Drv Pat;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Concave 24;
                                                      sort = Drv Pat;
                                                    },
                                                    {
                                                      shape = Concave 24;
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
                                                   "e7aa15e2-bd7a-4ced-ba46-813b5d8fd30b");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "39cd3cf4-8acc-4ae8-a516-6e9c1cea72e1");
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
                                                (Haz3lcore.Id.of_string
                                                   "bee8a36f-e527-42ee-8e38-503a5e01c199");
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
                                         "3e2a838d-0f76-4fad-9771-53547a81ede5");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ce6b1512-0d4a-412c-9a17-3dc134fcbae3");
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "d9f5af24-425b-4813-979e-10335e142f3a");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "97c62c72-152c-4fe9-b486-186880ddda97");
                                  label = [ ">" ];
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7a915289-da00-4fb5-9aa7-05fe4e8d123c");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "3bac0de5-1020-4b78-a1ff-95b52dbf935f");
                                  label = [ "2" ];
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
                                       "a68f620e-a9ce-46fc-b93b-f71be3a2871d");
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
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "128a0ed0-8c4e-44fa-81b4-808c99f2a993");
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
                                             "e8d40669-496d-41be-92a9-6a40f6e77685");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "48e724be-d32b-4dc2-be4a-64b5439d1c43");
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
                                                       "7729b073-7b57-4d3d-8d43-23b66fc1ff4e");
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
                                             "749b8cea-b730-4d05-844c-f6a51204617c");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "07a89e30-bb8d-4a5d-b8f7-0b919dbd9eed");
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
                                             "d4d4c818-b89d-4821-b686-96c3d93cc23a");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "bea81661-eb34-45da-bdf6-e6b24017826e");
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
