let exercise : DerivationTree.spec =
  {
    title = "Task 1 of 9: Transcription";
    version = 0;
    module_name = "t1";
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
              ( [],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "48f68a62-6d99-47ae-b65c-113a71529aea");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
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
                             "98ae3536-48b2-44bb-b3a6-607261587fe5");
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
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "2e56a1c7-9e40-4f0a-9e71-a9116d0eda34");
                                  label = [ "5" ];
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
                                         "cd880c6b-a835-491c-989b-c1e3f5fc0c57");
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
                                         "f32ee4e9-4063-4bc7-9480-c0d90b42fcfc");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "807c6f73-5144-4de5-bff4-52a64c2eb192");
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
                                       "ace79e34-7d45-4c45-865c-18910878a11e");
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
                                             "47feb1cd-5bff-42d3-b99d-39e0f006c515");
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [ Drv Exp; Drv Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              {
                                                shape = Concave 35;
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
                                                    (Haz3lcore.Id.of_string
                                                       "96309005-8e05-4801-8eb1-13aa5619f42d");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "5c9ec1f1-0407-4697-b382-fbf74dbb3813");
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
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "3b6ee485-f0f6-43a5-8010-32c335c32d26");
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "03ce9e42-f852-4fce-85fa-97ee6341fa1f");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "b56192d5-33cd-4e92-b8ba-f4885605cbc4");
                                                label = [ "L" ];
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
                                                       "0c8d49ab-e55d-44fc-b0d2-fa3c8e0404ab");
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
                                                                 "13f588db-e86f-4032-b52a-9700b8b0944d");
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
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "2517bf71-eceb-4232-8098-6a1f77ef047e");
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
                                             "fdc7139f-8e1a-4924-93cb-d2e7356680bc");
                                      content = Whitespace " ";
                                    };
                                ],
                                [
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "eb6ce178-593e-4f59-a32c-03e3b867183a");
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
                                             "096713dc-d16b-41fd-b9a0-577f0f96311f");
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
                                             "2ad04a35-b4c0-4a08-bbda-6ac9ccb19079");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5373051e-5d9d-479f-a126-cfbbc2fe0b14");
                                      label = [ "\\=/" ];
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
                                             "402cdce7-13e8-4990-bee3-4c92e84e4b83");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f9e99930-6020-4538-80b9-372d0e0b679c");
                                      label = [ "1" ];
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
