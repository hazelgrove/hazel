let exercise : DerivationTree.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "be23a46d-5ce1-4f08-b899-ed8984b20b2a");
    title = "Task 2 of 7: Debugging";
    module_name = "t2";
    prompt =
      "Try to fix the derivation until all the node turn marked correct (turn \
       green).";
    prelude =
      {
        selection = { focus = Left; content = []; mode = Normal };
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
                                         "842f9248-7088-48f7-be19-6aa2ed2dc946");
                                  label = [ "False" ];
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
                                         "958e7a1f-7bd8-4560-8b8c-99f32b49929f");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "4ca4d05c-f08c-4c49-ac2a-25a6192b36ec");
                                  label = [ "\\=/" ];
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
                                         "2ab0a436-3010-4df2-bc0e-db9efb5bb4db");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "952d1c2d-617e-4903-b201-a7b16e5b4003");
                                  label = [ "False" ];
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
                        ancestors = [];
                      };
                    caret = Outer;
                  };
                rule = Some E_Val;
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
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b86f357b-d143-4d54-9280-e3b7241dcdef");
                                        label = [ "val"; "end" ];
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
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "c828e256-976c-4185-9422-eb88eab49149");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "af379f66-346f-4fbf-a631-f0d62b75e122");
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
                                                         "094c7d5a-7373-4e48-88d9-80df1832324c");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                          ];
                                      };
                                  ],
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some V_False;
                    },
                  [] );
            ] );
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
                                         "47feb1cd-5bff-42d3-b99d-39e0f006c515");
                                  label = [ "if"; "then"; "else" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Exp; Drv Exp ];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Concave 35; sort = Drv Exp }
                                        );
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
                                                   "d631daf8-b4b4-42ac-a33a-0a8fe3e5026d");
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
                              Tile
                                {
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
                                  shards = [ 0; 1 ];
                                  children =
                                    [
                                      [
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
                                                   "cd880c6b-a835-491c-989b-c1e3f5fc0c57");
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
                                         "eb6ce178-593e-4f59-a32c-03e3b867183a");
                                  label = [ "." ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 22; sort = Drv Exp },
                                          { shape = Concave 22; sort = Drv Exp }
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
                                         "096713dc-d16b-41fd-b9a0-577f0f96311f");
                                  label = [ "snd" ];
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
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                            ],
                            [] );
                        ancestors = [];
                      };
                    caret = Outer;
                  };
                rule = Some E_If_F;
              },
            [
              Node (Abbr (Some 0), []);
              Node (Abbr (Some 0), []);
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
                                               "362bd93b-cc1a-4f75-b6d9-f824898bb614");
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
                                                         "244b1adb-f2d6-452e-a5b9-8131fcba8d72");
                                                  label = [ "5" ];
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
                                                         "988619f3-a270-48aa-a369-778f63d4af7d");
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
                                                         "a0de1d92-9ad5-4d8f-a4cf-fa10c3b2a6dd");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "df5ce17e-b9bf-4650-abe4-92f20ba9df58");
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
                                               "bca27127-33a0-46e1-9997-e21f9607efac");
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
                                               "f5b40bac-b6ef-4b37-b3e7-282ad8376d9a");
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
                                               "270366d4-36fc-4b1f-88d0-093d45005247");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "517f16fd-324b-4eaf-9213-d3f54f44e0e1");
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
                                               "fcdf86a3-cd85-411f-ba27-d1ded2fc4fb0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f365474f-91ff-459f-893d-d89454565895");
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
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some E_PrjR;
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "c4b6eb89-e3b6-46dd-93ee-7c2011a94a80");
                                              label = [ "5" ];
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ffb63540-ebe7-4c6a-8bc7-df878f5badae");
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
                                                     "53d21ba2-c063-48bd-a319-1465b2de792b");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "47d7d53e-7bf5-4187-bf4a-07df8b776d43");
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
                                        ] );
                                    ancestors =
                                      [
                                        ( {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "c9ccb61f-7ac7-46bb-b499-2cd4bbcca01e");
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
                                            shards = ([ 0 ], [ 1 ]);
                                            children = ([], []);
                                          },
                                          ( [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "c4118023-746c-4cce-b6d7-37b21764875a");
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
                                                                   "97d76029-d645-46f7-9847-8808301f8445");
                                                            label = [ "5" ];
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
                                                                   "9801f949-4df3-45ab-9027-0342a6b51953");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort =
                                                                        Drv Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "f51a222e-1029-4f53-bdee-295baae0ea6f");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "6e3897c5-1955-4d38-b5a3-52064239aa37");
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
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "fda956a6-7da3-4f6d-b615-f8055b681631");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "941933ee-b3f4-44e5-a0df-e8e0c7993b06");
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
                                                         "1f00d42c-415a-4add-a2cc-d656de9aa10e");
                                                  content = Whitespace " ";
                                                };
                                            ],
                                            [] ) );
                                      ];
                                  };
                                caret = Outer;
                              };
                            rule = Some E_Val;
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
                                      relatives =
                                        {
                                          siblings =
                                            ( [
                                                Secondary
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "a435e3d9-534b-488b-9a20-afcb93094adf");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9b2e86dd-0f58-466d-b280-67156f0ea89f");
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
                                                                     "f484682b-0173-44be-a60f-026b007e5ec3");
                                                              label = [ "5" ];
                                                              mold =
                                                                {
                                                                  out = Drv Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Drv
                                                                            Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Drv
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
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "19605e17-bd6a-4fc9-9632-d7d02231b30a");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Drv Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            47;
                                                                        sort =
                                                                          Drv
                                                                            Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            47;
                                                                        sort =
                                                                          Drv
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
                                                                     "a3dc1ce1-2cc8-496d-8c89-e54022b487c9");
                                                              content =
                                                                Whitespace " ";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "5a29fd4e-bde4-4483-91f7-1d611030f7c8");
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
                                                                          Drv
                                                                            Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Drv
                                                                            Exp;
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
                                                           "d8476517-9a68-4fc6-a01c-b8915c273315");
                                                    content = Whitespace " ";
                                                  };
                                              ] );
                                          ancestors =
                                            [
                                              ( {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "d033848e-1cc5-4601-a950-cdd157c4abc6");
                                                  label = [ "val"; "end" ];
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
                                                  shards = ([ 0 ], [ 1 ]);
                                                  children = ([], []);
                                                },
                                                ([], []) );
                                            ];
                                        };
                                      caret = Outer;
                                    };
                                  rule = Some V_Pair;
                                },
                              [] );
                        ] );
                  ] );
            ] );
      ];
  }
