let exercise : DerivationTree.spec =
  {
    id = Haz3lcore.Id.mk ();
    title = "Task 5 of 7: Deriving";
    module_name = "t5";
    prompt = "Derive the following judgment.";
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
                             "fed76840-7b91-43eb-b2a5-da58941b8043");
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
                             "a47299f3-ce58-4682-a95a-80343af328f2");
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
                                         "ec0eeaae-e4d5-4045-98d5-b64c31203768");
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
                                  shards = [ 0; 1; 2 ];
                                  children =
                                    [
                                      [
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "89262d98-c634-4ac9-8112-7b86b805fd9a");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "40534e23-c5c6-435d-90f3-8efb2eed88db");
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
                                                             "24ce076b-8c7e-46e1-a689-8c45a59f87f0");
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
                                                             "e567650b-400c-44e1-9241-43f91b10e87e");
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
                                                             "0333f9de-26e7-4229-8917-21ff53716800");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "20ed45f3-0273-4833-9636-1ad28f189c67");
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
                                                   "310fca7b-eb8f-4337-b94c-cff056f91dcc");
                                            content = Whitespace " ";
                                          };
                                      ];
                                      [
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "b16a76c3-5031-414f-9784-ca8dd3f18bfb");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "c8dd768f-89d6-4adb-95e0-3a6f65b41856");
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
                                                             "697ca123-dc30-429d-a4e9-a568cae63bd1");
                                                      label = [ "2" ];
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
                                                             "ef2aa4d4-4113-4fd6-9865-90613312b809");
                                                      label = [ "," ];
                                                      mold =
                                                        {
                                                          out = Drv Exp;
                                                          in_ = [];
                                                          nibs =
                                                            ( {
                                                                shape =
                                                                  Concave 47;
                                                                sort = Drv Exp;
                                                              },
                                                              {
                                                                shape =
                                                                  Concave 47;
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
                                                             "c225471e-2654-4415-b194-4bcf258ed82b");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "9dc8ccc9-0cca-497b-976a-ca52b230cd82");
                                                      label = [ "2" ];
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
                                                   "100144e2-90a4-42c4-9669-476b669c3c66");
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
                                         "3764b17b-85a3-439a-9c9f-ad71874f63de");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "7466f7fe-1f36-4691-8177-7aec6ed68884");
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
                                         "5b5d499b-b628-4a93-89c0-aeeda44cf244");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "bef3aca1-1658-4e76-847f-14abfa6d51db");
                                  label = [ "<" ];
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
                                         "4ddd189e-d334-4011-b83d-f4a0b05196e1");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a1048fca-580f-4f68-9c17-6d94bd441203");
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
                                         "a51ca4d6-4c87-47e9-9f5c-7ca1165f8e53");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "e989a992-55bb-4eae-ad4f-8ca83bc80ebf");
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
                                         "756b1761-d341-456e-9603-bb76479e3d5a");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "aad756f0-0203-4494-a359-022df0dbe8ea");
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
                rule = None;
              },
            [] );
      ];
  }
