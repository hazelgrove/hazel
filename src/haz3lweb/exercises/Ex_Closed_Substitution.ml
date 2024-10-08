let prompt = Ex_Closed_Substitution_prompt.prompt

let exercise : Exercise.spec =
  {
    header =
      {
        title = "Closed Substitution";
        version = 1;
        module_name = "Ex_Closed_Substitution";
        prompt;
      };
    pos = Proof Prelude;
    model =
      Proof
        {
          prelude =
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
                                   "31347ee2-fed9-4a06-b029-93c090413a99");
                            shape = Convex;
                          };
                      ],
                      [] );
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
                                   "81a3a47c-4cb6-4b7c-975a-99b18190003c");
                            shape = Convex;
                          };
                      ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          trees =
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
                                               "a3c0826b-4dd0-44c6-8317-8d5b1902b198");
                                        label = [ "let"; "="; "in" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat; Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 17;
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
                                                         "c21426f3-eaad-4f79-9956-920edebf65d4");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "d83412fc-077d-4b31-a518-be75985d3b5b");
                                                  label = [ "incr" ];
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
                                                         "87da8e24-4b19-4e21-a084-f11fd26aafcc");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "deea7e89-07e9-4d10-8921-dd38d4d91f74");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a84ae0ce-e862-47d5-9c97-1e6b9c2a88bf");
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
                                                                   "66b3203e-399c-4dd4-9c94-61c051bb6175");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "bb7eb7c4-f502-4f86-b46f-9b0620ad5806");
                                                            label = [ "x" ];
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
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "6f6b134b-a561-4edc-9409-c94139a8f69f");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                    ];
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "e2772ef1-d7b5-4e1c-b7b6-34c87aa4e49e");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f1bb7e80-f9af-4df2-ba97-a87dc1b26518");
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
                                                         "cd88dcb1-198d-47d7-9644-aeec89991084");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "ae3465cc-7055-4bb0-9391-e59fbd7e5c35");
                                                  label = [ "+" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 6;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 6;
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
                                                         "604b0f90-8de2-4346-a139-268d1a91d65d");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "3b912307-16ec-402a-b228-fda3d18a0bd1");
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
                                                         "fa84e024-3f6a-400f-8056-0678af4e6f62");
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
                                               "1288ddd5-a751-40bd-b25c-9f229f0b3c7d");
                                        content = Whitespace " ";
                                      };
                                  ],
                                  [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ebfe86ab-5be0-44f0-812b-642efbd02e51");
                                        label = [ "incr" ];
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
                                               "b7bffab2-038f-4a55-9420-ce69c85049c1");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 2;
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
                                                         "2eddfac3-7e9b-4620-ace1-2874005749d8");
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
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "94fed024-4c98-4711-b743-f12953f3659d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ec2edb1c-a78b-4176-b057-d57ef1220436");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "607a6141-eda2-4659-bbdb-bac9812894eb");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f91fd96f-d510-4a36-9542-eea8535d2252");
                                        label = [ "6" ];
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
                              ancestors = [];
                            };
                          caret = Inner (0, 0);
                        };
                      rule = None;
                    },
                  [] );
            ];
        };
  }
