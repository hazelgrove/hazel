let exercise : DerivationTree.spec =
  {
    title = "Task 1 of 7: Transcription";
    version = 0;
    module_name = "t1";
    prompt =
      "Transcribe the derivation tree into Hazel Deriver. No worry if the \
       derivation is not correct.";
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
                             "0f530980-f1ee-408f-b1a6-aee0bdc0f760");
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
                             "9681034c-5307-42b8-9987-aa60c950a44d");
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
                                         "8cf8d9cc-a387-40ee-8b8f-5a2bf1e71ca1");
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
                                                   "5daf48e5-4c58-498a-8e29-9f95ff6a0399");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "aacb79fe-0937-4152-8207-a9650974efc9");
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
                                                   "d2e3ec59-8767-4054-bdfc-79b303ca0ff7");
                                            content = Whitespace " ";
                                          };
                                      ];
                                      [
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "851d3049-7b8e-490c-848b-5713b6f7ad40");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "b815e459-9a90-42f0-a1c6-f30ae4e41519");
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
                                                   "467644a6-c206-4cb5-b1a3-98e826ca78c1");
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
                                         "9086ae21-d559-4da1-aaa3-a8d3d5f85409");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ca233059-ee78-4b0b-8db8-bb60875808f9");
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
                                                   "5a484d82-2e54-4e21-8e26-3d7409c18f64");
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
                                                   "1f8fefae-568c-45cd-a253-17f6a5ab1504");
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
                                                   "bd99ca15-b2a1-4231-a543-f7ffa1c70d4f");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "2188759b-310c-44fd-b76a-5eec0a5eef63");
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
                                         "18b87fab-187e-4aa0-8853-c7bf118f8184");
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
                                         "547311cb-cdbb-4407-9c2f-3ad50296baa6");
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
                                         "4dc4f614-815e-43e3-bbe4-341fbef7ff9d");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "22b57499-ea84-49d1-9166-ee5262a01e8a");
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
                                         "53e1889b-df66-4e70-8f74-70620b8d3e3e");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "b7933283-6522-4629-80b8-ae695fff7415");
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
            [] );
      ];
  }
