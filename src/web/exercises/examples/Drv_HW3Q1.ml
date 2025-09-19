let exercise : DerivationTree.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "2f0137e3-672e-47f8-8493-5a593e1959c3");
    title = "Question 1: Evaluation Derivation";
    module_name = "hw3q1";
    prompt =
      "Derive the following judgement as above, naming the rule or definition \
       applied at each step, or citing arithmetic for operations involving \
       mathematical numbers as above. Perform all substitutions explicitly as \
       in the example above. You may define sub-derivation abbreviations using \
       the letter D with a suitable subscript as in the example above. Do not \
       define any other abbreviations";
    prelude =
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
                             "0f530980-f1ee-408f-b1a6-aee0bdc0f760");
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
    corpus = ALF;
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
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "feeaa875-c7ad-4001-bb39-2f69e8f72295");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "3120d0a9-4725-465b-8460-f7ffa10248b5");
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
                                                   "fdf3abee-92e3-4069-bf1e-8d8c3881c5aa");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "0debb1ec-2216-4029-b261-3a3f9d36d0d2");
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
                                                (Haz3lcore.Id.of_string
                                                   "6df30f3e-540b-4c4d-8474-87fdeabdbaf6");
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
                                         "bbac3692-324e-418d-804c-2e1c03ef60e1");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ce46452e-f182-4816-a30f-58fbb2d17ed8");
                                  label = [ "a" ];
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
                                         "87e87708-0023-4bf2-9800-fbc2baf70402");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "9b51efab-8f18-46b4-9494-d2b52f11f567");
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
                            ],
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "6e238f04-bace-4d66-a023-61871e9001d0");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c97bff70-dffa-449f-8ebf-e0d8a7edefe6");
                                  label = [ "-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Concave 25; sort = Drv Exp }
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
                                         "686c3ec6-0830-4e08-92c6-0520c438d081");
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
                                         "f82bd049-65f3-458f-b27f-f50d26eba7f9");
                                  content = Whitespace " ";
                                };
                            ] );
                        ancestors =
                          [
                            ( {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "07170114-5eb0-4d93-ab53-0c9bb5124ef4");
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
                                                   "518843c6-05a5-4552-b9e0-40f7156280f3");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "475f2980-b06a-46c4-9400-684c0d862a3c");
                                            label = [ "isNat" ];
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
                                                   "507d1b35-8a0c-4e63-91fc-9c97aebab56f");
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
                                             "aead0e2c-f7d3-45d9-beb4-03cc171e8a18");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "b292658d-7d71-49dc-b889-a18e0f87b41f");
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
                                                       "3cde0a24-ecec-4a1c-a6ce-783b99ccce31");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "f766ee7b-5551-4970-bb29-8d54a84b981d");
                                                label = [ "isNat" ];
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
                                                       "4699ec25-b935-477b-8b41-2982a54d5cde");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "c36d81ac-9c18-40d8-b7ac-4ea09eb728cd");
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
                                                                 "7d86f575-b6b9-4752-88d4-cba6107fc59c");
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
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "3d3893bc-e718-4d0b-903d-f0cf6b8486a3");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1881f18a-683f-4d7b-87f9-16774b6c9c42");
                                                          label = [ "-" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 28;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 28;
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
                                                                 "c8f5a15f-f2ff-4cf7-b785-449d05a52d56");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "5071d903-ea95-4d10-a1e2-9b266aad2711");
                                                          label = [ "3" ];
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
                                                       "2e422ac9-cb01-4e56-92ee-ed5ab81a82ba");
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "6446b1be-f61f-408c-a728-79f44b18ba61");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "30475dc9-44a2-476e-aa76-70227dc94d81");
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
                                                       "5f8ba38b-6498-4ef0-82b0-88cc3c4c6333");
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
                                             "7d58aa2f-0510-4024-b196-90f1064f332d");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f311727e-6b50-4943-9d2c-c0079dcb4a83");
                                      label = [ "2" ];
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
                                             "4d94ea80-7277-4c13-9d5e-d5965f2c8f33");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "be6d7529-f487-44e1-a5d3-67494a320e55");
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
                                             "6a5888dd-1d9b-4da2-83e6-4441bddb598c");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d08431fa-8cfe-4d28-ba99-0cab62a48528");
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
